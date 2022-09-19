### FUNCTION TO INTERPOLATE AND ASSIGN TO EXISTING TABLES ######################

interpolate_scales <- function(data, base_scale, all_tables, 
                               add_to_grid = FALSE,
                               weight_by = "households") {
  
  tables <- if (add_to_grid) all_tables else all_tables[all_tables != "grid"]
  
  if ("sf" %in% class(data)) geom_type <- unique(st_geometry_type(data))
  
  out <- 
    ## Manage the census cases first -------------------------------------------
  if (base_scale %in% c("DA", "CT", "borough")) {
    
    identifier <- case_when(base_scale == "DA" ~ "DAUID",
                            base_scale == "CT" ~ "CTUID",
                            base_scale == "borough" ~ "CSDUID",
                            TRUE ~ "ID")
    
    base <- 
      get(base_scale) |> 
      left_join(data, by = c("ID" = identifier)) |>
      st_transform(32618) |> 
      mutate(area = st_area(geometry)) |>
      st_set_agr("constant") |> 
      select(any_of(c("ID", "CTUID", "CSDUID", "area", "households", 
                      "population", names(data))))
    
    # Remove any IDs
    data <- data[, names(data)[!names(data) |> str_detect("ID$")]]
    
    construct_for <- 
      if (base_scale == "DA") c("DA", "CT", "borough") else
        if (base_scale == "DA") c("CT", "borough") else
          if (base_scale == "CSD") c("borough")
    
    z <- 
      map(set_names(construct_for), function(scale) {
        re_identifier <- case_when(scale == "DA" ~ "DAUID",
                                   scale == "CT" ~ "CTUID",
                                   scale == "borough" ~ "CSDUID")
        
        if (scale == base_scale) 
          return(base |> 
                   st_drop_geometry() |> 
                   select(any_of(c("ID", names(data)))))
        
        base |> 
          st_drop_geometry() |> 
          group_by(grouped = base[[re_identifier]]) |> 
          (\(x) summarize(x, across(which(names(x) %in% names(data)),
                                    ~weighted.mean(.x, .data[[weight_by]], na.rm = TRUE))))() |> 
          right_join(get(scale), by = c("grouped" = "ID")) |> 
          rename(ID = grouped) |> 
          select(all_of(c("ID", names(data))))
        
      })
    # Continue with the other tables!
    next_tables <- tables[!tables %in% names(z)]
    
    c(z,
      map(set_names(next_tables), function(scale) {
        base_base <- base |> select(all_of(c(names(data), "area")))
        
        get(scale) |>
          select(ID, households, population) |>
          st_transform(32618) |>
          st_set_agr("constant") |>
          st_intersection(base_base) |>
          mutate(area_prop = st_area(geometry) / area) |>
          (\(x) mutate(x, across(which(names(x) %in% names(data)),
                                 ~{.x * units::drop_units(area_prop)})))() |> 
          st_drop_geometry() |>
          group_by(ID) |> 
          (\(x) summarize(x, across(which(names(x)[names(x) != "ID"] %in% names(data)),
                                    ~weighted.mean(.x, .data[[weight_by]], na.rm = TRUE))))()
      })
    )
  } else if (str_detect(geom_type, "POLYGON")) {
    
    map(set_names(tables), function(scale) {
      
      if (base_scale == scale) return(st_drop_geometry(data))
      
      data_int <- get(scale)
      
      data_int <- 
        data_int |> 
        select(ID) |> 
        st_transform(32618) |> 
        mutate(area_x = units::drop_units(st_area(geometry))) |> 
        st_set_agr("constant")
      
      var_list <- names(data)[!names(data) %in% c("ID", "geometry")]
      
      data |> 
        select(dat_ID = ID) |> 
        st_transform(32618) |> 
        st_set_agr("constant") |> 
        st_intersection(data_int) |> 
        st_drop_geometry() |> 
        select(dat_ID, ID, area_x) |>
        inner_join(rename(data, dat_ID = ID) , by = "dat_ID") |>
        mutate(area_int = units::drop_units(st_area(geometry))) |> 
        group_by(ID) |>
        (\(x) summarize(x,
          across(which(names(x)[!names(x) %in% "ID"] %in% var_list), ~{
            if (sum(area_int[!is.na(.x)]) >= 0.5 * max(area_x)) {
              weighted.mean(.x, area_int, na.rm = TRUE)
            } else NA_real_}), .groups = "drop"))() |> 
        right_join(data_int, by = "ID") |> 
        select(all_of(c("ID", var_list)))
      
    })
    
  }
  
  return(out)
  
}


# Assign to all tables ----------------------------------------------------

assign_tables <- function(module_tables) {
  
  if ("tables_list" %in% names(module_tables)) 
    module_tables <- module_tables$tables_list
  
  walk(names(module_tables), function(scale) {
    
    if ("geometry" %in% module_tables[[scale]])
      stop("Drop the geometry!")
    
    out <- 
      get(scale) |> 
      left_join(module_tables[[scale]], by = "ID") |> 
      relocate(any_of(c("centroid", "buffer", "building", "geometry")), 
               .after = last_col())
    
    assign(scale, value = out, envir = .GlobalEnv)
  })
  
}
