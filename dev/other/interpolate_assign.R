### FUNCTION TO INTERPOLATE AND ASSIGN TO EXISTING TABLES ######################

interpolate_scales <- function(data, base_scale, all_tables, 
                               weight_by = "households", crs) {

  if ("sf" %in% class(data)) geom_type <- unique(st_geometry_type(data))
  
  # Only interpolate for bigger geometries than the base one
  construct_for <- 
    map(all_tables, function(scales) {
      scales[seq_len(which(scales == base_scale))]
    })
  
  out <- 
    ## Manage the census cases first -------------------------------------------
  if (base_scale %in% c("DB", "DA", "CT")) {
    
    imap(construct_for, function(scales, geo) {
      
      geo_base_scale <- paste(geo, base_scale, sep = "_")
      
      base <- 
        get(geo_base_scale) |> 
        left_join(data, by = c("ID" = "ID")) |>
        st_transform(crs) |> 
        mutate(area = st_area(geometry)) |>
        st_set_agr("constant") |> 
        select(any_of(c("ID", "DAUID", "CTUID", "CSDUID", "geo_ID", "area", 
                        "households", "population", names(data))))
      
      # Remove any IDs
      data <- data[, names(data)[!names(data) |> str_detect("ID$")]]
      
      z <- 
        map(set_names(scales), function(scale) {
          geo_scale <- paste(geo, scale, sep = "_")
          
          re_identifier <- case_when(scale == "DB" ~ "DBUID",
                                     scale == "DA" ~ "DAUID",
                                     scale == "CT" ~ "CTUID",
                                     scale == "borough" ~ "CSDUID",
                                     TRUE ~ "geo_ID")
          
          if (scale == base_scale) 
            return(base |> 
                     filter(ID %in% get(geo_base_scale)$ID) |>
                     st_drop_geometry() |> 
                     select(any_of(c("ID", names(data)))))
          
          base |> 
            st_drop_geometry() |> 
            group_by(grouped = base[[re_identifier]]) |> 
            (\(x) summarize(x, across(which(names(x) %in% names(data)),
                                      ~weighted.mean(.x, .data[[weight_by]], 
                                                     na.rm = TRUE))))() |> 
            right_join(get(geo_scale), by = c("grouped" = "ID")) |> 
            rename(ID = grouped) |> 
            select(all_of(c("ID", names(data))))          
        })
      
      # Continue with the other tables!
      next_tables <- scales[!scales %in% names(z)]
      
      c(z,
        map(set_names(next_tables), function(scale) {
          base_base <- base |> select(all_of(c(names(data), "area")))
          
          get(scale) |>
            select(ID, households, population) |>
            st_transform(crs) |>
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
    })
  } else if (str_detect(geom_type, "POLYGON")) {
    
    imap(construct_for, function(scales, geo) {
      map(set_names(scales), function(scale) {
        
        geo_scale <- paste(geo, scale, sep = "_")
        
        if (base_scale == scale) return(st_drop_geometry(data) |> 
                                          filter(ID %in% get(geo_scale)$ID))
        
        data_int <- 
          get(geo_scale) |> 
          select(ID) |> 
          st_transform(crs) |> 
          mutate(area_x = units::drop_units(st_area(geometry))) |> 
          st_set_agr("constant")
        
        var_list <- names(data)[!names(data) %in% c("ID", "geometry")]
        
        data |> 
          select(dat_ID = ID) |> 
          st_transform(crs) |> 
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
    })
    
  }
  
  out_final <- list()
  for (a in seq_len(length(out))) {
    geo <- names(out)[[a]]
    for (b in seq_len(length(out[[geo]]))) {
      scale <- names(out[[geo]])[[b]]
      df_name <- paste(geo, scale, sep = "_")
      df <- get(df_name)
      
      out_final[[df_name]] <- out[[geo]][[scale]]
    }
  }
  
  
  return(out_final)
  
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
