### FUNCTION TO ADD COLUMNS IF POLYGON IS PART OF A GREATER GEOMETRY ###########

is_in_geometry <- function(all_tables, add_to = c("DA", "CT"), crs) {
  
  add_from <- all_tables[!all_tables %in% c("DA", "CT", "grid")]
  
  walk(add_from, function(from) {
    walk(add_to, function(to) {

      from_df <- get(from) |> 
        st_transform(crs) |> 
        st_union()
      to_df <- get(to) |> 
        st_transform(crs)
      
      part_of_from <-
        to_df |>
        mutate(previous_area = units::drop_units(st_area(geometry))) |>
        st_intersection(from_df) |>
        st_set_agr("constant") |> 
        mutate(new_area = units::drop_units(st_area(geometry))) |>
        filter({new_area / previous_area} > 0.33) |> 
        pull(ID)
      
      out <- to_df |> 
        mutate(new_col = if_else(ID %in% part_of_from,
                                 TRUE, FALSE)) |> 
        relocate(new_col, .after = "CSDUID")
      
      if (from == "borough") from <- "CMA"
      names(out) <- str_replace(names(out), "^new_col$", from)
      
      out <- out |> 
        st_transform(4326)
      
      assign(to, out, envir = .GlobalEnv)
      
    })
  })
}