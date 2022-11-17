### FUNCTION TO RECREATE CT, DA, building FOR ALL GEOMETRIES ###################

is_in_geometry <- function(all_tables, crs, update_name_2_for) {
  
  walk(names(all_tables), function(geo) {
    
    new_tables <- 
      unlist(all_tables[names(all_tables) == geo], use.names = FALSE)
    
    geo_shp <- paste0(geo, "_shp")
    
    walk(new_tables, function(scale) {
      
      from_df <- 
        get(geo_shp) |> 
        st_transform(crs) |> 
        st_union()
      to_df <- 
        if (scale == "building") {
          get("DA") |> 
            st_transform(crs)            
        } else {
          get(scale) |> 
            st_transform(crs)
        }
      
      part_of_from <- 
        to_df |>
        st_transform(crs) |> 
        st_set_agr("constant") |> 
        st_point_on_surface() |> 
        st_filter(st_transform(from_df, crs)) |> 
        pull(ID)
      
      df_name <- paste(geo, scale, sep = "_")
      
      out <- if (scale == "building") {
        building[building$DAUID %in% part_of_from, ]
      } else {
        to_df[to_df$ID %in% part_of_from, ]
      }
      
      if (geo %in% update_name_2_for) {
        # Add new geo_ID
        geo_for_ID <- 
          out |> 
          st_transform(crs) |> 
          st_set_agr("constant") |> 
          st_point_on_surface() |> 
          st_intersection(get(if (exists(geo)) geo else all_tables[[geo]][[1]]) |> 
                            st_transform(crs) |> 
                            st_set_agr("constant") |>
                            dplyr::select(geo_ID = ID)) |> 
          select(ID, geo_ID) |> 
          st_drop_geometry()
        
        out <- 
          out |> 
          left_join(geo_for_ID, by = "ID") |> 
            (\(x) if ("CSDUID" %in% names(x)) {
              relocate(x, geo_ID, .after = CSDUID)
            } else {
              relocate(x, geo_ID, .after = name_2)
            })()
        
        # Update name_2
        if (scale != all_tables[[geo]][[1]]) {
          out <- 
            out |> 
            select(-name_2) |> 
            left_join({
              get(if (exists(geo)) geo else all_tables[[geo]][[1]]) |> 
                st_drop_geometry() |> 
                select(geo_ID = ID, name_2 = name)},
              by = "geo_ID") |> 
            relocate(name_2, .after = name)          
        }
        
      } else {
        out <- 
          out |> 
          (\(x) if ("CSDUID" %in% names(x)) {
            mutate(x, geo_ID = CSDUID)
          } else {
            mutate(x, geo_ID = ID)
          })() |> 
          relocate(geo_ID, .after = CSDUID)
      }
      
      assign(df_name, out, envir = .GlobalEnv)
    })
    
  })
  
}
