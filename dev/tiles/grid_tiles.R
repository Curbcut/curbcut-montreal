tileset_upload_grid <- function(all_scales, map_zoom_levels, max_zoom,
                                vars, prefix, username, access_token) {
  
  # Reset
  mapply(\(scale_name, scale_df) {
    name <- paste(prefix, scale_name, sep = "_")
    
    tileset_delete_tileset_source(
      id = name,
      username = username,
      access_token = access_token
    )
    
    tileset_delete_tileset(
      id = name,
      username = username,
      access_token = access_token
    )
  }, names(all_scales), all_scales, SIMPLIFY = FALSE)
  
  # DO THE SAME FOR AUTOZOOMS
  lapply(names(map_zoom_levels), \(x) {
    x <- gsub("mzl_", "", x)
    x <- paste(prefix, x, sep = "_")
    
    tileset_delete_tileset_source(
      id = x,
      username = username,
      access_token = access_token
    )
    
    tileset_delete_tileset(
      id = x,
      username = username,
      access_token = access_token
    )
  })
  
  # Tileset sources
  mapply(function(scale_name, scale_df) {
    scale_n <- paste(prefix, scale_name, sep = "_")
    df <- scale_df
    
    vars_col <- grepl(paste0(vars, collapse = "|"), names(df))
    vars_col[1] <- TRUE
    
    # Subset
    df <- df[, vars_col]
    if (scale_name == "grd250") df$ID_color <- df$ID
    
    # Add the delta column
    for (var in vars) {
      v <- paste0(var, "_delta")
      
      v_1 <- paste0(var, "_2015")
      v_2 <- paste0(var, "_2022")
      
      delta <- df[[v_2]] - df[[v_1]]
      
      df[[v]] <- 5
      df[[v]][delta == 1] <- 4
      df[[v]][delta == 0] <- 3
      df[[v]][delta == -1] <- 2
      df[[v]][delta < -1] <- 1
      df[[v]][is.na(delta)] <- NA
      
      # As characters
      df[[v]] <- as.character(df[[v]])
      df[[v_1]] <- as.character(df[[v_1]])
      df[[v_2]] <- as.character(df[[v_2]])
      
    }
    
    df <- sf::st_transform(df, 4326)
    
    tileset_upload_tile_source_large(
      id = scale_n,
      df = df,
      username = username,
      access_token = access_token
    )
  }, names(all_scales), all_scales)
  
  # Create recipe, create tileset and publish
  maxzooms <- tibble::tibble(scale = names(max_zoom),
                             maxzoom = unlist(max_zoom))
  
  all_recipes <-
    mapply(\(scale_name, scale_df) {
        
      source_names <- paste(prefix, scale_name, sep = "_")
      sources <- paste0("mapbox://tileset-source/", username, "/", source_names)
      names(sources) <- source_names
      minzooms <- 3
      names(minzooms) <- source_names
        
      default_maxzoom <- maxzooms$maxzoom[maxzooms$scale == scale_name]
      new_maxzoom <- max(default_maxzoom, 14)
      maxzooms_ <- new_maxzoom
      names(maxzooms_) <- source_names
      layer_sizes <- 2500
      names(layer_sizes) <- source_names
        
      recipe <- tileset_create_recipe(
        layer_names = source_names,
        source = sources,
        minzoom = minzooms,
        maxzoom = maxzooms_,
        recipe_name = source_names,
        layer_sizes = layer_sizes
      )
      
      tileset_create_tileset(source_names,
                             recipe = recipe,
                             username = username,
                             access_token = access_token
      )
      
      tileset_publish_tileset(source_names,
                              username = username,
                              access_token = access_token
      )
    }, names(all_scales), all_scales, SIMPLIFY = FALSE)
  
  # Function to calculate on autozoom when the scale starts and when it ends
  calculate_zoom_levels <- function(zoom_levels) {
    # Initialize the output tibble
    result <- tibble::tibble(
      scale = character(), min_zoom = integer(),
      max_zoom = integer()
    )
    
    # Loop through the named numeric vector
    for (i in seq_along(zoom_levels)) {
      scale_name <- names(zoom_levels)[i]
      zoom_value <- zoom_levels[i]
      
      # Calculate min zoom
      min_zoom <- ifelse(i == 1, 0, result$max_zoom[i - 1] + 1)
      
      # Calculate max zoom
      max_zoom <-
        if (length(zoom_levels) == 1) {
          10
        } else {
          ifelse(i == length(zoom_levels), zoom_value, zoom_levels[i + 1] - 1)
        }
      
      
      # Add the min and max zoom to the result tibble
      result <-
        tibble::add_row(result,
                        scale = scale_name, min_zoom = min_zoom,
                        max_zoom = max_zoom
        )
    }
    
    return(result)
  }

  auto_zoom_recipes <-
    mapply(\(mzl_name, zoom_levels) {
      
      az_name <- gsub("^mzl", prefix, mzl_name)
      scale_names <- paste(prefix, names(zoom_levels), sep = "_")
      
      
      sources <- stats::setNames(paste0(
        "mapbox://tileset-source/", username, "/",
        scale_names
      ), scale_names)
      
      zooms <- calculate_zoom_levels(zoom_levels)
      minzooms <- zooms$min_zoom
      maxzooms <- zooms$max_zoom
      maxzooms[length(maxzooms)] <- 16
      names(minzooms) <- scale_names
      names(maxzooms) <- scale_names
      
      layer_sizes <-
        stats::setNames(rep(2500, length(scale_names)), scale_names)
      
      recipe <-
        tileset_create_recipe(
          layer_names = scale_names,
          source = sources,
          minzoom = minzooms,
          maxzoom = maxzooms,
          layer_size = layer_sizes,
          recipe_name = az_name
        )

      # Reset
      tileset_delete_tileset_source(
        id = az_name,
        username = username,
        access_token = access_token
      )
      tileset_delete_tileset(
        id = az_name,
        username = username,
        access_token = access_token
      )
      # Give some time so the deletion is completed
      Sys.sleep(5)

      # New tileset
      tileset_create_tileset(az_name,
                             recipe = recipe,
                             username = username,
                             access_token = access_token
      )
      tileset_publish_tileset(az_name,
                              username = username,
                              access_token = access_token
      )
    }, names(map_zoom_levels), map_zoom_levels, SIMPLIFY = FALSE)

  
  return(invisible(NULL))
}