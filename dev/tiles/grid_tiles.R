tileset_upload_grid <- function(region, all_scales, map_zoom_levels, max_zoom,
                                  vars, prefix, username, access_token) {
  tn <- function(geo, scale_name) paste(prefix, geo, scale_name, sep = "_")
  
  # Subset the right region from everything
  map_zoom_levels <- map_zoom_levels[region]
  all_scales <- all_scales[region]
  
  # All tables
  all_tables <- reconstruct_all_tables(all_scales)
  
  # Reset
  mapply(\(geo, scales) {
    sapply(scales, \(scale) {
      tileset_delete_tileset_source(
        id = tn(geo, scale),
        username = username,
        access_token = access_token
      )
      
      tileset_delete_tileset(
        id = tn(geo, scale),
        username = username,
        access_token = access_token
      )
    })
  }, names(all_tables), all_tables, SIMPLIFY = FALSE)
  
  # Tileset sources
  mapply(function(scales, geo) {
    lapply(scales, function(scale) {
      geo_scale <- tn(geo, scale)
      df <- all_scales[[geo]][[scale]]
      
      df <- df[!grepl("_q3|_q5", names(df))]
      vars_col <- grepl(paste0(vars, collapse = "|"), names(df))
      vars_col[1] <- TRUE

      # Subset
      df <- df[, vars_col]
      if (scale == "grid250") df$ID_color <- df$ID
      
      # Add the delta column
      for (var in vars) {
        v <- paste0(var, "_delta")
        
        v_1 <- paste0(var, "_2015")
        v_2 <- paste0(var, "_2022")
        
        delta <- (df[[v_2]] - df[[v_1]]) / df[[v_1]]
        
        df[[v]] <- 5
        df[[v]][delta < 0.1] <- 4
        df[[v]][delta < 0.02] <- 3
        df[[v]][delta < -0.02] <- 2
        df[[v]][delta < -0.1] <- 1
        df[[v]][is.na(delta)] <- NA
        
        # As characters
        df[[v]] <- as.character(df[[v]])
        df[[v_1]] <- as.character(df[[v_1]])
        df[[v_2]] <- as.character(df[[v_2]])
        
      }
      
      df <- sf::st_transform(df, 4326)
      
      tileset_upload_tile_source_large(
        id = geo_scale,
        df = df,
        username = username,
        access_token = access_token
      )
      
    })
  }, all_tables, names(all_tables))
  
  # TKTK CHECK IF ALL SCALES HAVE BEEN CORRECTLY UPLOADED
  
  # Create recipe, create tileset and publish
  maxzooms <- tibble::tibble(scale = names(max_zoom),
                             maxzoom = unlist(max_zoom))
  
  all_recipes <-
    mapply(\(scales, geo) {
      mapply(function(scale, level) {
        name <- tn(geo, scale)
        
        recipe <-
          tileset_create_recipe(
            layer_names = name,
            source = paste0("mapbox://tileset-source/", username, "/", name),
            minzoom = 3,
            maxzoom = maxzooms$maxzoom[maxzooms$scale == scale],
            layer_size = 2500,
            recipe_name = name
          )
        
        tileset_create_tileset(name,
                               recipe = recipe,
                               username = username,
                               access_token = access_token
        )
        
        tileset_publish_tileset(name,
                                username = username,
                                access_token = access_token
        )
      }, scales, seq_along(scales), SIMPLIFY = FALSE)
    }, all_tables, names(all_tables), SIMPLIFY = FALSE)
  
  # Verify if all scales have been published
  t_list <- tileset_list_tilesets(username = username, access_token = access_token)
  mapply(\(scales, geo) {
    mapply(function(scale, level) {
      scale_for_dict <- if (level == 1) "first_level" else scale
      name <- tn(geo, scale)
      if (!name %in% t_list$id)
        warning("Tileset `", name, "` was not succesfully published.")
    }, scales, seq_along(scales), SIMPLIFY = FALSE)
  }, all_tables, names(all_tables), SIMPLIFY = FALSE)
  
  # Function to calculate on autozoom when the scale starts and when it ends
  calculate_zoom_levels <- function(zoom_levels) {
    
    # Initialize the output tibble
    result <- tibble::tibble(scale = character(), min_zoom = integer(),
                             max_zoom = integer())
    
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
          ifelse(i == length(zoom_levels), zoom_value + 0.5, zoom_levels[i + 1] - 0.5)
        }
      
      
      # Add the min and max zoom to the result tibble
      result <-
        tibble::add_row(result, scale = scale_name, min_zoom = min_zoom,
                        max_zoom = max_zoom)
    }
    
    return(result)
  }
  
  
  auto_zoom_recipes <-
    mapply(\(geo, zoom_levels) {
      mapply(\(mzl_name, mzl) {
        suffix <- gsub(paste0(".*_", geo), "", mzl_name)
        suffix <- if (grepl("_", suffix)) suffix else ""
        name <- tn(geo, scale_name = paste0("auto_zoom", suffix))
        scale_names <- tn(geo, names(mzl))
        
        sources <- stats::setNames(paste0(
          "mapbox://tileset-source/", username, "/",
          scale_names
        ), scale_names)
        
        zooms <- calculate_zoom_levels(mzl)
        minzooms <- zooms$min_zoom
        maxzooms <- zooms$max_zoom
        names(minzooms) <- scale_names
        names(maxzooms) <- scale_names
        
        layer_sizes <-
          stats::setNames(rep(NA, length(scale_names)), scale_names)
        
        recipe <-
          tileset_create_recipe(
            layer_names = scale_names,
            source = sources,
            minzoom = minzooms,
            maxzoom = maxzooms,
            layer_size = layer_sizes,
            recipe_name = name
          )
        
        # Reset
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
        # Give some time so the deletion is completed
        Sys.sleep(5)
        
        # New tileset
        tileset_create_tileset(name,
                               recipe = recipe,
                               username = username,
                               access_token = access_token
        )
        tileset_publish_tileset(name,
                                username = username,
                                access_token = access_token
        )
      }, names(zoom_levels), zoom_levels, SIMPLIFY = FALSE)
    }, names(map_zoom_levels), map_zoom_levels, SIMPLIFY = FALSE)
  
  # Verify if all auto zooms have been published
  t_list <- tileset_list_tilesets(username = username, access_token = access_token)
  mapply(\(geo, zoom_levels) {
    mapply(\(mzl_name, mzl) {
      suffix <- gsub(paste0(".*_", geo), "", mzl_name)
      suffix <- if (grepl("_", suffix)) suffix else ""
      name <- tn(geo, scale_name = paste0("auto_zoom", suffix))
      if (!name %in% t_list$id)
        warning("Tileset `", name, "` was not succesfully published.")
    }, names(zoom_levels), zoom_levels, SIMPLIFY = FALSE)
  }, names(map_zoom_levels), map_zoom_levels, SIMPLIFY = FALSE)
  
  return(invisible(NULL))
}