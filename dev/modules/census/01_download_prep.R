#### CENSUS DOWNLOAD FUNCTIONS ##################################


# Get census function update ----------------------------------------------

# Sometimes, cancensus is not able to access a particular data in the cache.
# If it does happen, just re-download it!
get_census_timeout <- function(...) {
  tryCatch(R.utils::withTimeout(
    cancensus::get_census(..., use_cache = TRUE, quiet = TRUE), timeout = 2), 
    error = function(e) {cancensus::get_census(..., use_cache = FALSE,
                                               quiet = FALSE)})
}

# Get empty geometries ----------------------------------------------------

get_empty_geometries <- function(scales, years, region = "24", crs = 32618) {
  
  get_c <- function(census_dataset, re = list(PR = region), sc) {
    out <- 
      get_census_timeout(
        dataset = census_dataset,
        regions = re,
        level = sc,
        geo_format = "sf") |> 
      select(GeoUID, geometry) |> 
      st_transform(crs) |> 
      mutate(area = st_area(geometry), .before = geometry) |> 
      st_set_agr("constant")
    
    GeoUIDs <- 
      out |>
      st_transform(crs) |> 
      st_point_on_surface() |> 
      st_filter(st_transform(master_polygon, crs)) |> 
      pull(GeoUID)
    
    out |> filter(GeoUID %in% GeoUIDs)
  }
  
  out <- 
    map(set_names(scales), function(scale) {
      map(set_names(years), function(year) {
        
        census_dataset <- paste0("CA", sub("20", "", year))
        
        # CTs must be filled with CSDs
        if (scale == "CT") return({
          CT <- get_c(census_dataset = census_dataset, sc = "CT")
          
          csds <- 
            get_c(census_dataset = census_dataset, sc = "CSD") |> 
            st_transform(crs)
          csds_in_CT <- 
            csds |> 
            st_point_on_surface() |> 
            st_filter(CT, crs) |> 
            pull(GeoUID)
          
          filling_CTs <- csds[!csds$GeoUID %in% csds_in_CT, ] |> 
            st_transform(crs)
          
          rbind(CT, filling_CTs)
        })
        
        if (scale == "DA") return({
          CDs <- get_c(census_dataset = census_dataset, sc = "CD")$GeoUID
          get_c(census_dataset = census_dataset, 
                re = list(CD = CDs), 
                sc = scale)
        })
        
        return(get_c(census_dataset = census_dataset, sc = scale))
      })
    })
  
  out
}


# Download variables ------------------------------------------------------

get_census_vectors <- function(census_vec, geoms, scales, years, 
                               parent_vectors = NULL) {
  
  out <- 
  map2(set_names(scales), geoms, function(scale, geom) {
    map2(set_names(years), geom, function(year, df) {
      
      census_dataset <- paste0("CA", sub("20", "", year))
      
      # Get named versions of vectors
      vec_named <- 
        census_vec |> 
        pull(all_of(paste0("vec_", year))) |> 
        set_names(census_vec$var_code) |> 
        unlist() |> 
        na.omit()
      
      # Get original vectors. Different for CT, as there are not always CTs
      # in all geometries and they get filled with CSDs.
      vec_retrieved <- 
        if (scale == "CT") {
          # First retrieveal, for CTs
          sep <- 
            df |> 
            mutate(CT_id = if_else(str_detect(GeoUID, "\\.\\d{2}$"), 
                                   TRUE, FALSE))
          
          list_regions <- list(df$GeoUID[sep$CT_id])
          names(list_regions) <- scale
          
          vec_retrieved_CT <- 
            get_census_timeout(
              dataset = census_dataset,
              regions = list_regions,
              level = "CT",
              vectors = vec_named,
              geo_format = NA) |> 
            select(GeoUID, starts_with(census_vec$var_code))
          # Second retrieval, for CSDs
          list_regions <- list(df$GeoUID[!sep$CT_id])
          names(list_regions) <- scale
          
          vec_retrieved_CSD <- 
            get_census_timeout(
              dataset = census_dataset,
              regions = list_regions,
              level = "CSD",
              vectors = vec_named,
              geo_format = NA) |> 
            select(GeoUID, starts_with(census_vec$var_code))
          
          # Bind!
          rbind(vec_retrieved_CT, vec_retrieved_CSD)
          
        } else {
          list_regions <- list(df$GeoUID)
          names(list_regions) <- scale
          
          get_census_timeout(
            dataset = census_dataset,
            regions = list_regions,
            level = scale,
            vectors = vec_named,
            geo_format = NA) |> 
            select(GeoUID, starts_with(census_vec$var_code))
        }
      
      # Add up vectors that were retrieved through the same var_code
      vec_to_sum <- 
        vec_named |> 
        names() |> 
        str_remove("\\d*$") |> 
        table()
      
      vec_to_sum <- names(vec_to_sum[vec_to_sum > 1])
      
      if (length(vec_to_sum > 0)) {
        vec_to_sum <- vec_named[str_detect(names(vec_named), 
                                           paste0(vec_to_sum, collapse = "|"))]
        
        agg_vec_to_sum <-
          (cancensus::list_census_vectors(census_dataset) |>
             filter(vector %in% vec_to_sum, aggregation != "Additive"))$vector
        
        # Throw error if they aren't additive
        if (length(agg_vec_to_sum) != 0) {
          stop(paste0(
            "Vector `", agg_vec_to_sum, "` contains multiple variables, but it ", 
            "isn't registered as `additive` in cancensus."))
        }
        
        # Sum them up
        if (ncol(vec_retrieved) > 1) {
          vec_retrieved <-
            vec_retrieved |>
            pivot_longer(-GeoUID) |>
            mutate(name = ifelse(str_detect(name, "\\d$"), 
                                 str_remove(name, "\\d*$"), name)) |>
            group_by(GeoUID, name) |>
            summarize(value = sum(value), .groups = "drop") |>
            pivot_wider(GeoUID) |>
            ungroup()
        }
      }
      
      # Some vectors share denominators, so use map to get them multiple times
      parent_vec <-
        cancensus::list_census_vectors(census_dataset) |>
        filter(vector %in% vec_named) |> 
        arrange(match(vector, vec_named)) |> 
        # For pct, use parent_vector to weight aggregation averages
        mutate(parent_vector = if_else(is.na(parent_vector), 
                                       str_extract(aggregation, "v_.*$"),
                                       parent_vector)) |>
        pull(parent_vector)
      
      if (length(parent_vec) > 0) {
        parent_vec <- 
          parent_vec |> 
          set_names(paste0(str_remove(names(vec_named), "\\d*$"), "_parent"))
      }
      
      # Replace here the parent_vec with the parent_vectors
      if (!is.null(parent_vectors)) {
        parent_vectors_which <- str_which(parent_vectors, census_dataset)
        parent_vectors <- parent_vectors[parent_vectors_which]
        if (length(parent_vectors) > 0) {
          # renaming fed parent_vectors with _parent
          names(parent_vectors) <- paste0(str_remove(names(parent_vectors), "\\d*$"), "_parent")
          parent_vec <- map(names(parent_vec), ~{
            if (.x %in% names(parent_vectors)) {
              parent_vectors[.x == names(parent_vectors)]
            } else set_names(parent_vec[.x], .x)
          }) |> unique() |> unlist()
          parent_vec <- with(stack(parent_vec), split(values, ind)) |> unlist()
        }}
      
      # Parents should be retrieved only once
      parent_vec <-
        parent_vec |> 
        names() |> 
        unique() |> 
        map(~{
          value <- unique(parent_vec[names(parent_vec) == .x])
          name <- unique(names(parent_vec)[names(parent_vec) == .x])
          if (length(value) > 1) {
            stop(paste0(
              "Parent vectors of `", name, "` in ", year, " aren't unique. A var_code ",
              "sharing multiple numerators should have a unique parent."))}
          set_names(value, name)}) |> 
        unlist()
      
      # Check for non-additive parent vectors
      non_add_parent_vec <- 
        cancensus::list_census_vectors(census_dataset) |>
        filter(vector %in% parent_vec) |> 
        filter(aggregation != "Additive")
      
      if (nrow(non_add_parent_vec) > 0) {
        stop(paste0("Non-additive parent vector: ",
                    parent_vec[parent_vec %in% non_add_parent_vec]))
      }
      
      # Retrieve the values of all parent vectors
      parent_vec_values <- map(names(parent_vec), function(v) {

        vec <- set_names(parent_vec[v], v)
        
        retrieved_parent <- 
          if (scale == "CT") {
            # First retrieveal, for CTs
            sep <- 
              df |> 
              mutate(CT_id = if_else(str_detect(GeoUID, "\\.\\d{2}$"), 
                                     TRUE, FALSE))
            
            list_regions <- list(df$GeoUID[sep$CT_id])
            names(list_regions) <- scale
            
            vec_retrieved_CT <- 
              get_census_timeout(
                dataset = census_dataset,
                regions = list_regions,
                level = "CT",
                vectors = vec[!is.na(vec)],
                geo_format = NA) |> 
              select(GeoUID, any_of(v)) |> 
              filter(GeoUID %in% df$GeoUID)
            # Second retrieval, for CSDs
            list_regions <- list(df$GeoUID[!sep$CT_id])
            names(list_regions) <- scale
            
            vec_retrieved_CSD <- 
              get_census_timeout(
                dataset = census_dataset,
                regions = list_regions,
                level = "CSD",
                vectors = vec[!is.na(vec)],
                geo_format = NA) |> 
              select(GeoUID, any_of(v)) |> 
              filter(GeoUID %in% df$GeoUID)
            
            # Bind!
            rbind(vec_retrieved_CT, vec_retrieved_CSD)
            
          } else {
            list_regions <- list(df$GeoUID)
            names(list_regions) <- scale
            
            get_census_timeout(
              dataset = census_dataset,
              regions = list_regions,
              level = scale,
              vectors = vec[!is.na(vec)],
              geo_format = NA) |> 
              select(GeoUID, any_of(v)) |> 
              filter(GeoUID %in% df$GeoUID)
            
          }
        
        # Throw error for missing parent vectors
        if (ncol(retrieved_parent) != 2) {
          stop(paste0(year, ", ", scale, ", no parent vector for ", v))
        }
        
        retrieved_parent
      })
      
      # Join the dfs
      if (length(parent_vec_values) > 0) {
        vec_retrieved <- 
          vec_retrieved |> 
          left_join(reduce(parent_vec_values, left_join, by = "GeoUID"),
                    by = "GeoUID")
      }
      
      # Sum parent vectors if need be:
      # Add up vectors that were retrieved through the same var_code
      vec_to_sum <- 
        vec_retrieved |> 
        names() |> 
        str_remove("\\d*$") |> 
        table()
      
      vec_to_sum <- names(vec_to_sum[vec_to_sum > 1])
      
      if (length(vec_to_sum > 0)) {
        vec_to_sum <- vec_named[str_detect(names(vec_named), 
                                           paste0(vec_to_sum, collapse = "|"))]
        
        agg_vec_to_sum <-
          (cancensus::list_census_vectors(census_dataset) |>
             filter(vector %in% vec_to_sum, aggregation != "Additive"))$vector
        
        # Throw error if they aren't additive
        if (length(agg_vec_to_sum) != 0) {
          stop(paste0(
            "Vector `", agg_vec_to_sum, "` contains multiple variables, but it ", 
            "isn't registered as `additive` in cancensus."))
        }
        
        # Sum them up
        if (ncol(vec_retrieved) > 1) {
          vec_retrieved <-
            vec_retrieved |>
            pivot_longer(-GeoUID) |>
            mutate(name = ifelse(str_detect(name, "\\d$"), 
                                 str_remove(name, "\\d*$"), name)) |>
            group_by(GeoUID, name) |>
            summarize(value = sum(value), .groups = "drop") |>
            pivot_wider(GeoUID) |>
            ungroup()
        }
      }
      
      vec_retrieved |> 
        right_join(df, by = "GeoUID") |>
        st_as_sf() |>
        st_set_agr("constant")
      
    })
  })
  
  out
  
}
