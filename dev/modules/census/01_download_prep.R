#### CENSUS DOWNLOAD FUNCTIONS ##################################

# Get empty geometries ----------------------------------------------------

get_empty_geometries <- function(scales, years, region = "24", crs = 32618) {
  
  progressr::with_progress({
    p <- progressr::progressor(length(scales) * length(years))
    
    out <- 
      map(scales, function(scale) {
        furrr::future_map(years, function(year) {
          census_dataset <- paste0("CA", sub("20", "", year))
          
          get_c <- function(re = list(PR = region), sc = scale) {
            cancensus::get_census(
              dataset = census_dataset,
              regions = re,
              level = sc,
              geo_format = "sf",
              quiet = TRUE) |> 
              select(GeoUID, geometry) |> 
              st_make_valid() |> 
              st_transform(crs) |> 
              mutate(area = st_area(geometry), .before = geometry) |> 
              st_set_agr("constant") |> 
              st_intersection(st_make_valid(master_polygon))
          }
          
          p()
          
          tryCatch(get_c(), error = function(e) {
            # if the function doesn't work with default parameters, go fetch the CDs
            # instead of the whole Province.
            CDs <- get_c(sc = "CD")$GeoUID
            get_c(re = list(CD = CDs), sc = scale)
          })
          
        })
      })
  })
  out
}


# Download variables ------------------------------------------------------

get_census_vectors <- function(census_vec, geoms, scales, years, 
                               parent_vectors = NULL) {
  
  progressr::with_progress({
    p <- progressr::progressor(length(scales) * length(years))
    
    out <- 
      map2(set_names(scales), geoms, function(scale, geom) {
        furrr::future_map2(set_names(years), geom, function(year, df) {
          
          census_dataset <- paste0("CA", sub("20", "", year))
          
          # Get named versions of vectors
          vec_named <- 
            census_vec |> 
            pull(all_of(paste0("vec_", year))) |> 
            set_names(census_vec$var_code) |> 
            unlist() |> 
            na.omit()
          
          # Get original vectors
          list_regions <- list(df$GeoUID)
          names(list_regions) <- scale
          
          vec_retrieved <- 
            cancensus::get_census(
              dataset = census_dataset,
              regions = list_regions,
              level = scale,
              vectors = vec_named,
              geo_format = NA,
              quiet = TRUE) |> 
            select(GeoUID, starts_with(census_vec$var_code))
          
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
          parent_vec_values <- map(names(parent_vec), ~{
            
            vec <- set_names(parent_vec[.x], .x)
            
            retrieved_parent <- cancensus::get_census(
              dataset = census_dataset,
              regions = list_regions,
              level = scale,
              vectors = vec[!is.na(vec)],
              geo_format = NA,
              quiet = TRUE) |> 
              select(GeoUID, any_of(.x)) |> 
              filter(GeoUID %in% df$GeoUID)
            
            # Throw error for missing parent vectors
            if (ncol(retrieved_parent) != 2) {
              stop(paste0(year, ", ", scale, ", no parent vector for ", .x))
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
          
          p()
          
          vec_retrieved |> 
            right_join(df, by = "GeoUID") |>
            st_as_sf() |>
            st_set_agr("constant")
          
        })
      })
  })
  
  out
  
}
