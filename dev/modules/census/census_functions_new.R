#### CENSUS DOWNLOAD AND PROCESSING FUNCTIONS ##################################

# Get empty geometries ----------------------------------------------------

get_empty_geometries <- function(scales, years) {
  map(scales, function(scale) {
    map(years, function(year) {
      cancensus::get_census(
        dataset = paste0("CA", sub("20", "", year)),
        regions = list(CMA = "24462"),
        level = scale,
        geo_format = "sf",
        quiet = TRUE) |> 
        select(GeoUID, geometry) |> 
        st_transform(32618) |> 
        mutate(area = st_area(geometry), .before = geometry)
    })
  })
}


# Download variables ------------------------------------------------------

get_census_vectors <- function(census_vec, geoms, scales, years, parent_vectors = NULL) {
  map2(scales, geoms, function(scale, g) {
    map2(years, g, function(year, df_g) {
      
      census_dataset <- paste0("CA", sub("20", "", year))
      
      original_vectors_named <- set_names(pull(census_vec, all_of(paste0("vec_", year))), 
                                          census_vec$var_code)
      
      original_vectors_named <- original_vectors_named[!is.na(original_vectors_named)]
      
      # Get original vectors
      original_vectors_retrieved <- 
      cancensus::get_census(
        dataset = census_dataset,
        regions = list(CMA = "24462"),
        level = scale,
        vectors = original_vectors_named,
        geo_format = NA,
        quiet = TRUE) |> 
        select(GeoUID, any_of(census_vec$var_code))
      
      # Parent vectors must be "mapped", as they might not be unique census vectors.
      # Some vectors share the same denominators, yet we want them all to have their
      # _parent suffix.
      parent_vecs <- 
        cancensus::list_census_vectors(census_dataset) %>% 
        filter(vector %in% original_vectors_named) %>% 
        arrange(match(vector, original_vectors_named)) %>% 
        # In cases of pct, there is no parent_vector. Yet, they tell what's the
        # vector that will be used to weight the averages in the aggregation column.
        mutate(parent_vector = ifelse(is.na(parent_vector), str_extract(aggregation, "v_.*$"),
                                      parent_vector)) %>% 
        pull(parent_vector) %>% 
        set_names(paste0(names(original_vectors_named), "_parent"))
      
      # Retrieve the values of all parent vectors
      parent_vector_values <- map(names(parent_vecs), ~{
        vec <- set_names(parent_vecs[.x], .x)
        retrieved_parent <- 
        cancensus::get_census(
          dataset = census_dataset,
          regions = list(CMA = "24462"),
          level = scale,
          vectors = vec[!is.na(vec)],
          geo_format = NA,
          quiet = TRUE) %>% 
          select(GeoUID, any_of(.x))
        
        # In cases of errors in cancensus, we can use the parent_vectors argument to
        # add parent vectors.
        if (.x %in% paste0(names(parent_vectors), "_parent")) {
          names(parent_vectors) <- paste0(names(parent_vectors), "_parent")
          vec <-  parent_vectors[names(parent_vectors) == .x[.x == names(parent_vectors)]]

          retrieved_parent <-
            cancensus::get_census(
              dataset = census_dataset,
              regions = list(CMA = "24462"),
              level = scale,
              vectors = vec[!is.na(vec)],
              geo_format = NA,
              quiet = TRUE) %>%
            select(GeoUID, any_of(.x))
        }
          
        # If there's missing parent vectors, it gives an error and tells us where
        if(ncol(retrieved_parent) != 2) {
          stop(paste0(year, ", ", scale, ", no parent vector for ", .x))
        }
        
          retrieved_parent
      })

      # Join the dfs
      left_join(original_vectors_retrieved,
                reduce(parent_vector_values, left_join, by = "GeoUID"),
                by = "GeoUID") |>
        right_join(df_g, by = "GeoUID") |>
        st_as_sf() |>
        st_set_agr("constant")
      
    })
  })
  
}


# Interpolate -------------------------------------------------------------

interpolate <- function(df_list, scales, years) {
  map2(df_list, scales, function(df_l, scale) {
    map2(df_l, years, function(df, year) {
      
      # Don't interpolate the current year
      if (year == max(years)) return({
        df |> 
          st_drop_geometry() |> 
          select(-area) |> 
          rename(ID = GeoUID)
      })
      
      # Otherwise interpolate!
      df_l[[length(df_l)]] |> 
        select(ID = GeoUID, geometry) |> 
        st_intersection(df) |> 
        filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |> 
        mutate(int_area = units::drop_units(st_area(geometry)),
               area_prop = int_area / units::drop_units(area), 
               .before = geometry) |>
        mutate(across(all_of(var_count), ~{.x * area_prop})) |> 
        st_drop_geometry() |> 
        group_by(ID) |> 
        summarize(across(all_of(var_count), ~{
          out <- sum(.x * area_prop, na.rm = TRUE)
          # Round to the nearest 5 to match non-interpolated census values
          out <- round(out / 5) * 5
          # Only keep output polygons with a majority non-NA inputs
          na_pct <- sum(is.na(.x) * int_area)
          if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
          out}), .groups = "drop")
      
    })
  })

}


# Normalize percentage variables ------------------------------------------

normalize <- function(df_list, census_vec) {
 map(df_list, function(df_l) {
   map(df_l, function(df) {
     map_dfc(names(df), ~{
       
       denom <- 
         census_vec |> 
         filter(var_code == .x) |> 
         pull(denominator)
       
       if (length(denom) > 0 && !is.na(denom)) {
         df |> 
           # Cap values at 1, under assumption they are all percentages
           mutate(across(all_of(.x), ~{pmin(1, . / get(denom))})) |> 
           select(all_of(.x))
         
       } else select(df, all_of(.x))
     })
   })
 }) 
}


# Drop variables ----------------------------------------------------------

drop_vars <- function(df_list, census_vec) {
  
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      # Keep "ID" and any variable with include == TRUE
      to_keep <- 
        census_vec |> 
        filter(include) |> 
        pull(var_code) |> 
        (\(x) c("ID", x))()
      
      select(df, all_of(to_keep))
    })
  })
}


# q3 breaks ---------------------------------------------------------------

add_q3 <- function(df_list) {
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      mutate(df, across(c(-ID), ntile, 3, .names = "{.col}_q3"))  
    })
  })
}

get_breaks_q3 <- function(df_list, census_vec) {
  
  var_q3 <- 
    census_vec |> 
    filter(include) |> 
    pull(var_code)
  
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      map_dfc(var_q3, ~{
        df |> 
          select(all_of(.x), all_of(paste0(.x, "_q3"))) |> 
          set_names(c("v", "q3")) |> 
          summarize(
            ranks = c(
              min(v, na.rm = TRUE),
              min(v[q3 == 2], na.rm = TRUE),
              min(v[q3 == 3], na.rm = TRUE),
              max(v, na.rm = TRUE)
            )
          ) |> 
          set_names(.x)
      })
    })
  })
}


# q5 breaks ---------------------------------------------------------------

get_categories_q5 <- function(df_list, census_vec) {
  
  categories <- unique(census_vec$category)
  categories <- categories[!is.na(categories)]
  
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      
      if (length(categories) > 0) {
        categories <- map(categories, ~{
          census_vec |> 
            filter(category == .x) |> 
            pull(var_code)})
        
        var_categories <-
          names(df) |> 
          setdiff("ID") |> 
          setdiff(unique(unlist(categories))) |> 
          as.list()
        
        c(var_categories, categories)
        
      } else {
        
        names(df) |> 
          setdiff("ID") |> 
          as.list()
      }
    })
  })
}

find_breaks_q5 <- function(min_val, max_val) {
  
  breaks <- unlist(map(-3:7, ~{(10 ^ .x) * c(1, 1.5, 2, 2.5, 3, 4, 5, 6)}))
  
  range <- max_val - min_val
  break_val <- range / 5
  break_val <- breaks[as.numeric(cut(break_val, breaks)) + 1]
  break_digits <- floor(log10(break_val))
  new_min <- floor(min_val / (10 ^ break_digits)) * 10 ^ break_digits
  
  return(c(new_min + 0:5 * break_val))
  
}

get_breaks_q5 <- function(df_list, categories) {
  map2(df_list, categories, function(df_l, cat_l) {
    map2(df_l, cat_l, function(df, cats) {
      
      cat_min <-
        map(cats, ~{
          df |> 
            select(all_of(.x)) |> 
            as.matrix() |> 
            min(na.rm = TRUE)
        })
      
      cat_max <-
        map(cats, ~{
          df |> 
            select(all_of(.x)) |> 
            as.matrix() |> 
            max(na.rm = TRUE)
        })
      
      pmap_dfc(list(cat_min, cat_max, cats), function(x, y, z) {
        breaks <- find_breaks_q5(x, y)
        tibble(v = breaks) |> 
          set_names(z)
      })
      
    })
  })
}

add_q5 <- function(df_list, breaks_list) {
  map2(df_list, breaks_list, function(df_l, breaks_l) {
    map2(df_l, breaks_l, function(df, breaks) {
      map2_dfc(names(breaks), breaks, function(x, y) {
        df |> 
          transmute(across(all_of(x), 
                           ~as.numeric(cut(.x, y, include.lowest = TRUE)),
                           .names = "{.col}_q5"))
      })
    })
  })
}

merge_breaks <- function(df_list, df_list_q3, df_list_q5) {
  pmap(list(df_list, df_list_q3, df_list_q5), function(df_l, df_l_q3, df_l_q5) {
    pmap(list(df_l, df_l_q3, df_l_q5), function(df, df_q3, df_q5) {
      full_join(df, df_q3, by = names(df)) |> 
        bind_cols(df_q5)
    })
  })
}


# Add/reduce years --------------------------------------------------------

add_years <- function(df_list, years) {
  map(df_list, function(df_l) {
    map2(df_l, years, function(df, year) {
      rename_with(df, .fn = ~paste0(.x, "_", year), .cols = -ID)
    })
  })
}

reduce_years <- function(df_list) {
  map(df_list, function(df_l) {
    reduce(df_l, full_join, by = "ID")
  })
}


# Add to variable table ---------------------------------------------------

add_vars <- function(data_to_add, census_vec, breaks_q3, breaks_q5) {
  
  # Get all variables at all scales
  all_vars <- unique(unlist(sapply(data_to_add, names)))
  all_vars <- setdiff(all_vars, "ID")
  
  # Get variables to add as rows to variables table
  add_vars <- all_vars[!grepl("_q(3|5)", all_vars)]
  add_vars <- str_remove(add_vars, "_\\d{4}$")
  add_vars <- unique(add_vars)
  
  map_dfr(add_vars, ~{
    
    # Get starting var table subset
    dat <- filter(census_vec, var_code == .x)
    
    # Get unique dates
    dates_active <- str_subset(all_vars, .x)
    dates_active <- str_extract(dates_active, "\\d{4}$")
    dates_active <- unique(dates_active)
    
    # Get scales
    scales_active <- c("borough", "building", "CT", "DA", "grid", "street")
    scales_active <- scales_active[map_lgl(scales_active, function(df) {
      get(df) |> 
        st_drop_geometry() |> 
        select(starts_with(.x)) |> 
        length() |> 
        (\(x) x > 0)()
    })]
    
    # Get breaks_q3
    breaks_q3_active <- map2_dfr(breaks_q3, scales, function(x, scale) {
      map2_dfr(x, dates, function(y, date) {
        y |> mutate(rank = 0:3, date = date)}) |> 
        mutate(scale = scale)}) |> 
      select(scale, date, rank, everything())
    
    # Get breaks_q5
    breaks_q5_active <- map2_dfr(breaks_q5, scales, function(x, scale) {
      map2_dfr(x, dates, function(y, date) {
        y |> mutate(rank = 0:5, date = date)}) |> 
        mutate(scale = scale)}) |> 
      select(scale, date, rank, everything())
    
    tibble(
      var_code = .x,
      var_title = dat$var_title,
      var_short = if (is.na(dat$var_short)) var_title else dat$var_short,
      explanation = dat$explanation,
      category = dat$category,
      private = dat$private,
      dates = list(dates_active),
      scales = list(scales_active),
      breaks_q3 = list(breaks_q3_active),
      breaks_q5 = list(breaks_q5_active))
  })
  
  
  
  
  
  
}

