#### CENSUS DOWNLOAD AND PROCESSING FUNCTIONS ##################################


# Global ------------------------------------------------------------------

# For interpolation: weighted.mean na.rm = TRUE does not handle NA weights
weighted_mean <- function(x, w, ..., na.rm = FALSE) {
  if (na.rm) {
    x_1 <- x[!is.na(x) & !is.na(w)]
    w <- w[!is.na(x) & !is.na(w)]
    x <- x_1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}


# Get empty geometries ----------------------------------------------------

get_empty_geometries <- function(scales, years, CMA = "24462", crs = 32618) {
  map(scales, function(scale) {
    map(years, function(year) {
      
      cancensus::get_census(
        dataset = paste0("CA", sub("20", "", year)),
        regions = list(CMA = CMA),
        level = scale,
        geo_format = "sf",
        quiet = TRUE) |> 
        select(GeoUID, geometry) |> 
        st_transform(crs) |> 
        mutate(area = st_area(geometry), .before = geometry)
      
    })
  })
}


# Download variables ------------------------------------------------------

get_census_vectors <- function(census_vec, geoms, scales, years, 
                               parent_vectors = NULL, CMA = "24462") {
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
      
      # Get original vectors
      vec_retrieved <- cancensus::get_census(
        dataset = census_dataset,
        regions = list(CMA = CMA),
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
      vec_to_sum <- vec_named[str_detect(names(vec_named), vec_to_sum)]
      
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
      vec_retrieved <-
        vec_retrieved |>
        pivot_longer(-GeoUID) |>
        mutate(name = ifelse(str_detect(name, "\\d$"), 
                             str_remove(name, "\\d*$"), name)) |>
        group_by(GeoUID, name) |>
        summarize(value = sum(value), .groups = "drop") |>
        pivot_wider(GeoUID) |>
        ungroup()

      # Some vectors share denominators, so use map to get them multiple times
      parent_vec <-
        cancensus::list_census_vectors(census_dataset) |>
        filter(vector %in% vec_named) |> 
        arrange(match(vector, vec_named)) |> 
        # For pct, use parent_vector to weight aggregation averages
        mutate(parent_vector = if_else(is.na(parent_vector), 
                                       str_extract(aggregation, "v_.*$"),
                                       parent_vector)) |>
        pull(parent_vector) |> 
        set_names(paste0(str_remove(names(vec_named), "\\d*$"), "_parent"))
      
      # Parents should be retrieved only once
      parent_vec <-
        parent_vec |> 
        names() |> 
        unique() |> 
        map(~{
          value <- unique(parent_vec[names(parent_vec) == .x])
          name <- unique(names(parent_vec)[names(parent_vec) == .x])
          if (length(value) > 1) stop(
              "Parent vectors of `", name, "` aren't unique. A var_code ", 
              "sharing multiple numerators should have a unique parent.")
          set_names(value, name)}) |> 
        unlist()
      
      # Check for non-additive parent vectors
      non_additive_parent_vecs <-
        cancensus::list_census_vectors(census_dataset) |>
        filter(vector %in% parent_vecs) |>
        filter(aggregation != "Additive")

      if (nrow(non_additive_parent_vecs) > 0) {
        stop(paste0(
          "Non-additive parent vector: ",
          parent_vecs[parent_vecs %in% non_additive_parent_vecs]
        ))
      }


      # Retrieve the values of all parent vectors
      parent_vector_values <- map(names(parent_vecs), ~ {
        # In cases of errors in cancensus, we can use the parent_vectors argument to
        # add parent vectors.
        if (.x %in% paste0(names(parent_vectors), "_parent")) {
          names(parent_vectors) <- paste0(names(parent_vectors), "_parent")
          vec <- parent_vectors[names(parent_vectors) == .x[.x == names(parent_vectors)]]
        } else {
          vec <- set_names(parent_vecs[.x], .x)
        }

        retrieved_parent <-
          cancensus::get_census(
            dataset = census_dataset,
            regions = list(CMA = "24462"),
            level = scale,
            vectors = vec[!is.na(vec)],
            geo_format = NA,
            quiet = TRUE
          ) |>
          select(GeoUID, any_of(.x))

        # If there's missing parent vectors, it gives an error and tells us where
        if (ncol(retrieved_parent) != 2) {
          stop(paste0(year, ", ", scale, ", no parent vector for ", .x))
        }

        retrieved_parent
      })

      # Join the dfs
      left_join(original_vectors_retrieved,
        reduce(parent_vector_values, left_join, by = "GeoUID"),
        by = "GeoUID"
      ) |>
        right_join(df_g, by = "GeoUID") |>
        st_as_sf() |>
        st_set_agr("constant")
    })
  })
}



# Retrieve aggregation type -----------------------------------------------
get_aggregation_type <- function(census_vec, scales, years) {
  for_years <- map(rev(years)[!rev(years) == 2001], function(year) {
    census_dataset <- paste0("CA", sub("20", "", year))
    original_vectors_named <- set_names(
      pull(census_vec, all_of(paste0("vec_", year))),
      census_vec$var_code
    ) |> unlist()
    original_vectors_named <- original_vectors_named[!is.na(original_vectors_named)]

    cancensus::list_census_vectors(census_dataset) |>
      filter(vector %in% original_vectors_named) |>
      arrange(match(vector, original_vectors_named)) |>
      mutate(aggregation = str_extract(aggregation, ".[^ ]*")) |>
      mutate(var_code = names(original_vectors_named)) |>
      select(var_code, aggregation) |> 
      mutate(var_code = ifelse(str_detect(var_code, "\\d$"), 
                           str_remove(var_code, "\\d*$"), var_code)) |> 
      distinct()
  })
  vars_aggregation <-
    reduce(for_years, left_join, by = "var_code") |>
    pivot_longer(!var_code) |>
    filter(!is.na(value)) |>
    group_by(var_code) |>
    summarize(aggregation = list(value)) |>
    rowwise() |>
    mutate(aggregation = list(unique(aggregation)))

  if (all(lengths(pull(vars_aggregation)) == 1)) {
    vars_aggregation <-
      vars_aggregation |>
      unnest(aggregation)
  } else {
    non_unique_aggregation_type <-
      vars_aggregation[(lengths(pull(vars_aggregation)) > 1), ] |>
      pull(var_code)
    stop(paste0(
      "Different `aggregation` types detected for `",
      non_unique_aggregation_type, "`.\n"
    ))
  }

  vars_aggregation
}

# 2001 census have been taken out of the previous function, here is why:
# cancensus::list_census_vectors("CA01") |>
#   filter(vector == "v_CA01_1667") |>
#   select(label, aggregation)
# It is labelled as an average, like this variable in every census years. However,
# it is aggregated as an additive. In the 5 other census, it is aggregated as
# an average. This problem happened twice with housing variables.

# Interpolate -------------------------------------------------------------

interpolate <- function(df_list, scales, years, data_aggregation, census_vec) {
  var_count <-
    data_aggregation |>
    filter(aggregation == "Additive") |>
    pull(var_code)
  var_avg <-
    data_aggregation |>
    filter(aggregation %in% c("Average", "Median")) |>
    pull(var_code)
  if (length(c(var_count, var_avg)) != length(census_vec$var_code)) {
    stop(
      "The length of var_count and var_avg isn't the same as the number of ",
      "variables."
    )
  }

  map2(df_list, scales, function(df_l, scale) {
    map2(df_l, years, function(df, year) {

      # Don't interpolate the current year
      if (year == max(years)) {
        return({
          df |>
            st_drop_geometry() |>
            select(-area) |>
            rename(ID = GeoUID)
        })
      }

      # Otherwise interpolate! --------
      interpolated_ids <-
        df_l[[length(df_l)]] |>
        select(ID = GeoUID, geometry) |>
        st_intersection(df) |>
        filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
        mutate(
          int_area = units::drop_units(st_area(geometry)),
          area_prop = int_area / units::drop_units(area),
          .before = geometry
        ) |>
        mutate(across(any_of(var_count) | ends_with("_parent"), ~ {
          .x * area_prop
        })) |>
        st_drop_geometry() |>
        group_by(ID)

      left_join(
        # For averaging variables
        interpolated_ids |>
          summarize(across(any_of(var_avg), ~ {
            out <- weighted_mean(.x, get(paste0(cur_column(), "_parent")), na.rm = TRUE)
            # Only keep output polygons with a majority non-NA inputs
            na_pct <- sum(is.na(.x) * int_area)
            if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
            out
          }), .groups = "drop"),
        # For additive variables
        interpolated_ids |>
          summarize(across(any_of(var_count) | ends_with("_parent"), ~ {
            out <- sum(.x * area_prop, na.rm = TRUE)
            # Round to the nearest 5 to match non-interpolated census values
            out <- round(out / 5) * 5
            # Only keep output polygons with a majority non-NA inputs
            na_pct <- sum(is.na(.x) * int_area)
            if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
            out
          }), .groups = "drop"),
        by = "ID"
      )
    })
  })
}


# Swap CSD to borough -----------------------------------------------------

swap_csd_to_borough <- function(df_list, years, var_count, var_avg) {
  if (!exists("borough")) {
    stop(paste0(
      "Dataframe `borough`, coming from `dev/build_data.R`, must be ",
      "in the global environment."
    ))
  }

  borough_data <- map(set_names(years), function(year) {
    # Get geometry and areas of already-interpolated DAs.
    DA_n <-
      eval(parse(text = (paste0("df_list$DA$`", year, "`")))) |>
      left_join(select(DA, ID), by = "ID") |>
      st_as_sf() |>
      st_transform(32618) |>
      mutate(area = st_area(geometry)) |>
      st_set_agr("constant") |>
      select(-ID)

    interpolated_ids <-
      borough |>
      select(ID) |>
      filter(str_starts(ID, "2466023")) |>
      st_transform(32618) |>
      st_set_agr("constant") |>
      st_intersection(DA_n, .) |>
      mutate(
        int_area = units::drop_units(st_area(geometry)),
        area_prop = int_area / units::drop_units(area),
        .before = geometry
      ) |>
      mutate(across(any_of(var_count) | ends_with("_parent"), ~ {
        .x * area_prop
      })) |>
      st_drop_geometry() |>
      group_by(ID)

    left_join(
      # For averaging variables
      interpolated_ids |>
        summarize(across(any_of(var_avg), ~ {
          out <- weighted_mean(.x, get(paste0(cur_column(), "_parent")), na.rm = T)
          # Only keep output polygons with a majority non-NA inputs
          na_pct <- sum(is.na(.x) * int_area)
          if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
          out
        }), .groups = "drop"),
      # For additive variables
      interpolated_ids |>
        summarize(across(any_of(var_count) | ends_with("_parent"), ~ {
          out <- sum(.x * area_prop, na.rm = TRUE)
          # Round to the nearest 5 to match non-interpolated census values
          out <- round(out / 5) * 5
          # Only keep output polygons with a majority non-NA inputs
          na_pct <- sum(is.na(.x) * int_area)
          if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
          out
        }), .groups = "drop"),
      by = "ID"
    ) |>
      filter(str_starts(ID, "2466023"))
  })

  borough_data <- map(set_names(years), function(year) {
    # Get geometry and areas of interpolated DAs.
    CSD_n <-
      eval(parse(text = (paste0("df_list$CSD$`", year, "`"))))

    borough_n <-
      eval(parse(text = (paste0("borough_data$`", year, "`"))))

    bind_rows(
      filter(CSD_n, !str_starts(ID, "2466023")),
      borough_n
    )
  })

  df_list$CSD <- borough_data

  # switch name of the first df_list from CSD to borough
  scales[scales == "CSD"] <- "borough"
  names(df_list) <- scales
  df_list
}


# Interpolate to building, grid & street ----------------------------------

interpolate_other_geoms <- function(to_interpolate, df_list, years, var_count, var_avg) {
  new_geos <-
    map(set_names(to_interpolate), function(geo) {
      map(set_names(years), function(year) {

        # Get geometry and areas of already-interpolated DAs.
        DA_n <-
          eval(parse(text = (paste0("df_list$DA$`", year, "`")))) |>
          left_join(select(DA, ID), by = "ID") |>
          st_as_sf() |>
          st_transform(32618) |>
          mutate(area = st_area(geometry)) |>
          st_set_agr("constant") |>
          select(-ID)

        geom_type <- switch(as.character(unique(st_geometry_type(get(geo)))),
          "POLYGON" = "polygon",
          "MULTIPOLYGON" = "polygon",
          "LINESTRING" = "line",
          "MULTILINESTRING" = "line",
          "error"
        )

        if (geom_type == "error") {
          stop(paste0(
            "Geometry type for ", geo, " is not either polygon or line. ",
            "It cannot be interpolated with area or length."
          ))
        }

        interpolated_ids <-
          get(geo) |>
          select(ID) |>
          st_transform(32618) |>
          st_set_agr("constant") |>
          st_intersection(DA_n, .) %>%
          {
            if (geom_type == "polygon") {
              mutate(.,
                int_area = units::drop_units(st_area(geometry)),
                area_prop = int_area / units::drop_units(area),
                .before = geometry
              )
            } else {
              mutate(.,
                int_area = units::drop_units(st_length(geometry)),
                area_prop = int_area / units::drop_units(area),
                .before = geometry
              )
            }
          } |>
          mutate(across(any_of(var_count) | ends_with("_parent"), ~ {
            .x * area_prop
          })) |>
          st_drop_geometry() |>
          group_by(ID)

        left_join(
          # For averaging variables
          interpolated_ids |>
            summarize(across(any_of(var_avg), ~ {
              out <- weighted_mean(.x, get(paste0(cur_column(), "_parent")), na.rm = T)
              # Only keep output polygons with a majority non-NA inputs
              na_pct <- sum(is.na(.x) * int_area)
              if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
              out
            }), .groups = "drop"),
          # For additive variables
          interpolated_ids |>
            summarize(across(any_of(var_count) | ends_with("_parent"), ~ {
              out <- sum(.x * area_prop, na.rm = TRUE)
              # Only keep output polygons with a majority non-NA inputs
              na_pct <- sum(is.na(.x) * int_area)
              if (na_pct >= 0.5 * sum(int_area)) out <- NA_real_
              out
            }), .groups = "drop"),
          by = "ID"
        )
      })
    })

  c(df_list, new_geos)
}


# Retrieve units type -----------------------------------------------------

get_unit_type <- function(census_vec, scales, years) {
  for_years <- map(rev(years)[!rev(years) == 2001], function(year) {
    census_dataset <- paste0("CA", sub("20", "", year))
    original_vectors_named <- set_names(
      pull(census_vec, all_of(paste0("vec_", year))),
      census_vec$var_code
    ) |> unlist()
    original_vectors_named <- original_vectors_named[!is.na(original_vectors_named)]

    cancensus::list_census_vectors(census_dataset) |>
      filter(vector %in% original_vectors_named) |>
      arrange(match(vector, original_vectors_named)) |>
      mutate(units = str_extract(units, ".[^ ]*")) |>
      mutate(var_code = names(original_vectors_named)) |>
      select(var_code, units) |> 
      mutate(var_code = ifelse(str_detect(var_code, "\\d$"), 
                               str_remove(var_code, "\\d*$"), var_code)) |> 
      distinct()
  })
  vars_units <-
    reduce(for_years, left_join, by = "var_code") |>
    pivot_longer(!var_code) |>
    filter(!is.na(value)) |>
    group_by(var_code) |>
    summarize(units = list(value)) |>
    rowwise() |>
    mutate(units = list(unique(units)))

  if (all(lengths(pull(vars_units)) == 1)) {
    vars_units <-
      vars_units |>
      unnest(units)
  } else {
    non_unique_units_type <-
      vars_units[(lengths(pull(vars_units)) > 1), ] |>
      pull(var_code)
    stop(paste0(
      "Different `units` types detected for `",
      non_unique_units_type, "`.\n"
    ))
  }

  vars_units
}

# 2001 census have been taken out of the previous function, here is why:
# cancensus::list_census_vectors("CA01") |>
#   filter(vector == "v_CA01_1674") |>
#   select(label, units)
# It is labelled as "value ... %", like this variable in every census years. However,
# it is noted as an "Number" units In the 5 other census, it is aggregated as
# "Currency". This problem happened twice with housing variables.

# Normalize percentage variables ------------------------------------------

normalize <- function(df_list, census_vec, data_unit) {
  if (!exists("data_unit")) {
    stop(paste0(
      "Dataframe `data_unit`, the output of the `get_unit_type` function ",
      "doesn't exist. It is necessary to evaluate if variables are ",
      "already treated as percentages by the census."
    ))
  }
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      map_dfc(names(df), ~ {
        nominator <-
          census_vec |>
          filter(
            var_code == .x,
            str_detect(var_code, "_pct")
          ) |>
          pull(var_code)

        nominator <-
          data_unit |>
          filter(var_code %in% nominator) |>
          mutate(out = set_names(var_code, units)) |>
          pull(out)

        if (length(nominator) > 0 && !names(nominator) %in% c("Number", "Percentage")) {
          stop(paste0(
            "Nominator ", nominator, " isn't classified as 'Number' or ",
            " as 'Percentage' by the census, but as '", names(nominator),
            "'. Should it really have the `_pct` suffix?"
          ))
        }

        if (length(nominator) > 0 && !is.na(nominator)) {
          if (names(nominator) == "Percentage") {
            # Some census variables are already percentages.
            df |>
              mutate(across(all_of(.x), ~ {
                pmin(1, . / 100)
              })) |>
              select(all_of(.x))
          } else {
            # If the variable is classified as Number and has _pct suffix,
            # it must be normalized with its parent variable.
            denom <- paste0(nominator, "_parent")

            df |>
              # Cap values at 1, under assumption they are all percentages
              mutate(across(all_of(.x), ~ {
                pmin(1, . / get(denom))
              })) |>
              select(all_of(.x))
          }
        } else {
          select(df, all_of(.x))
        }
      })
    })
  })
}


# Drop variables ----------------------------------------------------------

drop_vars <- function(df_list, census_vec) {
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      # Keep "ID" and any variable present in census_vec
      to_keep <-
        census_vec |>
        pull(var_code) |>
        (\(x) c("ID", x))()

      select(df, any_of(to_keep))
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
    pull(var_code)

  map(df_list, function(df_l) {
    map(df_l, function(df) {
      map_dfc(var_q3, ~ {
        if (.x %in% names(df)) {
          df |>
            select(any_of(.x), any_of(paste0(.x, "_q3"))) |>
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
        }
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
        categories <- map(categories, ~ {
          census_vec |>
            filter(category == .x) |>
            pull(var_code)
        })

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
  breaks <- unlist(map(-3:7, ~ {
    (10^.x) * c(1, 1.5, 2, 2.5, 3, 4, 5, 6)
  }))

  range <- max_val - min_val
  break_val <- range / 5
  break_val <- breaks[as.numeric(cut(break_val, breaks)) + 1]
  break_digits <- floor(log10(break_val))
  new_min <- floor(min_val / (10^break_digits)) * 10^break_digits

  return(c(new_min + 0:5 * break_val))
}

get_breaks_q5 <- function(df_list, categories) {
  map2(df_list, categories, function(df_l, cat_l) {
    map2(df_l, cat_l, function(df, cats) {
      cat_min <-
        map(cats, ~ {
          df |>
            select(all_of(.x)) |>
            as.matrix() |>
            min(na.rm = TRUE)
        })

      cat_max <-
        map(cats, ~ {
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
            ~ as.numeric(cut(.x, y, include.lowest = TRUE)),
            .names = "{.col}_q5"
          ))
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
      rename_with(df, .fn = ~ paste0(.x, "_", year), .cols = -ID)
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

  map_dfr(add_vars, ~ {

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
        y |> mutate(rank = 0:3, date = date)
      }) |>
        mutate(scale = scale)
    }) |>
      select(scale, date, rank, everything())

    # Get breaks_q5
    breaks_q5_active <- map2_dfr(breaks_q5, scales, function(x, scale) {
      map2_dfr(x, dates, function(y, date) {
        y |> mutate(rank = 0:5, date = date)
      }) |>
        mutate(scale = scale)
    }) |>
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
      breaks_q5 = list(breaks_q5_active)
    )
  })
}


# Full census data gather function ----------------------------------------

# Full census data gather function ----------------------------------------

census_data_gather <- function(census_vec, scales, years, parent_vectors = NULL) {
  message("Data census startup")
  ## Get empty geometries
  message("Getting empty geometries ...", appendLF = TRUE)
  geoms <- get_empty_geometries(scales, years)
  ## Download data
  message("Downloading census data ...", appendLF = TRUE)
  data_raw <- get_census_vectors(
    census_vec, geoms, scales, years,
    parent_vectors
  )
  # Get aggregation type
  message("Interpolating ...", appendLF = TRUE)
  data_aggregation <- get_aggregation_type(census_vec, scales, years)
  ## Interpolate
  data_inter <- interpolate(data_raw, scales, years, data_aggregation, census_vec)
  ## Swap CSD to borough
  message("Swapping CSD to borough ...", appendLF = TRUE)
  data_swaped <- swap_csd_to_borough(data_inter, years, var_count, var_avg)
  # From here, no CSD, but borough
  scales[scales == "CSD"] <- "borough"
  ## Interpolate to building, grid & street
  message("Interpolating other geometries (grid, ... ) ...",
    appendLF = TRUE
  )
  data_other_inter <- interpolate_other_geoms(
    c("grid"),
    data_swaped, years,
    var_count, var_avg
  )
  ## Get units type
  message("Normalizing all data ...", appendLF = TRUE)
  data_unit <- get_unit_type(census_vec, scales, years)
  ## Normalize pct variables
  data_norm <- normalize(data_other_inter, census_vec, data_unit)
  ## Drop variables which aren't included in final tables
  message("Other manipulations (dropping variables, q3, q5, ...) ...", appendLF = TRUE)
  data_final <- drop_vars(data_norm, census_vec)
  ## Add q3 and q5 versions
  cat_q5 <- get_categories_q5(data_final, census_vec)
  data_q3 <- add_q3(data_final)
  breaks_q3 <- get_breaks_q3(data_q3, census_vec)
  breaks_q5 <- get_breaks_q5(data_final, cat_q5)
  data_q5 <- add_q5(data_final, breaks_q5)
  data_breaks <- merge_breaks(data_final, data_q3, data_q5)
  ## Add years
  data_years <- add_years(data_breaks, years)
  ## Finalize output
  message("Reducing ...", appendLF = TRUE)
  reduce_years(data_years)
}
