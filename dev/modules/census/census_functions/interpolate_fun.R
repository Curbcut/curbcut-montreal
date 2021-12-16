#### CENSUS INTERPOLATE FUNCTIONS #########################################


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

# Retrieve aggregation type -----------------------------------------------

get_agg_type <- function(census_vec, scales, years) {

  # Skip 2001 Census; vectors are labelled average but aggregated as additive
  # cancensus::list_census_vectors("CA01") |>
  #   filter(vector == "v_CA01_1667") |>
  #   select(label, aggregation)
  for_years <- map(rev(years)[!rev(years) == 2001], function(year) {
    census_dataset <- paste0("CA", sub("20", "", year))

    # Get named versions of vectors
    vec_named <-
      census_vec |>
      pull(all_of(paste0("vec_", year))) |>
      set_names(census_vec$var_code) |>
      unlist() |>
      na.omit()

    cancensus::list_census_vectors(census_dataset) |>
      filter(vector %in% vec_named) |>
      arrange(match(vector, vec_named)) |>
      mutate(aggregation = str_extract(aggregation, ".[^ ]*")) |>
      mutate(var_code = names(vec_named)) |>
      select(var_code, aggregation) |>
      mutate(var_code = if_else(str_detect(var_code, "\\d$"),
        str_remove(var_code, "\\d*$"), var_code
      )) |>
      distinct()
  })

  vars_agg <-
    for_years |>
    reduce(left_join, by = "var_code") |>
    pivot_longer(!var_code) |>
    filter(!is.na(value)) |>
    group_by(var_code) |>
    summarize(aggregation = list(value)) |>
    rowwise() |>
    mutate(aggregation = list(unique(aggregation))) |>
    ungroup()

  # Check to make sure there aren't conflicting aggregation types
  if (all(lengths(pull(vars_agg)) == 1)) {
    vars_agg <- unnest(vars_agg, aggregation)
  } else {
    vars_agg[(lengths(pull(vars_agg)) > 1), ] |>
      pull(var_code) |>
      paste(collapse = ", ") |>
      (\(x) paste("Different `aggregation` types detected for", x))() |>
      stop()
  }

  vars_agg
}


# Interpolate -------------------------------------------------------------

interpolate <- function(df_list, scales, years, data_agg, census_vec) {

  # Get variables to be added
  var_add <-
    data_agg |>
    filter(aggregation == "Additive") |>
    pull(var_code)

  # Get variables to be averaged
  var_avg <-
    data_agg |>
    filter(aggregation %in% c("Average", "Median")) |>
    pull(var_code)

  # Error checking
  if (length(c(var_add, var_avg)) != length(census_vec$var_code)) {
    stop(
      "The length of var_add and var_avg isn't the same as the number of ",
      "variables."
    )
  }

  # Main interpolation function
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

      # Otherwise interpolate!
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
        mutate(across(
          any_of(var_add) | ends_with("_parent"),
          ~ {
            .x * area_prop
          }
        )) |>
        st_drop_geometry() |>
        group_by(ID)

      # Interpolate additive variables
      agg_add <- function(x, area_prop, int_area) {
        out <- sum(x * {{ area_prop }}, na.rm = TRUE)
        # Round to the nearest 5 to match non-interpolated census values
        out <- round(out / 5) * 5
        # Only keep output polygons with a majority non-NA inputs
        na_pct <- sum(is.na(x) * {{ int_area }})
        if (na_pct >= 0.5 * sum({{ int_area }})) out <- NA_real_
        out
      }

      # Interpolate average variables
      agg_avg <- function(x, parent, int_area) {
        out <- weighted_mean(x, {{ parent }}, na.rm = TRUE)
        # Only keep output polygons with a majority non-NA inputs
        na_pct <- sum(is.na(x) * {{ int_area }})
        if (na_pct >= 0.5 * sum({{ int_area }})) out <- NA_real_
        out
      }

      interpolated_ids |>
        summarize(across(
          any_of(var_add) | ends_with("_parent"),
          agg_add, area_prop, int_area
        ),
        across(
          any_of(var_avg), agg_avg,
          as.name(paste0(cur_column(), "_parent")), int_area
        ),
        .groups = "drop"
        ) |>
        glimpse()
    })
  })
}


# Swap CSD to borough -----------------------------------------------------

swap_csd_to_borough <- function(df_list, years, var_add, var_avg) {
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
      mutate(across(any_of(var_add) | ends_with("_parent"), ~ {
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
        summarize(across(any_of(var_add) | ends_with("_parent"), ~ {
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


# Interpolate to grid -----------------------------------------------------

interpolate_other_geoms <- function(to_interpolate, df_list, years, var_add, var_avg) {
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
          mutate(across(any_of(var_add) | ends_with("_parent"), ~ {
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
            summarize(across(any_of(var_add) | ends_with("_parent"), ~ {
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
