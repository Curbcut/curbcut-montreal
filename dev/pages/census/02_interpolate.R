#### CENSUS INTERPOLATE FUNCTIONS #########################################


# Helper functions --------------------------------------------------------

# For interpolation: weighted.mean na.rm = TRUE does not handle NA weights
weighted_mean <- function(x, w, ..., na.rm = FALSE) {
  if (na.rm) {
    x_1 <- x[!is.na(x) & !is.na(w)]
    w <- w[!is.na(x) & !is.na(w)]
    x <- x_1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

# Interpolate additive variables
agg_add <- function(x, area_prop, int_area, other_geom = FALSE) {
  out <- sum(x * {{ area_prop }}, na.rm = TRUE)
  # Round to the nearest 5 to match non-interpolated census values, except
  # for geoms out of CSD/borough, CT and DA
  if (!other_geom) out <- round(out / 5) * 5
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


# Retrieve aggregation type -----------------------------------------------

get_agg_type <- function(df_list, census_vec, scales, years) {

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
                                str_remove(var_code, "\\d*$"), var_code)) |>
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

  # Get variables to be added
  var_add <-
    vars_agg |>
    filter(aggregation == "Additive") |>
    pull(var_code) |>
    c(df_list |>
      sapply(sapply, names) |>
      unlist() |>
      unique() |>
      str_subset("_parent"))

  # Get variables to be averaged
  var_avg <-
    vars_agg |>
    filter(aggregation %in% c("Average", "Median")) |>
    pull(var_code)

  list(var_add = var_add, var_avg = var_avg)
}


# Interpolate -------------------------------------------------------------

interpolate <- function(df_list, scales, years, data_agg) {

  # Get progress bar
  pb <- progressr::progressor(steps = sum(sapply(df_list, sapply, nrow)))

  # Main interpolation function
  map2(df_list, scales, function(df_l, scale) {
    map2(df_l, years, function(df, year) {
      pb(amount = nrow(df))

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
          any_of(data_agg$var_add) | ends_with("_parent"),
          ~ {
            .x * area_prop
          }
        )) |>
        st_drop_geometry() |>
        group_by(ID)

      interpolated_ids |>
        # agg_avg has to be calculated first, so parent vectors are untouched!
        summarize(across(
          any_of(data_agg$var_avg), agg_avg,
          eval(parse(text = paste0(cur_column(), "_parent"))),
          int_area
        ),
        across(any_of(data_agg$var_add), agg_add, area_prop, int_area),
        .groups = "drop"
        )
    })
  })
}


# Swap CSD to borough -----------------------------------------------------

swap_csd_to_borough <- function(df_list, years, crs = 32618, data_agg, scales) {
  
  # Only proceed if `borough` exists
  if (!exists("CSD")) stop("`CSD` must be in the global environment.")
  
  # Initialize progress bar
  pb <- progressr::progressor(steps = sum(sapply(df_list$DA, nrow)))
  
  borough_data <- map(set_names(years), function(year) {
    DA_n <- df_list$DA[[as.character(year)]]
    pb(amount = nrow(DA_n))
    
    # Get geometry and areas of already-interpolated DAs.
    DA_n <-
      DA_n |>
      left_join(select(DA, ID), by = "ID") |>
      st_as_sf() |>
      st_transform(crs) |>
      mutate(area = st_area(geometry)) |>
      st_set_agr("constant") |>
      select(-ID)
    
    DA_na_columns <-
      DA_n |>
      st_drop_geometry() |>
      select(where(~ all(is.na(.)))) |>
      colnames()
    
    DA_na_columns <- if (length(DA_na_columns) != 0) {
      c(DA_na_columns, paste0(DA_na_columns, "_parent"))
    }
    
    DA_n <-
      DA_n |>
      select(-all_of(DA_na_columns))
    
    interpolated_ids <-
      CSD |>
      select(ID) |>
      filter(str_detect(ID, "_")) |>
      st_transform(32618) |>
      st_set_agr("constant") |>
      st_intersection(DA_n, .) |>
      filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
      mutate(
        int_area = units::drop_units(st_area(geometry)),
        area_prop = int_area / units::drop_units(area),
        .before = geometry
      ) |>
      mutate(across(any_of(data_agg$var_add), ~ {
        .x * area_prop
      })) |>
      st_drop_geometry() |>
      group_by(ID)
    
    DA_out <-
      interpolated_ids |>
      # agg_avg has to be calculated first, so parent vectors are untouched!
      summarize(across(
        any_of(data_agg$var_avg), agg_avg,
        eval(parse(text = paste0(cur_column(), "_parent"))),
        int_area
      ),
      across(any_of(data_agg$var_add), agg_add, area_prop, int_area),
      .groups = "drop"
      ) |>
      filter(str_detect(ID, "_"))
    
    if (length(DA_na_columns) > 0) {
      # Get geometry and areas of already-interpolated DAs.
      CT_n <-
        df_list$CT[[as.character(year)]] |>
        left_join(select(CT, ID), by = "ID") |>
        st_as_sf() |>
        st_transform(crs) |>
        mutate(area = st_area(geometry)) |>
        st_set_agr("constant") |>
        select(all_of(DA_na_columns), area)
      
      interpolated_ids <-
        CSD |>
        select(ID) |>
        filter(str_detect(ID, "_")) |>
        st_transform(crs) |>
        st_set_agr("constant") |>
        st_intersection(CT_n, .) |>
        filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
        mutate(
          int_area = units::drop_units(st_area(geometry)),
          area_prop = int_area / units::drop_units(area),
          .before = geometry
        ) |>
        mutate(across(any_of(data_agg$var_add), ~ {
          .x * area_prop
        })) |>
        st_drop_geometry() |>
        group_by(ID)
      
      CT_out <-
        interpolated_ids |>
        # agg_avg has to be calculated first, so parent vectors are untouched!
        summarize(across(
          any_of(data_agg$var_avg), agg_avg,
          eval(parse(text = paste0(cur_column(), "_parent"))),
          int_area
        ),
        across(any_of(data_agg$var_add), agg_add, area_prop, int_area),
        .groups = "drop"
        ) |>
        filter(str_detect(ID, "_"))
      
      left_join(DA_out, CT_out, by = "ID")
    } else {
      DA_out
    }
  })
  
  borough_data <- map(set_names(years), function(year) {
    # Get geometry and areas of interpolated DAs.
    CSD_n <- df_list$CSD[[as.character(year)]]
    borough_n <- borough_data[[as.character(year)]]
    bind_rows(filter(CSD_n, !str_detect(ID, "_")), borough_n)
  })
  
  df_list$CSD <- borough_data
  
  df_list
}


# Interpolate to grid -----------------------------------------------------

interpolate_other <- function(df_list, targets, years, crs = 32618, data_agg) {
  
  # Check if targets exist
  stopifnot(map_lgl(targets, exists))

  pb <- progressr::progressor(steps = sum(sapply(df_list$DA, nrow)) * 
                                length(targets))

  new_geos <-
    map(set_names(targets), function(geo) {
      map(set_names(years), function(year) {
        DA_n <- df_list$DA[[as.character(year)]]
        pb(amount = nrow(DA_n))

        # Get geometry and areas of already-interpolated DAs.
        DA_n <-
          DA_n |>
          left_join(select(DA, ID), by = "ID") |>
          st_as_sf() |>
          st_transform(crs) |>
          mutate(area = st_area(geometry)) |>
          st_set_agr("constant") |>
          select(-ID) |> 
          st_make_valid()
        
        DA_na_columns <-
          DA_n |>
          st_drop_geometry() |>
          select(where(~ all(is.na(.)))) |>
          colnames()
        
        DA_na_columns <- if (length(DA_na_columns) != 0) {
          c(DA_na_columns, paste0(DA_na_columns, "_parent"))
        }
        
        DA_n <-
          DA_n |>
          select(-all_of(DA_na_columns))

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
          st_transform(crs) |>
          st_set_agr("constant") |>
          st_intersection(DA_n)
        
        if (geom_type == "polygon") {
          interpolated_ids <- 
            interpolated_ids |> 
              mutate(int_area = units::drop_units(st_area(geometry)),
                     area_prop = int_area / units::drop_units(area),
                     .before = geometry)
        } else {
          interpolated_ids <- 
            interpolated_ids |> 
            mutate(int_area = units::drop_units(st_length(geometry)),
                   area_prop = int_area / units::drop_units(area),
                   .before = geometry)
        }
        
        interpolated_ids <- 
          interpolated_ids |>
          mutate(across(any_of(data_agg$var_add) | ends_with("_parent"), 
                        ~{.x * area_prop})) |>
          st_drop_geometry() |>
          group_by(ID)

        DA_out <- interpolated_ids |>
          # agg_avg has to be calculated first, so parent vectors are untouched!
          summarize(across(any_of(data_agg$var_avg), agg_avg,
                           eval(parse(text = paste0(cur_column(), "_parent"))),
                           int_area),
                    across(any_of(data_agg$var_add), agg_add, area_prop, 
                           int_area, other_geom = TRUE), .groups = "drop")
      
        if (length(DA_na_columns) > 0) {
          # Get geometry and areas of already-interpolated DAs.
          CT_n <-
            df_list$CT[[as.character(year)]] |>
            left_join(select(CT, ID), by = "ID") |>
            st_as_sf() |>
            st_transform(crs) |>
            mutate(area = st_area(geometry)) |>
            st_set_agr("constant") |>
            select(all_of(DA_na_columns), area) |> 
            st_make_valid()
          
          interpolated_ids <-
            get(geo) |>
            select(ID) |>
            st_transform(crs) |>
            st_set_agr("constant") |>
            st_intersection(CT_n)
          
          if (geom_type == "polygon") {
            interpolated_ids <- 
              interpolated_ids |> 
              mutate(int_area = units::drop_units(st_area(geometry)),
                     area_prop = int_area / units::drop_units(area),
                     .before = geometry)
          } else {
            interpolated_ids <- 
              interpolated_ids |> 
              mutate(int_area = units::drop_units(st_length(geometry)),
                     area_prop = int_area / units::drop_units(area),
                     .before = geometry)
          }
          
          interpolated_ids <- 
            interpolated_ids |>
            mutate(across(any_of(data_agg$var_add) | ends_with("_parent"), 
                          ~{.x * area_prop})) |>
            st_drop_geometry() |>
            group_by(ID)
          
          CT_out <- interpolated_ids |>
            # agg_avg has to be calculated first, so parent vectors are untouched!
            summarize(across(any_of(data_agg$var_avg), agg_avg,
                             eval(parse(text = paste0(cur_column(), "_parent"))),
                             int_area),
                      across(any_of(data_agg$var_add), agg_add, area_prop, 
                             int_area, other_geom = TRUE), .groups = "drop")
          
          left_join(DA_out, CT_out, by = "ID")
        } else {
          DA_out
        }
        })
    })

  new_geos
}
