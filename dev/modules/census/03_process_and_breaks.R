#### CENSUS DOWNLOAD AND PROCESSING FUNCTIONS ##################################

# Retrieve units type -----------------------------------------------------

get_unit_type <- function(census_vec, scales, years) {
  
  # Skip 2001 Census; some vectors have `number` units instead of `currency`
  # cancensus::list_census_vectors("CA01") |>
  #   filter(vector == "v_CA01_1674") |>
  #   select(label, units)
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
      mutate(units = str_extract(units, ".[^ ]*")) |>
      mutate(var_code = names(vec_named)) |>
      select(var_code, units) |> 
      mutate(var_code = ifelse(str_detect(var_code, "\\d$"), 
                               str_remove(var_code, "\\d*$"), var_code)) |> 
      mutate(across(everything(), as.character)) |> 
      distinct()
  })
  
  var_units <-
    for_years |> 
    reduce(left_join, by = "var_code") |>
    pivot_longer(!var_code) |>
    filter(!is.na(value)) |>
    group_by(var_code) |>
    summarize(units = list(value)) |>
    rowwise() |>
    mutate(units = list(unique(units))) |> 
    ungroup()

  if (all(lengths(pull(var_units)) == 1)) {
    var_units <-
      var_units |>
      unnest(units)
  } else {
    non_unique <-
      var_units[(lengths(pull(var_units)) > 1), ] |>
      pull(var_code)
    stop(paste0("Different `units` types detected for `", non_unique, "`.\n"))
  }

  var_units
}


# Normalize percentage variables ------------------------------------------

normalize <- function(df_list, census_vec, data_unit) {
  
  stopifnot(exists("data_unit"))
  
  map(df_list, function(df_l) {
    map(df_l, function(df) {
      map_dfc(names(df), ~{
        
        numerator <-
          census_vec |>
          filter(var_code == .x, str_detect(var_code, "_pct")) |>
          pull(var_code)

        numerator <-
          data_unit |>
          filter(var_code %in% numerator) |>
          mutate(out = set_names(var_code, units)) |>
          pull(out)

        if (length(numerator) > 0 && !names(numerator) %in% 
            c("Number", "Percentage")) {
          stop(paste0(
            "numerator ", numerator, " isn't classified as 'Number' or ",
            " as 'Percentage' by the census, but as '", names(numerator),
            "'. Should it really have the `_pct` suffix?"
          ))
        }

        if (length(numerator) > 0 && !is.na(numerator)) {
          if (names(numerator) == "Percentage") {
            # Some census variables are already percentages.
            df |>
              mutate(across(all_of(.x), ~ {
                pmin(1, . / 100)
              })) |>
              select(all_of(.x))
          } else {
            # If the variable is classified as Number and has _pct suffix,
            # it must be normalized with its parent variable.
            denom <- paste0(numerator, "_parent")

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
      out <- full_join(df, df_q3, by = names(df))
      if (nrow(df_q5) > 0) out <- bind_cols(out, df_q5)
      out
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
