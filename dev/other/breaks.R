### BREAK FUNCTIONS #######################################################

# q3 breaks ---------------------------------------------------------------

add_q3 <- function(df) {
  mutate(df, across(c(-any_of("ID")), ntile, 3, .names = "{.col}_q3")) |> 
    rename_with(~paste0(str_remove(., "_\\d{4}"),
                        str_extract(., "_\\d{4}")), matches("_\\d{4}"))
}

get_breaks_q3 <- function(df, var_list = NULL) {
  
  # Automatically retrieve var_list if var_list is NULL
  if (is.null(var_list)) {
    var_list <- names(select(df, -contains(c("q3", "q5")), -any_of("ID")))
  }
  
  map_dfc(var_list, ~{
    if (.x %in% names(df)) {
      suppressWarnings(
        df |>
          select(any_of(.x), any_of(paste0(.x, "_q3")) |
                   any_of(paste0(str_remove(.x, "_\\d{4}"), 
                                 paste0("_q3", str_extract(.x, "_\\d{4}"))))
          ) |>
          set_names(c("v", "q3")) |>
          summarize(
            ranks = c(min(v, na.rm = TRUE),
                      min(v[q3 == 2], na.rm = TRUE),
                      min(v[q3 == 3], na.rm = TRUE),
                      max(v, na.rm = TRUE))) |>
          mutate(ranks = if_else(is.infinite(ranks), NA_real_, ranks)) |> 
          set_names(.x)
      )
    }
  })
}


# q5 breaks ---------------------------------------------------------------

add_q5 <- function(df, breaks) {
  map2_dfc(names(breaks), breaks, function(x, y) {
    y[1] <- -Inf
    y[length(y)] <- Inf
    
    var_names <- c(x, paste(x, 1900:2100, sep = "_"))
    
    df |> 
      transmute(across(any_of(var_names),
                       ~ as.numeric(cut(.x, y, include.lowest = TRUE)),
                       .names = "{.col}_q5")) |> 
      rename_with(~paste0(str_remove(., "_\\d{4}"),
                          str_extract(., "_\\d{4}")), matches("_\\d{4}"))
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
  new_min <- floor(min_val / (10 ^ break_digits)) * 10 ^ break_digits
  
  return(c(new_min + 0:5 * break_val))
}

get_breaks_q5 <- function(df, var_list = NULL) {
  
  # Automatically retrieve var_list if var_list is NULL
  if (is.null(var_list)) {
    var_list <- names(select(df, -contains(c("q3", "q5")), -any_of("ID")))
  }
  
  cat_min <- suppressWarnings(
    map(var_list, ~{
      df |>
        select(all_of(.x)) |>
        as.matrix() |>
        min(na.rm = TRUE)
    }))

  cat_max <- suppressWarnings(
    map(var_list, ~{
      df |>
        select(all_of(.x)) |>
        as.matrix() |>
        max(na.rm = TRUE)
    }))
  
  var_mean <- suppressWarnings(
    map(var_list, ~{
      df |>
        select(all_of(.x)) |>
        filter_all(all_vars(between(., quantile(., .01, na.rm = T), 
                                    quantile(., .99, na.rm = T)))) |> 
        as.matrix() |>
        mean(na.rm = TRUE)
    }))

  standard_d <- suppressWarnings(
    map(var_list, ~{
      df |>
        select(all_of(.x)) |>
        filter_all(all_vars(between(., quantile(., .01, na.rm = T), 
                                    quantile(., .99, na.rm = T)))) |> 
        as.matrix() |>
        sd(na.rm = TRUE)
    }))
  
  pmap_dfc(list(cat_min, cat_max, var_list,
                var_mean, standard_d), function(x, y, z, a, b) {
    breaks <- find_breaks_q5(max(a - (4 * b), x), 
                             min(a + (4 * b), y))
    tibble(v = breaks) |>
      set_names(z)
  })
  
}
