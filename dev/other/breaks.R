### BREAK FUNCTIONS #######################################################

# q3 breaks ---------------------------------------------------------------

add_q3 <- function(df) {
  mutate(df, across(c(-ID), ntile, 3, .names = "{.col}_q3"))
}

get_breaks_q3 <- function(df, var_list = NULL) {
  
  # Automatically retrieve var_list if var_list is NULL
  if (is.null(var_list)) {
    var_list <- names(select(df, -contains(c("q3", "q5")), -ID))
  }
  
  map_dfc(var_list, ~{
    if (.x %in% names(df)) {
      suppressWarnings(
        df |>
          select(any_of(.x), any_of(paste0(.x, "_q3"))) |>
          set_names(c("v", "q3")) |>
          summarize(
            ranks = c(min(v, na.rm = TRUE),
                      min(v[q3 == 2], na.rm = TRUE),
                      min(v[q3 == 3], na.rm = TRUE),
                      max(v, na.rm = TRUE))) |>
          mutate(ranks = if_else(is.infinite(ranks), NA_real_, ranks)) |> 
          set_names(.x))
    }
  })
}


# q5 breaks ---------------------------------------------------------------

add_q5 <- function(df, breaks) {
  map2_dfc(names(breaks), breaks, function(x, y) {
    df |>
      transmute(across(all_of(x),
                       ~ as.numeric(cut(.x, y, include.lowest = TRUE)),
                       .names = "{.col}_q5"))
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
    var_list <- names(select(df, -contains(c("q3", "q5")), -ID))
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
  
  pmap_dfc(list(cat_min, cat_max, var_list), function(x, y, z) {
    breaks <- find_breaks_q5(x, y)
    tibble(v = breaks) |>
      set_names(z)
  })
  
}
