### BREAK FUNCTIONS #######################################################

# q3 breaks ---------------------------------------------------------------

add_q3 <- function(df) {
  
  out <- 
    mutate(df, across(c(-any_of(c("ID", "name"))), ntile, 3, .names = "{.col}_q3"))
  
  names(out)[str_detect(names(out), "_\\d{4}_q3")] <-
    paste0(str_remove(names(out)[str_detect(names(out), "_\\d{4}_q3")],
                      "_\\d{4}_q3"),
           "_q3", 
           str_extract(names(out)[str_detect(names(out), "_\\d{4}_q3")],
                       "_\\d{4}(?=_q3)"))
  
  out
}

get_breaks_q3 <- function(df, var_list = NULL) {
  
  # Automatically retrieve var_list if var_list is NULL
  if (is.null(var_list)) {
    var_list <- names(dplyr::select(df, -contains(c("q3", "q5")), 
                                    -any_of(c("ID", "name"))))
  }
  
  map_dfc(var_list, function(var) {
    if (var %in% names(df)) {
      suppressWarnings(
        df |>
          dplyr::select(any_of(var), any_of(paste0(var, "_q3")) |
                   any_of(paste0(str_remove(var, "_\\d{4}$"), 
                                 paste0("_q3", str_extract(var, "_\\d{4}$"))))
          ) |>
          set_names(c("v", "q3")) |>
          summarize(
            ranks = as.numeric(c(min(v, na.rm = TRUE),
                      min(v[q3 == 2], na.rm = TRUE),
                      min(v[q3 == 3], na.rm = TRUE),
                      max(v, na.rm = TRUE)))) |>
          mutate(ranks = if_else(is.infinite(ranks), NA_real_, ranks)) |> 
          set_names(var)
      )
    }
  })
}


# q5 breaks ---------------------------------------------------------------

add_q5 <- function(df, breaks) {
  map2_dfc(names(breaks), breaks, function(x, y) {
    y[1] <- -Inf
    y[length(y)] <- Inf
    
    # Check any possible year suffixes
    var_names <- c(x, paste(x, 1900:2100, sep = "_"))
    
    out <- 
    df |> 
      dplyr::transmute(across(any_of(var_names),
                       ~ as.numeric(cut(.x, y, include.lowest = TRUE)),
                       .names = "{.col}_q5"))
    
    names(out)[str_detect(names(out), "_\\d{4}_q5")] <-
      paste0(str_remove(names(out)[str_detect(names(out), "_\\d{4}_q5")],
                        "_\\d{4}_q5"),
             "_q5", 
             str_extract(names(out)[str_detect(names(out), "_\\d{4}_q5")],
                         "_\\d{4}(?=_q5)"))
    
    out
    
  })
}

find_breaks_q5 <- function(min_val, max_val) {
  breaks <- unlist(map(-3:7, ~ {
    (10 ^ .x) * c(1, 1.5, 2, 2.5, 3, 4, 5, 6)
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
    var_list <- 
      df |> 
      dplyr::select(-contains(c("q3", "q5")), -any_of(c("ID", "name"))) |> 
      names()
  }
  
  cat_min <- suppressWarnings(
    map(var_list, ~{
      df |>
        dplyr::select(all_of(.x)) |>
        as.matrix() |>
        min(na.rm = TRUE)
    }))

  cat_max <- suppressWarnings(
    map(var_list, ~{
      df |>
        dplyr::select(all_of(.x)) |>
        as.matrix() |>
        max(na.rm = TRUE)
    }))
  
  var_mean <- suppressWarnings(
    map(var_list, ~{
      df |>
        dplyr::select(all_of(.x)) |>
        filter(if_all(everything(), 
                      ~between(.x, quantile(.x, .01, na.rm = TRUE), 
                               quantile(.x, .99, na.rm = TRUE)))) |> 
        as.matrix() |>
        mean(na.rm = TRUE)
    }))

  standard_d <- suppressWarnings(
    map(var_list, ~{
      df |>
        dplyr::select(all_of(.x)) |>
        filter(if_all(everything(), 
                      ~between(.x, quantile(.x, .01, na.rm = TRUE), 
                               quantile(.x, .99, na.rm = TRUE)))) |> 
        as.matrix() |>
        sd(na.rm = TRUE)
    }))
  
  breaks <- pmap(
    list(cat_min, cat_max, var_mean, standard_d), \(x, y, a, b) {
      breaks <- find_breaks_q5(max(a - (4 * b), x), min(a + (4 * b), y))
      tibble(v = breaks, .name_repair = "minimal")}) |> 
    bind_cols(.name_repair = "minimal") |> 
    map2_dfc(var_list, ~{
      map(.y, \(z) tibble(z = .x)) |> 
        bind_cols(.name_repair = "minimal") |> 
        set_names(.y)})
  
}
