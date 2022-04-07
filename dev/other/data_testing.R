#### SUS DATA TESTING ########################################################
#' @data is a named list of all the dataframes to evaluate. They must be the df
#' version prior to the joining by IDs to the usual scales.

data_testing <- function(data, ignore_year_diff = FALSE) {
  
  # List to store all the possible warnings
  warn_vec <- c()
  
  # Take q3s, q5s and all IDs out
  data <- map(data, ~{
    ids_col <- .x[, str_detect(names(.x), "ID")] |> names()
    .x |> 
      select(!contains(c("q3", "q5")), -any_of(ids_col))
  })
  
  # Prepare variables
  vars <- map(data, names) |> unlist() |> unique()
  dates <- str_extract(vars, "\\d{4}$") |> unique()
  vars_no_date <- str_remove(vars, "_\\d{4}$") |> unique()
  known_suffix <- c("pct", "avg", "median", "per1k", "sqkm", "ind", "count")
  
  # If no known suffix are detected
  if (!all(str_detect(vars, paste0(known_suffix, collapse = "|")))) {
    unknown_suff <- 
      vars[!str_detect(vars, paste0(known_suffix, collapse = "|"))]
    warn_vec <- 
      c(warn_vec, 
        paste0("There are no known suffix for `", unknown_suff, "`, ", 
               "which are ", paste(known_suffix, collapse = ", ")))
  }
  
  vars_pct <- str_subset(vars, "_pct")
  
  # Percentages need no to be under 0s, or above 1
  if (length(vars_pct) > 0 ) {
    warn_vec <- 
      c(warn_vec, 
        map(set_names(vars_pct), function(var) {
          out <- map(data, function(dat) {
            if (var %in% names(dat)) {
              out <- 
                dat |>
                select(any_of(var)) |> 
                set_names("new_var") |> 
                filter(new_var < 0 | new_var > 1)
              
              out <- if (nrow(out) > 0) paste0(var) else NULL
            } else NULL
          }) 
          out <- compact(out)
          out_scale <- names(out)
          if (length(out) > 0) {
            paste0("`", out, 
                   "`, a `_pct` variable, is under 0 or above 1 in `", 
                   out_scale, "`")
          }
        }) |> reduce(c))
  }
  
  # Are the mean for each variable, between years, similar?
  if (ignore_year_diff == FALSE) {
  if (all(!is.na(dates))) {
    warn_vec <- 
      c(warn_vec, 
        map2(data, names(data), function(dat, df_name) {
          out <- map(set_names(vars_no_date), function(var) {
            out <- 
              data[[1]] |>
              select(matches(paste0(var,"_\\d{4}"))) |> 
              rename_with(~str_extract( ., "\\d{4}$"), everything()) |> 
              summarize(across(everything(), mean, na.rm = T)) |> 
              pivot_longer(everything()) |> 
              mutate(diff = abs((value - lag(value)) / abs(lag(value)))) |> 
              filter(diff >= 1) |> 
              select(date = name, diff)
            
            out <- if (nrow(out) > 0) c(pull(out, date)) else NULL
          })
          date <- compact(out)
          out_var <- names(date)
          
          if (length(out) > 0) {
            map2(date, out_var, ~{
              paste0("`", .y,
                     "`, has an absolute average difference higher than 1 between `", 
                     .x, "` and the year prior in `", df_name, "`.")
            })
          }
        }) |> unlist() |> unname())
  }
  }
  
  if (length(warn_vec) > 0 ) {
    # Display all warning/error messages, be sure that they can be fixed all
    # at once. 8 lines fit before truncated.
    if (length(warn_vec) > 8) {
      lines_8 <- str_which(1:length(warn_vec) %% 8, "0")
      all_errors <- map(lines_8, ~{
        warn_vec[eval(parse(text = paste0(c(.x-7, .x), collapse = ":")))]
      })
      if (!length(warn_vec) %in% lines_8) {
        all_errors <- 
          c(all_errors, 
            list(warn_vec[(lines_8[length(lines_8)] + 1):length(warn_vec)]))
      }
    } else {
      all_errors <- warn_vec
    }
    map(all_errors, ~{
      warning(paste(.x, collapse = "\n  "))
    })
    stop("These warnings relate to problems in the data.")
    
  }
}

