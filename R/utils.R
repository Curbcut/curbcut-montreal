#### UTILS #####################################################################

convert_unit <- function(x, var_name = NULL, compact = FALSE) {
  
  if (length(x) == 0) return(x)
  if (length(x) == 1 && is.na(x)) return(x)
  # TKTK SHOULD THIS BE MAX DIGIT INSTEAD??
  if (compact) min_dig <- 
      x |> 
      setdiff(0) |> 
      abs() |> 
      min(na.rm = TRUE) |> 
      log10() |> 
      ceiling()
  
  if (!missing(var_name) && grepl("_pct", var_name)) {
    x <- paste0(round(x * 100, 1), "%")
  } else if (!missing(var_name) && grepl("_dollar", var_name) && compact) {
    if (min_dig >= 10) {
      x <- scales::dollar(x, 1, scale = 1 / 1e+09, suffix = "B")  
    } else if (min_dig >= 7) {
      x <- scales::dollar(x, 1, scale = 1 / 1e+06, suffix = "M")  
    } else if (min_dig >= 4) {
      x <- scales::dollar(x, 1, scale = 1 / 1e+03, suffix = "K")  
    } else x <- scales::dollar(x, 1)
  } else if (!missing(var_name) && grepl("_dollar", var_name)) {
    x <- scales::dollar(x, 1)
  } else if (compact && min_dig >= 4) {
    if (min_dig >= 10) {
      x <- scales::comma(x, 1, scale = 1 / 1e+09, suffix = "B")  
    } else if (min_dig >= 7) {
      x <- scales::comma(x, 1, scale = 1 / 1e+06, suffix = "M")  
    } else if (min_dig >= 4) {
      x <- scales::comma(x, 1, scale = 1 / 1e+03, suffix = "K")  
    }
  } else if (max(abs(x)) >= 100) {
    x <- scales::comma(x, 1)
  } else if (max(abs(x)) >= 10) {
    x <- scales::comma(x, 0.1)
  } else x <- scales::comma(x, 0.01)
  
  x
}

# ------------------------------------------------------------------------------
  
return_closest_year <- function(var, df = "borough") {
  
  if (df == "building") df <- DA else df <- get(df)
  
  if (!var %in% names(df)) {
    
    time <- as.numeric(str_extract(var, "\\d{4}"))
    
    x <- 
      df |> 
      select(contains(str_remove(var, "_\\d{4}$"))) |> 
      names() |> 
      str_extract("\\d{4}$") |> 
      as.numeric() |> 
      na.omit()
    
    closest_year <- x[which.min(abs(x - time))]
    var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
    var <- sub("_$", "", var)
    
  }
  
  return(var)
  
}

# ------------------------------------------------------------------------------

find_outliers <- function(x) {
  
  outside_quant <- 
    between(x, quantile(x, .01, na.rm = TRUE), 
            quantile(x, .99, na.rm = TRUE))
  
  inside <- x[outside_quant]
  
  avg <- mean(inside, na.rm = TRUE) 
  standard_d <- sd(inside, na.rm = TRUE)
  
  no_outliers <- 
    inside[between(inside, avg - 4 * standard_d, avg + 4 * standard_d)]
  
  x[!x %in% no_outliers] |> na.omit()
  
}
