#### UTILS #####################################################################

# return_closest_year -----------------------------------------------------

return_closest_year <- function(var, df, build_str_as_DA = TRUE) {
  
  # Not to do for grid - always 2016
  if (curbcut::is_scale_df("grid", df)) return(var)
  
  if (build_str_as_DA && curbcut::is_scale_df("building", df)) 
    df <- paste0(gsub("building", "DA", df))
  
  avail <- tables_in_sql[[df]][grepl(gsub("_\\d{4}$", "", var), 
                                     tables_in_sql[[df]])]
  
  if (!var %in% avail) {
    
    time <- as.numeric(str_extract(var, "\\d{4}"))
    
    x <-
      avail |> 
      str_subset(str_remove(var, "_\\d{4}$")) |> 
      str_extract("\\d{4}$") |> 
      as.numeric() |> 
      na.omit()
    
    closest_year <- x[which.min(abs(x - time))]
    out <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
    out <- sub("_$", "", out)
    
  } else out <- var
  
  return(out)
  
}


# find_outliers -----------------------------------------------------------

find_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- (q3 - q1) * 1.5
  which(x < q1 - iqr | x > q3 + iqr)
}


# remove_outliers ---------------------------------------------------------

remove_outliers <- function(x) {
  fo <- find_outliers(x)
  out <- if (length(fo) == 0) x else x[-find_outliers(x)]
  out[!is.na(out)]
}


# remove_outliers_df ------------------------------------------------------

remove_outliers_df <- function(x, var_1, var_2 = NULL) {
  
  left_na <- which(is.na(x[[var_1]]))
  left_out <- find_outliers(x[[var_1]])
  if (!is.null(var_2)) {
    right_na <- which(is.na(x[[var_2]]))
    right_out <- find_outliers(x[[var_2]])  
  } else {
    right_na <- NULL
    right_out <- NULL
  }
  comb <- unique(c(left_na, right_na, left_out, right_out))
  if (length(comb) == 0) x else x[-comb,]
  
}

