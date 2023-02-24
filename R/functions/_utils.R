#### UTILS #####################################################################


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

