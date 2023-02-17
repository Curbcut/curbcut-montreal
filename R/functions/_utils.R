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
  
# ntile -------------------------------------------------------------------

ntile <- function(x, n) {
  x <- rank(x, ties.method = "first", na.last = "keep")
  len <- length(x) - sum(is.na(x))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  }
  else {
    n <- as.integer(floor(n))
    n_larger <- as.integer(len %% n)
    n_smaller <- as.integer(n - n_larger)
    size <- len / n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))
    larger_threshold <- larger_size * n_larger
    bins <- ifelse(x <= larger_threshold, 
                   (x + (larger_size - 1L)) / larger_size, 
                   (x + (-larger_threshold + smaller_size - 1L)) / 
                     smaller_size + n_larger)
    as.integer(floor(bins))
  }
}


# Get distance in metres from lon/lat coordinate pairs --------------------

get_dist <- function(x, y) {

  # For consistent indexing
  if (!is.null(dim(x))) x <- as.matrix(x)
  # If x is matrix or df, take lon/lat vectors
  lon_1 <- if (!is.null(dim(x))) x[,1] else x[1]
  lat_1 <- if (!is.null(dim(x))) x[,2] else x[2]
  lon_2 <- y[1]
  lat_1_r <- lat_1 * pi / 180
  lat_2 <- y[2]
  lat_2_r <- lat_2 * pi / 180
  delta_lat <- (lat_2 - lat_1) * pi / 180
  delta_lon <- (lon_2 - lon_1) * pi / 180
  a_dist <- sin(delta_lat / 2) * sin(delta_lat / 2) + cos(lat_1_r) * 
    cos(lat_2_r) * sin(delta_lon / 2) * sin(delta_lon / 2)
  c_dist <- 2 * atan2(sqrt(a_dist), sqrt(1 - a_dist))
  6371e3 * c_dist
}



