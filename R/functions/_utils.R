#### UTILS #####################################################################

# convert_unit ------------------------------------------------------------

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
  } else if (max(abs(x)) >= 100 || all(round(x) == x)) {
    x <- scales::comma(x, 1)
  } else if (max(abs(x)) >= 10) {
    x <- scales::comma(x, 0.1)
  } else x <- scales::comma(x, 0.01)
  
  x
}


# return_closest_year -----------------------------------------------------

return_closest_year <- function(var, df, build_str_as_DA = TRUE) {
  
  # Not to do for grid - always 2016
  if (is_scale_in_df("grid", df)) return(var)
  
  if (build_str_as_DA && is_scale_in_df("building", df)) 
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
  

# ordinal_form ------------------------------------------------------------

ordinal_form <- function(r, x) {
  # English ordinal form
  if (is.null(getDefaultReactiveDomain()) || r$lang() == "en") {
    if (x > 20) {
      if (x %% 100 %in% c(11 , 12, 13)) {
        form <- "th "
      } else {
        form <- switch(as.character(x %% 10), "1" = "st ", "2" = "nd ",
                       "3" = "rd ", "th ")
      }
      paste0(x, form)
    } else {
      switch(as.character(x), "1" = "", "2" = "second ",
             "3" = "third ", "4" = "fourth ", "5" = "fifth ", 
             "6" = "sixth ",  "7" = "seventh ", "8" = "eighth ", 
             "9" = "ninth ", "10" = "tenth ",
             paste0(as.character(x), "th "))
    }
  } else {
    # French ordinal form
    switch(as.character(x), "1" = "", "2" = "deuxième ",
           "3" = "troisième ", paste0(as.character(x), "ième "))
  }
}


# vec_dep -----------------------------------------------------------------

# Reimplemntation of purrr::vec_depth in base R
vec_dep <- function(x) {
  if (is.null(x)) {
    0L
  } else if (is.atomic(x)) {
    1L
  } else if (is.list(x)) {
    depths <- vapply(x, vec_dep, vector("integer", 1))
    1L + max(depths, 0L)
  } else {
    abort("`x` must be a vector")
  }
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


# Logical if scale is in df -----------------------------------------------

is_scale_in_df <- function(scales, df) {
  scls <- paste0(scales, "$", collapse = "|")
  grepl(scls, df)
}


