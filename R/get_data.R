#### GET DATA ##################################################################

get_data <- function(df, var_left, var_right, island = FALSE,
                     point_df = NULL) {
  
  ## Setup ---------------------------------------------------------------------
  # Error checking
  stopifnot(!is.reactive(df))
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  stopifnot(!is.reactive(island))
  
  # Get data type
  data_type <- get_data_type(df, var_left, var_right)

  # Are var_left and var_right the same column?
  if (all(var_left == var_right)) {
    stop("`var_left` and `var_right` are the same.")
  }
  
  # Get time format; eventually this might need to be conditional
  time_format <- "_\\d{4}$"
  
  # Facilitate code legibility by pre-creating q3/q5 column names
  left_q3 <- paste0(str_remove(all_of(var_left), time_format), "_q3", 
                    na.omit(str_extract(var_left, time_format)))
  right_q3 <- paste0(str_remove(all_of(var_right), time_format), "_q3", 
                     na.omit(str_extract(var_right, time_format)))
  left_q5 <- paste0(str_remove(all_of(var_left), time_format), "_q5", 
                    na.omit(str_extract(var_left, time_format)))
  right_q5 <- paste0(str_remove(all_of(var_right), time_format), "_q5", 
                     na.omit(str_extract(var_right, time_format)))
  
  
  ## Return data ---------------------------------------------------------------
  
  data <- get_data_table(df, var_left, var_right, data_type, left_q3, right_q3,
                         left_q5, right_q5, time_format, point_df)
  
  ## Filter to island ----------------------------------------------------------
  
  island_CSDUID <- 
    c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
      "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
      "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
      "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
      "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
      "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
      "2466142", "2466072", "2466023")
  
  if (island && df %in% c("borough", "CT", "DA", "grid", "street", "building"))
    data <- filter(data, CSDUID %in% island_CSDUID)
  
  # Return output ----------------------------------------------------------
  
  return(data)
  
}
