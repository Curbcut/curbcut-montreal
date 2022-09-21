#### GET DATA TABLE ############################################################

get_data_table <- function(df, geo, var_left, var_right, data_type, 
                           point_df = FALSE) {
  
  # Get time format; eventually this might need to be conditional
  time_format <- "\\d{4}$"
  
  # Pre-create q3/q5 column names
  l_q3 <- paste0(str_remove(var_left, time_format), 
                 if (str_detect(var_left[1], time_format)) "q3_" else "_q3", 
                 na.omit(str_extract(var_left, time_format)))
  r_q3 <- paste0(str_remove(var_right, time_format), 
                 if (str_detect(var_right[1], time_format)) "q3_" else "_q3", 
                 na.omit(str_extract(var_right, time_format)))
  l_q5 <- paste0(str_remove(var_left, time_format), 
                 if (str_detect(var_left[1], time_format)) "q5_" else "_q5", 
                 na.omit(str_extract(var_left, time_format)))
  r_q5 <- paste0(str_remove(var_right, time_format), 
                 if (str_detect(var_right[1], time_format)) "q5_" else "_q5", 
                 na.omit(str_extract(var_right, time_format)))
  
  # Univariate
  if (data_type == "q5") {
    
    data <- 
      if (df == "grid") {
        dbGetQuery(db, paste(paste("SELECT ID, name, name_2, CSDUID, population", 
                                   var_left, l_q3, l_q5, sep = ", "), "FROM grid"))
      } else {
        data <- get(df)
        if (df %in% c("CT", "DA")) data <- data[data[[geo]], ]
        data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                       if (df %in% c("DA", "CT")) "CTUID", 
                       if (df %in% c("DA", "CT", "borough")) "CSDUID", "population", 
                       var_left, l_q3, l_q5)]
      }
    
    data <- 
      data |> 
      setNames(c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                 if (df %in% c("DA", "CT")) "CTUID", 
                 if (df %in% c("DA", "CT", "borough", "grid")) "CSDUID", "population",
                 "var_left", "var_left_q3", "var_left_q5"))
  }
  
  # Building univariate
  if (data_type == "building_q5") {
    data <- DA
    data <- data[data[[geo]], ]
    data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", var_left, l_q3, l_q5)] |> 
      setNames(c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                 "population", "var_left", "var_left_q3", "var_left_q5"))
  }
  
  # Bivariate
  if (data_type == "bivar") {
    data <- 
      if (df == "grid") {
        dbGetQuery(db, paste(paste("SELECT ID, name, name_2, CSDUID, population", 
                                   var_left, l_q3, l_q5, var_right, r_q3, r_q5, 
                                   sep = ", "), "FROM grid"))
    } else {
      data <- get(df)
      if (df %in% c("CT", "DA")) data <- data[data[[geo]], ]
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", 
                     if (df %in% c("DA", "CT", "borough")) "CSDUID", "population", 
                     var_left, l_q3, l_q5, var_right, r_q3, r_q5)]
    }
    
    data <- 
    data |> 
      setNames(c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                 if (df %in% c("DA", "CT")) "CTUID", 
                 if (df %in% c("DA", "CT", "borough", "grid")) "CSDUID", "population", 
                 "var_left", "var_left_q3", "var_left_q5", "var_right", 
                 "var_right_q3", "var_right_q5"))
    data$group = paste(data$var_left_q3, data$var_right_q3, sep = " - ")
  }
  
  # Building bivariate
  if (data_type == "building_bivar") {
    data <- DA
    data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", var_left, l_q3, l_q5, var_right, r_q3, 
                   r_q5)] |> 
      setNames(c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                 "population", "var_left", "var_left_q3", "var_left_q5", 
                 "var_right", "var_right_q3", "var_right_q5"))
    data$group <- paste(data$var_left_q3, data$var_right_q3, sep = " - ")
  }
  
  # Delta
  if (data_type == "delta") {
    
    data <- get(df)
    if (df %in% c("CT", "DA")) data <- data[data[[geo]], ]
    
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   var_left)]
    
    data$var_left <- (data[[var_left[2]]] - data[[var_left[1]]]) / 
      data[[var_left[1]]]
    
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data_med <- median(abs(data$var_left[abs(data$var_left) > 0.02]), 
                       na.rm = TRUE)
    data$var_left_q3 <- "5"
    data$var_left_q3[data$var_left < data_med] <- "4"
    data$var_left_q3[data$var_left < 0.02] <- "3"
    data$var_left_q3[data$var_left < -0.02] <- "2"
    data$var_left_q3[data$var_left < -1 * data_med] <- "1"
    data$var_left_q3[is.na(data$var_left)] <- NA_character_
    data$group <- data$var_left_q3
    
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   "var_left", "var_left_q3", var_left, "group")]
    names(data) <- c("ID", "name", "name_2", if (df == "DA") "DAUID",
                     if (df %in% c("DA", "CT")) "CTUID", 
                     if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                     "var_left", "var_left_q3", "var_left_1", "var_left_2",
                     "group")
    
  }
  
  # Building delta
  if (data_type == "building_delta") {

    data <- DA
    
    data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", var_left)]
    
    data$var_left <- (data[[var_left[2]]] - data[[var_left[1]]]) / 
      data[[var_left[1]]]
    
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data_med <- median(abs(data$var_left[abs(data$var_left) > 0.02]), 
                       na.rm = TRUE)
    data$var_left_q3 <- "5"
    data$var_left_q3[data$var_left < data_med] <- "4"
    data$var_left_q3[data$var_left < 0.02] <- "3"
    data$var_left_q3[data$var_left < -0.02] <- "2"
    data$var_left_q3[data$var_left < -1 * data_med] <- "1"
    data$var_left_q3[is.na(data$var_left)] <- NA_character_
    data$group <- data$var_left_q3
    
    data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", "var_left", "var_left_q3", var_left, "group")]
    names(data) <- c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                     "population", "var_left", "var_left_q3", "var_left_1", 
                     "var_left_2", "group", "geometry")
  }
  
  # NA_delta
  if (data_type == "NA_delta") {
    data <- get(df)
    if (df %in% c("CT", "DA")) data <- data[data[[geo]], ]
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population")]
    data$group <- "NA"
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_left_1 <- NA
    data$var_left_2 <- NA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID",
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   "var_left", "var_left_q3", "var_left_1", "var_left_2",
                   "group")]
  }
  
  # building_NA_delta
  if (data_type == "building_NA_delta") {
    data <- DA
    data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population")]
    data$group <- "NA"
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_left_1 <- NA
    data$var_left_2 <- NA
    data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", "var_left", "var_left_q3", "var_left_1", 
                   "var_left_2", "group")]
  }
  
  # Delta bivariate
  if (data_type == "delta_bivar") {
    data <- get(df)
    if (df %in% c("CT", "DA")) data <- data[data[[geo]], ]
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   var_left, var_right)]
    data$var_left <- (data[[var_left[2]]] - data[[var_left[1]]]) /
      abs(data[[var_left[1]]])
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data$var_left_q3 <- ntile(data$var_left, 3)
    data$var_right <- (data[[var_right[2]]] - data[[var_right[1]]]) /
      abs(data[[var_right[1]]])
    data$var_right <- replace(data$var_right, is.nan(data$var_right), NA)
    data$var_right <- replace(data$var_right, is.infinite(data$var_right), NA)
    data$var_right_q3 <- ntile(data$var_right, 3)
    data$group <- paste(data$var_left_q3, "-", data$var_right_q3)
    data$var_left_1 <- data[[var_left[1]]]
    data$var_left_2 <- data[[var_left[2]]]
    data$var_right_1 <- data[[var_right[1]]]
    data$var_right_2 <- data[[var_right[2]]]
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID",
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   "var_left", "var_right", "var_left_q3", "var_right_q3",
                   "group")]
  }
    
  # NA_delta_bivar
  if (data_type == "NA_delta_bivar") {
    data <- get(df)
    if (df %in% c("CT", "DA")) data <- data[data[[geo]], ]
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population")]
    data$group <- "NA - NA"
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_right <- NA
    data$var_right_q3 <- NA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID","population",
                   "var_left", "var_left_q3", "var_right", "var_right_q3",
                   "group")]
  }
  
  # Building delta bivariate
  if (data_type == "building_delta_bivar") {
    data <- DA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID",
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   var_left, var_right)]
    data$var_left <- (data[[var_left[2]]] - data[[var_left[1]]]) /
      abs(data[[var_left[1]]])
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data$var_left_q3 <- ntile(data$var_left, 3)
    data$var_right <- (data[[var_right[2]]] - data[[var_right[1]]]) /
      abs(data[[var_right[1]]])
    data$var_right <- replace(data$var_right, is.nan(data$var_right), NA)
    data$var_right <- replace(data$var_right, is.infinite(data$var_right), NA)
    data$var_right_q3 <- ntile(data$var_right, 3)
    data$group <- paste(data$var_left_q3, "-", data$var_right_q3)
    data$var_left_1 <- data[[var_left[1]]]
    data$var_left_2 <- data[[var_left[2]]]
    data$var_right_1 <- data[[var_right[1]]]
    data$var_right_2 <- data[[var_right[2]]]
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   "var_left", "var_right", "var_left_q3", "var_right_q3",
                   "group")]
  }
    
  # building_NA_delta_bivar
  if (data_type == "building_NA_delta_bivar") {
    data <- DA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population")]
    data$group <- "NA - NA"
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_right <- NA
    data$var_right_q3 <- NA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID",
                   if (df %in% c("DA", "CT")) "CTUID", 
                   if (df %in% c("DA", "CT", "borough")) "CSDUID", "population",
                   "var_left", "var_left_q3", "var_right", "var_right_q3",
                   "group")]
  }
    
  # Point data TKTK update with base R
  # if (data_type == "point") {
  #   
  #   type_pattern <- paste(unique(get(point_df)$type), collapse = "|")
  #   selected_type <- str_extract(var_left, type_pattern)
  #   time <- str_extract(var_left, time_format)
  #   
  #   data <- 
  #     point_df |> 
  #     get() %>%
  #     { if (str_detect(var_left[1], "_total_"))
  #       . else filter(., str_detect(type, selected_type))} %>%
  #     { if (length(time) == 2) {
  #       filter(., year %in% time[1]:time[2])
  #     } else {
  #       filter(., year == time)
  #     }}
  # }
  
  # Return data
  return(data) 
  
}
