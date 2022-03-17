#### GET DATA TABLE ############################################################

get_data_table <- function(df, var_left, var_right, data_type, point_df, new) {
  
  # Get time format; eventually this might need to be conditional
  time_format <- "\\d{4}$"
  
  # Pre-create q3/q5 column names
  left_q3 <- 
    paste0(str_remove(all_of(var_left), time_format), 
           if (str_detect(var_left[1], time_format)) "q3_" else "_q3", 
           na.omit(str_extract(var_left, time_format)))
  right_q3 <- 
    paste0(str_remove(all_of(var_right), time_format), 
           if (str_detect(var_right[1], time_format)) "q3_" else "_q3", 
           na.omit(str_extract(var_right, time_format)))
  left_q5 <- 
    paste0(str_remove(all_of(var_left), time_format), 
           if (str_detect(var_left[1], time_format)) "q5_" else "_q5", 
           na.omit(str_extract(var_left, time_format)))
  right_q5 <- 
    paste0(str_remove(all_of(var_right), time_format), 
           if (str_detect(var_right[1], time_format)) "q5_" else "_q5", 
           na.omit(str_extract(var_right, time_format)))
  
  # Univariate
  if (data_type == "q5") {
    
    data <- get(df)
    
    if (new) {
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     var_left, left_q3, left_q5)] |> 
        st_drop_geometry() |> 
        setNames(c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population",
                   "var_left", "var_left_q3", "var_left_q5"))
      
    } else {
     
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     var_left, left_q3, left_q5, "geometry")]
      data$group <- coalesce(as.character(data[[left_q5]]), "NA")
      data <- left_join(data, colour_left_5, by = "group")
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     var_left, left_q3, left_q5, "group", "fill", "geometry")]
      names(data) <- c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                       if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                       "var_left", "var_left_q3", "var_left_q5", "group", "fill", 
                       "geometry")
      
    }
    
  }
  
  # Building univariate
  if (data_type == "building_q5") {
    
    data <- DA
    
    if (new) {
      data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                     "population", var_left, left_q3, left_q5)] |> 
        st_drop_geometry() |> 
        setNames(c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", "var_left", "var_left_q3", "var_left_q5"))
      
    } else {
      
    data <- 
      data |> 
      st_set_geometry("building") |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left), 
             var_left_q3 = all_of(left_q3),
             var_left_q5 = all_of(left_q5),
             geometry = building) |>
      mutate(group = coalesce(as.character(var_left_q5), "NA"),
             .after = var_left_q5) |> 
      left_join(colour_left_5, by = "group") |> 
      relocate(fill, .after = group)
    }
  }
  
  # Bivariate
  if (data_type == "bivar") {
    data <- get(df)
    
    if (new) {
      
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     var_left, left_q3, left_q5, var_right, right_q3, 
                     right_q5)] |> 
        st_drop_geometry() |> 
        setNames(c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   "var_left", "var_left_q3", "var_left_q5", "var_right", 
                    "var_right_q3", "var_right_q5")) |> 
        mutate(group = paste(var_left_q3, var_right_q3, sep = " - "))
      
    } else {
      
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     var_left, left_q3, left_q5, var_right, right_q3, right_q5, 
                     "geometry")]
      data$group <- paste(data[[left_q3]], "-", data[[right_q3]])
      data <- left_join(data, colour_bivar, by = "group")
      data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     var_left, left_q3, left_q5, var_right, right_q3, right_q5, 
                     "group", "fill", "geometry")]
      names(data) <- c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                       if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                       "var_left", "var_left_q3", "var_left_q5", "var_right", 
                       "var_right_q3", "var_right_q5", "group", "fill", 
                       "geometry")
    }
  }
  
  # Building bivariate TKTK update with base R
  if (data_type == "building_bivar") {
    
    data <- DA
    
    if (new) {
      data <- data[c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                     "population", var_left, left_q3, left_q5, var_right, 
                     right_q3, right_q5)] |> 
        st_drop_geometry() |> 
        setNames(c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                   "population", "var_left", "var_left_q3", "var_left_q5", 
                   "var_right", "var_right_q3", "var_right_q5")) |> 
        mutate(group = paste(var_left_q3, var_right_q3, sep = " - "))
      
      
    } else {
      data <- 
        DA |> 
        st_set_geometry("building") |> 
        select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
               population, var_left = all_of(var_left), 
               var_left_q3 = all_of(left_q3),
               var_left_q5 = all_of(left_q5),
               var_right = all_of(var_right), 
               var_right_q3 = all_of(right_q3),
               var_right_q5 = all_of(right_q5),
               geometry = building) |>
        mutate(group = paste(var_left_q3, "-", var_right_q3)) |>
        left_join(colour_bivar, by = "group") |> 
        relocate(fill, .after = group)
    }
  }
  
  # Delta
  if (data_type == "delta") {
    
    data <- get(df)
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   var_left, "geometry")]
    data$var_left <- (data[[var_left[2]]] - data[[var_left[1]]]) / 
      abs(data[[var_left[1]]])
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data_med <- median(abs(data$var_left[abs(data$var_left) > 0.02]), 
                       na.rm = TRUE)
    data$var_left_q3 <- case_when(
      is.na(data$var_left) ~ NA_character_,
      data$var_left < -1 * data_med ~ "1",
      data$var_left < -0.02 ~ "2",
      data$var_left < 0.02 ~ "3",
      data$var_left < data_med ~ "4",
      TRUE ~ "5")
    
    data$group <- as.character(data$var_left_q3)
    data$group <- paste(if_else(is.na(data$group), "NA", data$group), "- 1")
    data <- left_join(data, colour_delta, by = "group")
    
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   "var_left", "var_left_q3", var_left, "group", "fill", 
                   "geometry")]
    names(data) <- c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                     if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                     "var_left", "var_left_q3", "var_left_1", "var_left_2", 
                     "group", "fill", "geometry")
    
  }
  
  # Building delta TKTK update with base R
  if (data_type == "building_delta") {
    
    data <- 
      DA |> 
      st_set_geometry("building") |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left), geometry = building) |>
      mutate(
        var_left = (var_left2 - var_left1) / abs(var_left1), 
        var_left_q3 = case_when(
          is.na(var_left) ~ NA_character_,
          var_left < -1 * median(abs(var_left[abs(var_left) > 0.02]), 
                                 na.rm = TRUE) ~ "1",
          var_left < -0.02 ~ "2",
          var_left < 0.02 ~ "3",
          var_left < median(abs(var_left[abs(var_left) > 0.02]), 
                            na.rm = TRUE) ~ "4",
          TRUE ~ "5"),
        across(where(is.numeric), ~replace(., is.nan(.), NA)),
        across(where(is.numeric), ~replace(., is.infinite(.), NA))) |>
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left, var_left_q3, var_left_1 = var_left1, 
             var_left_2 = var_left2) |> 
      mutate(group = as.character(var_left_q3),
             group = if_else(is.na(group), "NA", group),
             group = paste(group, "- 1")) |> 
      left_join(colour_delta, by = "group") |> 
      relocate(fill, .after = group)
  }
  
  # NA_delta
  if (data_type == "NA_delta") {
    data <- get(df)
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population",
                   "geometry")]
    data$group <- "NA - 1"
    data <- left_join(data, colour_delta, by = "group")
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_left_1 <- NA
    data$var_left_2 <- NA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   "var_left", "var_left_q3", "var_left_1", "var_left_2", 
                   "group", "fill", "geometry")]
  }
  
  # building_NA_delta TKTK update with base R
  if (data_type == "building_NA_delta") {
    data <- 
      DA |> 
      st_set_geometry("building") |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, geometry = building) |> 
      mutate(var_left = NA, var_left_q3 = NA, var_left_1 = NA,
             var_left_2 = NA, group = "NA - 1", .after = population) |> 
      left_join(colour_delta, by = "group") |> 
      relocate(fill, .after = group)
  }
  
  # Delta bivariate
  if (data_type == "delta_bivar") {
    data <- get(df)
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   var_left, var_right, "geometry")]
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
    data <- left_join(data, colour_bivar, by = "group")
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   "var_left", "var_right", "var_left_q3", "var_right_q3",
                   "group", "fill", "geometry")]
  }
    
  # NA_delta_bivar
  if (data_type == "NA_delta_bivar") {
    data <- get(df)
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population",
                   "geometry")]
    data$group <- "NA - NA"
    data <- left_join(data, colour_bivar, by = "group")
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_right <- NA
    data$var_right_q3 <- NA
    data <- data[c("ID", "name", "name_2", if (df == "DA") "DAUID", 
                   if (df %in% c("DA", "CT")) "CTUID", "CSDUID", "population", 
                   "var_left", "var_left_q3", "var_right", "var_right_q3", 
                   "group", "fill", "geometry")]
  }
  
  # Building delta bivariate TKTK update with base R
  if (data_type == "building_delta_bivar") {
    data <-
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left), 
             var_right = all_of(var_right), geometry = building) |>
      mutate(var_left = (var_left2 - var_left1) / abs(var_left1),
             var_left_q3 = ntile(var_left, 3),
             var_right = (var_right2 - var_right1) / abs(var_right1),
             var_right_q3 = ntile(var_right, 3),
             across(where(is.numeric), ~replace(., is.nan(.), NA)),
             across(where(is.numeric), ~replace(., is.infinite(.), NA))) |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left, var_right, var_left_q3, var_right_q3) |> 
      mutate(group = paste(var_left_q3, "-", var_right_q3)) |>
      left_join(colour_bivar, by = "group")
  }
    
  # building_NA_delta_bivar TKTK update with base R
  if (data_type == "building_NA_delta_bivar") {
    data <- 
      DA |> 
      st_set_geometry("building") |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, geometry = building) |> 
      mutate(var_left = NA, var_left_q3 = NA, var_right = NA, 
             var_right_q3 = NA, group = "NA - NA", .after = population) |> 
      left_join(colour_bivar, by = "group") |> 
      relocate(fill, .after = group)
  }
    
  # Point data TKTK update with base R
  if (data_type == "point") {
    
    type_pattern <- paste(unique(get(point_df)$type), collapse = "|")
    selected_type <- str_extract(var_left, type_pattern)
    time <- str_extract(var_left, time_format)
    
    data <- 
      point_df |> 
      get() %>%
      { if (str_detect(var_left[1], "_total_"))
        . else filter(., str_detect(type, selected_type))} %>%
      { if (length(time) == 2) {
        filter(., year %in% time[1]:time[2])
      } else {
        filter(., year == time)
      }}
  }
  
  # Return data
  return(data) 
  
}
