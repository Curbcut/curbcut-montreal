#### GET DATA TABLE ############################################################

get_data_table <- function(df, var_left, var_right, data_type, point_df) {
  
  # Get time format; eventually this might need to be conditional
  time_format <- "\\d{4}$"
  
  # Facilitate code legibility by pre-creating q3/q5 column names
  left_q3 <- paste0(str_remove(all_of(var_left), time_format), "q3_", 
                    na.omit(str_extract(var_left, time_format)))
  right_q3 <- paste0(str_remove(all_of(var_right), time_format), "q3_", 
                     na.omit(str_extract(var_right, time_format)))
  left_q5 <- paste0(str_remove(all_of(var_left), time_format), "q5_", 
                    na.omit(str_extract(var_left, time_format)))
  right_q5 <- paste0(str_remove(all_of(var_right), time_format), "q5_", 
                     na.omit(str_extract(var_right, time_format)))
  
  # Univariate
  if (data_type == "q5") {
    data <- 
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left), 
             var_left_q3 = all_of(left_q3),
             var_left_q5 = all_of(left_q5)) |>
      mutate(group = coalesce(as.character(var_left_q5), "NA"),
             .after = var_left_q5) |> 
      left_join(colour_left_5, by = "group") |> 
      relocate(fill, .after = group)
  }
  
  # Building univariate
  if (data_type == "building_q5") {
    data <- 
      DA |> 
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
  
  # Bivariate
  if (data_type == "bivar") {
    data <- 
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left), 
             var_left_q3 = all_of(left_q3),
             var_left_q5 = all_of(left_q5),
             var_right = all_of(var_right), 
             var_right_q3 = all_of(right_q3),
             var_right_q5 = all_of(right_q5)) |>
      mutate(group = paste(var_left_q3, "-", var_right_q3)) |>
      left_join(colour_bivar, by = "group") |> 
      relocate(fill, .after = group)
  }
  
  # Building bivariate
  if (data_type == "building_bivar") {
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
  
  # Delta
  if (data_type == "delta") {
    
    data <- 
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left)) |>
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
  
  # Building delta
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
    data <- 
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population) |> 
      mutate(var_left = NA, var_left_q3 = NA, var_left_1 = NA,
             var_left_2 = NA, group = "NA - 1", .after = population) |> 
      left_join(colour_delta, by = "group") |> 
      relocate(fill, .after = group)
  }
  
  # building_NA_delta
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
    data <-
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population, var_left = all_of(var_left), 
             var_right = all_of(var_right)) |>
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
    
  # NA_delta_bivar
  if (data_type == "NA_delta_bivar") {
    data <- 
      df |> 
      get() |> 
      select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
             population) |> 
      mutate(var_left = NA, var_left_q3 = NA, var_right = NA, 
             var_right_q3 = NA, group = "NA - NA", .after = population) |> 
      left_join(colour_bivar, by = "group") |> 
      relocate(fill, .after = group)
  }
  
  # Building delta bivariate
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
    
  # building_NA_delta_bivar
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
    
    #            select(., ID, name, name_2, any_of("CSDUID"), population, 
    #                   left_var, left_var_q3, right_var, right_var_q3,
    #                   any_of(c("left_var1", "left_var2", "right_var1", 
    #                            "right_var2"))) else .} %>%
    #        # Not always census variables: sometimes we will have data for
    #        # one variable in different year than the other, e.g. crash vs borough.
    #        # We might have to show different crash years vs same census year.
    #        { if (length(var_left) == 2 && length(var_right) == 1) 
    #          mutate(., left_var = (left_var2 - left_var1) / abs(left_var1),
    #                 left_var_q3 = ntile(left_var, 3),
    #                 right_var = var_right, 
    #                 right_var_q3 = eval(as.name(right_q3_col)),
    #                 across(where(is.numeric), ~replace(., is.nan(.), NA)),
    #                 across(where(is.numeric), ~replace(., is.infinite(.), 
    #                                                    NA))) %>% 
    #            select(., ID, name, name_2, any_of("CSDUID"), population, 
    #                   left_var, left_var_q3, right_var, right_var_q3,
    #                   any_of(c("left_var1", "left_var2", "right_var1", 
    #                            "right_var2"))) else .} %>%
    #        { if (length(var_left) == 1 && length(var_right) == 2)
    #          mutate(., left_var = var_left,
    #                 left_var_q3 = eval(as.name(left_q3_col)),
    #                 right_var = (right_var2 - right_var1) / abs(right_var1),
    #                 right_var_q3 = ntile(right_var, 3),
    #                 across(where(is.numeric), ~replace(., is.nan(.), NA)),
    #                 across(where(is.numeric), ~replace(., is.infinite(.), 
    #                                                    NA))) %>%
    #            select(., ID, name, name_2, any_of("CSDUID"), population,
    #                   left_var, left_var_q3, right_var, right_var_q3,
    #                   any_of(c("left_var1", "left_var2", "right_var1", 
    #                            "right_var2"))) else .} %>%
    #        mutate(group = paste(left_var_q3, "-", right_var_q3)) %>% 
    #        left_join(colour, by = "group"))
  
  
  # Point data
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
