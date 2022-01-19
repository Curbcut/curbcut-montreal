#### GET DATA TABLE ############################################################

get_data_table <- function(df, var_left, var_right, data_type, left_q3, 
                           right_q3, left_q5, right_q5, time_format, point_df) {

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
             fill = colour_left_5$fill[coalesce(var_left_q5, 6)], 
             .after = var_left_q5)
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
             fill = colour_left_5$fill[coalesce(var_left_q5, 6)], 
             .after = var_left_q5)
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
      left_join(colour_bivar, by = "group")
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
      left_join(colour_bivar, by = "group")
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
      left_join(colour_delta, by = "group")
    
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
      left_join(colour_delta, by = "group")
  }
  
  # Point data
  if (data_type == "point") {
    
    type_pattern <- paste(unique(get(point_df)$type), collapse = "|")
    selected_type <- str_extract(var_left, type_pattern)
    time <- str_extract(var_left, time_format)
    
    data <- 
      point_df |> 
      get() %>%
      { if (str_detect(var_left, "_total_"))
        . else filter(., str_detect(type, selected_type))} %>%
      { if (length(time) == 2) {
        filter(., year %in% time[1]:time[2])
      } else {
        filter(., year == time)
      }}
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
    
  }
  
  # Building delta bivariate
  if (data_type == "building_delta_bivar") {
    
  }
  
  # Return data
  return(data) 
  
}
