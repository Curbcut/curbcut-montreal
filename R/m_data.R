#### CENSUS DATA MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param df A reactive which resolves to a character string representing the
#' underlying data set to be loaded. Currently available options are 
#' `c("borough", "building", "CT", "DA", "grid", "street")`.
#' @return A reactive expression containing a data frame with the following
#' fields (If the `var_right` input is " ", the right_var_full and right_var 
#' fields will be omitted.):
#' - ID, name, name_2, population: The unmodified variables of the same names
#' from the input data frame.
#' - var_left, var_left_q3, var_left_q5: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_left`.
#' - var_right, var_right_q3, var_right_q5: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_right`.
#' - geometry: The unmodified geometry variable from the input data frame.
#' - group: A character field of form "X - Y", where X and Y are the quantile
#' values from var_left and var_right respectively.
#' - fill: A character vector of hex colour values to be passed to mapdeck for
#' colouring choropleths drawn from the data frame.

data_server <- function(id, var_left, var_right, df, island = FALSE) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))

  moduleServer(id, function(input, output, session) {
    reactive({
      
      # Setup ------------------------------------------------------------------
      
      # Get data type
      data_type <- get_data_type(df, var_left, var_right)
      
      # Simplify variables
      var_left <- var_left()
      var_right <- var_right()
      
      # Are var_left and var_right the same column?
      if (all(var_left == var_right)) {
        stop("`var_left` and `var_right` are the same.")
      }
      
      # Get time format; TKTK does this need to be more complex?
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

            
      # Simple univariate ------------------------------------------------------
     
      if (data_type == "q5") {
        
        data <- 
          df() |> 
          get() |> 
          select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
                 population, var_left = all_of(var_left), 
                 var_left_q3 = all_of(left_q3),
                 var_left_q5 = all_of(left_q5)) |>
          mutate(group = coalesce(as.character(var_left_q5), "NA")) |> 
          left_join(colour_left_5, by = "group")
        
        st_crs(data) <- 4326 ## TKTK TEST REMOVING THIS IN PRODUCTION

      }
      
      
      # Building univariate ----------------------------------------------------
      
      if (data_type == "building_q5") {
        
        data <- 
          DA |> 
          st_set_geometry("building") |> 
          select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
                 population, var_left = all_of(var_left), 
                 var_left_q3 = all_of(left_q3),
                 var_left_q5 = all_of(left_q5),
                 geometry = building) |>
          mutate(group = coalesce(as.character(var_left_q5), "NA")) |> 
          left_join(colour_left_5, by = "group")
        
        st_crs(data) <- 4326

      }
      
      
      # Simple bivariate ----------------—----------------—----------------—----
      
      if (data_type == "bivar") {
        
        data <- 
          df() |> 
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

      
      # Building bivariate ----------------—----------------—----------------—--
      
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
      
      
      # Simple delta -----------------------------------------------------------

      if (data_type == "delta") {
        
        data <- 
          df() |> 
          get() |> 
          select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
                 population, var_left = all_of(var_left), 
                 var_left = all_of(var_left)) |>
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
      
      
      # Building delta ---------------------------------------------------------

      if (data_type == "building_delta") {
        
        data <- 
          DA |> 
          st_set_geometry("building") |> 
          select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
                 population, var_left = all_of(var_left), 
                 var_left = all_of(var_left), geometry = building) |>
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
      
      
      # Filter to island -------------------------------------------------------
      
      if (island) data <- filter(data, CSDUID %in% island_CSDUID)
      
      
      # Return output ----------------------------------------------------------
      
      return(data)

      

      # # Add NA column if q3 doesn't exist
      # if (length(left_q3_col) == 1 && 
      #     !left_q3_col %in% names(data)) {
      #   data <- 
      #     data %>% 
      #     mutate(new_col = NA)
      #   
      #   names(data)[names(data) == "new_col"] <- left_q3_col
      # }
      # 
      # 
      # 
      # 
      # ## Univariate data -------------------------------------------------------
      # 
      # if (var_right[1] == " ") {} else {
      #   data <-
      #     (data %>%
      #        { if (length(var_left) == 1 && length(var_right) == 1) 
      #          dplyr::select(., ID, name, name_2, any_of("CSDUID"), population, 
      #                        left_var = all_of(var_left),
      #                        left_var_q3 = all_of(left_q3_col),
      #                        right_var = all_of(var_right), 
      #                        right_var_q3 = all_of(right_q3_col))
      #          else dplyr::select(., everything(),
      #                             left_var = all_of(var_left),
      #                             right_var = all_of(var_right))} %>% 
      #        { if (length(var_left) == 2 && length(var_right) == 2) 
      #          mutate(., left_var = (left_var2 - left_var1) / abs(left_var1),
      #                 left_var_q3 = ntile(left_var, 3),
      #                 right_var = (right_var2 - right_var1) / abs(right_var1),
      #                 right_var_q3 = ntile(right_var, 3),
      #                 across(where(is.numeric), ~replace(., is.nan(.), NA)),
      #                 across(where(is.numeric), ~replace(., is.infinite(.), 
      #                                                    NA))) %>% 
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
      # }
      # 
      # st_crs(data) <- 4326
      # data
    })
  })
}
