#### CENSUS DATA MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param df A reactive which resolves to a character string representing the
#' underlying data set to be loaded. Currently available options are 
#' `c("borough", "building", "CT", "DA", "grid", "street)`.
#' @param zoom A reactive which resolves to a character string representing the
#' amount of transparency to be applied to the fill aesthetic in maps made from
#' the data. Currently available options are 
#' `c("borough", "building", "CT", "DA", "street")` and by default this argument 
#' takes its value from the `df` parameter.
#' @return A reactive expression containing a data frame with the following
#' fields (If the `var_right` input is " ", the right_var_full and right_var 
#' fields will be omitted.):
#' - ID, name, name_2, population: The unmodified variables of the same names
#' from the input data frame.
#' - left_var_full, left_var: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_left`.
#' - right_var_full, right_var: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_right`.
#' - geometry: The unmodified geometry variable from the input data frame.
#' - group: A character field of form "X - Y", where X and Y are the quantile
#' values from left_var and right_var respectively.
#' - fill: A character vector of hex colour values to be passed to mapdeck for
#' colouring choropleths drawn from the data frame.

data_server <- function(id, var_left, var_right, df, zoom = df) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(zoom))
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      # Get borough/CT/DA/grid/etc
      data <- get(df())
      
      # Get data type
      data_type <- get_data_type(data, var_left, var_right)
      
      var_left <- unique(var_left())
      var_right <- unique(var_right())
      
      # Get time format
      time_format_var_left <- if (str_detect(var_left[1], "_\\d{4}$")) {
        # Yearly data
        "_\\d{4}$"
        # Will have to find a fix to allow for NO DATE (Or the last statement is
        # an open else for the yearly version, fixing the issue of no date)
      } else "_\\d{4}$"
      
      time_format_var_right <- if (str_detect(var_right[1], "_\\d{4}$")) {
        # Yearly data
        "_\\d{4}$"
        # Will have to find a fix to allow for NO DATE (Or the last statement is
        # an open else for the yearly version, fixing the issue of no date)
      } else "_\\d{4}$"
      
      # Set colour transparency
      colour <- 
        if (length(var_left) == 2 && var_right[1] == " ") {
          get(paste0("colour_delta_", zoom()))
        } else if (length(var_left) == 1 && var_right[1] == " "
                   && df() %in% c("borough", "CT", "DA", "grid")) {
          get(paste0("colour_left_3_", zoom()))
        } else if (length(var_left) == 1 && var_right[1] == " "
                   && df() %in% c("building", "street")) {
          get("colour_left_3_DA")
        } else get(paste0("colour_bivar_", zoom()))
      
      # Facilitate code legibility by pre-creating q3/q5 column names
      name_left_q3_col <- 
        paste0(str_remove(all_of(var_left), time_format_var_left), "_q3", 
               na.omit(str_extract(var_left, time_format_var_left)))
      name_right_q3_col <- 
        paste0(str_remove(var_right, time_format_var_right), "_q3", 
               na.omit(str_extract(var_right, time_format_var_right)))
      name_left_q5_col <- 
        paste0(str_remove(all_of(var_left), time_format_var_left), "_q5", 
               na.omit(str_extract(var_left, time_format_var_left)))
      name_right_q5_col <- 
        paste0(str_remove(var_right, time_format_var_right), "_q5", 
               na.omit(str_extract(var_right, time_format_var_right)))
      
      # Add NA column if q3 doesn't exist
      if (length(name_left_q3_col) == 1 && 
          !name_left_q3_col %in% names(data)) {
        data <- 
          data %>% 
          mutate(new_col = NA)
        
        names(data)[names(data) == "new_col"] <- name_left_q3_col
      }


      if (data_type == "uni_q5") {
        
        # Set colour transparency
        colour <- get(paste0("colour_left_5_", zoom()))
        
        # Get data
        data <- 
          data %>% 
          dplyr::select(ID, name, name_2, any_of("CSDUID"), population, 
                        left_var = all_of(var_left),
                        left_var_q5 = all_of(name_left_q5_col)) |>
          mutate(group = as.character(left_var_q5),
                 group = if_else(is.na(group), "NA", group)) |> 
          left_join(colour, by = "group")
        
        st_crs(data) <- 4326
        return(data)
        
        
      }
      
      
      
      ## Univariate data -------------------------------------------------------
      
      if (var_right[1] == " ") {

        # If there are two dates, make new left_var
        if (length(var_left) == 2) {
          data <- 
            data |> 
            dplyr::select(ID, name, name_2, any_of("CSDUID"), population, 
                          left_var = all_of(var_left)) |>
            mutate(
              left_var = (left_var2 - left_var1) / abs(left_var1), 
              left_var_q3 = case_when(
                is.na(left_var) ~ NA_character_,
                left_var < -1 * median(abs(left_var[abs(left_var) > 0.02]), 
                                       na.rm = TRUE) ~ "1",
                left_var < -0.02 ~ "2",
                left_var < 0.02 ~ "3",
                left_var < median(abs(left_var[abs(left_var) > 0.02]), 
                                  na.rm = TRUE) ~ "4",
                TRUE ~ "5"),
              across(where(is.numeric), ~replace(., is.nan(.), NA)),
              across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>% 
            select(ID, name, name_2, any_of("CSDUID"), population, left_var, 
                   left_var_q3, left_var_1 = left_var1, left_var_2 = left_var2) 
          
          # Finish up
          data <- 
            data |> 
            mutate(group = as.character(left_var_q3),
                   group = if_else(is.na(group), "NA", group),
                   group = paste(group, "- 1")) |> 
            left_join(colour, by = "group")
          
        } else {
            data <- 
              data %>% 
              dplyr::select(ID, name, name_2, any_of("CSDUID"), population, 
                            left_var = all_of(var_left),
                            left_var_q3 = all_of(name_left_q3_col)) |>
              mutate(group = as.character(left_var_q3),
                     group = if_else(is.na(group), "NA", group)) |> 
              left_join(colour, by = "group")
          
        }
        
        
        ## Bivariate data ------------------------------------------------------
        
      } else {
        data <-
          (data %>%
             { if (length(var_left) == 1 && length(var_right) == 1) 
               dplyr::select(., ID, name, name_2, any_of("CSDUID"), population, 
                             left_var = all_of(var_left),
                             left_var_q3 = all_of(name_left_q3_col),
                             right_var = all_of(var_right), 
                             right_var_q3 = all_of(name_right_q3_col))
               else dplyr::select(., everything(),
                                  left_var = all_of(var_left),
                                  right_var = all_of(var_right))} %>% 
             { if (length(var_left) == 2 && length(var_right) == 2) 
               mutate(., left_var = (left_var2 - left_var1) / abs(left_var1),
                      left_var_q3 = ntile(left_var, 3),
                      right_var = (right_var2 - right_var1) / abs(right_var1),
                      right_var_q3 = ntile(right_var, 3),
                      across(where(is.numeric), ~replace(., is.nan(.), NA)),
                      across(where(is.numeric), ~replace(., is.infinite(.), 
                                                         NA))) %>% 
                 select(., ID, name, name_2, any_of("CSDUID"), population, 
                        left_var, left_var_q3, right_var, right_var_q3,
                        any_of(c("left_var1", "left_var2", "right_var1", 
                                 "right_var2"))) else .} %>%
             # Not always census variables: sometimes we will have data for
             # one variable in different year than the other, e.g. crash vs borough.
             # We might have to show different crash years vs same census year.
             { if (length(var_left) == 2 && length(var_right) == 1) 
               mutate(., left_var = (left_var2 - left_var1) / abs(left_var1),
                      left_var_q3 = ntile(left_var, 3),
                      right_var = var_right, 
                      right_var_q3 = eval(as.name(name_right_q3_col)),
                      across(where(is.numeric), ~replace(., is.nan(.), NA)),
                      across(where(is.numeric), ~replace(., is.infinite(.), 
                                                         NA))) %>% 
                 select(., ID, name, name_2, any_of("CSDUID"), population, 
                        left_var, left_var_q3, right_var, right_var_q3,
                        any_of(c("left_var1", "left_var2", "right_var1", 
                                 "right_var2"))) else .} %>%
             { if (length(var_left) == 1 && length(var_right) == 2)
               mutate(., left_var = var_left,
                      left_var_q3 = eval(as.name(name_left_q3_col)),
                      right_var = (right_var2 - right_var1) / abs(right_var1),
                      right_var_q3 = ntile(right_var, 3),
                      across(where(is.numeric), ~replace(., is.nan(.), NA)),
                      across(where(is.numeric), ~replace(., is.infinite(.), 
                                                         NA))) %>%
                 select(., ID, name, name_2, any_of("CSDUID"), population,
                        left_var, left_var_q3, right_var, right_var_q3,
                        any_of(c("left_var1", "left_var2", "right_var1", 
                                 "right_var2"))) else .} %>%
             mutate(group = paste(left_var_q3, "-", right_var_q3)) %>% 
             left_join(colour, by = "group"))
      }
      
      st_crs(data) <- 4326
      data
    })
  })
}
