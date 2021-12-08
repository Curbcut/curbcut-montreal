#### DATA EXPORT MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. 
#' @param data A reactive dataframe coming from data_server.
#' @return A reactive expression containing a data frame with the following
#' fields

data_export_server <- function(id, df, var_left, var_right) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      data <- df()
      vars <- c(str_subset(names(data), "q3"), "fill", "group")
      data <- data %>% select(-any_of(vars))
      
      # Manage left_vars
      left_var_name <- str_remove(var_left(), "_\\d{4}$")
      data_left_vars <- str_subset(names(data), "left_var.")
      if (length(data_left_vars) > 1) {
        names(data)[names(data) == "left_var"] <- paste0("change_", left_var_name)
        names(data)[names(data) %in% data_left_vars] <- var_left()
      } else {
        names(data)[names(data) %in% "left_var"] <- var_left()
      }
      
      # Manage right_vars
      right_var_name <- str_remove(var_right(), "_\\d{4}$")
      data_right_vars <- str_subset(names(data), "right_var.")
      if (length(data_right_vars) > 1) {
        names(data)[names(data) == "right_var"] <- paste0("change_", right_var_name)
        names(data)[names(data) %in% data_right_vars] <- var_right()
      } else {
        names(data)[names(data) %in% "right_var"] <- var_right()
      }
      
      # Return the df
      data
      
    })
  })
}
