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

      data_left_vars <- str_subset(names(data), "left_var.")
      if (length(data_left_vars) > 1) {
        names(data)[names(data) == "left_var"] <- "change"
        names(data)[names(data) %in% data_left_vars] <- var_left()
      } else {
        names(data)[names(data) %in% "left_var"] <- var_left()
      }
      data
      
    })
  })
}
