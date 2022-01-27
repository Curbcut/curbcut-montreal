#### DISCLAIMER MODULE #########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. 
#' @param time A reactive dataframe coming from data_server.
#' @return A reactive expression containing a data frame with the following
#' fields


year_disclaimer_UI <- function(id) {
  tagList(htmlOutput(NS(id, "year_disclaimer")))
}

year_disclaimer_server <- function(id, data, var_left, var_right, 
                                   more = reactive(FALSE), 
                                   more_text = reactive(NULL)) {
  
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))

  moduleServer(id, function(input, output, session) {
    
    disclaimer <- reactive(get_disclaimer(data(), var_left(), var_right(),
                                          more(), more_text()))
    
    output$year_disclaimer <- renderText(paste0(disclaimer()))
  })
}
