#### EXPLORE MODULE ############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param data A reactive which resolves to a data frame.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be analyzed. Each 
#' should have a "raw" version and quantile versions with the suffixes "_q3"
#' and "_q5".
#' @param df 
#' @param select_id A reactive which resolves to a character string giving the 
#' ID of a row in the input data frame (`data`) which has been selected.
#' @param build_str_as_DA A reactive which resolves to a logical scalar. Should 
#' the "building" and "street" zoom levels show graphs and text for the DA zoom 
#' level instead?

explore_UI <- function(id) {
  
  tagList(
    
    div(id = NS(id, "explore_title"),
        fluidRow(column(width = 7, h4(cc_t(r = r, "Explore"))),
                 column(width = 5, align = "right", 
                        actionLink(inputId = NS(id, "hide_explore"), 
                                   class = "sus-small-link", 
                                   label = cc_t(r = r, "Hide"))))),
    
    div(id = NS(id, "explore_content"),
        htmlOutput(NS(id, "info_table")),
        plotOutput(NS(id, "explore_graph"), height = 150),
        hidden(actionLink(inputId = NS(id, "clear_selection"),
                          label = cc_t(r = r, "Clear selection"))))
  )
}

explore_server <- function(id, r, data, var_left, var_right, geo = r$geo,
                           df = r[[id]]$df, select_id = r[[id]]$select_id,
                           build_str_as_DA = reactive(TRUE), 
                           graph = reactive(explore_graph), 
                           graph_args = reactive(list(
                             r = r,
                             data = data(), var_left = var_left(), 
                             var_right = var_right(), df = df(), 
                             select_id = select_id(), geo = geo(),
                             build_str_as_DA = build_str_as_DA())),
                           table = reactive(info_table),
                           table_args = reactive(list(
                             r = r,
                             data = data(), var_left = var_left(),
                             var_right = var_right(), df = df(), 
                             select_id = select_id(), geo = geo(),
                             build_str_as_DA = build_str_as_DA()))) {
  
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(build_str_as_DA))

  moduleServer(id, function(input, output, session) {
    
    # Get var_type
    var_type <- reactive(tryCatch(
      get_var_type(
      data = data(),
      var_left = var_left(),
      var_right = unique(var_right()),
      df = df(),
      geo = geo(),
      select_id = select_id(),
      build_str_as_DA = build_str_as_DA()), 
      error = function(e) {
        print(e)
        return(NULL)
      }))
    
    # Reconstruct variable args
    table_args2 <- reactive(c(table_args(), var_type = var_type()))
    graph_args2 <- reactive(c(graph_args(), var_type = var_type()))
    
    # Make info table
    table_out <- reactive(
      tryCatch(
        do.call(table(), table_args2())
        , error = function(e) {
          print(e)
          return(NULL)
        })
      )
    
    # Display info table
    output$info_table <- renderUI(table_out())
    
    # Make graph
    graph_out <- reactive(tryCatch(do.call(graph(), graph_args2()),
                                   error = function(e) {
                                     print(e)
                                     return(NULL)
                                   }))
    
    # Display graph
    output$explore_graph <- renderPlot(graph_out())
    
    # Show/hide components
    observe({
      toggle("explore_content", condition =
               (!is.null(table_out()) || !is.null(graph_out())) && 
               input$hide_explore %% 2 == 0)
      toggle("info_table", condition = !is.null(table_out()))
      toggle("explore_graph", condition = !is.null(graph_out()))
      toggle("clear_selection", condition = !is.na(select_id()))
    })
    
    # Clear selection on button click
    observeEvent(input$clear_selection, select_id(NA), ignoreInit = TRUE)
    
    # Change show/hide button text
    observeEvent(input$hide_explore, {
      txt <- cc_t(r = r, switch(input$hide_explore %% 2 + 1, "Hide", "Show"))
      updateActionButton(session, "hide_explore", label = txt)
    })

    # Return info_table text and graph to export it in report afterwards
    reactive(list(info = table_out(), graph = graph_out()))
  })
}
