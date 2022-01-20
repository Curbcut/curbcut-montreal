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
#' @param build_str_as_DA A logical scalar. Should the "building" and "street"
#' zoom levels show graphs and text for the DA zoom level instead?

explore_UI <- function(id) {
  
  tagList(
    
    div(id = NS(id, "explore_title"),
        fluidRow(column(width = 7, h4(i18n$t("Explore"))),
                 column(width = 5, align = "right", 
                        actionLink(inputId = NS(id, "hide"), 
                                   label = i18n$t("Hide"))))),
    
    div(id = NS(id, "explore_content"),
        htmlOutput(NS(id, "info_table")),
        plotOutput(NS(id, "explore_graph"), height = 150),
        hidden(actionLink(inputId = NS(id, "clear_selection"),
                          label = "Clear selection")))
  )
}

explore_server <- function(id, data, var_left, var_right, df, select_id,
                           build_str_as_DA = TRUE) {
  
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(select_id))
  stopifnot(!is.reactive(build_str_as_DA))

  moduleServer(id, function(input, output, session) {
    
    # Get var_type
    var_type <- reactive(get_var_type(
      data = data(), 
      var_left = var_left(), 
      var_right = var_right(), 
      df = df(), 
      select_id = select_id()))
    
    # Make info table
    table <- reactive({
      info_table(
        data = data(), 
        var_type = var_type(), 
        var_left = var_left(), 
        var_right = var_right(), 
        df = df(), 
        select_id = select_id(), 
        build_str_as_DA = build_str_as_DA)
      })
    
    # Display info table
    output$info_table <- renderUI(table())
    
    # Make graph
    graph <- reactive(explore_graph(
      data = data(),
      var_type = var_type(), 
      var_left = var_left(), 
      var_right = var_right(), 
      df = df(), 
      select_id = select_id(),
      build_str_as_DA = build_str_as_DA,
      plot_type = "auto"))
    
    # Display graph
    output$explore_graph <- renderPlot(graph())
    
    # Show/hide components
    observe({
      toggle("explore_content", condition = 
               (!is.null(table()) || !is.null(graph())) && input$hide %% 2 == 0)
      toggle("explore_graph", condition = !is.null(graph()))
      toggle("clear_selection", condition = !is.na(select_id()))
    })
    
    # Change show/hide button text
    observeEvent(input$hide, {
      txt <- sus_translate(switch(input$hide %% 2 + 1, "Hide", "Show"))
      updateActionButton(session, "hide", label = txt)
    })
    
    # Return info_table text and graph to export it in report afterwards
    reactive({list(info = info_table(), graph = graph())})
  })
}
