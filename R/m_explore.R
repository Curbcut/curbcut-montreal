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
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      fluidRow(column(width = 7, h4(i18n$t("Explore"))),
               column(width = 5, align = "right", 
                      actionLink(inputId = NS(id, "hide"), 
                                 label = i18n$t("Hide"))))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      htmlOutput(NS(id, "info_table")),
      conditionalPanel(
        condition = "output.show_graph == true", ns = NS(id),
        plotOutput(NS(id, "explore_graph"), height = 150)),
      conditionalPanel(
        condition = "output.poly_selected == 1", ns = NS(id),
        actionLink(inputId = NS(id, "clear_selection"),
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
    
    # Display info_table if it isn't NULL
    output$info_table <- renderUI(if (!is.null(table())) table())
    
    graph <- reactive(explore_graph(
      data = data(),
      var_type = var_type(), 
      var_left = var_left(), 
      var_right = var_right(), 
      select_id = select_id(),
      df = df(), 
      build_str_as_DA = build_str_as_DA,
      plot_type = "auto"))
    
    # Display graph if it isn't NULL
    output$explore_graph <- renderPlot(if (!is.null(graph())) graph())
    
    # Only show panel if there's something to show
    show_panel <- reactive(!is.null(table()) || !is.null(graph()))
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    # Only show graph if there's something to show
    show_graph <- reactive(!is.null(graph()))
    output$show_graph <- show_graph
    outputOptions(output, "show_graph", suspendWhenHidden = FALSE)
    
    # Hide explore status
    output$hide_status <- reactive(show_panel() && input$hide %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide, {
      if (input$hide %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide", label = txt)
    })
    
    # Hook up "Clear selection" button
    output$poly_selected <- reactive({if (!is.na(select_id())) TRUE else FALSE})
    outputOptions(output, "poly_selected", suspendWhenHidden = FALSE)
    
    # Return info_table text and graph to export it in report afterwards
    reactive({list(info = info_table(), graph = graph())})
  })
}
