#### EXPLORE MODULE ############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param x A reactive which resolves to a data frame.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be analyzed. Each 
#' should have a "raw" version and quantile versions with the suffixes "_q3"
#' and "_q5".
#' @param selection A reactive which resolves to a character string giving the 
#' ID of a row in the input data frame (`x`) which has been selected.
#' @param zoom A reactive which resolves to a character string giving the
#' current zoom scale. Meaningful values are "borough", "CT", "DA" and "grid".
#' @param var_left_label,var_right_label A reactive which resolves to a named
#' character vector giving labels for the variable values of var_left or
#' var_right.
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

explore_server <- function(id, x, var_left, var_right, selection, df, 
                           var_left_label = NULL, var_right_label = NULL,
                           build_str_as_DA = TRUE,
                           standard = reactive(TRUE), custom_info = NULL, 
                           custom_graph = NULL) {
  
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(selection))
  stopifnot(is.reactive(df))

  moduleServer(id, function(input, output, session) {
    
    # Get var_type
    var_type <- explore_var_type(id, x, var_left, var_right, selection,
                                 var_left_label, var_right_label)
    
    standard_table <- info_table_server(id = "explore", 
                                        x = x, 
                                        var_type = var_type, 
                                        var_left = var_left, 
                                        var_right = var_right, 
                                        selection = selection, 
                                        df = df, 
                                        var_left_label = var_left_label, 
                                        var_right_label = var_right_label,
                                        build_str_as_DA = build_str_as_DA)
    
    custom_table <- tryCatch(custom_info(id = "explore", 
                                         x = x, 
                                         var_type = var_type, 
                                         var_left = var_left, 
                                         var_right = var_right, 
                                         selection = selection, 
                                         df = df, 
                                         var_left_label = var_left_label, 
                                         var_right_label = var_right_label,
                                         build_str_as_DA = build_str_as_DA),
                             error = function(e) reactive(NULL),
                             silent = TRUE)
    
    # Render info table
    info_table <- reactive({
      if (isTRUE(standard())) standard_table() else custom_table()
    })
    
    # Display info_table if it isn't NULL
    output$info_table <- renderUI({
      if (!is.null(info_table())) info_table()
    })
    
    standard_graph <- 
      tryCatch(explore_graph_server(id = "explore", 
                                    x = x, 
                                    var_type = var_type, 
                                    var_left = var_left, 
                                    var_right = var_right, 
                                    selection = selection, 
                                    df = df, 
                                    var_left_label = var_left_label,
                                    var_right_label = var_right_label,
                                    build_str_as_DA = build_str_as_DA),
               error = function(e) reactive(NULL),
               silent = TRUE)
    
    custom_exp_graph <- tryCatch(custom_graph(id = "explore", 
                                     x = x, 
                                     var_type = var_type, 
                                     var_left = var_left, 
                                     var_right = var_right, 
                                     selection = selection, 
                                     df = df, 
                                     var_left_label = var_left_label, 
                                     var_right_label = var_right_label,
                                     build_str_as_DA = build_str_as_DA),
                                 error = function(e) reactive(NULL),
                                 silent = TRUE)
    
    # Render the graph
    explore_graph <- reactive({
      if (isTRUE(standard())) standard_graph() else custom_exp_graph()
    })
    
    # Display graph if it isn't NULL
    output$explore_graph <- renderPlot({
      if (!is.null(explore_graph())) explore_graph()
    })
    
    # Only show panel if there's something to show
    show_panel <- reactive(!is.null(info_table()) || !is.null(explore_graph()))
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    # Only show graph if there's something to show
    show_graph <- reactive(!is.null(explore_graph()))
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
    output$poly_selected <- reactive({if (!is.na(selection())) TRUE else FALSE})
    outputOptions(output, "poly_selected", suspendWhenHidden = FALSE)
    
    # Return info_table text and graph to export it in report afterwards
    reactive({list(info = info_table(), graph = explore_graph())})
  })
}
