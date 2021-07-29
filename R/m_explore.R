#### EXPLORE MODULE ############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param x A reactive which resolves to a data frame.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param select A reactive which resolves to a character string giving the ID
#' of a row in the input data frame (`x`) which has been selected.
#' @param zoom A reactive which resolves to a character string giving the
#' current zoom scale. Meaningful values are "borough", "CT", "DA" and "grid".
#' @param title A non-reactive character string used for the x-axis label
#' in the graph and the name of var_left in the info table. Needs to be replaced
#' with var_left_title and var_right_title as reactives.
#' @param var_left_title,var_right_title A reactive which resolves to a 
#' character string representing the plain-language description of the left and 
#' right variables to be analyzed. These will be used for axis labels in the
#' explore graph, and for textual description in the info table.
#' @param var_left_label,var_right_label A reactive which resolves to a named
#' character vector giving labels for the variable values of var_left or
#' var_right.


explore_UI <- function(id) {
  
  tagList(
    hr(),
    fluidRow(column(width = 7, h4(i18n$t("Explore"))),
             column(width = 5, align = "right", 
                    actionLink(inputId = NS(id, "hide"), 
                               label = i18n$t("Hide")))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      info_table_UI(NS(id, "explore")),
      explore_graph_UI(NS(id, "explore")),
      conditionalPanel(
        condition = "output.poly_selected == 1", ns = NS(id),
        actionLink(inputId = NS(id, "clear_selection"),
                   label = "Clear selection")))
    )
}

explore_server <- function(id, x, var_left, var_right, select, zoom, 
                           var_left_title, var_right_title = NULL,
                           var_left_label = NULL, var_right_label = NULL) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(zoom))
  stopifnot(is.reactive(var_left_title))
  
  moduleServer(id, function(input, output, session) {
    
    # Get var_type
    var_type <- explore_var_type(id, x, var_left, var_right, select,
                                 var_left_label, var_right_label)

    observeEvent(var_type(), print(var_type()))
    
    # Render info table
    info_table_server("explore", x, var_type, var_left, var_right, select, zoom, 
                      var_left_title, var_right_title = var_right_title,
                      var_left_label = var_left_label,
                      var_right_label = var_right_label)
    
    # Render the graph
    explore_graph_server("explore", x, var_type, var_left, var_right, select, 
                         var_left_title, var_left_label = var_left_label,
                         var_right_label = var_right_label)
    
    # Hide explore status
    output$hide_status <- reactive(input$hide %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide, {
      if (input$hide %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide", label = txt)
    })
    
    # Hook up "Clear selection" button
    output$poly_selected <- reactive({if (!is.na(select())) TRUE else FALSE})
    outputOptions(output, "poly_selected", suspendWhenHidden = FALSE)
  })
}
