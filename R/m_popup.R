#### POPUP MODULE ##############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param x A reactive which resolves to a one-row data frame.
#' @param fields A non-reactive named character vector whose 
#' elements are the fields in the data frame containing data to be displayed, 
#' and whose names are the labels to be printed with the data.
#' @param plots A reactive which resolves to a named list of ggplot objects or 
#' similar, which can be displayed with `renderUI`.
#' @param images A reactive which resolves to a character vector (possibly 
#' named) of paths leading to images to be displayed in the popup. If it is
#' named, images will be captioned with the names.
#' @param order A non-reactive character vector with the order in which to 
#' display popup elements. Default is `c("fields", "plots", "photos")`. 

popup_UI <- function(id) {
  conditionalPanel(
    condition = "output.selection == 1", ns = NS(id),
    uiOutput(NS(id, "popup_fields"), width = 150),
    plotOutput(NS(id, "popup_plots"), width = 150),
    imageOutput(NS(id, "popup_images"), width = 150),
    actionLink(inputId = NS(id, "clear_selection"), label = "Clear selection")
    )
  }


popup_server <- function(id, x, fields = NULL, plots = NULL, images = NULL,
                         order = c("fields", "plots", "photos")) {
  stopifnot(is.reactive(x))
  stopifnot(missing(fields) || !is.reactive(fields))
  stopifnot(missing(plots) || is.reactive(plots))
  stopifnot(missing(images) || is.reactive(images))
  stopifnot(!is.reactive(order))
  
  moduleServer(id, function(input, output, session) {
    
    # Start with NULL outputs
    out_fields <- NULL
    out_plots <- NULL
    out_images <- NULL
    
    # Fields
    output$popup_fields <- renderUI({
      if (!is.null(fields) && nrow(x()) == 1) {
        
        text_fields <- paste0("<b>", names(fields), ":</b> ", 
                              map(fields, ~pull(x(), .x)), "<br>")
        
        map(text_fields, HTML)
        }
      })
    
    # Plots
    output$popup_plots <- renderPlot({if (!is.null(plots)) map(plots, HTML)})
    
    # Images
    output$popup_images <- renderImage({
      if (!is.null(images())) {
        # print(images())
        list(src = images(), width = 150)
        }}, deleteFile = FALSE)
    
    # Hook up "Clear selection" button
    output$selection <- reactive({
      if (!is.na(rv_covid$path_selected) || !is.na(rv_covid$point_selected)) 
        TRUE else FALSE
      })
    outputOptions(output, "selection", suspendWhenHidden = FALSE)
    
    # Order output
    # TKTK
    
  })
}
