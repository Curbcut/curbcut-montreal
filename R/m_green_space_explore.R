#### GREEN SPACE EXPLORE MODULE ################################################
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

green_space_explore_UI <- function(id) {
  
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
      plotOutput(NS(id, "explore_graph"), height = 150),
      conditionalPanel(
        condition = "output.poly_selected == 1", ns = NS(id),
        actionLink(inputId = NS(id, "clear_selection"),
                   label = "Clear selection")))
  )
}

green_space_explore_server <- function(id, x, var_left, var_right, selection, df, 
                           var_left_label = NULL, var_right_label = NULL,
                           build_str_as_DA = TRUE, 
                           standard_info = reactive(TRUE)) {
  
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(selection))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(standard_info))
  
  moduleServer(id, function(input, output, session) {
    
    # Get var_type
    var_type <- explore_var_type(id, x, var_left, var_right, selection,
                                 var_left_label, var_right_label)
    
    # Render info table
    info_table_standard <- info_table_server(id = "explore", 
                                             x = x, 
                                             var_type = var_type, 
                                             var_left = var_left, 
                                             var_right = var_right, 
                                             selection = selection, 
                                             df = df, 
                                             var_left_label = var_left_label, 
                                             var_right_label = var_right_label,
                                             build_str_as_DA = build_str_as_DA)
    
    info_table_green_space <- reactive({
      
      pnr <- function(x) prettyNum(round(x/1e+6, digits = 2), big.mark = ",")
      
      if (is.na(selection())) {
        type <- if (nrow(x()) == nrow(green_space)) {
          str_glue(sus_translate("a total of {nrow(x())} green spaces"))
        } else {
          str_glue(sus_translate("{nrow(x())} `{unique(x()$type)}`"))
        }
        
        HTML(str_glue(
          paste0("At the scale of the City of Montreal, there are {type}, ",
             "combining {pnr(sum(x()$area))} km^2. Their area range from ",
             "{pnr(min(x()$area))} and {pnr(max(x()$area))} km^2, with ",
             "an average of {pnr(mean(x()$area))} km^2.")))
      } else {
        x <- 
          x() |>
          mutate(total_rank = rank(-area)) |> 
          group_by(CSDUID) |> 
          mutate(borough_rank = rank(-area))
        
        type <- if (nrow(x()) == nrow(green_space)) {
          str_glue(sus_translate("green space"))
        } else {
          str_glue(sus_translate("`{unique(x()$type)}`"))
        }
        
        z <- x[x$ID == selection(), ]
        total_rank <- pull(z, total_rank)
        borough_rank <- pull(z, borough_rank)
        borough <- filter(borough, ID == pull(z, CSDUID))$name
        
        # tibble(number = 1:10,
        #        text = c('first','second','third','fourth','fifth','sixth',
        #                 'seventh','eighth','ninth','tenth'))
        
        ordinal_form <- function(x) {
          if (x > 20) {
            if (x %% 100 %in% c(11 , 12, 13)) {
              form <- "th "
            } else {
              form <- switch(as.character(x %% 10), "1" = "st ", "2" = "nd ", 
                             "3" = "rd ", "th ")
            }
            paste0(x, form)
          } else {
            switch(as.character(x), "1" = "", "2" = "second ", 
                   "3" = "third ", "4" = "fourth ", "5" = "fifth ", "6" = "sixth ", 
                   "7" = "seventh ", "8" = "eighth ", "9" = "ninth ", "10" = "tenth ", 
                   paste0(as.character(x), "th "))
          }
        }
        
        total_rank <- ordinal_form(total_rank)
        borough_rank <- ordinal_form(borough_rank)
        
        
        HTML(str_glue(
          paste0("<p><b>{z$name}</b><p>",
                 "<p>The green space {z$name} is a `{z$type}` of ",
                 "{prettyNum(z$area, big.mark = ',')} m^2. It is ",
                 "categorized as a `{z$type_2}`",
                 "and is of `{z$property}` property. Its ",
                 "management entity is `{z$management}`.</p>",
                 "<p>It is the {total_rank}biggest {type} in the ",
                 "City, and the {borough_rank} largest in {borough}.</p>")))
      }})
    
    info_table <- reactive({
      if (standard_info()) info_table_standard() else info_table_green_space()
    })
    
    # Display info_table if it isn't NULL
    output$info_table <- renderUI({
      if (!is.null(info_table())) info_table()
    })
    
    # Render the graph
    explore_graph_standard <- tryCatch(explore_graph_server(id = "explore",
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
    
    explore_graph_green_space <- reactive({
      if (is.na(selection())) {
      ggplot(x(), aes(area)) +
        geom_histogram(aes(fill = fill), alpha = 0.5, bins = 25) +
        scale_fill_manual(values = rev(col_left_5), na.translate = FALSE) + 
        labs(x = "Green space area (log10)", y = NULL) + 
        scale_x_log10() +
        theme_minimal() +
        theme(legend.position = "none", panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(size = 8))
      } else {
        select_id <- selection()
        
        ggplot(x(), aes(area)) +
          geom_histogram(aes(fill = round(area) == 
                               round(area[ID == select_id])),
                         bins = 25) +
          scale_fill_manual(values = col_left_5[c(1, 5)], 
                            na.translate = FALSE) +
          labs(x = "Green space area (log10)", y = NULL) + 
          scale_x_log10() +
          theme_minimal() +
          theme(legend.position = "none", panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.title = element_text(size = 8))
      }
    })
    
    explore_graph <- reactive({
      if (standard_info()) explore_graph_standard() else explore_graph_green_space()
    })

    # Display graph if it isn't NULL
    output$explore_graph <- renderPlot({
      if (!is.null(explore_graph())) explore_graph()
    })

    # Only show panel if there's something to show
    show_panel <- reactive(!is.null(info_table()) || !is.null(explore_graph()))
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
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
