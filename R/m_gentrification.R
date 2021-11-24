### GI MODULE ##############################################################

# UI ----------------------------------------------------------------------

gentrification_UI <- function(id) {
  tabItem(tabName = "gentrification index",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   div(style = widget_style, 
                       sliderInput(
                         NS(id, "slider_uni"), 
                         i18n$t("Select a year"),
                         min = gentrification_slider$min,
                         max = gentrification_slider$max,
                         step = gentrification_slider$interval, sep = "",
                         value = gentrification_slider$init,
                         width = "170px"))
          ),
          right_panel(id,
                      compare_UI(NS(id, "gentrification"), var_list_gentrification),
                      explore_UI(NS(id, "explore")),
                      dyk_UI(NS(id, "dyk"))
                      ),
          legend_bivar_UI(NS(id, "gentrification"))
          )
  }


# Server ------------------------------------------------------------------

gentrification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "gentrification")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_gentrification,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(gentrification_ind_q3, "- 1")) %>%
                      left_join(colour_borough, by = "group"),
          stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
          update_view = FALSE, id = "ID", auto_highlight = TRUE,
          highlight_colour = "#FFFFFF90")
      })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_gentrification$zoom <- case_when(input$map_view_change$zoom >= 12 ~ "DA",
                                  input$map_view_change$zoom >= 10.5 ~ "CT",
                                  TRUE ~ "borough")
      })
    
    # Compare panel
    var_right_gentrification <- compare_server("gentrification", var_list_gentrification,
                                       reactive(rv_gentrification$zoom))
    
    # Get time from slider
    time <- slider_server("left")
    
    # Construct left variable string
    var_left_gentrification <- reactive(
      stringr::str_remove(paste(
        "gentrification", 
        time(), 
        sep = "_"), "_ $")
    )

    # Data
    data <- data_server("gentrification", var_left_gentrification, var_right, 
                           reactive(rv_gentrification$zoom))
    
    # Explore panel
    explore_server("explore", data, reactive("gentrification_ind"),
                   var_right, reactive(rv_gentrification$poly_selected),
                   reactive(rv_gentrification$zoom), reactive("Gentrification index"))

    # Did-you-know panel
    dyk_server("dyk", reactive("gentrification_ind"), var_right)

    # Left map
    small_map_server("left", reactive(paste0(
      "left_", sub("_2", "", rv_gentrification$zoom), "_gentrification_ind")))
    
    # Bivariate legend
    legend_bivar_server("gentrification", var_right)
    
    # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      rv_gentrification$zoom}, {
        width <- switch(rv_gentrification$zoom, "borough" = 100, "CT" = 10, 2)
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data(), stroke_width = width,
            stroke_colour = "#FFFFFF", fill_colour = "fill",
            update_view = FALSE, id = "ID", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
        })

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_gentrification$poly_selected <- NA
      } else rv_gentrification$poly_selected <- lst$object$properties$id
      })

    # Clear poly_selected on zoom
    observeEvent(rv_gentrification$zoom, {rv_gentrification$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_gentrification$poly_selected, {
      if (!is.na(rv_gentrification$poly_selected)) {
        width <- switch(rv_gentrification$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() %>%
          filter(ID == rv_gentrification$poly_selected) %>%
          mutate(fill = substr(fill, 1, 7))

        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_to_add, stroke_width = width, stroke_colour = "#000000",
            fill_colour = "fill", update_view = FALSE,
            layer_id = "poly_highlight", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
        } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
        }
      })

    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_gentrification$poly_selected <- NA})
    
  })
}
