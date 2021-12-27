### CLIMATE RISK MODULE ########################################################

# UI ----------------------------------------------------------------------

climate_risk_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         select_var_UI(NS(id, "left"), var_list_climate_risk), 
                         checkboxInput(
                           inputId = NS(id, "grid"), value = TRUE,
                           label = i18n$t("250-metre grid")),
                         div(class = "bottom_sidebar",
                             tagList(legend_UI(NS(id, "legend")),
                                     zoom_UI(NS(id, "zoom"), climate_risk_zoom)))
      )),
      fillCol(
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
        right_panel(id, compare_UI(NS(id, "climate_risk"), make_dropdown()),
                    div(class = "explore_dyk",
                        explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))))),
      flex = c(1, 5)
    )
  )
}


# Server ------------------------------------------------------------------

climate_risk_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    sidebar_server("sidebar", "climate_risk", 
                   reactive(paste0("left_", zoom(), "_", var_left())))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_climate_risk, zoom = map_zoom, 
              location = map_location) %>%
        add_polygon(data = {grid %>% 
            dplyr::select(ID, name, name_2, population, 
                          left_var_full = climate_destructive_storms_ind,
                          left_var = climate_destructive_storms_ind_q5) %>% 
            mutate(group = paste(left_var, "- 1")) %>% 
            left_join(colour_bivar_borough, by = "group")}, stroke_width = 0, 
            stroke_colour = "#FFFFFF", fill_colour = "fill", update_view = FALSE,
            id = "ID", auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
    })
    
    # Zoom
    zoom_val <- reactiveVal(get_zoom(map_zoom, climate_risk_zoom))
    observeEvent(input$map_view_change$zoom, {
      zoom_val(get_zoom(input$map_view_change$zoom, climate_risk_zoom))
    })
    zoom <- zoom_server("zoom", zoom = zoom_val, zoom_levels = climate_risk_zoom)
    
    # If grid isn't clicked, toggle on the zoom menu
    observeEvent(input$grid, {
      shinyjs::toggle("zoom-auto", condition = !input$grid)
      shinyjs::toggle("zoom-slider", condition = !input$grid)
    })
    
    # Left variable server
    var_left <- select_var_server("left", reactive(var_list_climate_risk))
    
    # String to fetch maps and data
    df <- reactive(if (input$grid) "grid" else zoom())
    
    # Compare panel
    var_right <- compare_server("climate_risk", make_dropdown(), df)
    
    choropleth_zoom <- reactive(if (input$grid) "borough" else zoom_val())
    
    # Data
    data <- data_server("climate_risk", var_left, var_right, df, 
                        reactive(choropleth_zoom()))
    
    # # Explore panel
    explore_server(id = "explore", 
                   x = data, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_climate_risk$poly_selected),
                   zoom = df, 
                   var_left_label = climate_legend)
    
    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)
    
    # Left map
    small_map_server("left", reactive(paste0("left_", df(), "_", var_left())))
    
    # Legend
    legend_server("legend", var_left, var_right, zoom_val)
    
    # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      data()}, map_change(NS(id, "map"), df = data, zoom = df))
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_climate_risk$poly_selected <- NA
      } else rv_climate_risk$poly_selected <- lst$object$properties$id
    })
    
    # Clear poly_selected on zoom
    # observeEvent(zoom(), {
    #   if (!input$grid) rv_climate_risk$poly_selected <- NA}, ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observe({
      if (!is.na(rv_climate_risk$poly_selected)) {
        
        if (rv_climate_risk$poly_selected %in% data()$ID) {
          
          width <- 
            switch(zoom(), "borough" = 20, "CT" = 10, "DA" = 2)
          data_to_add <-
            data() %>%
            filter(ID == rv_climate_risk$poly_selected) %>%
            mutate(fill = substr(fill, 1, 7))
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            add_polygon(
              data = data_to_add, stroke_width = width, stroke_colour = "#000000",
              fill_colour = "fill", update_view = FALSE,
              layer_id = "poly_highlight", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90")
        } else if (!input$grid) {rv_climate_risk$poly_selected <- NA}
        
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
      }
    })
    
    # Update map in response to poly_selected change
    # observeEvent(rv_climate_risk$poly_selected, {
    #   if (!is.na(rv_climate_risk$poly_selected)) {
    #     width <- 
    #       switch(zoom(), "borough" = 20, "CT" = 10, "DA" = 2)
    #     data_to_add <-
    #       data() %>%
    #       filter(ID == rv_climate_risk$poly_selected) %>%
    #       mutate(fill = substr(fill, 1, 7))
    #     
    #     mapdeck_update(map_id = NS(id, "map")) %>%
    #       add_polygon(
    #         data = data_to_add, stroke_width = width, stroke_colour = "#000000",
    #         fill_colour = "fill", update_view = FALSE,
    #         layer_id = "poly_highlight", auto_highlight = TRUE,
    #         highlight_colour = "#FFFFFF90")
    #   } else {
    #     mapdeck_update(map_id = NS(id, "map")) %>%
    #       clear_polygon(layer_id = "poly_highlight")
    #   }
    # })
    
    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_climate_risk$poly_selected <- NA})
  
  # Bookmarking 
  onBookmark(function(state) {
    
    state$values$zoom_val <- zoom_val()
    state$values$numeric_zoom <- input$map_view_change$zoom
    state$values$location <- c(input$map_view_change$longitude, 
                               input$map_view_change$latitude)
    state$values$poly_selected <- rv_climate_risk$poly_selected
    state$values$var_right <- var_right()
    state$values$var_left <- var_left()
  })
  
  onRestored(function(state) {
    restored_numeric_zoom <- state$values$numeric_zoom
    restored_map_location <- state$values$location
    zoom_val(state$values$zoom_val)
    
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_climate_risk,
        zoom = restored_numeric_zoom, location = restored_map_location)
    })
    
    updatePickerInput(
      session = session,
      inputId = NS(id, "compare-var"),
      choices = sus_translate(make_dropdown()),
      selected = state$values$var_right
    )
    
    # Not working, no idea why?
    updatePickerInput(
      session = session,
      inputId = NS(id, "left-var"),
      choices = sus_translate(var_list_climate_risk),
      selected = state$values$var_left
    )
    
    if (input$grid) {map_change(NS(id, "map"), df = data, zoom = reactive("grid"))}
    
    rv_climate_risk$poly_selected <- state$values$poly_selected
  })
  
  })
}
