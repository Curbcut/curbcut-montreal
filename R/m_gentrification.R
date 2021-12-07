### GI MODULE ##############################################################

# UI ----------------------------------------------------------------------

gentrification_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         sliderInput(
                           NS(id, "slider_time"), 
                           i18n$t("Select two years"),
                           min = gentrification_slider$min,
                           max = gentrification_slider$max,
                           step = gentrification_slider$interval, sep = "",
                           value = gentrification_slider$init,
                           width = "95%"),
                         checkboxInput(NS(id, "check_single_var"),
                                       label = i18n$t(paste0("Review a single variable ",
                                                             "part of the index"))),
                         select_var_UI(NS(id, "left"), 
                                       var_list_left_gentrification),
                         div(class = "bottom_sidebar",
                             tagList(legend_UI(NS(id, "legend")),
                                     zoom_UI(NS(id, "zoom"), gentrification_zoom)))
      )),
      fillCol(
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
        right_panel(id, compare_UI(NS(id, "gentrification"), var_list_right_gentrification),
                    div(class = "explore_dyk",
                        explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))))),
      flex = c(1, 5)
    )
  )
  
}


# Server ------------------------------------------------------------------

gentrification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server("sidebar", "gentrification", 
                   reactive(paste0("left_", zoom(), "_", var_left())))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_gentrification,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(
          data = data(), stroke_width = 100,
          stroke_colour = "#FFFFFF", fill_colour = "fill",
          update_view = FALSE, id = "ID", auto_highlight = TRUE,
          highlight_colour = "#FFFFFF90")
    })
    
    # Zoom
    zoom_val <- reactiveVal(get_zoom(map_zoom, gentrification_zoom))
    observeEvent(input$map_view_change$zoom, {
      zoom_val(get_zoom(input$map_view_change$zoom, gentrification_zoom))
    })
    zoom <- zoom_server("zoom", zoom = zoom_val, zoom_levels = gentrification_zoom)
    
    # Get time from slider
    time <- reactive({input$slider_time})
    
    # Compare panel
    var_right <- compare_server(id = "gentrification", 
                                var_list = var_list_right_gentrification,
                                df = zoom,
                                time = time)
    
    # Get single_var value to use if check_single_var is clicked
    single_var <- select_var_server(
      "left", reactive(var_list_left_gentrification), time = time, df = zoom())
    
    
    # If check_single_var is clicked, toggle on the dropdown menu
    observeEvent(input$check_single_var, {
      shinyjs::toggle("left-var", condition = input$check_single_var)
    })
    
    # Construct left variable string
    var_left <- reactive({
      if (!input$check_single_var) {
        stringr::str_remove(paste(
          "gentrification_ind",
          time(),
          sep = "_"), "_ $")
      } else {
        single_var()
      }
    })
    
    # Data
    data <- data_server(id = "gentrification", var_left = var_left,
                        var_right = var_right, df = zoom, zoom = zoom_val)
    
    # Explore panel
    explore_content <- explore_server(id = "explore",
                                   x = data,
                                   var_left = var_left,
                                   var_right = var_right,
                                   select = reactive(rv_gentrification$poly_selected),
                                   zoom = zoom,
                                   build_str_as_DA = TRUE)
    
    # Legend
    legend_graph <- legend_server("legend", var_left, var_right, zoom_val)
    
    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)
    
    # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      zoom()}, map_change(NS(id, "map"), df = data, zoom = zoom))
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_gentrification$poly_selected <- NA
      } else rv_gentrification$poly_selected <- lst$object$properties$id
    })
    
    # Update map in response to poly_selected change
    observe({
      if (!is.na(rv_gentrification$poly_selected)) {
        
        if (rv_gentrification$poly_selected %in% data()$ID) {
          
          width <- switch(zoom(), "borough" = 100, "CT" = 10, 2)
          
          data_to_add <-
            data() %>%
            filter(ID == rv_gentrification$poly_selected) %>%
            mutate(fill = substr(fill, 1, 7))
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            add_polygon(
              data = data_to_add, elevation = 5, fill_colour = "fill",
              update_view = FALSE, layer_id = "poly_highlight",
              auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
          
        } else {rv_gentrification$poly_selected <- NA}
        
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
      }
    })
    
    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_gentrification$poly_selected <- NA})
    
    
    # Bookmarking 
    onBookmark(function(state) {
      state$values$zoom_val <- zoom_val()
      state$values$numeric_zoom <- input$map_view_change$zoom
      state$values$location <- c(input$map_view_change$longitude, 
                                 input$map_view_change$latitude)
      state$values$poly_selected <- rv_gentrification$poly_selected
      state$values$var_right <- var_right()
    })
    
    onRestored(function(state) {
      restored_numeric_zoom <- state$values$numeric_zoom
      restored_map_location <- state$values$location
      zoom_val(state$values$zoom_val)
      
      output$map <- renderMapdeck({
        mapdeck(
          style = map_style, token = token_gentrification,
          zoom = restored_numeric_zoom, location = restored_map_location)
      })
      
      updatePickerInput(
        session = session,
        inputId = NS(id, "compare-var"),
        choices = sus_translate(var_list_right_gentrification),
        selected = unique(str_remove(state$values$var_right, "_\\d{4}"))
      )
      
      rv_gentrification$poly_selected <- state$values$poly_selected
    })
    
    # OUT
    reactive({list(module_short_title = "gentrification",
                   module_id = "gentrification",
                   time = time(),
                   data = data(),
                   token = token_gentrification,
                   map_zoom = input$map_view_change$zoom,
                   map_location = c(input$map_view_change$longitude, 
                                    input$map_view_change$latitude),
                   zoom = zoom(),
                   explore_content = explore_content(),
                   poly_selected = rv_gentrification$poly_selected,
                   legend_graph = legend_graph())})
    
  })
}