### CLIMATE RISK MODULE ########################################################

# UI ----------------------------------------------------------------------

climate_risk_UI <- function(id) {
  tabItem(tabName = "climate_risk",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   select_var_UI(NS(id, "left"), var_list_climate_risk), 
                   shinyWidgets::materialSwitch(
                     inputId = NS(id, "grid"), label = i18n$t("250-metre grid"), 
                     status = "primary", value = TRUE)),
          right_panel(id, compare_UI(NS(id, "climate_risk"), var_list_canale),
                      explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "climate_risk")))
}


# Server ------------------------------------------------------------------

climate_risk_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "climate_risk")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_climate_risk, zoom = map_zoom, 
              location = map_location) %>%
        add_polygon(data = {grid %>% 
            dplyr::select(ID, name, name_2, population, 
                          left_var_full = destructive_storms_ind,
                          left_var = destructive_storms_ind_q3) %>% 
            mutate(group = paste(left_var, "- 1")) %>% 
            left_join(colour_borough, by = "group")}, stroke_width = 0, 
            stroke_colour = "#FFFFFF", fill_colour = "fill", update_view = FALSE,
            id = "ID", auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_climate_risk$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "building",
                                        input$map_view_change$zoom >= 12 ~ "DA",
                                        input$map_view_change$zoom >= 10.5 ~ "CT",
                                        TRUE ~ "borough")
    })
    
    # Left variable server
    var_left <- select_var_server("left", reactive(var_list_climate_risk))
    
    # String to fetch maps and data
    df <- reactive(if (input$grid) "grid" else rv_climate_risk$zoom)
    
    # Compare panel
    var_right <- compare_server("climate_risk", var_list_canale, df)
    
    # Data
    data <- data_server("climate_risk", var_left, var_right, df, 
                        reactive(rv_climate_risk$zoom))
    
    # # Explore panel
    explore_server(id = "explore", 
                   x = data, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_climate_risk$poly_selected),
                   zoom = df, 
                   var_left_label = sus_translate(climate_legend))
    
    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)
    
    # Left map
    small_map_server("left", reactive(paste0("left_", df(), "_", var_left())))
    
    # Bivariate legend
    legend_bivar_server("climate_risk", var_right)
    
    # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      df()}, map_change(NS(id, "map"), df = data, zoom = df))
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_climate_risk$poly_selected <- NA
      } else rv_climate_risk$poly_selected <- lst$object$properties$id
    })
    
    # Clear poly_selected on zoom
    observeEvent(rv_climate_risk$zoom, {
      if (!input$grid) rv_climate_risk$poly_selected <- NA}, ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_climate_risk$poly_selected, {
      if (!is.na(rv_climate_risk$poly_selected)) {
        width <- 
          switch(rv_climate_risk$zoom, "borough" = 20, "CT" = 10, "DA" = 2)
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
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
      }
    })
    
    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_climate_risk$poly_selected <- NA})
    
  })
}
