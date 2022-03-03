### COVID MODULE ###############################################################

# UI ----------------------------------------------------------------------

covid_UI <- function(id) {
  ns_id <- "covid"
  
  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      select_var_UI(NS(id, ns_id), var_list = var_list_covid),
      bottom = div(class = "bottom_sidebar",
                   tagList(
                     h5("Legend", style = "font-size: 12px;"),
                     uiOutput(NS(id, "legend_render"))))),
    
    # Map
    div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
    
    # Right panel
    # MUST ADD A `HIDDEN` ENTIRE RIGHT PANEL IF DYK IS EMPTY
    right_panel(
      id = id, 
      dyk_UI(NS(id, ns_id)))
  ))
}

# Server ------------------------------------------------------------------

covid_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "covid"
    
    # Initial reactives
    selection <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(id = ns_id, x = "covid")
    
    # Legend
    output$legend_render <- renderUI({
      output$legend <- renderPlot({
        cowplot::plot_grid(covid_legend)})
      plotOutput(NS(id, "legend"), height = 160, width = "100%")
    })
    
    renderPlot({cowplot::plot_grid(covid_legend)})
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Left variable server
    var_left <- select_var_server(ns_id, var_list = reactive(var_list_covid))
    
    # Data 
    data <- reactive(filter(covid, timeframe == var_left()))
    
    # Did-you-know panel
    dyk_server(ns_id, reactive(paste0("covid_", var_left(), "_2020")),
               reactive(" "))
    
    # Update map in response to variable change
    observeEvent(data(), {
      mapdeck_update(map_id = NS(id, "map")) %>%
        add_path(
          data = data(), update_view = FALSE, # id = "ID",
          stroke_width = 10, width_min_pixels = 2, width_max_pixels = 5,
          stroke_colour = "fill", tooltip = "type",
          auto_highlight = TRUE, highlight_colour = "#FFFFFF90") |> 
        add_scatterplot(data = covid_pics, update_view = FALSE,
                        fill_colour = "#006D2CAA", radius = 25, id = "ID",
                        radius_min_pixels = 8, auto_highlight = TRUE, 
                        highlight_colour = "#FFFFFF90")
    })
    
    # Update point_selected on click to trigger image popup
    observeEvent(input$map_scatterplot_click, {
      lst <- fromJSON(input$map_scatterplot_click)$index
      if (is.null(lst)) selection(NA) else {
        # This is a hack because of a mapdeck bug
        selection(covid_pics[lst + 1,]$ID)
      }
    })
    
    # Popup
    observeEvent(selection(), {
      if (!is.na(selection())) {
        
        street <- 
          covid_pics[covid_pics$ID == selection(), ]$street
        
        photo_id <- 
          str_glue("covid_pics/{selection()}.png")
        
        showModal(modalDialog(
          title = HTML(street),
          HTML(paste0('<img src="', photo_id, '", width = 100%>')),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ))
      }
    })
  })
}