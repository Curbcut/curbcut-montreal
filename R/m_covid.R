### COVID MODULE ###############################################################

# UI ----------------------------------------------------------------------

covid_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         select_var_UI(NS(id, "left"), var_list_covid),
                         div(class = "bottom_sidebar",
                             tagList(  tagList(
                               h5("Legend", style = "font-size: 12px;"),
                               uiOutput(NS(id, "legend_render"))
                             ))))
      ),
      fillCol(
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
      right_panel(id, dyk_UI(NS(id, "dyk")))),
      flex = c(1, 5)
    )
  )
  }

# Server ------------------------------------------------------------------

covid_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server("sidebar", "covid")
    
    # Legend
    output$legend_render <- renderUI({
      output$legend <- renderPlot({
        cowplot::plot_grid(covid_legend)
      })
      plotOutput(NS(id, "legend"), height = 160, width = "100%")
    })
      
      renderPlot({cowplot::plot_grid(covid_legend)})
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_covid, zoom = map_zoom_covid, 
        location = map_location) %>%
        add_path(data = filter(covid, timeframe == "may_2020"), update_view = FALSE, #id = "ID", 
                 stroke_colour = "fill", tooltip = "type",
                 stroke_width = 10, width_min_pixels = 2, width_max_pixels = 5,
                 auto_highlight = TRUE, highlight_colour = "#FFFFFF90") %>%
        add_scatterplot(data = covid_pics, update_view = FALSE, #id = "ID",
                        fill_colour = "#FF0000", radius = 30,
                        radius_min_pixels = 2, radius_max_pixels = 7,
                        auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      })
    
    # Left variable server
    var_left <- select_var_server("left", reactive(var_list_covid))
    
    # Data 
    data <- reactive(filter(covid, timeframe == var_left()))

    # Selection data
    data_to_add <- reactive({
      if (!is.na(rv_covid$path_selected)) {
        data_to_add <- data() %>% filter(ID == rv_covid$path_selected) 
        } else if (!is.na(rv_covid$point_selected)) {
          data_to_add <- covid_pics %>% filter(ID == rv_covid$point_selected) 
        } else data_to_add <- tibble()
      
      data_to_add
      })
    
    # Did-you-know panel
    dyk_server("dyk", reactive(paste0("covid_", var_left(), "_2020")),
               reactive(" "))
    
    # Update map in response to variable change
    observeEvent(data(), {
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_path(
            data = data(), update_view = FALSE, # id = "ID",
            stroke_width = 10, width_min_pixels = 2, width_max_pixels = 5,
            stroke_colour = "fill", tooltip = "type",
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      })
    
    # Update path_selected on click
    observeEvent(input$map_path_click, {
      lst_path <- jsonlite::fromJSON(input$map_path_click)
      if (is.null(lst_path$object$properties$id)) {
        rv_covid$path_selected <- NA
      } else {
        rv_covid$point_selected <- NA
        rv_covid$path_selected <- lst_path$object$properties$id
        }
      })
    
    # Update point_selected on click
    observeEvent(input$map_scatterplot_click, {
      lst_scatterplot <- jsonlite::fromJSON(input$map_scatterplot_click)
      if (is.null(lst_scatterplot$index)) {
        rv_covid$point_selected <- NA
      } else {
        rv_covid$path_selected <- NA
        # This is a hack because of a mapdeck bug
        rv_covid$point_selected <- covid_pics[lst_scatterplot$index + 1,]$ID
      }
      })
    
    # Update map in response to data_to_add change
    observeEvent(data_to_add(), {

      # print(data_to_add())

      # Draw path
      if (!is.na(rv_covid$path_selected)) {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_path(layer_id = "point_highlight") %>%
          add_path(
            data = data_to_add(), stroke_width = 10, stroke_colour = "#000000",
            width_min_pixels = 3, width_max_pixels = 6, tooltip = "type",
            update_view = FALSE, layer_id = "path_highlight",
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")

      # Draw point
      } else if (!is.na(rv_covid$point_selected)) {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_path(layer_id = "path_highlight") %>%
          add_scatterplot(
            data = data_to_add(), 
            stroke_colour = "#FFFFFF", fill_colour = "#000000",
            radius = 40, radius_min_pixels = 3, radius_max_pixels = 8,
            update_view = FALSE, layer_id = "point_highlight",
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")

      # Clear both
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_path(layer_id = "path_highlight") %>%
          clear_path(layer_id = "point_highlight")

      }
    })
    
    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    # observeEvent(input$`explore-clear_selection`, {
    #   rv_covid$path_selected <- NA})
    
    # Popup
    observeEvent(rv_covid$point_selected, {
      if (!is.na(rv_covid$point_selected)) {
        
        street <- 
        covid_pics[covid_pics$ID == rv_covid$point_selected, ]$street
        
        photo_id <- 
        str_glue("covid_pics/{rv_covid$point_selected}.png")
        
        showModal(modalDialog(
          title = HTML(street),
          HTML(paste0('<img src="', photo_id, '", width = 100%>')),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ))
      }
    })
    
    # Popup server
    # popup_server("popup", data_to_add, 
    #              fields = c("Street" = "street", "Type" = "type"),
    #              images = reactive(paste0("www/covid_pics/", data_to_add()$ID,
    #                                       ".png")))
    # 
  })
}