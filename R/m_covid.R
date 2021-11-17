### COVID MODULE ###############################################################

# UI ----------------------------------------------------------------------

covid_UI <- function(id) {
  tabItem(tabName = "covid",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"), 
                   select_var_UI(NS(id, "left"), var_list_covid)),
          right_panel(id, popup_UI(NS(id, "popup"))))
  }

# Server ------------------------------------------------------------------

covid_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    # Title bar
    title_server("title", "covid")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_covid, zoom = map_zoom_covid, 
        location = map_location) %>%
        add_path(data = covid_may_2020, update_view = FALSE, #id = "ID", 
                 stroke_colour = "Type",
                 # stroke_colour = "#5F940E", 
                 stroke_width = 10, width_min_pixels = 2, width_max_pixels = 5,
                 auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
                 legend = TRUE) %>%
        add_scatterplot(data = covid_pics, update_view = FALSE, #id = "ID",
                        fill_colour = "#FF0000", radius = 30,
                        radius_min_pixels = 2, radius_max_pixels = 7,
                        auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      })
    
    # Left variable server
    var_left_covid <- select_var_server("left", reactive(var_list_covid))
    
    # Data 
    data_covid <- reactive(get(var_left_covid()))

    # Selection data
    data_to_add <- reactive({
      if (!is.na(rv_covid$path_selected)) {
        data_to_add <- data_covid() %>% filter(ID == rv_covid$path_selected) 
        } else if (!is.na(rv_covid$point_selected)) {
          data_to_add <- covid_pics %>% filter(ID == rv_covid$point_selected) 
        } else data_to_add <- tibble()
      
      data_to_add
      })
    
    # # Left map
    # small_map_server("left", reactive(paste0(
    #   "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
    
    # Update map in response to variable change
    observeEvent(data_covid(), {
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_path(
            data = data_covid(), update_view = FALSE, # id = "ID",
            stroke_width = 10, width_min_pixels = 2, width_max_pixels = 5,
            stroke_colour = "type",
            # stroke_colour = "#5F940E", 
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
            legend = TRUE)
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
            width_min_pixels = 3, width_max_pixels = 6,
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
    
    # Popup server
    # popup_server("popup", data_to_add, 
    #              fields = c("Street" = "street", "Type" = "type"),
    #              images = reactive(paste0("www/covid_pics/", data_to_add()$ID,
    #                                       ".png")))
    # 
  })
}