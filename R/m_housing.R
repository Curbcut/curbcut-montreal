### HOUSING MODULE ##############################################################

# UI ----------------------------------------------------------------------

housing_UI <- function(id) {
  tabItem(tabName = "housing",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   select_var_UI(NS(id, "left"), var_list_housing_left), 
                   #can't hide and show widgets when it isn't the original sliderInput function
                   sliderInput(NS(id, "slider_housing"), "Select a year", 
                               min = housing_slider$min,
                               max = housing_slider$max, 
                               step = housing_slider$interval, sep = "", 
                               value = housing_slider$init),
                   # slider_UI(NS(id, "slider_housing"), 
                   #           slider_min = housing_slider$min, 
                   #           slider_max = housing_slider$max, 
                   #           slider_interval = housing_slider$interval, 
                   #           slider_init = housing_slider$init),
                   sliderInput(NS(id, "slider_bi_census"), "Select two census year", 
                               min = housing_slider$min,
                               max = housing_slider$max, 
                               step = housing_slider$interval, sep = "", 
                               value = c("2006", "2016")),
                   htmlOutput(NS(id, "year_displayed")), br(),
                   materialSwitch(inputId = NS(id, "bi_census"),
                     label = "Two census comparison", right = TRUE),
                   shinyjs::useShinyjs() # needed if we continue to have 2 sliders
                   ),
          right_panel(id, 
                      compare_UI(NS(id, "housing"), var_list_housing_right),
                      explore_UI(NS(id, "explore")),
                      dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "housing")))
}


# Server ------------------------------------------------------------------

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # observe({
    #   shinyjs::toggleState("slider_bi_census", condition = (input$bi_census == TRUE))
    # })

    
    # Title bar
    title_server("title", "housing")
    
    # memory <- reactiveValues(previous = "housing_tenant_prop")
    # 
    # previous_value <- reactive({
    #   
    #   isolate(previous <- memory$previous)
    #   
    #   memory$previous <- data.frame(previous, var_left_housing_1()) %>% dplyr::select(ncol(.)-1, ncol(.))
    #   
    #   return(memory$previous[1,1])
    # })
    # 
    output$year_displayed <- renderText({
      year_showed <- str_extract(var_left_housing(), "\\d{4}$")
      if (year_showed != time()){
        str_glue("Displayed data is for the closest available year (<b>{year_showed}</b>).")
      }

    })

    # # Map
    # output$map <- renderMapdeck({
    #   mapdeck(
    #     style = map_style, token = token_housing,
    #     zoom = map_zoom, location = map_location) %>%
    #     add_polygon(data = {borough %>% 
    #         dplyr::select(ID, name, name_2, population, 
    #                       left_var_full = housing_tenant_prop,
    #                       left_var = housing_tenant_prop_q3) %>% 
    #         mutate(group = paste(left_var, "- 1")) %>% 
    #         left_join(colour_borough, by = "group")}, stroke_width = 0, 
    #         stroke_colour = "#FFFFFF", fill_colour = "fill", update_view = FALSE,
    #         id = "ID", auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
    # })
    # 
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_housing,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(eval(as.name(paste0("housing_tenant_prop_q3", "_", 
                                                  current_census))), "- 1")) %>%
                      left_join(colour_borough, by = "group"),
                    stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_housing$zoom <- case_when(
        input$map_view_change$zoom >= 12 ~ "DA",
        input$map_view_change$zoom >= 10.5 ~ "CT",
        TRUE ~ "borough")
    })
    
    # String to fetch maps and data
    df <- reactive(rv_housing$zoom)
    #df <- reactive({if (input$grid) "grid" else rv_housing$zoom})
    
    # Enable or disable first and second slider.
    observeEvent(input$bi_census, {
      if(input$bi_census == F){
        shinyjs::hide("slider_bi_census")
      } else {
        shinyjs::show("slider_bi_census")
      }
    })
    
    observeEvent(input$bi_census, {
      if(input$bi_census == F){
        shinyjs::show("slider_housing")
      } else {
        shinyjs::hide("slider_housing")
      }
    })
    
    # Time variable depending on which slider
    time <- reactive({
      if (input$bi_census == F) {
        input$slider_housing
      } else {
        input$slider_bi_census
      }
      })
    
    # Greyed out left list options, depending of the year(s) chosen
    var_list_housing_left_available <- reactive({
      if (input$bi_census == F) {
      # !eval(unlist(
      #   purrr::modify_depth(var_list_housing_left, 2, paste0, "_", time())) %in%
      #   names(borough))
        NULL
      } else {
        # t_or_f <- unlist(purrr::modify_depth(var_list_housing_left, 2, paste0, "_", time())) %in% names(borough)
        # t_or_f <- unname(tapply(t_or_f, (seq_along(t_or_f)-1) %/% 2, sum)) - 1
        # t_or_f[!t_or_f %in% c(0,1)] <- 0
        # !t_or_f
        disabled_var_list_housing_left
      }
    })
    
    # Left variable server
    var_left_housing_1 <- select_var_server("left", reactive(var_list_housing_left),
                                            disabled_choices = reactive(var_list_housing_left_available())
                                            )
    
    # Construct left variable string
    var_left_housing <- reactive({
      
      if (input$bi_census == F) {
        var <- paste(var_left_housing_1(), time(), sep = "_")
        
        if (!var %in% names(borough)) {
          x <- borough %>% 
            select(contains(str_remove(var, "_\\d{4}$"))) %>% 
            names() %>% 
            str_extract(., "\\d{4}$") %>% 
            as.numeric() %>% na.omit()
          closest_year <-  x[which.min(x - time())]
          var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
        }
        
        var
      } else {
        paste(var_left_housing_1(), time(), sep = "_")
      }
    })

    # Greyed out right list options, depending of the year chosen
    var_list_housing_right_available <- reactive({
      if (input$bi_census == F) {
        # (!eval(unlist(
        #   purrr::modify_depth(var_list_housing_right, 2, paste0, "_", time())) %in%
        #     names(borough))) %>% replace(1, F)
        NULL
      } else {
        # t_or_f <- unlist(lapply(unlist(var_list_housing_right), paste0, "_", time())) %in% names(borough)
        # t_or_f <- unname(tapply(t_or_f, (seq_along(t_or_f)-1) %/% 2, sum)) - 1
        # t_or_f[!t_or_f %in% c(0,1)] <- 0
        # (!t_or_f) %>% replace(1, F)
        disabled_var_list_housing_right
      }
    })


    # Right variable server
    var_right_housing_1 <- compare_server("housing", var_list_housing_right, df,
                                          disabled_choices = reactive(var_list_housing_right_available())
                                          )

    var_right_housing <- reactive({
      if (var_right_housing_1() != " ") {
      
        var <- paste(var_right_housing_1(), time(), sep = "_")
        
        if (!var %in% names(borough)) {
          x <- borough %>% 
            select(contains(str_remove(var, "_\\d{4}$"))) %>% 
            names() %>% 
            str_extract(., "\\d{4}$") %>% 
            as.numeric() %>% na.omit()
          closest_year <-  x[which.min(x - time())]
          var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
        }
        
        var

        } else var_right_housing_1()
    })

    # Data
    data_housing <- data_server("housing", var_left_housing,
                                var_right_housing, df,
                                reactive(rv_housing$zoom))
    # # Explore panel
    # explore_server(id = "explore",
    #            x = data_housing,
    #            var_left = var_left_housing,
    #            var_right = var_right_housing,
    #            select = reactive(rv_housing$poly_selected),
    #            zoom = df,
    #            var_left_title = reactive(
    #              names(var_list_housing_left))
    #            #var_left_label = sus_translate(climate_legend)
    #            )

    # Explore panel
    explore_server("explore", data_housing, var_left_housing,
                   var_right_housing, reactive(rv_housing$poly_selected),
                   reactive(rv_housing$zoom), reactive(names(var_list_housing_left))
                   )

    # Did-you-know panel
    dyk_server("dyk", var_left_housing, var_right_housing)

    # Left map
    small_map_server("left", reactive(paste0("left_", df, "_",
                                             var_left_housing())))

    # Bivariate legend
    legend_bivar_server("housing", var_right_housing)

    # Update map in response to variable changes or zooming
    observeEvent({
      var_left_housing()
      var_right_housing()
      rv_housing$zoom
    }, {

      width <- switch(rv_housing$zoom, "borough" = 100, "CT" = 10, 2)

      mapdeck_update(map_id = NS(id, "map")) %>%
        add_polygon(data = data_housing(), stroke_width = width,
                    stroke_colour = "#FFFFFF", fill_colour = "fill",
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90")
    }
    )

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_housing$poly_selected <- NA
      } else rv_housing$poly_selected <- lst$object$properties$id
    })

    # Clear poly_selected on zoom
    observeEvent(rv_housing$zoom, {rv_housing$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_housing$poly_selected, {
      if (!is.na(rv_housing$poly_selected)) {
        width <- switch(rv_housing$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data_housing() %>%
          filter(ID == rv_housing$poly_selected) %>%
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
      rv_housing$poly_selected <- NA})

  })
}
