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
                   sliderInput(NS(id, "slider_bi_census"), "Select two census year", 
                               min = housing_slider$min,
                               max = housing_slider$max, 
                               step = housing_slider$interval, sep = "", 
                               value = c("2006", "2016")),
                   htmlOutput(NS(id, "year_displayed_left")),
                   htmlOutput(NS(id, "year_displayed_right")),
                   materialSwitch(inputId = NS(id, "bi_census"),
                     label = "Two census comparison", right = TRUE),
                   shinyjs::useShinyjs() # needed if we continue to have 2 sliders
                   ),
          right_panel(id, compare_UI(NS(id, "housing"), var_list_housing_right),
                      explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "housing")))
}


# Server ------------------------------------------------------------------

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "housing")
    
    # Nearest year of data disclaimer
    output$year_displayed_left <- renderText({
      
      if (!input$bi_census) {
        
      year_shown <- str_extract(var_left(), "\\d{4}$")
      var <- str_remove(var_left(), "_\\d{4}$")
      var <- sus_translate(var_exp[var_exp$var_code == var,]$var_name)
      
      if (year_shown != time()) {
        str_glue(sus_translate(paste0(
          "<p>Displayed data for <b>{var}</b> is for the ",
          "closest available year <b>({year_shown})</b>.</p>")))
      }
      }
    })
    
    output$year_displayed_right <- renderText({
      
      if (!input$bi_census) {
        
      year_shown <- str_extract(var_right(), "\\d{4}$")
      var <- str_remove(var_right(), "_\\d{4}$")
      var <- sus_translate(var_exp[var_exp$var_code == var,]$var_name)
      
      if (year_shown != time() && var_right() != " ") {
        str_glue(sus_translate(paste0(
          "<p>Displayed data for <b>{var}</b> is for the ",
          "closest available year <b>({year_shown})</b>.</p>")))
      }
      }
    })
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_housing, zoom = map_zoom, 
              location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(eval(as.name(paste0(
                        "housing_tenant_prop_q3", "_", current_census))), 
                        "- 1")) %>%
                      left_join(colour_borough, by = "group"),
                    stroke_width = 100, stroke_colour = "#FFFFFF", 
                    fill_colour = "fill", update_view = FALSE, id = "ID", 
                    auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_housing$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "building",
                                   input$map_view_change$zoom >= 12 ~ "DA",
                                   input$map_view_change$zoom >= 10.5 ~ "CT",
                                   TRUE ~ "borough")})
    
    # Enable or disable first and second slider.
    observeEvent(input$bi_census, {
      if (!input$bi_census) {
        shinyjs::hide("slider_bi_census") 
        shinyjs::show("slider_housing")
      } else {
        shinyjs::hide("slider_housing")
        shinyjs::show("slider_bi_census")
      }
    })
    
    # Time variable depending on which slider
    time <- reactive({
      if (!input$bi_census) input$slider_housing else input$slider_bi_census
    })
    
    # Greyed out left list options, depending of the year(s) chosen
    var_list_housing_left_available <- reactive({
      if (!input$bi_census) NULL else disabled_var_list_housing_left
    })
    
    # Left variable server
    var_left_1 <- select_var_server(
      "left", reactive(var_list_housing_left), 
      disabled_choices = reactive(var_list_housing_left_available()))
    
    # Construct left variable string
    var_left <- reactive({
      var <- paste(var_left_1(), time(), sep = "_")
      
      return_closest_year <- function(var) {
        if (!var %in% names(borough)) {
          time <- as.numeric(str_extract(var, "\\d{4}"))
          x <- borough %>% 
            select(contains(str_remove(var, "_\\d{4}$"))) %>% 
            names() %>% 
            str_extract("\\d{4}$") %>% 
            as.numeric() %>% 
            na.omit()
          closest_year <- x[which.min(abs(x - time))]
          var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
        }
        var
      }
      
      purrr::map_chr(var, return_closest_year)

    })

    # Greyed out right list options, depending of the year chosen
    var_list_housing_right_available <- reactive({
      if (!input$bi_census) NULL else disabled_var_list_housing_right
    })

    # Right variable server
    var_right_1 <- compare_server(
      "housing", var_list_housing_right, reactive(rv_housing$zoom), 
      disabled_choices = reactive(var_list_housing_right_available()))

    var_right <- reactive({
      
      if (var_right_1()[1] != " ") {
        
        var <- paste(var_right_1(), time(), sep = "_")
        
        return_closest_year <- function(var) {
          if (!var %in% names(borough)) {
            time <- as.numeric(str_extract(var, "\\d{4}"))
            x <- borough %>% 
              select(contains(str_remove(var, "_\\d{4}$"))) %>% 
              names() %>% 
              str_extract("\\d{4}$") %>% 
              as.numeric() %>% 
              na.omit()
            closest_year <- x[which.min(abs(x - time))]
            var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
          }
          var
        }
        
        purrr::map_chr(var, return_closest_year)

      } else var_right_1()
    })

    # Data
    data <- data_server("housing", var_left, var_right, 
                        reactive(rv_housing$zoom))

    # Explore panel
    explore_server(id = "explore", 
                   x = data, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_housing$poly_selected),
                   zoom = reactive(rv_housing$zoom), 
                   build_str_as_DA = TRUE)

    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)

    # Left map
    small_map_server("left", reactive(paste0("left_", reactive(rv_housing$zoom), 
                                             "_", var_left())))

    # Bivariate legend
    legend_bivar_server("housing", var_right)

    # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      rv_housing$zoom}, map_change(NS(id, "map"), df = data, 
                                   zoom = reactive(rv_housing$zoom)))

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
          data() %>%
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
