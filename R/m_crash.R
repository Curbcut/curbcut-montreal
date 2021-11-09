### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  tabItem(tabName = "crash",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   actionLink(NS(id, "crash_rmd"), label = "Analysis"),
                   hr(),
                   column(6, select_var_UI(NS(id, "left_1"), var_list_left_crash_1,
                                           label = i18n$t("Density preferred: "))),
                   column(6, select_var_UI(NS(id, "left_2"), var_list_left_crash_2,
                                 label = i18n$t("Type of crash: "))),
                   sliderInput(NS(id, "left"), i18n$t("Select a year"),
                               min = crash_slider$min,
                               max = crash_slider$max,
                               step = crash_slider$interval, sep = "",
                               value = crash_slider$init),
                   htmlOutput(NS(id, "bi_time_slider_label")),
                   sliderInput(NS(id, "left_bi_time"), label = NULL, 
                               min = crash_slider$min,
                               max = crash_slider$max, 
                               step = crash_slider$interval, sep = "", 
                               value = c("2012", "2019")),
                   materialSwitch(inputId = NS(id, "bi_time"),
                                  label = i18n$t("Two time periods"), 
                                  right = TRUE),
                   htmlOutput(NS(id, "year_displayed_right")),
                   htmlOutput(NS(id, "how_to_read_map")),
                   shinyjs::useShinyjs()
                   ),
          right_panel(id, compare_UI(NS(id, "crash"), var_list_right_crash),
                      explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "crash")))
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "crash")
    
    # If COUNT isn't selected, choropleth is TRUE 
    choropleth <- reactive(if (var_left_1() != " ") TRUE else FALSE)
    
    # Year displayed disclaimer
    output$year_displayed_right <- renderText({
      if (!input$bi_time && choropleth()) {
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
    
    # Bi census slider label explained
    output$bi_time_slider_label <- renderText({
      if (input$bi_time && !choropleth()) {
        paste(sus_translate("<b>Choose date range:</b>"))
      } else if (input$bi_time && choropleth()) {
        paste(sus_translate("<b>Compare two time periods:</b>"))
      }
    })
    
    output$how_to_read_map <- renderText({
      # No explanation needed for the heatmap. The label of slider updates
      # and it's written it's a range of dates. No explanation needed for the
      # choropleth with unique date. 
      if (choropleth() && input$bi_time) {
        if(var_right() == " "){
          type_crash <- case_when(var_left_2() == "total" ~ sus_translate("total"),
                                  var_left_2() == "ped" ~ sus_translate("pedestrian"),
                                  var_left_2() == "cyc" ~ sus_translate("cyclist"), 
                                  var_left_2() == "other" ~ sus_translate("other"))
          str_glue(
            sus_translate(
              paste0("<b>How to read the map</b><br>",
                     "The map displays the variation percentage in number of ",
                     "{type_crash} crash between {time()[1]} and {time()[2]}. ",
                     "A darker green means a relative increase in {type_crash} ",
                     "crash number between {time()[1]} and {time()[2]}.")))
        }
      } else {
        
      }
    })
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_crash, zoom = map_zoom, 
              location = map_location) %>%
        add_heatmap(data = crash, update_view = FALSE, intensity = 2,
                    colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                     "#70999B", "#6E8EA8", "#6C83B5"))
    })
    
    # Zoom level
    observeEvent({
      input$geography
      input$map_view_change$zoom}, {
          rv_crash$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "street",
                                     input$map_view_change$zoom >= 12 ~ "DA",
                                     input$map_view_change$zoom >= 10.5 ~ "CT",
                                     TRUE ~ "borough")
          })
    
    # Enable or disable first and second slider.
    observeEvent(input$bi_time, {
      if (!input$bi_time) {
        shinyjs::hide("left_bi_time") 
        shinyjs::show("left")
      } else {
        shinyjs::hide("left")
        shinyjs::show("left_bi_time")
      }
    })
    
    # Time variable depending on which slider
    time <- reactive({
      if (!input$bi_time) input$left else input$left_bi_time
    })

    # Left variable servers
    var_left_1 <- select_var_server("left_1", reactive(var_list_left_crash_1))
    var_left_2 <- select_var_server("left_2", reactive(var_list_left_crash_2))
    
    # Construct left variable string
    var_left <- reactive(
      stringr::str_remove(paste(
        "crash", 
        var_left_2(), 
        var_left_1(), 
        time(), 
        sep = "_"), "_ ")
    )
    
    # To hide compare panel when map displayed isn't choropleth
    show_panel <- reactive(if (choropleth()) TRUE else FALSE)
    # Compare panel
    var_right_1 <- compare_server("crash", var_list_right_crash, 
                                  reactive(rv_crash$zoom), 
                                  show_panel = show_panel)
    
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
    data_1 <- data_server("crash", var_left, var_right, reactive(rv_crash$zoom))
    
    data <- reactive({
      if (choropleth()) {
        data_1()
      } else {
          (crash %>%
             { if (var_left_2() %in% unique(crash$type))
               filter(., type == var_left_2()) else .} %>%
             { if (length(time()) == 2) {
               filter(., lubridate::year(date) >= time()[1],
                      lubridate::year(date) <= time()[2])
             } else {
               filter(., lubridate::year(date) == time())
             }} %>%
            mutate(fill = case_when(type == "ped" ~ "#91BD9AEE",
                                    type == "cyc" ~ "#6C83B5EE",
                                    type == "other" ~ "#F39D60EE",
                                    TRUE ~ "#E8E8E8EE")))
        }
    })
    
    # Explore panel
    explore_server(id = "explore", 
                   x = data_1, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_crash$poly_selected),
                   zoom = reactive(rv_crash$zoom), 
                   build_str_as_DA = TRUE)

    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)

    # Left map
    small_map_server("left", reactive(paste0(
      "left_", rv_crash$zoom, "_", canale_ind)))
    
    # Bivariate legend
    legend_bivar_server("crash", var_right)
    
    # # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      rv_crash$zoom}, {
        
        map_change(NS(id, "map"),
                   df = data,
                   zoom = reactive(rv_crash$zoom),
                   # TKTK will eventually need to be translated
                   legend = crash_legend_en)
        
      })

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_crash$poly_selected <- NA
      } else rv_crash$poly_selected <- lst$object$properties$id
    })
    
    # # Clear poly_selected on zoom
    observeEvent(rv_crash$zoom, {rv_crash$poly_selected <- NA},
                 ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_crash$poly_selected, {
      if (!is.na(rv_crash$poly_selected)) {
        width <- switch(rv_crash$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() %>%
          filter(ID == rv_crash$poly_selected)
        
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
    
    # Switch to analysis Rmd
    observeEvent(input$crash_rmd, {
      if (input$crash_rmd %% 2 != 0) {
        print("BUTTON")
        shinydashboard::updateTabItems(session, "tabs", "crash_analysis")
      }
    })
    
  })
}
