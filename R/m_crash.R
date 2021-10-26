### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  tabItem(tabName = "crash",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   actionLink(NS(id, "crash_rmd"), label = "Analysis"),
                   hr(),
                   select_var_UI(NS(id, "left_1"), var_list_left_crash_1),
                   select_var_UI(NS(id, "left_2"), var_list_left_crash_2),
                   slider_UI(NS(id, "left"), 
                             slider_min = crash_slider$min, 
                             slider_max = crash_slider$max, 
                             slider_interval = crash_slider$interval, 
                             slider_init = crash_slider$init),
                   htmlOutput(NS(id, "year_displayed_right"))
                   ),
          right_panel(id, 
                      compare_UI(NS(id, "crash"), var_list_right_crash),
                      # explore_UI(NS(id, "explore")),
                      # dyk_UI(NS(id, "dyk"))
                      ),
          legend_bivar_UI(NS(id, "crash")))
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "crash")
    
    # Year displayed disclaimer
    output$year_displayed_right <- renderText({
      year_shown <- str_extract(var_right_crash(), "\\d{4}$")
      var <- str_remove(var_right_crash(), "_\\d{4}$")
      var <- sus_translate(var_exp[var_exp$var_code == var,]$var_name)
      
      if (year_shown != time() && var_right_crash() != " "){
        str_glue(sus_translate(paste0("<p>Displayed data for <b>{var}</b> is for the ",
                                    "closest available year <b>({year_shown})</b>.</p>")))
      }
    })

    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_crash,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(crash_ped_prop_area_q3_2019, "- 1")) %>%
                      left_join(colour_borough, by = "group"),
                    stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90") #%>%
        # add_heatmap(data = crash, update_view = FALSE)
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_crash$zoom <- case_when(#input$map_view_change$zoom >= 14 ~ "DA_2",
                                 input$map_view_change$zoom >= 12 ~ "DA",
                                 input$map_view_change$zoom >= 10.5 ~ "CT",
                                 TRUE ~ "borough")
    })
    
    
    # Compare panel
    var_right_crash_1 <- compare_server("crash", var_list_right_crash,
                                      reactive(rv_crash$zoom))
    
    var_right_crash <- reactive({
      if (var_right_crash_1() != " ") {
        
        var <- paste(var_right_crash_1(), time(), sep = "_")
        
        if (!var %in% names(borough)) {
          x <- borough %>% 
            select(contains(str_remove(var, "_\\d{4}$"))) %>% 
            names() %>% 
            str_extract(., "\\d{4}$") %>% 
            as.numeric() %>% na.omit()
          closest_year <-  x[which.min(abs(x - time()))]
          var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
        }
        
        var
        
      } else var_right_crash_1()
    })
    
    # Left variable servers
    var_left_crash_1 <- select_var_server("left_1", 
                                          reactive(var_list_left_crash_1))
    var_left_crash_2 <- select_var_server("left_2", 
                                          reactive(var_list_left_crash_2))
    
    # Get time from slider
    time <- slider_server("left")

    # Construct left variable string
    var_left_crash <- reactive(
      stringr::str_remove(paste(
        "crash", 
        var_left_crash_1(), 
        var_left_crash_2(), 
        time(), 
        sep = "_"), "_ ")
    )
    
    # Data 
    data_crash <- data_server("crash", var_left_crash, var_right_crash, 
                              reactive(rv_crash$zoom))

    # # Explore panel
    # explore_server("explore", data_canale, reactive("canale_ind"),
    #                var_right_canale, reactive(rv_canale$poly_selected),
    #                reactive(rv_canale$zoom), reactive("CanALE index"))
    
    # Did-you-know panel
    # dyk_server("dyk", reactive("alley_ind"), var_right_alley)
    
    # # Left map
    # small_map_server("left", reactive(paste0(
    #   "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
    
    # Bivariate legend
    legend_bivar_server("crash", var_right_crash)
    
    # # Update map in response to variable changes or zooming
    observeEvent({
      var_left_crash()
      var_right_crash()
      rv_crash$zoom}, {
        width <- switch(rv_crash$zoom, "borough" = 100, "CT" = 10, 2)
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_crash(), stroke_width = width,
            stroke_colour = "#FFFFFF", fill_colour = "fill",
            update_view = FALSE, id = "ID", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
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
          data_crash() %>%
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
