### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         hr(),
                         actionLink(NS(id, "analysis"), 
                                    i18n$t("Road safety analysis")),
                         hr(id = NS(id, "hr")),
                         select_var_UI(NS(id, "left_1"), var_list_left_crash_1,
                                       label = i18n$t("Grouping")),
                         select_var_UI(NS(id, "left_2"), var_list_left_crash_2,
                                       label = i18n$t("Type of crash")),
                         div(id = NS(id, "slider"),
                             sliderInput(NS(id, "left"), 
                                         i18n$t("Select a year"),
                                         min = crash_slider$min,
                                         max = crash_slider$max,
                                         step = crash_slider$interval, sep = "",
                                         value = crash_slider$init,
                                         width = "95%"),
                             sliderInput(NS(id, "left_bi_time"), 
                                         i18n$t("Select two dates"), 
                                         min = crash_slider$min,
                                         max = crash_slider$max, 
                                         step = crash_slider$interval, sep = "", 
                                         value = c("2012", "2019"),
                                         width = "95%")),
                         div(id = NS(id, "slider_switch"),
                             checkboxInput(inputId = NS(id, "bi_time"),
                                           label = i18n$t("Compare dates"))),
                         htmlOutput(NS(id, "year_displayed_right")),
                         htmlOutput(NS(id, "how_to_read_map")),
                         div(class = "bottom_sidebar",
                             tagList(legend_UI(NS(id, "legend")),
                                     zoom_UI(NS(id, "zoom"), crash_zoom))))
      ),
      fillCol(
        shinyjs::hidden(htmlOutput(NS(id, "crash_analysis"),
                                   style = "position:absolute; margin: 40px;
                     max-width: 1000px; z-index:499")),
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
        right_panel(id, compare_UI(NS(id, "crash"), var_list_right_crash),
                    div(class = "explore_dyk",
                        explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))))),
      flex = c(1, 5)
    )
  )
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server("sidebar", "crash", 
                   reactive(paste0("left_", zoom(), "_", var_left())))
    
    # If COUNT isn't selected, choropleth is TRUE 
    choropleth <- reactive(var_left_1() != " ")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_crash, zoom = map_zoom, 
              location = map_location) %>%
        add_heatmap(data = crash, update_view = FALSE, intensity = 2,
                    colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                     "#70999B", "#6E8EA8", "#6C83B5"))})
    
    # Zoom
    zoom_val <- reactiveVal(get_zoom(map_zoom, crash_zoom))
    observeEvent(input$map_view_change$zoom, {
      zoom_val(get_zoom(input$map_view_change$zoom, crash_zoom))
    })
    zoom <- zoom_server("zoom", zoom = zoom_val, zoom_levels = crash_zoom)
    
    # Enable or disable first and second slider
    observeEvent(input$bi_time, {
      shinyjs::toggle("left_bi_time", condition = input$bi_time)
      shinyjs::toggle("left", condition = !input$bi_time)})
    
    # If we aren't in choropleth, toggle off the legend/zoom
    observeEvent(choropleth(), {
      shinyjs::toggle("zoom-auto", condition = choropleth())
      shinyjs::toggle("zoom-slider", condition = choropleth())
      shinyjs::toggle("legend-legend_render", condition = choropleth())
    })
    
    # Time variable depending on which slider
    time <- reactive({if (!input$bi_time) input$left else input$left_bi_time})
    
    # Left variable servers
    var_left_1 <- select_var_server("left_1", reactive(var_list_left_crash_1))
    var_left_2 <- select_var_server("left_2", reactive(var_list_left_crash_2))
    
    # Construct left variable string
    var_left <- reactive(str_remove(paste("crash", var_left_2(), var_left_1(), 
                                          time(), sep = "_"), "_ "))
    
    # Compare panel
    var_right <- compare_server(
      id = "crash", 
      var_list = var_list_right_crash, 
      df = zoom, 
      show_panel = choropleth,
      time = time)
    
    # Data 
    data_1 <- data_server("crash", var_left, var_right, df = zoom, zoom = zoom_val)
    
    data <- reactive({

      if (choropleth()) {
        data_1() %>% 
          {if (nrow(.) == nrow(borough))
            filter(., ID %in% island_csduid)
            else filter(., CSDUID %in% island_csduid)}
      } else {
        crash %>%
           { if (var_left_2() %in% unique(crash$type))
             filter(., type == var_left_2()) else .} %>%
           { if (length(time()) == 2) {
             filter(., year %in% time()[1]:time()[2])
           } else {
             filter(., year == time())
           }}     
  }
    })
    
    data_for_explore <- reactive({
      if (choropleth()) data() else {
        data() |> 
          st_drop_geometry() |> 
          count(date) |> 
          rename(left_var = n, right_var = date) |> 
          mutate(ID = seq_along(left_var), .before = left_var) |> 
          mutate(left_var_q3 = left_var)
      }
    })
    
    zoom_for_explore <- reactive({if (choropleth()) zoom else "date"})
    right_var_for_exp <- reactive({if (choropleth()) right_var() else "date"})
    
    # Explore panel
    explore_server(id = "explore", 
                   x = data_for_explore, 
                   var_left = var_left,
                   var_right = right_var_for_exp, 
                   select = reactive(rv_crash$poly_selected),
                   zoom = zoom_for_explore, 
                   build_str_as_DA = TRUE)
    
    # Did-you-know panel
    # dyk_server("dyk", var_left, var_right)
    
    # Legend
    legend_server("legend", var_left, var_right, zoom_val)
    
    # # Update map in response to variable changes or zooming
    observeEvent({
      var_left()
      var_right()
      zoom()}, {
        
        map_change(NS(id, "map"),
                   df = data,
                   zoom = zoom,
                   # TKTK will eventually need to be translated
                   legend = crash_legend_en)
        
      })
    
    # Update poly_selected on click
    # observeEvent(input$map_polygon_click, {
    #   lst <- jsonlite::fromJSON(input$map_polygon_click)
    #   if (is.null(lst$object$properties$id)) {
    #     rv_crash$poly_selected <- NA
    #   } else rv_crash$poly_selected <- lst$object$properties$id
    # })
    
    # Clear poly_selected on zoom
    # observeEvent(rv_crash$zoom, {rv_crash$poly_selected <- NA},
    #              ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    # observeEvent(rv_crash$poly_selected, {
    #   if (!is.na(rv_crash$poly_selected)) {
    #     width <- switch(rv_crash$zoom, "borough" = 100, "CT" = 10, 2)
    #     data_to_add <-
    #       data() %>%
    #       filter(ID == rv_crash$poly_selected)
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
    
    
    # Year displayed disclaimer
    output$year_displayed_right <- renderText({
      if (!input$bi_time && choropleth()) {
        year_shown <- str_extract(var_right(), "\\d{4}$")
        var <- str_remove(var_right(), "_\\d{4}$")
        var <- sus_translate(var_exp[var_exp$var_code == var,]$var_name)
        
        if (year_shown != time() && var_right() != " ") {
          str_glue(sus_translate(paste0(
            "<p style='font-size:11px;'>",
            "<i>Displayed data for <b>{var}</b> is for the ",
            "closest available year <b>({year_shown})</b>.</i></p>")))
        }
      }
    })
    
    # Bi slider label explained
    observe({
      if (!choropleth()) {
        updateSliderInput(session, inputId = "left_bi_time", 
                          label = sus_translate("Total between two dates"))
      } else if (choropleth()) {
        updateSliderInput(session, inputId = "left_bi_time", 
                          label = sus_translate("Compare two dates"))
      }
    })
    
    output$how_to_read_map <- renderText({
      # No explanation needed for heatmap and choropleth with unique date and
      # no right variable. The slider label updates, and makes sense of the map.
      if (choropleth() && input$bi_time) {
        type_crash <- switch(var_left_2(), 
                             "total" = sus_translate("total"),
                             "ped" = sus_translate("pedestrian"),
                             "cyc" = sus_translate("cyclist"), 
                             "other" = sus_translate("other"))
        
        if (var_right()[1] == " ") {
          str_glue(sus_translate(crash_read_uni))
          
        } else {
          var <- str_remove(var_right(), "_\\d{4}$")
          census_years <- unique(str_extract(var_right(), "\\d{4}$"))
          var <- str_to_lower(sus_translate(
            var_exp[var_exp$var_code == var,]$var_name))
          
          if (length(census_years) == 2) {
            str_glue(sus_translate(crash_read_bi_2))
          } else str_glue(sus_translate(crash_read_bi_1))
        }
      }
    })
    
    
    
    output$crash_analysis <- renderUI(
      
      tags$iframe(src = "crash/crash.html", width = "1000px", height = "800px",
                  style = "max-height: 83vh; overflow: auto; background-color: #fff;
                    border: 1px solid transparent; border-radius: 4px;
                    box-shadow: 0 50px 50px rgba(0,0,0,.6);")
      
    )
    
    observeEvent(input$analysis, {
      
      if (input$analysis %% 2 == 1) {
        txt <- sus_translate("Road safety map") 
      } else txt <- sus_translate("Road safety analysis")
      
      updateActionLink(session, "analysis", label = txt)
      
      shinyjs::toggle("hr", condition = !input$analysis %% 2)
      shinyjs::toggle("left_1-var", condition = !input$analysis %% 2)
      shinyjs::toggle("left_2-var", condition = !input$analysis %% 2)
      shinyjs::toggle("slider", condition = !input$analysis %% 2)
      shinyjs::toggle("slider_switch", condition = !input$analysis %% 2)
      shinyjs::toggle("right_panel", condition = !input$analysis %% 2)
      shinyjs::toggle("how_to_read_map", condition = !input$analysis %% 2)
      shinyjs::toggle("year_displayed_right", condition = !input$analysis %% 2)
      shinyjs::toggle("legend-legend_render", condition = !input$analysis %% 2)
      shinyjs::toggle("zoom-auto", condition = !input$analysis %% 2)
      shinyjs::toggle("zoom-slider", condition = !input$analysis %% 2)
      shinyjs::toggle("crash_analysis", condition = input$analysis %% 2)
      
    })
  })
}
