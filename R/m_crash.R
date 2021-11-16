### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  tabItem(tabName = "crash",
          shinyjs::useShinyjs(),
          shinyjs::hidden(htmlOutput(NS(id, "crash_analysis"),
                     style = "position:absolute; margin: 40px;
                     max-width: 1000px; z-index:499")),
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   actionLink(NS(id, "analysis"), "Road safety analysis"),
                   hr(id = NS(id, "hr")),
                   select_var_UI(NS(id, "left_1"), var_list_left_crash_1,
                                 label = i18n$t("Grouping of crashes"),
                                 width = "200px"),
                   select_var_UI(NS(id, "left_2"), var_list_left_crash_2,
                                 label = i18n$t("Type of crash"), 
                                 width = "200px"),
                   div(id = NS(id, "slider"), style = widget_style, 
                       sliderInput(NS(id, "left"), 
                                   i18n$t("Select a year"),
                                   min = crash_slider$min,
                                   max = crash_slider$max,
                                   step = crash_slider$interval, sep = "",
                                   value = crash_slider$init, width = "200px"),
                       sliderInput(NS(id, "left_bi_time"), 
                                   i18n$t("Select two years"), 
                                   min = crash_slider$min,
                                   max = crash_slider$max, 
                                   step = crash_slider$interval, sep = "", 
                                   value = c("2012", "2019"), width = "200px")),
                   div(id = NS(id, "slider_switch"), style = widget_style,
                       materialSwitch(inputId = NS(id, "bi_time"),
                                  label = i18n$t("Select two time periods"),
                                  width = "200px", right = TRUE)),
                   htmlOutput(NS(id, "year_displayed_right")),
                   htmlOutput(NS(id, "how_to_read_map"))
                   ),
          right_panel(id, compare_UI(NS(id, "crash"), var_list_right_crash),
                      # explore_UI(NS(id, "explore")), 
                      dyk_UI(NS(id, "dyk"))),
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
    observe({
      if (!choropleth()) {
        updateSliderInput(session, inputId = "left_bi_time", 
                          label = sus_translate("Total between two dates"))
      } else if (choropleth()) {
        updateSliderInput(session, inputId = "left_bi_time", 
                          label = sus_translate("Compare two time periods"))
      }
    })
    
    output$how_to_read_map <- renderText({
      # No explanation needed for the heatmap and choropleth with unique date and
      # no right variable. The label of slider updates, and makes sense of the map.
      if (choropleth() && input$bi_time) {
        type_crash <- switch(var_left_2(), 
                             "total" = sus_translate("total"),
                             "ped" = sus_translate("pedestrian"),
                             "cyc" = sus_translate("cyclist"), 
                             "other" = sus_translate("other"))
        
        if(var_right()[1] == " "){
          
          str_glue(
            sus_translate(
              paste0("<b>How to read the map</b><br>",
                     "The map displays the percent variation in number of ",
                     "{type_crash} crashes between {time()[1]} and {time()[2]}. ",
                     "Blue means an increase in {type_crash} crashes number, and red ",
                     "means a decrease.")))
        } else {
          var <- str_remove(var_right(), "_\\d{4}$")
          census_years <- unique(str_extract(var_right(), "\\d{4}$"))
          var <- str_to_lower(sus_translate(var_exp[var_exp$var_code == var,]$var_name))
          
          if(length(census_years) == 2) {
            str_glue(
              sus_translate(
                paste0("<b>How to read the map</b><br>",
                       "The map displays the comparison of two percent variations. ",
                       "In green, the percent variation in number of ",
                       "{type_crash} crashes between {time()[1]} and {time()[2]}. ",
                       "A darker green means a relative increase in {type_crash} ",
                       "crashes number. In blue, the percent variation of '{var}' between ",
                       "{census_years[1]} and {census_years[2]}, the closest census years available. ",
                       "A darker blue means a relative increase in '{var}'. ",
                       "You can find the comparison legend at ",
                       "the bottom left of the map.")))
          } else {
            str_glue(
              sus_translate(
                paste0("<b>How to read the map</b><br>",
                       "The map displays the comparison of a percent variation ",
                       "with a census variable. In green, the percent variation ",
                       "in number of {type_crash} crashes between {time()[1]} and {time()[2]}. ",
                       "A darker green means a relative increase in {type_crash} ",
                       "crashes number. Displayed in blue is '{var}' numbers in {census_years}, ",
                       "the closest census year available. ",
                       "A darker blue means a relatively higher number of '{var}'. ",
                       "You can find the comparison legend at the bottom left of the map.")))
          }
          
        }
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
        data_1() %>% 
          {if (nrow(.) == nrow(borough))
            filter(., ID %in% island_csduid)
           else filter(., CSDUID %in% island_csduid)}
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
    # explore_server(id = "explore", 
    #                x = data_1, 
    #                var_left = var_left,
    #                var_right = var_right, 
    #                select = reactive(rv_crash$poly_selected),
    #                zoom = reactive(rv_crash$zoom), 
    #                build_str_as_DA = TRUE)

    # Did-you-know panel
    # dyk_server("dyk", var_left, var_right)

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
    
    # Clear poly_selected on zoom
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
    
    output$crash_analysis <- renderUI(
      
      HTML('<div id = "main_panel_text_popup">',
           includeHTML("www/crash/crash.html"),
           '</div>')

      
      )
    observeEvent(input$analysis, {
      
      if (input$analysis %% 2 == 1) {
        txt <- sus_translate("Road safety map") 
        } else txt <- sus_translate("Road safety analysis")
      
      updateActionLink(session, "analysis", label = txt)
      
      print(input$analysis)
      print(input$analysis %% 2)
      
      shinyjs::toggle("hr", condition = !input$analysis %% 2)
      shinyjs::toggle("left_1-var", condition = !input$analysis %% 2)
      shinyjs::toggle("left_2-var", condition = !input$analysis %% 2)
      shinyjs::toggle("slider", condition = !input$analysis %% 2)
      shinyjs::toggle("slider_switch", condition = !input$analysis %% 2)
      shinyjs::toggle("title-title", condition = !input$analysis %% 2)
      shinyjs::toggle("title-title_extra", condition = !input$analysis %% 2)
      shinyjs::toggle("title-title_main", condition = !input$analysis %% 2)
      shinyjs::toggle("title-more_info", condition = !input$analysis %% 2)
      shinyjs::toggle("title-more_info", condition = !input$analysis %% 2)
      shinyjs::toggle("crash_analysis", condition = input$analysis %% 2)
      
    })
    
    
  })
}
