### NATURAL INFRASTRUCTURE MODULE #########################################

# UI ----------------------------------------------------------------------

natural_inf_UI <- function(id) {
  ns_id <- "natural_inf"
  ns_id_map <- paste0(ns_id, "-map")

  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        select_var_UI(NS(id, ns_id), 
                      select_var_id = "vl_1",
                      var_list = vars_natural_inf_left,
                      label = sus_translate("Theme")),
        select_var_UI(NS(id, ns_id), 
                      select_var_id = "vl_2",
                      var_list = list("----" = " "),
                      label = sus_translate("Indicator")),
        slider_UI(NS(id, ns_id),
                  label = sus_translate("Amount of territory to protect"),
                  min = 0,
                  max = 25,
                  step = 1,
                  value = 17,
                  post = "%"),
        checkbox_UI(NS(id, ns_id),
                    label = sus_translate("Custom priorities")),
        slider_text_UI(NS(id, ns_id),
                       slider_id = "s_bio",
                       label = sus_translate("Biodiversity conservation"),
                       choices = c("Not important", "Somewhat important", 
                                   "Important", "Very important", 
                                   "Extremely important"),
                       selected = "Important"),
        slider_text_UI(NS(id, ns_id),
                       slider_id = "s_hea",
                       label = sus_translate("Heat island reduction"),
                       choices = c("Not important", "Somewhat important", 
                                   "Important", "Very important", 
                                   "Extremely important"),
                       selected = "Important"),
        slider_text_UI(NS(id, ns_id),
                       slider_id = "s_flo",
                       label = sus_translate("Flood prevention"),
                       choices = c("Not important", "Somewhat important", 
                                   "Important", "Very important", 
                                   "Extremely important"),
                       selected = "Important")
        ),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, ns_id))))),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))
  )
}


# Server ------------------------------------------------------------------

natural_inf_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "natural_inf"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(9.5, map_zoom_levels))
    poi <- reactiveVal(NULL)
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = 9.5))
    })
    
    # Zoom and POI reactives
    observeEvent(get_view_state(ns_id_map), {
      zoom(get_zoom(get_view_state(ns_id_map)$zoom))
      new_poi <- observe_map(get_view_state(ns_id_map))
      if ((is.null(new_poi) && !is.null(poi())) || 
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    })
    
    # Zoom string reactive
    observeEvent(zoom(), {
      new_zoom_string <- get_zoom_string(zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    })
    
    # Choose tileset
    tile <- reactive(if (var_left() == "c_priority" && custom_priorities()) {
      "custom" 
      } else var_left())

    # Left variable
    var_left_1 <- select_var_server(
      id = ns_id,
      select_var_id = "vl_1",
      var_list = reactive(vars_natural_inf_left))
    
    var_left_2_list <- reactive({
      if (var_left_1() == "c_priority") {
        list("----" = " ")
      } else eval(parse(text = paste0("vars_natural_inf_left_", var_left_1())))
    })
    
    var_left_2 <- select_var_server(
      id = ns_id,
      select_var_id = "vl_2",
      var_list = reactive(var_left_2_list()))
    
    var_left <- reactive(if (var_left_2() != " ") var_left_2() else 
      var_left_1())
    
    observe(toggle(NS(id, "vl_2"), condition = var_left_1() != "c_priority")) |> 
      bindEvent(var_left_1())
    
    # Checkbox value
    custom_priorities <- checkbox_server(id = ns_id)
    observe(updateCheckboxInput(session = session, inputId = NS(id, "cbox"),
                                value = FALSE)) |> 
      bindEvent(var_left_1())
    observe(toggle(NS(id, "cbox"), condition = var_left_1() == "c_priority")) |> 
      bindEvent(var_left_1())
    
    # Main slider value
    main_slider <- slider_server(id = ns_id)
    
    # Custom priority sliders
    s_bio <- slider_text_server(id = ns_id, slider_id = "s_bio")
    s_hea <- slider_text_server(id = ns_id, slider_id = "s_hea")
    s_flo <- slider_text_server(id = ns_id, slider_id = "s_flo")
    ni_slider <- reactive(process_ni_sliders(s_bio(), s_hea(), s_flo()))
    observe({
      toggle(NS(id, "s_bio"), condition = custom_priorities())
      toggle(NS(id, "s_hea"), condition = custom_priorities())
      toggle(NS(id, "s_flo"), condition = custom_priorities())
      }) |> 
      bindEvent(custom_priorities())

    # Right variable / compare panel
    var_right <- reactive(" ")

    # Sidebar
    sidebar_server(id = ns_id, x = "natural_inf")
    
    # Composite variable for map
    map_var <- reactive(if (custom_priorities()) "ID" else var_left())
    
    # Enable or disable the main main_slider
    observe(toggle(NS(id, "slider"), condition = var_left() == "c_priority"))
    
    # Data
    data <- reactive({
      if (var_left() == "c_priority") {
        if (!custom_priorities()) {
          natural_inf$original_priorities[
            natural_inf$original_priorities$slider == main_slider(), ]
          } else {
            custom <- natural_inf$custom_explore
            custom <- custom[custom$slider == main_slider(), ]
            custom <- custom[custom$biodiversity == ni_slider()[1], ]
            custom <- custom[custom$heat_island == ni_slider()[2], ]
            custom <- custom[custom$flood == ni_slider()[3], ]
            custom
          }
        } else natural_inf$explore
        
      })
    
    # Map custom colours
    natural_inf_colours <- reactive({
      if (var_left() == "c_priority") {
        
        if (!custom_priorities()) {
          
          remove <- 50 - main_slider()
          ni_colour_table <- colour_table[colour_table$palette == "viridis", ]
          ni_colour_table$value[ni_colour_table$group <= remove] <- 
            paste0(substr(ni_colour_table$value[
              ni_colour_table$group <= remove], 1, 7), "00")
          ni_colour_table
          
        } else {
          
          if (main_slider() == 0) {
            data.frame(group = "ABCD", value = "#FFFFFF00")
          } else {
            custom <- natural_inf$custom
            custom <- custom[[main_slider()]]
            custom <- custom[custom$biodiversity == ni_slider()[1], ]
            custom <- custom[custom$heat_island == ni_slider()[2], ]
            custom <- custom[custom$flood == ni_slider()[3], ]
            custom[, c("group", "value")] 
          }
        }
      } else NULL
      
    })
    
    legend_raster <- reactive(
      if (var_left() %in% c("heat", "cool", "flood")) NULL else "raster"
    )

    # Legend
    legend <- legend_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      df = legend_raster)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      map_id = "map", 
      tile = tile,
      tile2 =  reactive(""),
      map_var = map_var, 
      zoom = zoom,
      select_id = reactive(NA),
      lwd = scale_lwd_natural_inf,
      lwd_args = reactive(list()),
      fill = scale_fill_natural_inf,
      fill_args = reactive(list(map_var(), tile(), natural_inf_colours())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)

    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = reactive(NULL),
      select_id = reactive(NA),
      graph = reactive(explore_graph_natural_inf),
      table = reactive(info_table_natural_inf))

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      select_id = reactive(NA),
      map_id = "map",
    )

  })
}
