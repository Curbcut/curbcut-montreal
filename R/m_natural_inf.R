### NATURAL INFRASTRUCTURE MODULE #########################################

# UI ----------------------------------------------------------------------

natural_inf_UI <- function(id) {
  id_map <- paste0(id, "-map")

  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        select_var_UI(NS(id, id), 
                      select_var_id = "d_1",
                      var_list = var_left_list_1_natural_inf,
                      label = cc_t(r = r, "Theme")),
        select_var_UI(NS(id, id), 
                      select_var_id = "d_2",
                      var_list = list("----" = " "),
                      label = cc_t(r = r, "Indicator")),
        slider_UI(NS(id, id),
                  label = cc_t(r = r, "Amount of territory to protect"),
                  min = 0,
                  max = 25,
                  step = 1,
                  value = 17,
                  post = "%"),
        checkbox_UI(NS(id, id),
                    label = cc_t(r = r, "Custom priorities")),
        slider_text_UI(NS(id, id),
                       slider_id = "s_bio",
                       label = cc_t(r = r, "Biodiversity conservation"),
                       choices = custom_slider_choices,
                       selected = "Important"),
        slider_text_UI(NS(id, id),
                       slider_id = "s_hea",
                       label = cc_t(r = r, "Heat island reduction"),
                       choices = custom_slider_choices,
                       selected = "Important"),
        slider_text_UI(NS(id, id),
                       slider_id = "s_flo",
                       label = cc_t(r = r, "Flood prevention"),
                       choices = custom_slider_choices,
                       selected = "Important")
        ),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, id))))),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id)))
  )
}


# Server ------------------------------------------------------------------

natural_inf_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")

    # # Initial reactives
    zoom_string <- reactiveVal(get_zoom_string(9.5, map_zoom_levels_CMA))
    poi <- reactiveVal(NULL)
    
    # Map
    output[[id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = isolate(r[[id]]$zoom())))
    })

    # Zoom and POI reactives
    observe({
      r[[id]]$zoom(get_zoom(get_view_state(id_map)$zoom))
      new_poi <- observe_map(get_view_state(id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    }) |> bindEvent(get_view_state(id_map))

    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels_CMA)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom())
    
    # Choose tileset
    tile <- reactive(if (var_left() == "c_priority" && custom_priorities()) {
      "custom" 
      } else var_left())

    # Left variable
    var_left_1 <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_1",
      var_list = reactive(var_left_list_1_natural_inf))
    
    var_left_2_list <- reactive({
      if (var_left_1() == "c_priority") {
        list("----" = " ")
      } else var_left_list_2_natural_inf[[var_left_1()]]
    })
    
    var_left_2 <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_2",
      var_list = reactive(var_left_2_list()))
    
    var_left <- reactive(if (var_left_2() != " ") var_left_2() else 
      var_left_1())
    
    observe(toggle(NS(id, "d_2"), condition = var_left_1() != "c_priority")) |> 
      bindEvent(var_left_1())
    
    # Checkbox value
    custom_priorities <- checkbox_server(id = id)
    observe(updateCheckboxInput(session = session, inputId = NS(id, "cbox"),
                                value = FALSE)) |> 
      bindEvent(var_left_1())
    observe(toggle(NS(id, "cbox"), condition = var_left_1() == "c_priority")) |> 
      bindEvent(var_left_1())
    
    # Main slider value
    main_slider <- slider_server(id = id)
    
    # Custom priority sliders
    s_bio <- slider_text_server(id = id, r = r, slider_id = "s_bio",
                                choices = reactive(custom_slider_choices))
    s_hea <- slider_text_server(id = id, r = r, slider_id = "s_hea",
                                choices = reactive(custom_slider_choices))
    s_flo <- slider_text_server(id = id, r = r, slider_id = "s_flo",
                                choices = reactive(custom_slider_choices))
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
    sidebar_server(id = id, r = r, x = "natural_inf")
    
    # Composite variable for map
    map_var <- reactive(if (custom_priorities()) "ID" else var_left())
    
    # Enable or disable the main main_slider
    observe(toggle(NS(id, "slider"), condition = var_left() == "c_priority"))
    
    # SQL retrieval
    data <- reactive({
      db_call <- 
        if (var_left() == "c_priority") {
          if (!custom_priorities()) {
            paste0("SELECT * FROM natural_inf_original_priorities ",
                   "WHERE slider = ", main_slider())
          } else {
            paste0("SELECT * FROM natural_inf_custom_explore ",
                   "WHERE slider = ", main_slider(), 
                   " AND biodiversity = ", ni_slider()[1], 
                   " AND heat_island = ", ni_slider()[2], 
                   " AND flood = ", ni_slider()[3])
          }
        } else {
          paste0("SELECT * FROM natural_inf_explore")
        }
      
      do.call("dbGetQuery", list(rlang::sym("natural_inf_conn"), db_call))
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
            db_call <- paste0("SELECT * FROM natural_inf_custom_", main_slider(),
                              " WHERE biodiversity = ", ni_slider()[1], 
                              " AND heat_island = ", ni_slider()[2], 
                              " AND flood = ", ni_slider()[3])
            do.call("dbGetQuery", list(rlang::sym("natural_inf_conn"), db_call))[, c("group", "value")] 
          }
        }
      } else NULL
      
    })
    
    legend_raster <- reactive(
      if (var_left() %in% c("heat", "cool", "flood")) NULL else "raster"
    )

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      df = legend_raster)

    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right)#,
      #poi = poi)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      tile = reactive(paste0(id, "-", tile())),
      data_color = reactive(data.frame()),
      select_id = reactive(NA),
      lwd = scale_lwd_natural_inf,
      lwd_args = reactive(list()),
      fill = scale_fill_natural_inf,
      fill_args = reactive(list(map_var(), tile(), natural_inf_colours())))
    
    # Update map labels
    label_server(
      id = id, 
      r = r,
      map_id = "map", 
      tile = tile)

    # Explore panel
    explore_content <- explore_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      geo = r$geo,
      df = reactive(NULL),
      select_id = reactive(NA),
      graph = reactive(explore_graph_natural_inf),
      table = reactive(info_table_natural_inf))

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      var_left = var_left,
      map_viewstate = reactive(get_view_state(id_map)),
      s_id = reactive(NA),
      more_args = reactive(c(
        "s-slider" = main_slider()))
    )
    
    # Data transparency and export
    observe({
      r[[id]]$export_data <- reactive(data_export(id = id, 
                                      data = data(), 
                                      var_left = var_left(), 
                                      df = "raster"))
    })
    
  })
}
