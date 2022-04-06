### ACCESS MODULE ##############################################################

# UI ----------------------------------------------------------------------

access_UI <- function(id) {
  ns_id <- "access"
  ns_id_map <- paste0(ns_id, "-map")

  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
      select_var_UI(NS(id, ns_id), select_var_id = "d_2",
                    var_list = var_left_list_2_access,
                    label = sus_translate("Timing")),
      select_var_UI(NS(id, ns_id), select_var_id = "d_1",
                    var_list = var_left_list_1_access,
                    label = sus_translate("Destination type")),
      slider_UI(NS(id, ns_id), label = sus_translate("Time threshold"),
                  min = 10, max = 60, step = 1, value = 30)),
      bottom = div(class = "bottom_sidebar",
          tagList(legend_UI(NS(id, ns_id)),
                  hidden(zoom_UI(NS(id, ns_id), map_zoom_levels))))),
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))
  ))
}


# Server ------------------------------------------------------------------

access_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "access"
    ns_id_map <- paste0(ns_id, "-map")
    
    # Sidebar
    sidebar_server(id = ns_id, x = "access")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(id = ns_id) |> 
        add_mvt_layer(id = "metro_lines",
                      data = mvt_url("sus-mcgill.metro_lines"),
                      get_line_color = !!rlang::sym("fill"),
                      get_line_width = 2,
                      line_width_units = "pixels")
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
    
    # Click reactive
    observeEvent(get_clicked_object(ns_id_map), {
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(select_id()) && selection == select_id()) {
        select_id(NA)
      } else select_id(selection)
    })
    
    # Choose tileset
    tile <- zoom_server(
      id = ns_id, 
      zoom_string = zoom_string, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Get df for explore/legend/etc
    df <- reactive("CT")
    
    # Time
    time <- reactive("2016")
    
    # Choose tileset
    tile <- reactive("CT")
    
    # Additional tileset identifier
    tile2 <- reactive(
      tile_lookup$suffix[tile_lookup$module == "access" & 
                           tile_lookup$tile2 == var_left()])

    # Enable or disable slider + type of destination
    observeEvent({select_id()
      var_right()}, {
        toggle("access-slider", condition = !is.na(select_id()) && 
                 var_right() == " ")
        toggle("access-d_1", condition = is.na(select_id()) || 
                 var_right() != " ")
      })

    # Slider widget
    slider <- slider_server(id = ns_id)

    # Manual legend breaks
    breaks <- reactive({
      if (!is.na(select_id())) {
        setNames(slider() / 5 * 5:0, 
                 sus_translate("Minutes to reach census tract"))  
      } else NULL
    })
    
    # Left variable servers
    var_left_1 <- select_var_server(ns_id, select_var_id = "d_1",
                                    var_list = reactive(var_left_list_1_access))
    var_left_2 <- select_var_server(ns_id, select_var_id = "d_2",
                                    var_list = reactive(var_left_list_2_access))

    # Construct left variable string
    var_left <- reactive(paste0(var_left_1(), "_", var_left_2(), "_count"))
    
    # Compare panel
    var_right <- compare_server(
      id = ns_id,
      var_list = make_dropdown(compare = TRUE),
      df = df,
      time = time)
    
    # Composite variable for map
    map_var <- reactive({
      if (!is.na(select_id()) && var_right() == " ") return("ID")
      
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $")
      })

    # If there's a select_id, update the compare to " "
    observeEvent(select_id(), {
      updatePickerInput(
        session,
        inputId = "access-access-var",
        choices = sus_translate(make_dropdown(compare = TRUE)),
        selected = " ")
    }, priority = 1)

    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right()))
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)

    # Legend
    legend <- legend_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      df = df,
      breaks = breaks)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    access_colors <- reactive({
      if (!is.na(select_id()) && var_right() == " ") {
        tt_thresh <- slider() * 60

        CTs_to_map <- tt_matrix[c("timing", "destination", select_id())]
        names(CTs_to_map) <- c("timing", "destination", "travel_time")
        CTs_to_map <- CTs_to_map[CTs_to_map$timing == var_left_2(), ]
        CTs_to_map <- CTs_to_map[CTs_to_map$travel_time <= tt_thresh, ]
        CTs_to_map <- CTs_to_map[CTs_to_map$destination != select_id(),]
        CTs_to_map$group <- as.character(6 - ceiling((
          CTs_to_map$travel_time) / tt_thresh * 5))
        CTs_to_map <- CTs_to_map[, c("destination", "group")]
        CTs_to_map <- merge(CTs_to_map, colour_left_5, by = "group", 
                            all.x = TRUE)
        names(CTs_to_map) <- c("group", "ID", "fill")
        data_1 <- data()[, "ID"] |> merge(CTs_to_map, by = "ID")
        data_1 <- data_1[, c("ID", "fill")]
        data_2 <- data()[data()$ID == select_id(), "ID"]
        data_2$fill <- "#000000"
        out <- rbind(data_1, data_2)
        names(out) <- c("group", "value")
        out
      } else NULL
    })

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      zoom = zoom,
      select_id = select_id,
      fill = scale_fill_access,
      fill_args = reactive(list(map_var(), tile(), access_colors())),
    )
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)
    
    # De-select
    observeEvent(input[[paste0(ns_id, "-clear_selection")]], select_id(NA))
    observeEvent(df(), select_id(NA), ignoreInit = TRUE)
    # Error check
    observeEvent(data(), if (!select_id() %in% data()$ID) select_id(NA),
                 ignoreInit = TRUE)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(paste0(ns_id, "-map"))),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      map_id = NS(id, "map"),
      more_args = reactive(c("s-slider" = slider()))
    )

    # Update select_id() on bookmark
    observeEvent(sus_bookmark$active, {
      if (isTRUE(sus_bookmark$active)) {
        if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
        delay(1000, {
          if (!is.null(sus_bookmark$select_id))
            if (sus_bookmark$select_id != "NA") 
              select_id(sus_bookmark$select_id)
        })
      }
      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})
    }, priority = -2)
    
    # Update select_id() on module link
    observeEvent(sus_link$activity, {
      if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
      delay(1000, {
        if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
      })
    }, priority = -2)

  })
}
