### GREEN ALLEY MODULE #########################################################


# Dropdown menu -----------------------------------------------------------

var_list_left_alley <-
  list("Borough summary" = " ",
       "Per sq km" = "green_alley_sqkm",
       "Per 1,000 residents" = "green_alley_per1k")


# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  ns_id <- "alley"
  ns_id_map <- paste0(ns_id, "-map")

  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        checkbox_UI(id = NS(id, ns_id),
                    label = sus_translate(r = r, "Green alleys visited by our team")),
        select_var_UI(NS(id, ns_id), var_list = var_list_left_alley,
                      label = sus_translate(r = r, "Grouping"))),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))

  )
}


# Server ------------------------------------------------------------------

alley_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "alley"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    df <- reactiveVal("borough_empty")
    
    # Sidebar
    sidebar_server(id = ns_id, r = r, x = "alley")

    # Enter in choropleth() depending on var_left select_id
    choropleth <- reactive(!(var_left() == " " || visited()))

    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, 
            initial_view_state = view_state(center = map_loc, zoom = 12)) |> 
        add_mvt_layer(id = "alley-alley",
                      data = mvt_url("sus-mcgill.alley-alley"),
                      pickable = FALSE,
                      auto_highlight = FALSE,
                      get_fill_color = paste0(colour_table$value[1], "90"),
                      get_line_color = paste0(colour_table$value[1], "90"),
                      line_width_units = "pixels",
                      get_line_width = 2)
    })

    # Zoom and POI reactives
    observeEvent(get_view_state(ns_id_map), {
      zoom({
        if (!is.null(r$sus_bookmark$zoom)) {
          r$sus_bookmark$zoom
        } else if (!is.null(r$sus_link$zoom)) {
          r$sus_link$zoom
        } else get_zoom(get_view_state(ns_id_map)$zoom)})
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
    tile_choropleth <- zoom_server(
      id = ns_id, 
      r = r,
      zoom_string = zoom_string, 
      zoom_levels = reactive(map_zoom_levels))
    
    tile <- reactive({
      if (choropleth()) {
        tile_choropleth()
      } else if (!visited()) {
        "borough_empty"
      } else "alley"
    })

    # Additional tileset identifier
    tile2 <- reactive("")
    
    # Get df for explore/legend/etc
    observe(df(get_df(tile(), zoom_string(), r = r)))
    
    # Focus on visited alleys
    visited <- checkbox_server(id = ns_id)

    # Time
    time <- reactive("2016")

    # Left variable
    var_left_1 <- select_var_server(id = ns_id, r = r,
                                    var_list = reactive(var_list_left_alley))
    
    var_left <- reactive(if (visited()) "alley_qual" else var_left_1())

    # Compare panel
    var_right <- compare_server(
      id = ns_id,
      r = r,
      var_list = make_dropdown(compare = TRUE),
      show_panel = choropleth,
      time = time)

    # Data
    data <- reactive(get_data(df(), var_left(), var_right(), island = TRUE))
    
    # Composite variable for map
    map_var <- reactive({
      if (choropleth()) {
        str_remove(paste(var_left(), var_right(), sep = "_"), "_ $")
      } else if (!visited()) {
        ""
      } else "type"
    })

    # Legend
    legend_server(
      id = ns_id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      df = df,
      hide = reactive(tile() == "borough_empty"))
    
    # Choose explore graph
    alley_graph <- reactive({
      if (df() %in% c("alley", "borough_empty")) {
        explore_graph_alley
      } else explore_graph
    })
    
    # Choose explore graph
    alley_table <- reactive({
      if (df() %in% c("alley", "borough_empty")) {
        info_table_alley
      } else info_table
    })
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id,
      graph = alley_graph,
      table = alley_table)
    
    # Popup the alley image if it's clicked on
    onclick(
      "alley_img", 
      {showModal(modalDialog(
        title = alley[alley$ID == select_id(),]$name,
        HTML(paste0('<img src="alleys/',
                    alley[alley$ID == select_id(),]$photo_ID,
                    '" width = 100%>')),
        easyClose = TRUE,
        size = "m",
        footer = NULL
      ))})
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      zoom = zoom,
      select_id = select_id,
      fill = scale_fill_alley,
      fill_args = reactive(list(map_var(), tile())),
      colour = scale_colour_alley,
      colour_args = reactive(list(map_var(), tile())),
      lwd = scale_lwd_alley,
      lwd_args = reactive(list(select_id(), tile())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Toggle zoom
    observeEvent({
      choropleth()
      visited()}, {
        toggle("alley-zoom_auto", condition = choropleth() && !visited())
        toggle("alley-zoom_slider", condition = choropleth() && !visited())
        # If focus is clicked, toggle dropdown menu
        toggle("alley-var", condition = !visited())
      })

    # Hook up "Clear select_id" button and other variables that clears it
    observeEvent(input[[paste0(ns_id, "-clear_selection")]], select_id(NA))
    observeEvent(choropleth(), select_id(NA))
    observeEvent(visited(), select_id(NA))
    observeEvent(data(), if (choropleth()) select_id(NA))
    observeEvent(df(), select_id(NA), ignoreInit = TRUE)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      r = r,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
      more_args = reactive(c("c-cbox" = visited()))
    )

    # Update select_id() on bookmark
    observeEvent(r$sus_bookmark$active, {
      if (isTRUE(r$sus_bookmark$active)) {
        delay(1000, {
          if (!is.null(r$sus_bookmark$select_id))
            if (r$sus_bookmark$select_id != "NA") 
              select_id(r$sus_bookmark$select_id)
          df(r$sus_bookmark$df)
        })
      }
      # So that bookmarking gets triggered only ONCE
      delay(1500, {
        r$sus_bookmark$active <- FALSE
        r$sus_bookmark$zoom <- NULL
      })
    }, priority = -2)
    
    # Update select_id() on module link
    observeEvent(r$sus_link$activity, {
      delay(1000, {
        if (!is.null(r$sus_link$select_id)) select_id(r$sus_link$select_id)
        df(r$sus_link$df)
        r$sus_link$zoom <- NULL
      })
    }, priority = -2)

  })
}
