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
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    poi <- reactiveVal(NULL)
    
    # Sidebar
    sidebar_server(id = ns_id, r = r, x = "alley")

    # Enter in choropleth() depending on var_left select_id
    choropleth <- reactive(!(var_left() == " " || visited()))

    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, 
            initial_view_state = view_state(center = map_loc, 
                                            zoom = isolate(r[[ns_id]]$zoom()))) |> 
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
    observe({
      r[[ns_id]]$zoom(get_zoom(get_view_state(ns_id_map)$zoom))
      new_poi <- observe_map(get_view_state(ns_id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    }) |> bindEvent(get_view_state(ns_id_map))
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[ns_id]]$zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[ns_id]]$zoom())
    
    # Click reactive
    observe({
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(r[[ns_id]]$select_id()) && 
          selection == r[[ns_id]]$select_id()) {
        r[[ns_id]]$select_id(NA)
      } else r[[ns_id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(ns_id_map))

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
    observe(r[[ns_id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)
    
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
    data <- reactive(get_data(
      df = r[[ns_id]]$df(),
      var_left = var_left(), 
      var_right = var_right(), 
      island = TRUE))
    
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
      hide = reactive(tile() == "borough_empty"))
    
    # Choose explore graph
    alley_graph <- reactive({
      if (r[[ns_id]]$df() %in% c("alley", "borough_empty")) {
        explore_graph_alley
      } else explore_graph
    })
    
    # Choose explore graph
    alley_table <- reactive({
      if (r[[ns_id]]$df() %in% c("alley", "borough_empty")) {
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
      graph = alley_graph,
      table = alley_table)
    
    # Popup the alley image if it's clicked on
    onclick(
      "alley_img", 
      {showModal(modalDialog(
        title = alley[alley$ID == r[[ns_id]]$select_id(),]$name,
        HTML(paste0('<img src="alleys/',
                    alley[alley$ID == r[[ns_id]]$select_id(),]$photo_ID,
                    '" width = 100%>')),
        easyClose = TRUE,
        size = "m",
        footer = NULL
      ))})
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      r = r,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      fill = scale_fill_alley,
      fill_args = reactive(list(map_var(), tile())),
      colour = scale_colour_alley,
      colour_args = reactive(list(map_var(), tile())),
      lwd = scale_lwd_alley,
      lwd_args = reactive(list(r[[ns_id]]$select_id(), tile())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      r = r,
      map_id = "map", 
      tile = tile)
    
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

    # Bookmarking
    bookmark_server(
      id = ns_id,
      r = r,
      map_viewstate = get_view_state(ns_id_map),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c("c-cbox" = visited()))
    )

  })
}
