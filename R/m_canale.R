### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id), 
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id)))
    
  )
}


# Server ------------------------------------------------------------------

canale_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
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
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom())
    
    # Click reactive
    observe({
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) && 
          selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else r[[id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(id_map))
    
    # Choose tileset
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = reactive(map_zoom_levels))

    # Get df for explore/legend/etc
    # Must be inactive at init: the bookmark might want to set this value
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)
    
    # Time
    time <- reactive("2016")

    # Left variable
    var_left <- reactive(paste0("canale_ind_", time()))

    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = make_dropdown(compare = TRUE),
      time = time)

    # Additional tileset identifier
    tile2 <- reactive("")

    # Composite variable for map
    map_var <- reactive(
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))

    # Sidebar
    sidebar_server(id = id, r = r, x = "canale")

    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      var_left = var_left(),
      var_right = var_right()))

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right)

    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id, 
      r = r, 
      map_id = "map",
      tile = tile, 
      tile2 = tile2,
      map_var = map_var)
    
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
      var_right = var_right)

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(id_map)),
      var_right = var_right,
    )

  })
}
