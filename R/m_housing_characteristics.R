#### VULNERABLE POPULATION MODULE ##############################################

# UI ----------------------------------------------------------------------

housing_characteristics_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        select_var_UI(NS(id, id), select_var_id = "d_1",
                      label = sus_translate(r = r, "Tenure status"),
                      var_list = var_left_list_1_housing_characteristics), 
        select_var_UI(NS(id, id), select_var_id = "d_2",
                      label = sus_translate(r = r, "Shelter cost"),
                      var_list = var_left_list_2_housing_characteristics), 
        select_var_UI(NS(id, id), select_var_id = "d_3",
                      label = sus_translate(r = r, "Housing characteristic"),
                      var_list = var_left_list_3_housing_characteristics)), 
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

housing_characteristics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal("CT")
    poi <- reactiveVal(NULL)
    new_poi <- reactiveVal(NULL)

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
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), c("CT" = 0))
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
    
    # Sidebar
    sidebar_server(id = id, r = r)

    # Choose tileset
    tile <- reactive("CT")
    
    # Time
    time <- reactive("2016")

    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)

    # Left variable server
    vl_tenant <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_1",
      var_list = reactive(var_left_list_1_housing_characteristics))
    
    vl_sc <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_2",
      var_list = reactive(var_left_list_2_housing_characteristics))
    
    vl_charact <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_3",
      var_list = reactive(var_left_list_3_housing_characteristics))
    
    var_left <- reactive({
      paste("housing_characteristics",
        vl_tenant(), vl_sc(), vl_charact(), time(), sep = "_")
    })

    # Composite variable for map
    map_var <- var_left
    
    # Right var 
    var_right <- reactive(" ")

    # Additional tileset identifier
    tile2 <- reactive("")
    
    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      var_left = var_left(),
      var_right = var_right()))

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
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
    
    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(paste0(id, "-map"))),
      var_left = var_left,
      var_right = var_right
    )
    
  })
}
