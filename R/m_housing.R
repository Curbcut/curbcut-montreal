#### HOUSING MODULE ############################################################

# UI ----------------------------------------------------------------------

housing_UI <- function(id) {
  ns_id <- "housing"
  ns_id_map <- paste0(ns_id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        select_var_UI(NS(id, ns_id), var_list = vars_housing_left), 
        slider_UI(NS(id, ns_id), slider_id = "slu"), 
        slider_UI(NS(id, ns_id), slider_id = "slb",
                  label = sus_translate(r = r, "Select two years"),
                  value = c("2006", "2016")), 
        checkbox_UI(NS(id, ns_id),
                    label = sus_translate(r = r, "Compare dates")),
        year_disclaimer_UI(NS(id, ns_id))),
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), vars_housing_right),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
    
  )
}


# Server ------------------------------------------------------------------

housing_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "housing"
    ns_id_map <- paste0(ns_id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    new_poi <- reactiveVal(NULL)
    df <- reactiveVal("borough")
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = isolate(r[[ns_id]]$zoom())))
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
    tile <- zoom_server(
      id = ns_id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = reactive(map_zoom_levels))

    # Checkbox value
    slider_switch <- checkbox_server(id = ns_id)

    # Enable or disable first and second slider
    observeEvent(slider_switch(), {
      toggle(NS(id, "slu"), condition = !slider_switch())
      toggle(NS(id, "slb"), condition = slider_switch())
    })

    # Get df for explore/legend/etc
    observe(r[[ns_id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)
    
    # Time variable depending on which slider is active
    slider_uni <- slider_server(id = ns_id, slider_id = "slu")
    slider_bi <- slider_server(id = ns_id, slider_id = "slb")
    time <- reactive(if (slider_switch()) slider_bi() else slider_uni())

    # Left variable server
    var_left <- select_var_server(
      id = ns_id,
      r = r,
      var_list = reactive(vars_housing_left),
      disabled = reactive(if (!slider_switch()) NULL else
        vars_housing_left_dis),
      time = time)

    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id,
      r = r,
      var_list = vars_housing_right,
      disabled = reactive(if (!slider_switch()) NULL else
        vars_housing_right_dis),
      time = time)

    # Composite variable for map
    map_var <- reactive({
      if (length(var_left()) == 1) {
        year <- str_extract(var_left(), "\\d{4}.*$")
        vl <- str_remove(var_left(), "_\\d{4}.*$")
        vr <- str_remove(var_right(), "_\\d{4}.*$")
        paste(str_remove(paste(vl, vr, sep = "_"), "_ $"), year, sep = "_")
      } else {
        year <- str_extract(var_left(), "\\d{4}.*$") |> paste(collapse = "_")
        vl <- str_remove(var_left(), "_\\d{4}.*$") |> unique()
        vr <- str_remove(var_right(), "_\\d{4}.*$") |> unique()
        paste(str_remove(paste(vl, vr, sep = "_"), "_ $"), year, sep = "_")
      }
    })

    # Additional tileset identifier
    tile2 <- reactive(tile_lookup$suffix[tile_lookup$tile2 == map_var()])

    # Sidebar
    sidebar_server(id = ns_id, r = r, x = "housing")

    # Data
    data <- reactive(get_data(
      df = r[[ns_id]]$df(),
      var_left = var_left(),
      var_right = var_right()))

    # Legend
    legend <- legend_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # Year disclaimer
    year_disclaimer_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      time = time)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      r = r,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var)

    # Update map labels
    label_server(
      id = ns_id,
      r = r,
      map_id = "map",
      tile = tile)
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      r = r,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c(
        "c-cbox" = str_extract(slider_switch(), "^."),
        "s-slu" = slider_uni(),
        "s-slb" = paste(slider_bi(), collapse = "-")))
    )
  })
}
