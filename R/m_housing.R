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
                  label = sus_translate("Select two years"),
                  value = c("2006", "2016")), 
        checkbox_UI(NS(id, ns_id),
                    label = sus_translate("Compare dates")),
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

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "housing"
    ns_id_map <- paste0(ns_id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    new_poi <- reactiveVal(NULL)
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom))
    })
    
    # Zoom and POI reactives
    observeEvent(get_view_state(ns_id_map), {
      zoom(get_zoom(get_view_state(ns_id_map)$zoom))
      new_poi(observe_map(get_view_state(ns_id_map)))
      if ((is.null(new_poi()) && !is.null(poi())) || 
          (!is.null(new_poi()) && (is.null(poi()) || !all(new_poi() == poi()))))
        poi(new_poi())
    })
    
    # Zoom string reactive
    observeEvent(zoom(), {
      new_zoom_string <- get_zoom_string(zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    })
    
    # Click reactive
    observeEvent(get_clicked_object(ns_id_map), {
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(select_id()) && selection == select_id()) return(select_id(NA))
      
      select_id(selection)
    })
    
    # Choose tileset
    tile <- zoom_server(
      id = ns_id, 
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
    df <- reactive(get_df(tile(), zoom_string()))
    
    # Time variable depending on which slider is active
    slider_uni <- slider_server(id = ns_id, slider_id = "slu")
    slider_bi <- slider_server(id = ns_id, slider_id = "slb")
    time <- reactive(if (slider_switch()) slider_bi() else slider_uni())
    
    # Left variable server
    var_left <- select_var_server(
      id = ns_id, 
      var_list = reactive(vars_housing_left), 
      disabled = reactive(if (!slider_switch()) NULL else 
        vars_housing_left_dis),
      time = time, 
      df = df)

    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = vars_housing_right, 
      disabled = reactive(if (!slider_switch()) NULL else 
        vars_housing_right_dis),
      df = df,
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
    sidebar_server(id = ns_id, x = "housing")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right()))
    
    # Legend
    legend <- legend_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      df = df)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Year disclaimer
    year_disclaimer_server(
      id = ns_id, 
      data = data,
      var_left = var_left,
      var_right = var_right)
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      tile2 =  tile2,
      map_var = map_var, 
      zoom = zoom,
      select_id = select_id)
    
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
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
      more_args = reactive(c(
        "c-cbox" = str_extract(slider_switch(), "^."),
        "s-slu" = slider_uni(), 
        "s-slb" = paste(slider_bi(), collapse = "-")))
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
