#### VACANCY RATE MODULE #######################################################

# UI ----------------------------------------------------------------------

vac_rate_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        select_var_UI(NS(id, id), select_var_id = "d_1", 
                      var_list = var_left_list_1_vac_rate,
                      label = cc_t(r = r, "Vacancy rate distribution")), 
        select_var_UI(NS(id, id), select_var_id = "d_2", 
                      var_list = var_left_list_2_vac_rate,
                      label = cc_t(r = r, "Bedroom type")), 
        select_var_UI(NS(id, id), select_var_id = "d_3", 
                      var_list = var_left_list_3_vac_rate,
                      label = cc_t(r = r, "Year of construction")), 
        select_var_UI(NS(id, id), select_var_id = "d_4", 
                      var_list = var_left_list_4_vac_rate,
                      label = cc_t(r = r, "Rent ranges")), 
        slider_UI(NS(id, id), slider_id = "slu",
                  min = 2010, max = 2021, step = 1, value = 2021), 
        slider_UI(NS(id, id), slider_id = "slb",
                  label = cc_t(r = r, "Select two years"),
                  value = c("2011", "2020"),
                  min = 2010, max = 2021, step = 1), 
        checkbox_UI(NS(id, id),
                    label = cc_t(r = r, "Compare dates")),
        year_disclaimer_UI(NS(id, id))),
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, id))))),
    
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

vac_rate_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels_CMA))
    select_id <- reactiveVal(NA)
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
    
    # Map zoom levels change depending on r$geo()
    map_zoom_levels <- eventReactive(r$geo(), {
      get_zoom_levels(default = "cmhc", 
                      geo = r$geo(),
                      var_left = isolate(var_left()))
    })
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels()$levels,
                                         map_zoom_levels()$region)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom(), map_zoom_levels()$levels)
    
    # Click reactive
    observe({
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) && 
          selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else r[[id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(id_map))
    
    # Default location
    observe({
      if (is.null(r$default_select_id())) return(NULL)
      
      new_id <- data()$ID[data()$ID %in% 
                            r$default_select_id()[[gsub("_.*", "", r[[id]]$df())]]]
      if (length(new_id) == 0) return(NULL)
      
      r[[id]]$select_id(new_id)
    }) |> bindEvent(r$default_select_id(), r[[id]]$df())

    # Choose tileset
    tile <- reactive("cmhc_cmhczone")

    # Checkbox value
    slider_switch <- checkbox_server(id = id)

    # Enable or disable first and second slider
    observeEvent(slider_switch(), {
      toggle(NS(id, "slu"), condition = !slider_switch())
      toggle(NS(id, "slb"), condition = slider_switch())
    })
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string())
    
    # Time variable depending on which slider is active
    slider_uni <- slider_server(id = id, slider_id = "slu")
    slider_bi <- slider_server(id = id, slider_id = "slb")
    time <- reactive(if (slider_switch()) slider_bi() else slider_uni())
    
    # Left variable server
    gr <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_1",
      var_list = reactive(var_left_list_1_vac_rate))
    bed <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_2",
      var_list = reactive(var_left_list_2_vac_rate))
    year <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_3",
      var_list = reactive(var_left_list_3_vac_rate))
    rent_range <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_4",
      var_list = reactive(var_left_list_4_vac_rate))
    
    var_left <- reactive({
      second_drop <- 
        if (gr() == "bed") bed() else if (gr() == "year") year() else rent_range()
      paste("vac_rate", gr(), second_drop, time(), sep = "_")
    })
    
    # Hide/show dropdown divs depending on the first dropdown
    observeEvent(gr(), {
      toggle(paste0(id, "-d_2"), condition = gr() == "bed")
      toggle(paste0(id, "-d_3"), condition = gr() == "year")
      toggle(paste0(id, "-d_4"), condition = gr() == "rent_range")
    })
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = make_dropdown(compare = TRUE),
      disabled = reactive(if (!slider_switch()) NULL else
        vars_housing_right_dis),
      time = time)

    # Sidebar
    sidebar_server(id = id, r = r)

    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      geo = map_zoom_levels()$region,
      var_left = var_left(),
      var_right = var_right()))

    # Data for tile coloring
    data_color <- reactive(get_data_color(
      map_zoom_levels = map_zoom_levels()$levels,
      geo = map_zoom_levels()$region,
      var_left = var_left(),
      var_right = var_right()
    ))

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      geo = reactive(map_zoom_levels()$region))

    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # Year disclaimer
    year_disclaimer_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      time = time)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile,
      data_color = data_color,
      zoom_levels = reactive(map_zoom_levels()$levels))

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
      geo = reactive(map_zoom_levels()$region),
      var_left = var_left,
      var_right = var_right)

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(id_map)),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c(
        "c-cbox" = str_extract(slider_switch(), "^."),
        "s-slu" = slider_uni(),
        "s-slb" = paste(slider_bi(), collapse = "-")))
    )

    # Data transparency and export
    r[[id]]$export_data <- reactive(data_export(id = id,
                                                data = data(),
                                                var_left = var_left(),
                                                var_right = var_right(),
                                                df = r[[id]]$df()))

  })
}
