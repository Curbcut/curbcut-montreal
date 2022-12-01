### GREEN ALLEY MODULE #########################################################


# Dropdown menu -----------------------------------------------------------

var_list_left_alley <-
  list("Borough summary" = " ",
       "Per sq km" = "green_alley_sqkm",
       "Per 1,000 residents" = "green_alley_per1k")


# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  id_map <- paste0(id, "-map")

  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        checkbox_UI(id = NS(id, id),
                    label = cc_t(r = r, "Green alleys visited by our team")),
        select_var_UI(NS(id, id), var_list = var_list_left_alley,
                      label = cc_t(r = r, "Grouping"))),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels_CMA)))),
    
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

alley_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")

    # Initial reactives
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels_CMA))
    poi <- reactiveVal(NULL)
    
    # Sidebar
    sidebar_server(id = id, r = r, x = "alley")

    # Enter in choropleth() depending on var_left select_id
    choropleth <- reactive(!(var_left() == " " || visited()))

    # Map
    output[[id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, 
            initial_view_state = view_state(center = map_loc, 
                                            zoom = isolate(r[[id]]$zoom()))) |> 
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
      r[[id]]$zoom(get_zoom(get_view_state(id_map)$zoom))
      new_poi <- observe_map(get_view_state(id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    }) |> bindEvent(get_view_state(id_map))
    
    # Map zoom levels change depending on "city"
    map_zoom_levels <- eventReactive(r$geo(), {
      out <- get_zoom_levels(default = "city", 
                             geo = "city",
                             var_left = isolate(var_left()))
    })
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels()$levels,
                                         "city")
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
    
    # Default location
    observe({
      if (is.null(r$default_select_id())) return(NULL)
      
      new_id <- data()$ID[data()$ID %in% 
                            r$default_select_id()[[gsub("_.*", "", r[[id]]$df())]]]
      if (length(new_id) == 0) return(NULL)
      
      r[[id]]$select_id(new_id)
    }) |> bindEvent(r$default_select_id(), r[[id]]$df())

    # Choose tileset
    tile_choropleth <- zoom_server(
      id = id, 
      r = r,
      zoom_string = zoom_string, 
      zoom_levels = map_zoom_levels)
    
    tile <- reactive({
      if (choropleth()) {
        tile_choropleth()
      } else if (!visited()) {
        "borough_empty"
      } else "alley"
    })
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string())
    
    # Focus on visited alleys
    visited <- checkbox_server(id = id)

    # Time
    time <- reactive("2016")

    # Left variable
    var_left_1 <- select_var_server(id = id, r = r,
                                    var_list = reactive(var_list_left_alley))
    
    var_left <- reactive(if (visited()) "alley_qual" else var_left_1())

    # Compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = make_dropdown(compare = TRUE),
      show_panel = choropleth,
      time = time)

    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      geo = "city",
      var_left = var_left(), 
      var_right = var_right()))
    
    # Data for tile coloring
    data_color <- reactive({
      if (!choropleth()) return(NULL)
      get_data_color(
        map_zoom_levels = map_zoom_levels()$levels,
        geo = "city",
        var_left = var_left(),
        var_right = var_right())
    })
    
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
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      hide = reactive(tile() == "borough_empty"))
        
    # Choose explore graph
    alley_graph <- reactive({
      if (is_scale_in_df(c("alley", "borough_empty"), r[[id]]$df())) {
        explore_graph_alley
      } else explore_graph
    })
    
    # Choose explore graph
    alley_table <- reactive({
      if (is_scale_in_df(c("alley", "borough_empty"), r[[id]]$df())) {
        info_table_alley
      } else info_table
    })
    
    # Explore panel
    explore_content <- explore_server(
      id = id,
      r = r,
      data = data,
      geo = reactive(map_zoom_levels()$region),
      var_left = var_left,
      var_right = var_right,
      graph = alley_graph,
      table = alley_table)
    
    # Popup the alley image if it's clicked on
    onclick(
      "alley_img", 
      {showModal(modalDialog(
        title = alley[alley$ID == r[[id]]$select_id(),]$name,
        HTML(paste0('<img src="alleys/',
                    alley[alley$ID == r[[id]]$select_id(),]$photo_ID,
                    '" width = 100%>')),
        easyClose = TRUE,
        size = "m",
        footer = NULL
      ))})
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      data_color = data_color,
      tile = reactive(if (tile() %in% c("borough_empty", "alley")) 
        paste(id, tile(), sep = "-") else tile()),
      fill = scale_fill_alley,
      fill_args = reactive(list(map_var(), tile(), data_color())),
      colour = scale_colour_alley,
      colour_args = reactive(list(map_var(), tile())),
      lwd = scale_lwd_alley,
      lwd_args = reactive(list(r[[id]]$select_id(), tile())))
    
    # Did-you-know panel
    dyk_server(
      id = id,
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
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(id_map)),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c("c-cbox" = visited()))
    )
    
  })
}
