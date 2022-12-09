#### DEMOGRAPHICS MODULE ######################################################

# UI ----------------------------------------------------------------------

demographics_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        select_var_UI(NS(id, id), select_var_id = "d_1",
                      label = cc_t(r = r, "Grouping"),
                      var_list = var_left_list_1_demographics), 
        select_var_UI(NS(id, id), select_var_id = "d_2",
                      label = cc_t(r = r, "Gender"),
                      var_list = var_left_list_2_demographics), 
        select_var_UI(NS(id, id), select_var_id = "d_3",
                      label = cc_t(r = r, "Immigration status"),
                      var_list = var_left_list_3_demographics), 
        select_var_UI(NS(id, id), select_var_id = "d_4",
                      label = cc_t(r = r, "Shelter cost"),
                      var_list = var_left_list_4_demographics), 
        
        br(),
        select_var_UI(NS(id, id), select_var_id = "d_5",
                      label = cc_t(r = r, "Additional characteristic"),
                      var_list = var_left_list_5_demographics)), 
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels_CMA_max_CT)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, id), cent_compare),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id)))
  )
}


# Server ------------------------------------------------------------------

demographics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels_CMA_max_CT))
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
    
    # Map zoom levels change depending on r$geo(). Listening only to the latter
    # to not have to recalculate everytime var_left() changes.
    map_zoom_levels <- eventReactive(r$geo(), {
      get_zoom_levels(default = "CMA", 
                      geo = r$geo(),
                      var_left = isolate(var_left()),
                      suffix_zoom_levels = "_max_CT")
    })
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels()$levels,
                                         map_zoom_levels()$region)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom(), map_zoom_levels()$region)
    
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
    
    # Sidebar
    sidebar_server(id = id, r = r)
    # Centraide logo
    observe({
      insertUI(selector = paste0("#", paste(id, id, "title", sep = "-")),
               where = "beforeEnd",
               tags$a(href = "https://www.centraide-mtl.org/", target = "_blank",
                      img(src = paste0("centraide_logo/centraide_logo_", r$lang(), ".png"), 
                          style = 'width:70%; display:block; margin:auto; margin-top:15px; margin-bottom:15px;')))
    })
    
    # Choose tileset
    tile_1 <- zoom_server(
      id = id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = map_zoom_levels)
    
    tile <- reactive({
      if (!grepl("auto_zoom", tile_1())) return(tile_1())
      paste0(tile_1(), "_max_CT")
    })
    
    # Time
    time <- reactive("2016")
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string())
    
    # Left variable server
    vl_gr <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_1",
      var_list = reactive(var_left_list_1_demographics))
    
    vl_gn <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_2",
      var_list = reactive(var_left_list_2_demographics))
    
    vl_im <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_3",
      var_list = reactive(var_left_list_3_demographics))
    
    vl_sc <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_4",
      var_list = reactive(var_left_list_4_demographics))
    
    vl_add <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_5",
      var_list = reactive(var_left_list_5_demographics),
      disabled = reactive(if (vl_im() == "immigrants") 
        vars_demographics_add_dis_imm else
          vars_demographics_add_dis_nimm))
    
    # Final left variable server creation
    var_left <- reactive({
      paste("cent_p",
            vl_im(), vl_add(), vl_sc(), vl_gn(), vl_gr(), 
            time(), sep = "_")
    })
    
    # Composite variable for map
    map_var <- var_left
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = cent_compare,
      time = time)
    
    # Additional tileset identifier
    tile2 <- reactive("")
    
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
      var_right = var_right)
    
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
      var_right = var_right)
    
    # Data transparency and export
    r[[id]]$export_data <- reactive(data_export(id = id,
                                                data = data(),
                                                var_left = var_left(),
                                                var_right = var_right(),
                                                df = r[[id]]$df()))
    
  })
}
