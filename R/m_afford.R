#### HOUSING AFFORDABILITY MODULE ##############################################

# UI ----------------------------------------------------------------------

afford_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        
        select_var_UI(NS(id, id), select_var_id = "d_1",
                      label = sus_translate(r = r, "Grouping"),
                      var_list = var_left_list_1_afford), 
        select_var_UI(NS(id, id), select_var_id = "d_2",
                      label = sus_translate(r = r, "Shelter cost"),
                      var_list = var_left_list_2_afford), 
        checkbox_UI(NS(id, id),
                    label = sus_translate(r = r, 
                                          #### TO CHANGE DYNAMICALLY TO PERCENT OF HOUSEHOLDS VS INDIVIDUALS
                                          "Normalized (percent of households)")),
        br(),
        div(id = NS(id, "household_dropdowns"), 
            select_var_UI(NS(id, id), select_var_id = "d_3",
                          label = sus_translate(r = r, "Tenure status"),
                          var_list = var_left_list_3_afford),
            select_var_UI(NS(id, id), select_var_id = "d_4",
                          label = sus_translate(r = r, "Additional characteristic"),
                          var_list = var_left_list_3_afford)),
        
          div(id = NS(id, "population_dropdowns"),
              select_var_UI(NS(id, id), select_var_id = "d_5",
                            label = sus_translate(r = r, "Gender"),
                            var_list = var_left_list_3_afford),
              select_var_UI(NS(id, id), select_var_id = "d_6",
                            label = sus_translate(r = r, "Immigration status"),
                            var_list = var_left_list_3_afford),
              select_var_UI(NS(id, id), select_var_id = "d_7",
                            label = sus_translate(r = r, "Additional characteristic"),
                            var_list = var_left_list_3_afford))),
      
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels_max_CT)))),
    
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

afford_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels_max_CT))
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
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels_max_CT)
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
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = reactive(map_zoom_levels_max_CT))
    
    # Time
    time <- reactive("2016")
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)
    
    # Checkbox value
    as_pct <- checkbox_server(id = id)
    
    # Update checkbox label depending on the grouping
    observeEvent(vl_gr(), {
      grp <- if (vl_gr() == "cent_d") "households" else "population"
      updateCheckboxInput(inputId = "afford-cbox",
                          label = 
                            sus_translate(r = r,
                                          paste0("Normalized (percent of ", 
                                                 grp, ")")))
    })
    
    # Left variable server
    vl_gr <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_1",
      var_list = reactive(var_left_list_1_afford))
    
    vl_sc <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_2",
      var_list = reactive(var_left_list_2_afford))
    
    # Left variable server - Households
    vl_tn <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_3",
      var_list = reactive(var_left_list_3_afford))
    
    vl_add_h <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_4",
      var_list = reactive(var_left_list_4_afford))
    
    # Left variable server - population
    vl_gn <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_5",
      var_list = reactive(var_left_list_5_afford))
    
    vl_im <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_6",
      var_list = reactive(var_left_list_6_afford))
    
    vl_add_p <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_7",
      var_list = reactive(var_left_list_7_afford),
      disabled = reactive(if (vl_im() == "immigrants") 
        vars_afford_add_dis_imm else
          vars_afford_add_dis_nimm))
    
    # Hide/show dropdown divs depending on the first dropdown
    observeEvent(vl_gr(), {
      if (vl_gr() == "cent_d") {
        shinyjs::hide(id = "population_dropdowns", anim = TRUE)
        shinyjs::show(id = "household_dropdowns", anim = TRUE)}
      if (vl_gr() == "cent_p") {
        shinyjs::hide(id = "household_dropdowns", anim = TRUE)
        shinyjs::show(id = "population_dropdowns", anim = TRUE)}
    })
    
    # Final left variable server creation
    var_left <- reactive({
      if (vl_gr() == "cent_d")
        return(paste(vl_gr(), 
                     vl_tn(), vl_sc(), vl_add_h(), 
                     if (as_pct()) "pct" else "count",
                     time(), sep = "_"))
        
      if (vl_gr() == "cent_p")
        return(paste(vl_gr(), 
                     vl_im(), vl_add_p(), vl_sc(), vl_gn(), 
                     if (as_pct()) "pct" else "count",
                     time(), sep = "_"))
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
      var_left = var_left(),
      var_right = var_right()))
    
    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)
    
    afford_colors <- reactive({
      if (var_right() == " ") {
        selected <- data()[, c("ID", "var_left_q5")]
        out <- merge(selected, colour_table, by.x = "var_left_q5", 
                     by.y = "group")[, c("ID", "value")]
        names(out) <- c("group", "value")
        out
      } else {
        selected <- data()[, c("ID", "var_left_q3", "var_right_q3")]
        selected$group <- 
          paste(selected$var_left_q3, selected$var_right_q3, sep = " - ")
        out <- merge(selected, colour_bivar, by.x = "group", 
                     by.y = "group")[, c("ID", "fill")]
        names(out) <- c("group", "value")
        out
      }
    })
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      id_override = vl_gr,
      r = r,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      fill = scale_fill_cent,
      fill_args = reactive(list(map_var(), tile(), afford_colors())))
    
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
      var_right = var_right,
      more_args = reactive(c(
        "c-cbox" = str_extract(as_pct(), "^.")))
    )
    
  })
}
