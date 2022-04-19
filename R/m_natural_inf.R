### NATURAL INFRASTRUCTURE MODULE #########################################

# UI ----------------------------------------------------------------------

natural_inf_UI <- function(id) {
  ns_id <- "natural_inf"
  ns_id_map <- paste0(ns_id, "-map")

  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        select_var_UI(NS(id, ns_id), 
                      select_var_id = "vl_1",
                      var_list = vars_natural_inf_left),
        select_var_UI(NS(id, ns_id), 
                      select_var_id = "vl_2",
                      var_list = list("----" = " "),
                      label = sus_translate(
                        "Dive further in the selected ",
                        "natural infrastructure's contributions:")),
        slider_UI(NS(id, ns_id),
                  label = sus_translate("Natural infrastructure protection:"),
                  min = 0,
                  max = 25,
                  step = 1,
                  value = 17,
                  post = "%"),
        checkbox_UI(NS(id, ns_id),
                    label = sus_translate("Custom priorities")),
        slider_UI(NS(id, ns_id),
                  slider_id = "s_bio",
                  label = sus_translate("Biodiversity conservation:"),
                  min = 0,
                  max = 2,
                  step = 0.5,
                  value = 1),
        slider_UI(NS(id, ns_id),
                  slider_id = "s_hea",
                  label = sus_translate("Heat island reduction:"),
                  min = 0,
                  max = 2,
                  step = 0.5,
                  value = 1),
        slider_UI(NS(id, ns_id),
                  slider_id = "s_flo",
                  label = sus_translate("Flood prevention:"),
                  min = 0,
                  max = 2,
                  step = 0.5,
                  value = 1)
        ),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, ns_id))))),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))
  )
}


# Server ------------------------------------------------------------------

natural_inf_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "natural_inf"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(9.5, map_zoom_levels))
    poi <- reactiveVal(NULL)
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = 9.5))
    })
    
    # Zoom and POI reactives
    observeEvent(get_view_state(ns_id_map), {
      zoom(get_zoom(get_view_state(ns_id_map)$zoom))
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
    
    # Choose tileset
    tile <- reactive({
      if (var_left() == "c_priority" &&
          custom_priorities()) return("custom")
      
      var_left()
      })

    # Left variable
    var_left_1 <- select_var_server(
      id = ns_id,
      select_var_id = "vl_1",
      var_list = reactive(vars_natural_inf_left))
    
    var_left_2_list <- reactive({
      if (var_left_1() == "c_priority") 
        return(list("----" = " "))
      
      eval(parse(text =
                   paste0("vars_natural_inf_left_",
                          var_left_1())))
    })
    
    var_left_2 <- select_var_server(
      id = ns_id,
      select_var_id = "vl_2",
      var_list = reactive(var_left_2_list()))
    
    var_left <- reactive({
      if (var_left_2() != " ") return(var_left_2())
      var_left_1()
    })
    
    observeEvent(var_left_1(), {
      toggle(NS(id, "vl_2"), 
             condition = var_left_1() != "c_priority")
    })
    
    # Checkbox value
    custom_priorities <- checkbox_server(id = ns_id)
    observeEvent(var_left_1(), {
      updateCheckboxInput(session = session,
                          inputId = NS(id, "cbox"),
                          value = FALSE)
    })
    observeEvent(var_left_1(), {
      toggle(NS(id, "cbox"), 
             condition = var_left_1() == "c_priority")
    })
    

    # Main slider value
    main_slider <- slider_server(id = ns_id)
    
    # Three other sliders
    s_bio <- slider_server(id = ns_id, slider_id = "s_bio")
    s_hea <- slider_server(id = ns_id, slider_id = "s_hea")
    s_flo <- slider_server(id = ns_id, slider_id = "s_flo")
    observeEvent(custom_priorities(), {
      toggle(NS(id, "s_bio"), 
             condition = custom_priorities())
      toggle(NS(id, "s_hea"), 
             condition = custom_priorities())
      toggle(NS(id, "s_flo"), 
             condition = custom_priorities())
    })

    # Right variable / compare panel
    var_right <- reactive(" ")

    # Sidebar
    sidebar_server(id = ns_id, x = "natural_inf")
    
    # Composite variable for map
    map_var <- reactive({
      if (custom_priorities()) return("ID")
      var_left()
    })
    
    # Enable or disable the main main_slider
    observe({
      toggle(NS(id, "slider"), 
             condition = var_left() == "c_priority")
    })
    
    # Data
    data <- reactive({
        if (var_left() == "c_priority") {
          
          slider <- main_slider() * 4
          
          if (!custom_priorities()) {
          
            dat <- 
              natural_inf$original_priorities[
                natural_inf$original_priorities$conservation_pct == 
                  slider, ]
            
            return(list(flood_prevention = dat$flood,
                        biodiversity_conservation = dat$biodiversity,
                        heat_island_reduction = dat$heat_island,
                        ni_protection = slider))
          } else {
            custom <- natural_inf$custom_explore
            custom <- custom[custom$conservation_pct == main_slider(), ]
            custom <- custom[custom$biodiversity == s_bio(), ]
            custom <- custom[custom$heat_island == s_hea(), ]
            custom <- custom[custom$flood == s_flo(), ]
            
            return(list(flood_prevention = custom$flood_prevention,
                        biodiversity_conservation = custom$biodiversity_conservation,
                        heat_island_reduction = custom$heat_island_reduction,
                        ni_protection = slider))
          }
        } else NULL
        
      })
    
    # Map custom colours
    natural_inf_colours <- reactive({
      if (var_left() == "c_priority" && custom_priorities()) {
        
        custom <- natural_inf$custom
        custom <- custom[custom$conservation_pct == main_slider(), ]
        custom <- custom[custom$biodiversity == s_bio(), ]
        custom <- custom[custom$heat_island == s_hea(), ]
        custom <- custom[custom$flood == s_flo(), ]
        
        out <-
          data.frame(group = as.character(seq_along(natural_inf_custom$ID)),
                     value = "#FFFFFF00")
        out <- out[!out$group %in% custom$group, ]
        rbind(out, custom[, c("group", "value")])
        
      } else NULL
      
    })
    
    legend_raster <- reactive({
      if (var_left() %in% c("heat", "cool", "flood")) return(NULL)
      "raster"
    })

    # Legend
    legend <- legend_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      df = legend_raster)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      map_id = "map", 
      tile = tile,
      tile2 =  reactive(""),
      map_var = map_var, 
      zoom = zoom,
      select_id = reactive(NA),
      lwd = scale_lwd_natural_inf,
      lwd_args = reactive(list()),
      fill = scale_fill_natural_inf,
      fill_args = reactive(list(map_var(), tile(), natural_inf_colours())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)

    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = reactive(NULL),
      select_id = reactive(NA),
      graph = reactive(explore_graph_natural_inf),
      table = reactive(info_table_natural_inf))

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      select_id = reactive(NA),
      map_id = "map",
    )

  })
}
