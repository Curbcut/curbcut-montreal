### NATURAL INFRASTRUCTURE MODULE #########################################

# UI ----------------------------------------------------------------------

natural_infrastructure_UI <- function(id) {
  ns_id <- "natural_infrastructure"
  ns_id_map <- paste0(ns_id, "-map")

  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        select_var_UI(NS(id, ns_id), var_list = vars_natural_infrastructure_left),
        checkbox_UI(NS(id, ns_id),
                    label = sus_translate("Focus on prioritization")),
        hidden(slider_UI(NS(id, ns_id),
                         label = sus_translate("Slide to visualize more/less importance"),
                         min = round(min(natural_infrastructure$habitat_quality, na.rm = TRUE),
                                     digits = 2),
                         max = round(max(natural_infrastructure$habitat_quality, na.rm = TRUE),
                                     digits = 2),
                         step = NULL,
                         value = round(min(natural_infrastructure$habitat_quality, na.rm = TRUE),
                                       digits = 2))),
        htmlOutput(NS(id, "explainer"))),
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

natural_infrastructure_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "natural_infrastructure"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom))
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
    
    # Click reactive
    observeEvent(get_clicked_object(ns_id_map), {
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(select_id()) && selection == select_id()) {
        select_id(NA)
      } else select_id(selection)
    })
    
    # Choose tileset
    tile <- reactive(which(vars_natural_infrastructure_left == var_left()))

    # Checkbox value
    focus <- checkbox_server(id = ns_id)

    # Enable or disable first and second slider
    observeEvent(focus(), toggle(NS(id, "slider"), condition = focus()))
    
    # df reactive to grid
    df <- reactive("grid")

    # Left variable
    var_left <- select_var_server(
      id = ns_id,
      var_list = reactive(vars_natural_infrastructure_left))

    slider <- slider_server(id = ns_id,
                            value = reactive(min(natural_infrastructure[[var_left()]],
                                                 na.rm = TRUE) |>
                                               round(digits = 2)),
                            min = reactive(min(natural_infrastructure[[var_left()]],
                                               na.rm = TRUE) |>
                                             round(digits = 2)),
                            max = reactive(max(natural_infrastructure[[var_left()]],
                                               na.rm = TRUE) |>
                                             round(digits = 2)))

    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = make_dropdown(compare = TRUE))

    # Sidebar
    sidebar_server(id = ns_id, x = "natural_infrastructure")
    
    # Composite variable for map
    map_var <- reactive(paste0(var_left(), "_q5"))
    
    # Data
    data <- eventReactive({var_left()
      focus()
      slider()}, {

      var_left <- var_left()
      left_q3 <- paste0(var_left, "_q3")
      left_q5 <- paste0(var_left, "_q5")

      out <-
        natural_infrastructure |>
        st_drop_geometry() |>
        select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")),
               population, var_left = all_of(var_left),
               var_left_q3 = all_of(left_q3),
               var_left_q5 = all_of(left_q5)) |>
        filter(!is.na(var_left)) |>
        (\(x) if (focus() && slider() <= max(x$var_left)) {
          x |>
            filter(var_left >= slider()) |>
            mutate(fill = colour_left_5$fill[[6]])
        } else {
          x |>
            mutate(group = coalesce(as.character(var_left_q5), "NA"),
                   .after = var_left_q5) |>
            left_join(colour_left_5, by = "group") |>
            relocate(fill, .after = group)
        })()

      right_join(select(natural_infrastructure, ID),
                 out, by = "ID")

    })

    # Disclaimers and how to read the map
    output$explainer <- renderText({

      "COLORS OF VALUES MUST BE CHANGED. A ~VIRIDIS COLOR PALETTE. LOW VALUES
      ARE GOOD, HIGH VALUES ARE BAD."

    })

    # Legend
    # legend <- legend_server(
    #   id = ns_id,
    #   data = data,
    #   var_left = var_left,
    #   var_right = var_right,
    #   df = df,
    #   zoom = zoom)

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
      select_id = select_id,
      lwd = scale_lwd_natural_infrastructure,
      lwd_args = reactive(list()))
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)

    # Explore panel
    # explore_content <- explore_server(
    #   id = ns_id,
    #   data = data,
    #   var_left = var_left,
    #   var_right = var_right,
    #   df = df,
    #   zoom = zoom,
    #   select_id = select_id)
    
    # De-select
    observeEvent(input[[paste0(ns_id, "-clear_selection")]], select_id(NA))
    observeEvent(df(), select_id(NA), ignoreInit = TRUE)
    # Error check
    observeEvent(data(), if (!select_id() %in% data()$ID) select_id(NA),
                 ignoreInit = TRUE)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
    )
    
    # Update select_id() on bookmark
    observeEvent(sus_bookmark$active, {
      if (isTRUE(sus_bookmark$active)) {
        # if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
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
      # if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
      delay(1000, {
        if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
      })
    }, priority = -2)

  })
}
