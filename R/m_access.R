### ACCESS MODULE ##############################################################

# UI ----------------------------------------------------------------------

access_UI <- function(id) {
  ns_id <- "access"

  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      select_var_UI(NS(id, ns_id), select_var_id = "d_2",
                    var_list = var_left_list_2_access,
                    label = sus_translate("Timing")),
      select_var_UI(NS(id, ns_id), select_var_id = "d_1",
                    var_list = var_left_list_1_access,
                    label = sus_translate("Destination type")),
      slider_UI(NS(id, ns_id), label = sus_translate("Time threshold"),
                  min = 10, max = 60, step = 1, value = 30),
      bottom = div(class = "bottom_sidebar",
          tagList(legend_UI(NS(id, ns_id)),
                  hidden(zoom_UI(NS(id, ns_id), map_zoom_levels))))),
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, paste0(ns_id, "-map")), 
                                           height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))
  ))
}


# Server ------------------------------------------------------------------

access_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "access"

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)

    # Sidebar
    sidebar_server(id = ns_id, x = "access")

    # Map
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(id = ns_id) |> 
        add_mvt_layer(id = paste0(ns_id, "_street_1")) |> 
        add_mvt_layer(id = paste0(ns_id, "_street_2")) |> 
        add_mvt_layer(id = paste0(ns_id, "_street_2")) |> 
        add_mvt_layer(id = paste0(ns_id, "_building")) |> 
        add_mvt_layer(id = paste0(ns_id, "_borough_labels"))
    })

    # Zoom and POI reactives
    observeEvent(get_view_state(paste0(ns_id, "-map")), {
      zoom(get_zoom(get_view_state(paste0(ns_id, "-map"))$zoom))
      poi(observe_map(get_view_state(paste0(ns_id, "-map"))))
    })
    
    # Click reactive
    observeEvent(get_clicked_object(paste0(ns_id, "-map")), {
      selection <- get_clicked_object(paste0(ns_id, "-map"))$ID
      if (!is.na(select_id()) && selection == select_id()) return(select_id(NA))
      
      select_id(selection)
    })

    # Zoom level for data is always CT
    df <- reactive("CT")

    # Time
    time <- reactive("2016")
    
    # Choose tileset
    tile_1 <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Additional tileset identifier
    tile2 <- reactive({
      if (!is.na(select_id()) && var_right() == " ") return("")
      ""
      })
    
    tile <- reactive({
      if (!is.na(select_id()) && var_right() == " ") return("empty_CT")
      "empty_CT"# tile_1()
    })

    # Enable or disable slider + type of destination
    observeEvent({select_id()
      var_right()}, {
        toggle("access-slider", condition = !is.na(select_id()) && var_right() == " ")
        toggle("access-d_1", condition = is.na(select_id()) || var_right() != " ")
      })

    # Slider widget
    slider <- slider_server(id = ns_id)

    # Left variable servers
    var_left_1 <- select_var_server(ns_id, select_var_id = "d_1",
                                    var_list = reactive(var_left_list_1_access))
    var_left_2 <- select_var_server(ns_id, select_var_id = "d_2",
                                    var_list = reactive(var_left_list_2_access))

    # Construct left variable string
    var_left <- reactive(paste0(var_left_1(), "_", var_left_2(), "_count"))

    # Compare panel
    var_right <- compare_server(
      id = ns_id,
      var_list = make_dropdown(compare = TRUE),
      df = df,
      time = time)
    
    # Composite variable for map
    map_var <- reactive({
      if (!is.na(select_id()) && var_right() == " ") return("ID")
      
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $")
      })

    # If there's a select_id, update the compare to " "
    observeEvent(select_id(), {
      updatePickerInput(
        session,
        inputId = "access-access-var",
        choices = sus_translate(make_dropdown(compare = TRUE)),
        selected = " ")
    }, priority = 1)

    # Data
    data <- reactive(get_data(df(), var_left(), var_right()))

    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)

    # Legend
    legend_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right)
    
    access_colors <- reactive({
      if (!is.na(select_id()) && var_right() == " ") {
      tt_thresh <- slider() * 60

      CTs_to_map <-
        tt_matrix |>
        filter(origin == select_id(), travel_time <= tt_thresh,
               timing == var_left_2()) |>
        mutate(group = as.character(4 - pmax(1, ceiling(travel_time / tt_thresh * 3)))) |>
        select(destination, group) |>
        left_join(colour_iso, by = "group")

      rbind(data() |>
              select(ID) |>
              inner_join(CTs_to_map, by = c("ID" = "destination")) |>
              select(-group),
            data() |>
              filter(ID == select_id()) |>
              select(ID) |>
              mutate(fill = "#00000033")) |> 
        rename(group = ID, value = fill)
      } else NULL
    })

    # Update map in response to variable changes
    # observeEvent({
    #   select_id()
    #   var_left()
    #   var_right()
    #   slider()
    # }, {
    #   if (!is.na(select_id()) && var_right() == " ") {
    #     
        # slider <- slider()
        # select_id <- select_id()
        # var_left_2 <- var_left_2()
        # data <- data()
        # 
        # tt_thresh <- slider * 60
        # 
        # CTs_to_map <-
        #   tt_matrix |>
        #   filter(origin == select_id, travel_time <= tt_thresh,
        #          timing == var_left_2) |>
        #   mutate(group = as.character(4 - pmax(1, ceiling(travel_time / tt_thresh * 3)))) |>
        #   select(destination, group) |>
        #   left_join(colour_iso, by = "group")
        # 
        # data_to_add <-
        #   data |>
        #   select(ID) |>
        #   inner_join(CTs_to_map, by = c("ID" = "destination"))
        # 
        # poly_to_add <-
        #   data |>
        #   filter(ID == select_id) |>
        #   mutate(fill = "#00000033")
    # 
    #     mapdeck_update(map_id = NS(id, "map")) |>
    #       clear_path() |>
    #       clear_polygon() |>
    #       add_polygon(
    #         data = CT, stroke_width = 10, stroke_colour = "#FFFFFF", id = "ID",
    #         fill_colour = "#EDF8E9CC", update_view = FALSE,
    #         layer_id = "poly_bg", auto_highlight = TRUE,
    #         highlight_colour = "#FFFFFF90") |>
    #       add_polygon(
    #         data = data_to_add, stroke_width = 10, stroke_colour = "#FFFFFF",
    #         fill_colour = "fill", update_view = FALSE, id = "ID",
    #         layer_id = "poly_iso", auto_highlight = TRUE,
    #         highlight_colour = "#FFFFFF90") |>
    #       add_polygon(
    #         data = poly_to_add, fill_colour = "fill", stroke_width = 20,
    #         stroke_colour = "#000000", update_view = FALSE, id = "ID",
    #         layer_id = "poly_highlight", auto_highlight = TRUE,
    #         highlight_colour = "#FFFFFF90") |>
    #       add_path(data = metro_lines, stroke_colour = "fill",
    #                stroke_width = 50, update_view = FALSE)
    # 
    #   } else if (is.na(select_id()) && var_right() == " ") {
    # Start with this here first:
        # mapdeck_update(map_id = NS(id, "map"))  |>
        #   clear_path() |>
        #   clear_polygon() |>
        #   clear_polygon(layer_id = "poly_bg") |>
        #   clear_polygon(layer_id = "poly_iso") |>
        #   clear_polygon(layer_id = "poly_highlight") |>
        #   add_sf(data =
        #            {data() |>
        #                rowwise() |>
        #                mutate(fill_val = list(which.max((
        #                  filter(colour_access, category == var_left_1()))$value >= var_left))) |>
        #                mutate(fill_val = if (length(fill_val) == 0) NA_integer_ else fill_val) |>
        #                ungroup() |>
        #                select(-fill) |>
        #                left_join(colour_absolute, by = "fill_val") |>
        #                mutate(fill = if_else(is.na(fill), "#B3B3BBCC", fill))},
        #          stroke_width = 10, stroke_colour = "#FFFFFF",
        #          fill_colour = "fill", update_view = FALSE, id = "ID",
        #          auto_highlight = TRUE, highlight_colour = "#FFFFFF90") |>
        #   add_path(data = metro_lines, stroke_colour = "fill",
        #            stroke_width = 50, update_view = FALSE)
    #   } else {
    #     map_change(
    #       id = ns_id,
    #       map_id = NS(id, "map"),
    #       data = data,
    #       df = df,
    #       zoom = zoom,
    #       click = select_id,
    #       polygons_to_clear = c("poly_bg", "poly_iso", "poly_highlight")
    #     )
    #     }
    #   })
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      zoom = zoom,
      select_id = select_id,
      access_colors = access_colors
    )
    
    # Clear click status if prompted
    observeEvent(input$`access-clear_selection`, select_id(NA))

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(paste0(ns_id, "-map"))),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      map_id = NS(id, "map"),
      more_args = reactive(c("s-slider" = slider()))
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
