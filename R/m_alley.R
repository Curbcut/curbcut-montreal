### GREEN ALLEY MODULE #########################################################

# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  ns_id <- "alley"

  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
      checkbox_UI(id = NS(id, ns_id),
                  label = sus_translate("Focus on green alleys visited by our team")),
      select_var_UI(NS(id, ns_id), var_list = var_list_left_alley,
                    label = sus_translate("Grouping"))
      ),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, paste0(ns_id, "-map")), 
                                           height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown(compare_default = TRUE)),
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))

  ))
}


# Server ------------------------------------------------------------------

alley_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "alley"

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)

    # Sidebar
    sidebar_server(id = ns_id, x = "alley")

    # Enter in choropleth() depending on var_left select_id
    choropleth <- reactive(!(var_left() == " " || focus_visited()))

    # Map
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(id = "alleys",
                      data = tile_json("maxbdb2.alley-individual"),
                      pickable = FALSE,
                      auto_highlight = FALSE,
                      get_fill_color = "#00000035",
                      get_line_color = "#00000035",
                      line_width_units = "pixels",
                      get_line_width = 2) |> 
        add_mvt_layer(id = ns_id)
    })

    # Zoom and POI reactives
    observeEvent(input[[paste0(ns_id, "-map_viewstate")]], {
      zoom(get_zoom(input[[paste0(ns_id, "-map_viewstate")]]$viewState$zoom))
      poi(observe_map(input[[paste0(ns_id, "-map_viewstate")]]$viewState))
    })

    # Click reactive
    observeEvent(get_clicked_object(paste0(ns_id, "-map")), {
      selection <- get_clicked_object(paste0(ns_id, "-map"))$ID
      if (!is.na(select_id()) && selection == select_id()) return(select_id(NA))
      
      select_id(selection)
    })

    # Choose tileset
    tile_choropleth <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Additional tileset identifier
    tile2 <- reactive("")
    
    tile <- reactive({
      if (choropleth()) return(tile_choropleth())
      if (!focus_visited()) return("empty_borough")
      return("individual")
    })
    
    # Get df for explore/legend/etc
    df <- reactive(get_df(tile(), zoom(), map_zoom_levels))

    # Focus on visited alleys
    focus_visited <- checkbox_server(id = ns_id)

    # Time
    time <- reactive("2016")

    # Left variable
    var_left <- select_var_server(id = ns_id,
                                  var_list = reactive(var_list_left_alley))

    # Compare panel
    var_right <- compare_server(
      id = ns_id,
      var_list = make_dropdown(compare_default = TRUE),
      df = df,
      show_panel = choropleth,
      time = time)

    # Data
    data <- reactive({
      if (choropleth()) 
        return(get_data(df(), var_left(), var_right(), island = TRUE))

      return(NULL)
    })
    
    # Composite variable for map
    map_var <- reactive({
      if (choropleth()) 
        return(str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))
      if (!focus_visited()) return("")
      return("type")
    })

    
    # We need to be able to deactivate the legend. Not only hide it, as its 
    # presence crashes the module initially
    # Legend
    # legend_server(
    #   id = ns_id,
    #   var_left = var_left,
    #   var_right = var_right,
    #   df = df)

    # Extract alley name and photo_ID for the following click event
    alley_info <- reactive({
      if (select_id() %in% alleys[alleys$visited,]$ID) {
        x <-
          alleys |>
          st_drop_geometry() |>
          filter(ID == select_id()) |>
          mutate(name = sus_translate(
            "<p><b>{str_to_title(name)} in ",
            "{name_2}</b></p>")) |>
          select(-ID, -CSDUID, -visited, -name_2, -fill) |>
          select_if(~sum(!is.na(.)) > 0) %>%
          {if (nrow(.) > 0) as.list(.) else NULL}

        out <- alley_alleys_text(x)

        list(name = out$name,
             photo_ID = out$photo_ID)
      }
    })

    # Popup the image if it's clicked on
    # onclick(
    #   "alley-explore-alley_img",
    #   { showModal(modalDialog(
    #     title = HTML(alley_info()$name),
    #     HTML(paste0('<img src="', alley_info()$photo_ID, '", width = 100%>')),
    #     easyClose = TRUE,
    #     size = "l",
    #     footer = NULL
    #   ))})

    # Explore panel
    # explore_content <- explore_server(
    #   id = ns_id,
    #   x = data,
    #   var_left = var_left,
    #   var_right = var_right,
    #   select_id = select_id,
    #   df = df,
    #   standard = choropleth,
    #   custom_info = alley_info_table)
    
    rdeck_server(
      id = ns_id,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      zoom = zoom,
      select_id = select_id
    )

    # If we aren't in choropleth, toggle off the legend/zoom
    observeEvent({choropleth()
      focus_visited()}, {
        toggle("alley-zoom_auto",
                        condition = choropleth() && !focus_visited())
        toggle("alley-zoom_slider",
                        condition = choropleth() && !focus_visited())
        # If focus is clicked, toggle off the dropdown menu
        toggle("alley-var", condition = !focus_visited())
      })

    # Hook up "Clear select_id" button and other variables that clears it
    observeEvent(input$`alley-clear_select_id`, select_id(NA))
    observeEvent(choropleth(), select_id(NA))
    observeEvent(focus_visited(), select_id(NA))
    observeEvent(df(), if (choropleth()) select_id(NA))

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(
        input[[paste0(ns_id, "-map_viewstate")]]$viewState),
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
      more_args = reactive(c("c-cbox" = focus_visited()))
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
