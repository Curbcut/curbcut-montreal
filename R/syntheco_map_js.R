heatmap_filter_syn <- function(var) {
  return(list("all"))
}

heatmap_radius_syn <- function(var) {
  return(list(
    "interpolate",
    list("linear"),
    list("zoom"),
    3, 1,    # For zoom level 3
    4, 1.5,  # For zoom level 4
    5, 2,    # For zoom level 5
    6, 2.5,  # For zoom level 6
    7, 3,    # For zoom level 7
    8, 4,    # For zoom level 8
    9, 5,    # For zoom level 9
    10, 6,   # For zoom level 10
    11, 8,   # For zoom level 11
    12, 10,  # For zoom level 12
    13, 12,  # For zoom level 13
    14, 15   # For zoom level 14 and above
  ))
}

syntheco_map_js_server <- function(id, r, tile, coords, zoom,
                                   pickable = shiny::reactive(TRUE)) {
  stopifnot(shiny::is.reactive(tile))

  shiny::moduleServer(id, function(input, output, session) {
    inst_prefix <- get_from_globalenv("inst_prefix")
    
    # Populate the empty container created in map_js_UI
    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[1],
        latitude = map_loc[2],
        zoom = map_zoom,
        map_style_id = map_base_style,
        inst_prefix = inst_prefix
      )
    })
    
    # Form the tileset with stability. Do not get it to trigger the cc.map::map_choropleth
    # if it hasn't changed.
    tileset <- shiny::reactive(sprintf("%s_%s", inst_prefix, tile()))
    
    tileset_trigger <- shiny::reactiveVal(NULL)
    shiny::observeEvent(tileset(), {
      if (is.null(tileset_trigger())) {
        return(tileset_trigger(tileset()))
      }
      if (tileset() == tileset_trigger()) {
        return()
      }
      tileset_trigger(tileset())
    })
    
    # Update map coordinates if needed
    shiny::observeEvent(coords(), {
      cc.map::map_viewstate(
        session = session,
        map_ID = "map",
        longitude = as.numeric(unname(coords()[1])),
        latitude = as.numeric(unname(coords()[2])),
        zoom = zoom()
      )
    }, ignoreNULL = TRUE)
    
    # Whenever the tileset changes, load it
    shiny::observeEvent(tileset_trigger(),
                        {
                          colours <- colours_dfs$left_5$fill[1:5]
                          colours[1] <- sprintf("%s00", colours[1])
                          colours <- sapply(colours, hex_to_rgb_or_rgba, USE.NAMES = FALSE)
                          
                          cc.map::map_heatmap(
                            session = session,
                            map_ID = "map",
                            tileset = tileset_trigger(),
                            radius = heatmap_radius_syn(),
                            filter = heatmap_filter_syn(),
                            pickable = TRUE,
                            colours = colours, 
                            # min_zoom = 16, 
                            stroke_color = "transparent"
                          )
                        },
                        ignoreNULL = TRUE
    )
    
    # Grab the viewstate (lat, lon, zoom)
    viewstate <- curbcut::get_viewstate("map")
    
    # Return
    return(viewstate)
  })
}
