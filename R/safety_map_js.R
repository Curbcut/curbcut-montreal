heatmap_filter <- function(var) {
  if (grepl("crash_ped", var)) {
    return(list("==", list("get", "ped"), TRUE))
  }
  if (grepl("crash_cyc", var)) {
    return(list("==", list("get", "cyc"), TRUE))
  }
  return(list("all"))
}

heatmap_radius <- function(var) {
  if (grepl("crash_ped", var)) {
    return(list(
      "interpolate",
      list("linear"),
      list("zoom"),
      0, 1, 10, 10, 12, 20, 15, 45
    ))
  }
  if (grepl("crash_cyc", var)) {
    return(list(
      "interpolate",
      list("linear"),
      list("zoom"),
      0, 1, 10, 10, 12, 20, 15, 45
    ))
  }
  return(list("interpolate", list("linear"), list("zoom"), 0, 1, 10, 3, 12, 10, 15, 30))
}

safety_map_js_server <- function(id, r, tile, coords, zoom, select_id, data_colours,
                                 heatmap, vars, stories = NULL, stories_min_zoom = 13,
                                 outline_width = shiny::reactive(1),
                                 outline_color = shiny::reactive("transparent"),
                                 pickable = shiny::reactive(TRUE),
                                 fill_fun = shiny::reactive(cc.map::map_choropleth_fill_fun),
                                 fill_fun_args = shiny::reactive(list(
                                   df = data_colours(),
                                   get_col = names(data_colours())[1],
                                   fallback = "#B3B3BB"
                                 ))) {
  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(data_colours))

  shiny::moduleServer(id, function(input, output, session) {
    tileset_prefix <- get_from_globalenv("tileset_prefix")

    # Form the tileset with stability. Do not get it to trigger the cc.map::map_choropleth
    # if it hasn't changed.
    tileset <- shiny::reactive(sprintf("%s_%s", tileset_prefix, tile()))
    
    # Populate the empty container created in map_js_UI
    output$map_ph <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[1],
        latitude = map_loc[2],
        zoom = map_zoom,
        map_style_id = map_base_style,
        tileset_prefix = tileset_prefix,
        stories = stories,
        stories_min_zoom = stories_min_zoom
      )
    })
    
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

    # Remove the previous map mode (heatmap vs choropleth)
    shiny::observeEvent(heatmap(), {
      if (heatmap()) {
        cc.map::map_choropleth_remove(
          session = session,
          map_ID = "map"
        )
      } else {
        cc.map::map_heatmap_remove(
          session = session,
          map_ID = "map"
        )
      }
    })

    # Whenever the tileset changes, load it
    shiny::observeEvent(tileset_trigger(),
      {
        # If it is not a point tileset, add a choropleth
        if (!grepl("mtl_crash_\\d{4}", tileset_trigger())) {
          cc.map::map_choropleth(
            session = session,
            map_ID = "map",
            tileset = tileset_trigger(),
            fill_colour = data_colours(),
            select_id = select_id()
          )

          # If it is a point tileset, make it a heatmap
        } else {
          var <- vars()$var_left[[1]]
          
          colours <- colours_dfs$left_5$fill[1:5]
          colours[1] <- sprintf("%s00", colours[1])
          colours <- sapply(colours, hex_to_rgb_or_rgba, USE.NAMES = FALSE)
          
          cc.map::map_heatmap(
            session = session,
            map_ID = "map",
            tileset = tileset_trigger(),
            radius = heatmap_radius(var),
            filter = heatmap_filter(var),
            pickable = FALSE,
            colours = colours
          )
        }
      },
      ignoreNULL = TRUE
    )

    # If on heatmap and vars change, update the filter and radius
    shiny::observeEvent(vars(), {
      if (!heatmap()) {
        return()
      }

      var <- vars()$var_left
      
      cc.map::map_heatmap_update_filter(
        session = session,
        map_ID = "map",
        filter = heatmap_filter(var)
      )
      cc.map::map_heatmap_update_radius(
        session = session,
        map_ID = "map",
        radius = heatmap_radius(var)
      )
      
    })

    # Only update the fill_colour when data_colours change
    shiny::observe({
      cc.map::map_choropleth_update_fill_colour(
        session = session,
        map_ID = "map",
        fill_colour = data_colours()
      )
    })

    # Change language for stories hover text
    shiny::observeEvent(r$lang(), {
      cc.map::map_update_lang(session, map_ID = "map", lang = r$lang())
    })

    # Grab the viewstate (lat, lon, zoom)
    viewstate <- curbcut::get_viewstate("map")

    # Return
    return(viewstate)
  })
}
