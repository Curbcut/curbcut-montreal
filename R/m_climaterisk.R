### CLIMATERISK PAGE ###########################################################

# GLOBAL ------------------------------------------------------------------

`climaterisk_default_region` <- unlist(modules$regions[modules$id == "climaterisk"])[1]
`climaterisk_mzp` <-
  eval(parse(text = paste0("map_zoom_levels_", `climaterisk_default_region`)))
default_region <- modules$regions[modules$id == "climaterisk"][[1]][1]
vars_right <- modules$var_right[modules$id == "climaterisk"][[1]]

# UI ----------------------------------------------------------------------

`climaterisk_UI` <- function(id) {
  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      id = shiny::NS(id, id),
      curbcut::autovars_UI(NS(id, id)),
      curbcut::checkbox_UI(id = NS(id, id), label = cc_t("View with grids"),
                           value = FALSE),
      curbcut::warnuser_UI(shiny::NS(id, id)),
      bottom = shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        curbcut::zoom_UI(shiny::NS(id, id), `climaterisk_mzp`)
      )
    ),

    # Map
    curbcut::map_UI(NS(id, id)),

    # Change view (Map/Data/Place explorer)
    curbcut::panel_view_UI(id = NS(id, id)),

    # Right panel
    curbcut::right_panel(
      id = id,
      curbcut::compare_UI(
        id = NS(id, id),
        var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
      ),
      curbcut::explore_UI(NS(id, id)),
      curbcut::dyk_UI(NS(id, id))
    )
  )
}


# Server ------------------------------------------------------------------

`climaterisk_server` <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Initial reactives
    rv_zoom_string <- reactiveVal(
      curbcut::zoom_get_string(
        zoom = map_zoom,
        zoom_levels = `climaterisk_mzp`,
        region = default_region
      )
    )
    
    # Zoom and POI reactives when the view state of the map changes.
    observeEvent(map_viewstate(), {
      r[[id]]$zoom(curbcut::zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(curbcut::update_poi(
        id = id, poi = r[[id]]$poi(),
        map_viewstate = map_viewstate()
      ))
    })
    
    # Switch the region depending on inputs
    region <- shiny::reactive({
      if (grid()) return("grid")
      r$region()
    })
    
    # Map zoom levels change depending on r$region()
    zoom_levels <-
      shiny::reactive(curbcut::zoom_get_levels(id = id, region = region()))

    # Zoom string reactive
    observe({
      rv_zoom_string({
        curbcut::zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = zoom_levels()$zoom_levels,
          region = zoom_levels()$region
        )
      })
    })

    # Update selected ID
    curbcut::update_select_id(id = id, r = r, data = data)

    # Choose tileset
    tile <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels
    )

    # Get df
    observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        r[[id]]$df(curbcut::update_df(
          tile = tile(),
          zoom_string = rv_zoom_string()
        ))
      }
    )

    # Construct the left-hand UIs / servers automatically
    autovars <- 
      curbcut::autovars_server(
        id = id, 
        r = r, 
        main_dropdown_title = "Climate vulnerability indicator",
        default_year = 2022)
    
    var_left <- shiny::reactive(autovars()$var)
    time <- shiny::reactive(autovars()$time)

    # 250-m grid checkbox
    grid <- curbcut::checkbox_server(
      id = id,
      r = r,
      label = shiny::reactive(cc_t("Grids", lang = r$lang())))

    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      ),
      time = time
    )

    # Update the `r[[id]]$vars` reactive
    curbcut::update_vars(id = id, r = r, var_left = var_left, 
                         var_right = var_right)

    # Sidebar
    curbcut::sidebar_server(id = id, r = r)

    # Data
    data <- reactive(curbcut::data_get(
      vars = r[[id]]$vars(),
      df = r[[id]]$df()
    ))

    # Data for tile coloring
    data_colours <- shiny::reactive({
      print(zoom_levels()$zoom_levels)
      curbcut::data_get_colours(
      vars = r[[id]]$vars(),
      region = zoom_levels()$region,
      zoom_levels = zoom_levels()$zoom_levels
    )})

    # Warn user
    curbcut::warnuser_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      time = time,
      data = data
    )

    # Legend
    curbcut::legend_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      data = data,
      df = r[[id]]$df
    )

    # Did-you-know panel
    curbcut::dyk_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      poi = r[[id]]$poi,
      df = r[[id]]$df
    )

    # Control the `lwd` of the polygon borders. No borders on grid + high zoom.
    lwd <- shiny::reactive({
      if (zoom_levels()$region == "grid") return(0)
      return(1)
    })

    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_server(
      id = id,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      tileset_ID_color = if (grid()) "ID" else "ID_color",
      lwd_args = shiny::reactive(list(select_id = r[[id]]$select_id(),
                                      tile = tile(),
                                      zoom = r[[id]]$zoom(),
                                      zoom_levels = zoom_levels()$zoom_levels,
                                      lwd = lwd()))
    )

    # Update map labels
    curbcut::label_server(
      id = id,
      tile = tile,
      zoom = r[[id]]$zoom,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      region = reactive(zoom_levels()$region)
    )

    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = reactive(zoom_levels()$region),
      vars = r[[id]]$vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id
    )

    # Bookmarking
    curbcut::bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )

    # Change view
    curbcut::panel_view_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      data = data,
      zoom_levels = reactive(zoom_levels()$zoom_levels)
    )
  })
}
