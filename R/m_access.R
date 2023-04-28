### ACCESS PAGE ################################################################

access_UI <- function(id) {
  default_region <- modules$regions[modules$id == id][[1]][1]
  mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
  
  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      id = shiny::NS(id, id),
      autovars_UI(shiny::NS(id, id)),
      curbcut::warnuser_UI(shiny::NS(id, id)),
      bottom = shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        curbcut::zoom_UI(shiny::NS(id, id), zoom_levels = mzp)
      )
    ),
    
    # Map
    curbcut::map_UI(shiny::NS(id, id)),
    
    # Change view (Map/Data/Place explorer)
    curbcut::panel_view_UI(id = shiny::NS(id, id)),
    
    # Right panel
    curbcut::right_panel(
      id = id,
      curbcut::compare_UI(
        id = shiny::NS(id, id),
        var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
      ),
      curbcut::explore_UI(shiny::NS(id, id)),
      curbcut::dyk_UI(shiny::NS(id, id))
    )
  )
}


access_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    default_region <- modules$regions[modules$id == id][[1]][1]
    mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
    main_dropdown_title <- modules$main_dropdown_title[modules$id == id]
    default_year <- modules$dates[modules$id == id][[1]]
    default_year <- if (is.null(default_year)) NULL else max(default_year)
    vars_right <- modules$var_right[modules$id == id][[1]]
    
    # Initial reactives
    rv_zoom_string <- shiny::reactiveVal(
      curbcut::zoom_get_string(
        zoom = map_zoom,
        zoom_levels = mzp,
        region = default_region
      )
    )
    
    # Zoom and POI reactives when the view state of the map changes.
    shiny::observeEvent(map_viewstate(), {
      r[[id]]$zoom(curbcut::zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(curbcut::update_poi(
        id = id, poi = r[[id]]$poi(),
        map_viewstate = map_viewstate()
      ))
    })
    
    # Map zoom levels change depending on r$region()
    zoom_levels <-
      shiny::reactive(curbcut::zoom_get_levels(id = id, region = r$region()))
    
    # Zoom string reactive
    shiny::observe({
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
    shiny::observeEvent(
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
      autovars_server(
        id = id,
        r = r,
        main_dropdown_title = main_dropdown_title,
        default_year = default_year)
    
    var_left <- shiny::reactive(autovars()$var)
    time <- shiny::reactive("")
    
    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      ),
      time = if (!is.null(default_year)) time else shiny::reactive(2021)
    )
    
    # Update the `r[[id]]$vars` reactive
    curbcut::update_vars(id = id, r = r, var_left = var_left,
                         var_right = var_right)
    
    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Data
    data <- shiny::reactive(curbcut::data_get(
      vars = r[[id]]$vars(),
      df = r[[id]]$df()
    ))
    
    # Data for tile coloring
    data_colours <- shiny::reactive(curbcut::data_get_colours(
      vars = r[[id]]$vars(),
      region = zoom_levels()$region,
      zoom_levels = zoom_levels()$zoom_levels
    ))
    
    # # Warn user
    # curbcut::warnuser_server(
    #   id = id,
    #   r = r,
    #   vars = r[[id]]$vars,
    #   time = time,
    #   data = data
    # )
    
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
    
    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_server(
      id = id,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom_levels = shiny::reactive(zoom_levels()$zoom_levels),
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords
    )
    
    # Update map labels
    curbcut::label_server(
      id = id,
      tile = tile,
      zoom = r[[id]]$zoom,
      zoom_levels = shiny::reactive(zoom_levels()$zoom_levels),
      region = shiny::reactive(zoom_levels()$region)
    )
    
    # # Explore panel
    # curbcut::explore_server(
    #   id = id,
    #   r = r,
    #   data = data,
    #   region = shiny::reactive(zoom_levels()$region),
    #   vars = r[[id]]$vars,
    #   df = r[[id]]$df,
    #   select_id = r[[id]]$select_id
    # )
    
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
      zoom_levels = shiny::reactive(zoom_levels()$zoom_levels)
    )
  })
}