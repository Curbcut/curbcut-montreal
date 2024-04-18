### climate_risk PAGE ###########################################################

map_scale_fill_grid <- function(vars, time) {
  var <- vars$var_left
  
  clr <- if (length(time$var_left) == 1) colours_dfs$left_5 else colours_dfs$delta
  
  if (length(time$var_left) == 2) {
    var <- sprintf("%s_delta", var)
  } else {
    var <- sprintf("%s_%s", var, time$var_left)
  }
  
  cc.map::map_choropleth_fill_fun(df = clr[c("group", "fill")],
                                  get_col = var,
                                  fallback = "transparent")
}


# UI ----------------------------------------------------------------------

`climate_risk_UI` <- function(id) {
  page <- modules[modules$id == id, ]
  regions <- page$regions[[1]]
  if (is.null(regions)) {
    stop(sprintf(paste0("Page `%s` does not have available regions. Please ",
                        "check the `regions` column in the `modules` ",
                        "dataframe."), id))
  }
  avail_scale_combinations <- page$avail_scale_combinations[[1]]
  mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))
  stories <- get_from_globalenv("stories")
  
  # Grab the possible regions for the module
  possible_regions <- page$regions[[1]][1]
  
  shiny::tagList(
    
    # Add a piece of CSS to ensure we do not see the small grid ticks on
    # the time sliders
    shiny::tags$head(shiny::tags$style(
      sprintf(
      "#climate_risk-climate_risk-year_sliders .irs-grid-pol.small {
    display: none;
    }"))),
    
    shiny::div(
      `data-theme` = theme_lowercased,
      # Sidebar
      curbcut::sidebar_UI(
        id = shiny::NS(id, id),
        curbcut::autovars_UI(NS(id, id)),
        curbcut::warnuser_UI(shiny::NS(id, id)),
        curbcut::compare_UI(
          id = NS(id, id),
          var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
        ),
        geography_UI(shiny::NS(id, id), regions = regions,
                     avail_scale_combinations = avail_scale_combinations),
        shiny::hr(),
        curbcut::zoom_UI(shiny::NS(id, id), mzp),
        bottom = shiny::tagList(
          curbcut::legend_UI(shiny::NS(id, id)),
        )
      ),
      
      # Map
      curbcut::map_js_UI(shiny::NS(id, id)),
      
      # Tutorial
      curbcut::tutorial_UI(id = shiny::NS(id, id)),
      
      # Change view (Map/Data/Place explorer)
      curbcut::panel_view_UI(id = NS(id, id)),
      
      # Right panel
      curbcut::right_panel(
        id = id,
        curbcut::explore_UI(NS(id, id)),
        curbcut::dyk_UI(NS(id, id))
      )
    )
  )
}


# Server ------------------------------------------------------------------

`climate_risk_server` <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    page <- modules[modules$id == id, ]
    regions <- page$regions[[1]]
    if (is.null(regions)) {
      stop(sprintf(paste0("Page `%s` does not have available regions. Please ",
                          "check the `regions` column in the `modules` ",
                          "dataframe.", id)))
    }
    avail_scale_combinations <- page$avail_scale_combinations[[1]]
    mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
    
    main_dropdown_title <- page$main_dropdown_title
    default_year <- page$dates[[1]]
    default_year <- if (is.null(default_year)) NULL else max(default_year)
    vars_right <- page$var_right[[1]]
    stories <- get_from_globalenv("stories")
    
    map_zoom <- get_from_globalenv("map_zoom")
    map_loc <- get_from_globalenv("map_loc")
    inst_prefix <- get_from_globalenv("inst_prefix")
    map_token <- get_from_globalenv("map_token")
    map_base_style <- get_from_globalenv("map_base_style")
    mapbox_username <- get_from_globalenv("mapbox_username")
    
    # Highest grid
    highest_grd <- names(mzp)[1]
    
    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[1],
        latitude = map_loc[2],
        zoom = map_zoom,
        map_style_id = map_base_style,
        inst_prefix = inst_prefix,
        stories = stories,
        stories_min_zoom = 13
      )
    })
    
    # Initial zoom string reactive value
    rv_zoom_string <- shiny::reactiveVal(
      zoom_get_string(
        zoom = map_zoom,
        zoom_levels = mzp
      )
    )
    
    # Zoom and POI reactives when the view state of the map changes.
    shiny::observeEvent(map_viewstate(), {
      r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(update_poi(
        id = id, poi = r[[id]]$poi(),
        map_viewstate = map_viewstate()
      ))
    }, ignoreInit = TRUE)
    
    # If on grid, hide the slider and the auto-zoom
    shiny::observe({
      
      # Add one namespace as these are inside other module servers
      shinyjs::toggle(shiny::NS(id, "zoom_auto-cccheckbox_cbx"), condition = !grid())
      # Follow the rest of the slidertext addition ID for the zoom slider
      shinyjs::toggle(shiny::NS(id, "zoom_slider_div"), condition = !grid())
      
      # Get the zoom label
      zoom_name <- rv_zoom_string()
      names(zoom_name) <- zoom_name
      zoom_name <- curbcut::zoom_get_label(zoom_name, lang = r$lang())
      
      if (grid()) {
        shiny::removeUI(sprintf("#%s_grid_zn", id))
        shiny::insertUI(selector = sprintf("#%s-%s-zoom_cbx_loc", id, id),
                        where = "beforeEnd",
                        ui = shiny::div(id = sprintf("%s_grid_zn", id), zoom_name))
      } else {
        # Revert to default HTML
        shiny::removeUI(sprintf("#%s_grid_zn", id))
      }
    })
    
    # Region and zoom levels change depending on the geography widget
    zl <- geography_server(id = id,
                           r = r,
                           var_right = var_right,
                           regions = regions,
                           avail_scale_combinations = avail_scale_combinations)
    update_region(id = id, r = r, new_region = shiny::reactive(zl()$region))
    update_zoom_levels(id = id, r = r, new_zl = shiny::reactive(zl()$zoom_levels))
    
    # Zoom string reactive
    shiny::observe({
      rv_zoom_string({
        zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = r[[id]]$zoom_levels()
        )
      })
    })
    
    # Update selected ID
    update_select_id(id = id, r = r, data = data)
    
    
    # Default to tileset values
    grid <- shiny::reactive(names(r[[id]]$zoom_levels())[1] == highest_grd)
    grid_compare <- shiny::reactive(grid() && var_right()[1] != " ")
    
    # Choose tileset
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      region = r[[id]]$region,
      zoom_levels = r[[id]]$zoom_levels
    )
    
    # Get df
    observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        scale <- update_scale(
          tile = tile(),
          zoom_string = rv_zoom_string()
        )
        
        scale <- if (grepl("grd", scale)) highest_grd else scale
        r[[id]]$scale(scale)
      }
    )
    
    # Construct the left-hand UIs / servers automatically
    autovars <-
      curbcut::autovars_server(
        id = id,
        r = r, 
        main_dropdown_title = main_dropdown_title,
        default_year = 2022)

    update_rv(id, r, rv_name = "var_left", new_val = shiny::reactive(autovars()$var))
    widget_time <- shiny::reactive(autovars()$time)
    
    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_left = r[[id]]$var_left,
      var_list = shiny::reactive(curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      zoom_levels = r[[id]]$zoom_levels,
      time = r[[id]]$time
    )

    # Update the `r[[id]]$vars` reactive
    update_vars(
      id = id, r = r, var_left = r[[id]]$var_left,
      var_right = var_right, 
      scale = r[[id]]$scale,
      widget_time = widget_time
    )

    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Data
    data <- shiny::reactive(data_get(
      vars = r[[id]]$vars(),
      scale = r[[id]]$scale(),
      region = r[[id]]$region(),
      time = r[[id]]$time(),
      schemas = r[[id]]$schemas()
    ))
    
    # Data for tile coloring
    data_colours <- shiny::reactive({
      # If the color shown is extracted from the tileset, do not calculate
      if (grid() & !grid_compare()) return(data.frame())

      z <- data_get_colours(
        vars = r[[id]]$vars(),
        region = r[[id]]$region(),
        time = r[[id]]$time(),
        zoom_levels = r[[id]]$zoom_levels(),
        schemas = r[[id]]$schemas()
      )
      # On mapbox, grids don't hold ID_color
      if (grid_compare()) names(z)[1] <- "ID"
      z
    })
    
    # Warn user
    warnuser_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      time = r[[id]]$time,
      widget_time = widget_time,
      data = data
    )
    
    # Tutorial
    tutorial_server(
      id = id,
      r = r
    )
    
    # Legend
    legend_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      data = data,
      scale = r[[id]]$scale,
      time = r[[id]]$time
    )
    
    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      scale = r[[id]]$scale,
      region = r[[id]]$region,
      select_id = r[[id]]$select_id,
      time = r[[id]]$time,
      poi = r[[id]]$poi,
      zoom_levels = r[[id]]$zoom_levels
    )
    
    # Switch the fill function of the map server when on grid
    fill_fun_args <- shiny::reactive({
      if (grid() & !grid_compare()) {
        return(list(fun = map_scale_fill_grid,
                    args = list(vars = r[[id]]$vars(), time = r[[id]]$time())))
      } else {
        return(list(fun = cc.map::map_choropleth_fill_fun,
                    args = list(df = data_colours(),
                                get_col = names(data_colours())[1],
                                fallback = "transparent")))
      }
    })
    
    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_js_server(
      id = id,
      r = r,
      tile = tile,
      select_id = r[[id]]$select_id,
      coords = r[[id]]$coords,
      zoom = r[[id]]$zoom,
      data_colours = data_colours,
      fill_fun = shiny::reactive(fill_fun_args()$fun),
      fill_fun_args = shiny::reactive(fill_fun_args()$args),
      stories = stories,
      vars = r[[id]]$vars
    )
    
    val <- shiny::reactive({
      val_get_db(vars = r[[id]]$vars(), 
                 grid = grid(), 
                 grid_compare = grid_compare(), 
                 rv_zoom_string = rv_zoom_string(), 
                 highest_grd = highest_grd,
                 select_id = r[[id]]$select_id(), 
                 time = r[[id]]$time())
    })

    shown_scale <- shiny::reactive({
      # Return nothing if we're not in grid mode
      if (!grid()) return(NULL)
      if (grid_compare()) return(NULL)
      if (rv_zoom_string() == highest_grd) return(NULL)
      if (is.na(r[[id]]$select_id())) return(NULL)
      
      # In grid mode with a selection, make sure to adapt the text for the right
      # scale.
      rv_zoom_string()
    })
    
    # Update the selection when on grid() and the `df` changes (to redraw the graph)
    shiny::observeEvent(r[[id]]$scale(), {
      if (grid() & !is.na(r[[id]]$select_id())) r[[id]]$select_id(NA)
    })
    
    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = r[[id]]$region,
      vars = r[[id]]$vars,
      scale = r[[id]]$scale,
      select_id = r[[id]]$select_id,
      time = r[[id]]$time,
      zoom_levels = r[[id]]$zoom_levels,
      schemas = r[[id]]$schemas,
      val = val,
      shown_scale = shown_scale,
    )
    
    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )
    
    # Change view
    panel_view_server(
      id = id,
      r = r,
      region = r[[id]]$region,
      scale = r[[id]]$scale,
      vars = r[[id]]$vars,
      data = data,
      zoom_levels = r[[id]]$zoom_levels,
      time = r[[id]]$time,
      schemas = r[[id]]$schemas
    )
  })
}
