### climate_risk PAGE ###########################################################

map_scale_fill_grid <- function(vars) {
  var <- vars$var_left
  
  clr <- if (length(var) == 1) colours_dfs$left_5 else colours_dfs$delta
  
  if (length(var) == 2) {
    var <- curbcut::var_remove_time(var)
    var <- sprintf("%s_delta", var)
  }

  rdeck::scale_color_category(col = !!as.name(var), 
                              palette = clr$fill, 
                              unmapped_color = "#B3B3BB", 
                              levels = clr$group, 
                              legend = FALSE)
}

explore_graph_grid <- function(vars, lang, data, select_id) {
  plot <- curbcut::explore_graph(vars = vars,
                                 select_id = NA,
                                 df = "grid_grid250",
                                 data = data_get(vars, df = "grid_grid250"),
                                 lang)
  
  if (!is.na(select_id)) {
    if ("q5_ind" %in% class(vars)) {
      plot <-
        plot +
        ggplot2::geom_vline(
          xintercept = data$var_left[data$ID == select_id] + 1,
          colour = "black", linewidth = 1.5)
    }

    if ("delta_ind" %in% class(vars)) {
      plot <-
        plot +
        ggplot2::geom_tile(data = data[data$ID == select_id, ],
                           color = "white", fill = "transparent", size = 1.5)
    }
  }
  
  return(plot)
  
}

# GLOBAL ------------------------------------------------------------------

`climate_risk_default_region` <- unlist(modules$regions[modules$id == "climate_risk"])[1]
`climate_risk_mzp` <-
  eval(parse(text = paste0("map_zoom_levels_", `climate_risk_default_region`)))
default_region <- modules$regions[modules$id == "climate_risk"][[1]][1]
vars_right <- modules$var_right[modules$id == "climate_risk"][[1]]


# UI ----------------------------------------------------------------------

`climate_risk_UI` <- function(id) {
  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      id = shiny::NS(id, id),
      curbcut::autovars_UI(NS(id, id)),
      curbcut::checkbox_UI(id = NS(id, id), label = cc_t("View with grids"),
                           value = TRUE),
      curbcut::warnuser_UI(shiny::NS(id, id)),
      bottom = shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        shinyjs::hidden(
          curbcut::zoom_UI(shiny::NS(id, id), `climate_risk_mzp`)
        )
      )
    ),

    # Map
    curbcut::map_UI(shiny::NS(id, id)),
    
    # Tutorial
    curbcut::tutorial_UI(id = shiny::NS(id, id)),

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

`climate_risk_server` <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Initial reactives
    rv_zoom_string <- reactiveVal(
      curbcut::zoom_get_string(
        zoom = map_zoom,
        zoom_levels = `climate_risk_mzp`,
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
    
    # Hide the zoom slider when on 'grid()'
    shiny::observe({
      shinyjs::toggle(id = shiny::NS(id, "zoom_div"), condition = !grid())
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
    update_select_id(id = id, r = r, data = data)
    
    
    # Default to tileset values
    grid_compare <- shiny::reactive(grid() && var_right()[1] != " ")

    # Choose tileset
    tile_1 <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels
    )
    tile <- shiny::reactive(if (grid_compare()) "grid_grid250" else tile_1())
    
    # Get df
    observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        if (grid_compare()) return(r[[id]]$df("grid_grid250"))
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
      label = shiny::reactive("View with grids"))

    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = shiny::reactive(curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      time = time
    )

    # Update the `r[[id]]$vars` reactive
    curbcut::update_vars(
      id = id, 
      r = r, 
      var_left = var_left, 
      # Force an empty var_right when on `grid`
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
      # If the color shown is extracted from the tileset, do not calculate
      if (grid() & !grid_compare()) return(data.frame())
      zoom_levels <- if (grid_compare()) {
        stats::setNames("grid250", "grid250") 
      } else {
        zoom_levels()$zoom_levels
      }
      
      curbcut::data_get_colours(
        vars = r[[id]]$vars(),
        region = zoom_levels()$region,
        zoom_levels = zoom_levels
      )
    })

    # Warn user
    curbcut::warnuser_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      time = time,
      data = data
    )
    
    # Tutorial
    curbcut::tutorial_server(
      id = id,
      r = r,
      skip_elements = shiny::reactive(if (grid()) "zoom_div" else NULL)
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
      if (grid()) return(0)
      return(1)
    })
    
    # Switch the fill function of the map server when on grid
    fill_fun_args <- shiny::reactive({
      if (grid() & !grid_compare()) {
        return(list(fun = map_scale_fill_grid,
                    args = list(vars = r[[id]]$vars())))
      } else {
        return(list(fun = curbcut::map_scale_fill,
                    args = list(data_colours(), tileset_ID_color = "ID_color")))
      }
    })
    color_fun_args <- shiny::reactive({
      if (grid() & !grid_compare()) {
        return(list(fun = map_scale_fill_grid,
                    args = list(vars = r[[id]]$vars())))
      } else {
        return(list(fun = curbcut::map_scale_colour,
                    args = list(r[[id]]$select_id(), data_colours(), tileset_ID_color = "ID_color")))
      }
    })
    
    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_server(
      id = id,
      r = r,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      lwd_args = shiny::reactive(list(select_id = r[[id]]$select_id(),
                                      tile = tile(),
                                      zoom = r[[id]]$zoom(),
                                      zoom_levels = zoom_levels()$zoom_levels,
                                      lwd = lwd())),
      fill_fun = shiny::reactive(fill_fun_args()$fun),
      fill_args = shiny::reactive(fill_fun_args()$args),
      colour_fun = shiny::reactive(color_fun_args()$fun),
      colour_args = shiny::reactive(color_fun_args()$args)
    )

    # Update map labels
    curbcut::label_server(
      id = id,
      tile = tile,
      zoom = r[[id]]$zoom,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      region = reactive(zoom_levels()$region),
      show_buildings = shiny::reactive(!grid())
    )
    
    # Switch the graph to a static one when on grid q5
    explore_graph_fun_args <- shiny::reactive({
      if (grid() & !grid_compare()) {
        return(list(fun = explore_graph_grid,
                    args = list(vars = r[[id]]$vars(), 
                                lang = r$lang(), 
                                data = shiny::isolate(data()), 
                                select_id = r[[id]]$select_id())))
      } else {
        return(list(fun = curbcut::explore_graph,
                    args = list(r = r, 
                                data = data(), 
                                vars = r[[id]]$vars(), 
                                df = r[[id]]$df(),
                                select_id = r[[id]]$select_id(), 
                                region = zoom_levels()$region, 
                                lang = r$lang())))
      }
    })
    
    # Update the selection when on grid() and the `df` changes (to redraw the graph)
    shiny::observeEvent(r[[id]]$df(), {
      if (grid() & !is.na(r[[id]]$select_id())) r[[id]]$select_id(NA)
    })

    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = reactive(zoom_levels()$region),
      vars = r[[id]]$vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id, 
      graph = shiny::reactive(explore_graph_fun_args()$fun), 
      graph_args = shiny::reactive(explore_graph_fun_args()$args)
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
      region = reactive(zoom_levels()$region),
      vars = r[[id]]$vars,
      data = data,
      zoom_levels = reactive(zoom_levels()$zoom_levels)
    )
  })
}
