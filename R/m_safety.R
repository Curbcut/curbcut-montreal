### safety PAGE ###########################################################

safety_graph <- function(vars, year, lang) {
  var <- vars$var_left
  
  # Which mode
  df <- (\(x) {
    if (grepl("crash_ped", var)) return(crash$ped)
    if (grepl("crash_cyc", var)) return(crash$cyc)
    return(crash$all)
  })()

  # Grab the list for the year
  df <- df[[as.character(year[[1]])]]
  
  # Set up the x-axis labels for the first label of each quarter
  month_labels <- c('Jan', 'Mar', 'May', 'Jul', "Sep", "Nov")
  month_labels <- sapply(month_labels, \(x) mean(df$week[df$month == x]))
  if (lang == "fr") {
    names(month_labels) <- c('Janv.', 'Mars', 'Mai', 'Juill.', "Sept.", "Nov.")
  }
  
  y_label <- if (lang == "en") "Crash number" else "Nombre de collisions"
  
  df |> 
    ggplot2::ggplot(ggplot2::aes(x = week, y = count)) +
    ggplot2::geom_col(fill = colours_dfs$left_5$fill[5]) +
    ggplot2::scale_y_continuous(name = y_label) +
    ggplot2::scale_x_continuous(name = NULL,
                       breaks = month_labels,
                       labels = names(month_labels)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                   legend.position = "none", 
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(), 
                   panel.grid.minor.y = ggplot2::element_blank())  
  
}


safety_UI <- function(id) {
  default_region <- modules$regions[modules$id == id][[1]][1]
  mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
  page <- modules[modules$id == id, ]
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))

  shiny::tagList(
    # Sidebar
    shiny::div(
      `data-theme` = theme_lowercased,
      curbcut::sidebar_UI(
        id = shiny::NS(id, id),
        curbcut::autovars_UI(shiny::NS(id, id)),
        curbcut::warnuser_UI(shiny::NS(id, id)),
        curbcut::compare_UI(
          id = shiny::NS(id, id),
          var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
        ),
        shiny::hr(),
        curbcut::zoom_UI(shiny::NS(id, id), zoom_levels = mzp),
        curbcut::checkbox_UI(id = NS(id, id), label = cc_t("Heatmap"),
                             value = TRUE),
        bottom = shiny::tagList(
          curbcut::legend_UI(shiny::NS(id, id))
        )
      ),
      
      # Map
      curbcut::map_js_UI(shiny::NS(id, id)),
      
      # Tutorial
      curbcut::tutorial_UI(id = shiny::NS(id, id)),
      
      # Change view (Map/Data/Place explorer)
      curbcut::panel_view_UI(id = shiny::NS(id, id)),
      
      # Right panel
      curbcut::right_panel(
        id = id,
        curbcut::explore_UI(shiny::NS(id, id)),
        curbcut::dyk_UI(shiny::NS(id, id))
      )
    )
  )
}

# Create the basic server function
safety_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    map_zoom <- get_from_globalenv("map_zoom")
    default_region <- modules$regions[modules$id == id][[1]][1]
    mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
    main_dropdown_title <- modules$main_dropdown_title[modules$id == id]
    default_year <- modules$dates[modules$id == id][[1]]
    default_year <- if (is.null(default_year)) NULL else max(default_year)
    vars_right <- modules$var_right[modules$id == id][[1]]
    suffix_zoom_levels <- modules$suffix_zoom_levels[modules$id == id]
    
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
        tileset_prefix = tileset_prefix,
        stories = stories,
        stories_min_zoom = 13
      )
    })

    # Initial zoom string reactive value
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
    
    # If on heatmap, hide the slider and the auto-zoom
    shiny::observe({
      
      # Add one namespace as these are inside other module servers
      shinyjs::toggle(shiny::NS(id, "zoom_auto-cccheckbox_cbx"), condition = !heatmap())
      # Follow the rest of the slidertext addition ID for the zoom slider
      shinyjs::toggle(shiny::NS(id, "zoon_slider_div"), condition = !heatmap())
      
      if (heatmap()) {
        shiny::removeUI("#safety_heatmap_zn")
        shiny::insertUI(selector = "#safety-safety-zoom_cbx_loc",
                        where = "beforeEnd",
                        ui = shiny::div(id = "safety_heatmap_zn", cc_t("Crash locations", force_span = TRUE)))
      } else {
        # Revert to default HTML
        shiny::removeUI("#safety_heatmap_zn")
      }
    })
    
    # When the page first initiates, the compare dates checkbox is not yet 
    # shown. Instead of following `heamtp` at first, we follow when the checkbox
    # first inputs a value. We make sure the first toggle to hide acts when
    # it's first initiated.
    shiny::observeEvent(input[["safety-safety-cccheckbox_cbx"]], {
      shinyjs::toggle("safety-safety-cccheckbox_cbx", condition = !heatmap())
      shinyjs::toggle("safety-compare_panel", condition = !heatmap())
      shinyjs::toggle("safety-compare_widgets", condition = !heatmap())
    }, once = TRUE)

    # If on heatmap, there can be only single year
    shiny::observe({
      
      # Toggle to not show the compare dates checkbox when on heatmap
      shinyjs::toggle("safety-safety-cccheckbox_cbx", condition = !heatmap())
      shinyjs::toggle("safety-compare_panel", condition = !heatmap())
      shinyjs::toggle("safety-compare_widgets", condition = !heatmap())
      
      # If on heatmap, turn off the compare dates checkbox
      if (heatmap()) {
        shiny::updateCheckboxInput(
          session = session,
          inputId = "safety-safety-cccheckbox_cbx",
          value = FALSE
        )
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "safety-compare-ccpicker_var",
          selected = " "
        )
      }
    })
    
    # Map zoom levels change depending on r$region()
    zoom_levels <-
      shiny::reactive(curbcut::zoom_get_levels(
        id = id,
        region = r$region(),
        suffix_zoom_levels = suffix_zoom_levels
      ))
    current_region <- shiny::reactive(zoom_levels()$region)
    current_zl <- shiny::reactive(zoom_levels()$zoom_levels)
    
    # Zoom string reactive
    shiny::observe({
      rv_zoom_string({
        curbcut::zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = current_zl(),
          region = current_region()
        )
      })
    })
    
    # Update selected ID
    curbcut::update_select_id(id = id, r = r, data = data)
    
    # When `heatmap` changes, revert back the selection to NA
    shiny::observeEvent(heatmap(), r[[id]]$select_id(NA), ignoreInit = TRUE)
    
    # Choose tileset
    tile_choropleth <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels,
      suffix_zoom_levels = suffix_zoom_levels
    )
    
    tile <- shiny::reactive({
      # If it's not a heatmap, show the choropleth
      if (!heatmap()) return(tile_choropleth())
      
      # If there are multiple years, we can not show the heatmap.
      if (length(time()) > 1) return(sprintf("crash_%s", time()[[1]]))
      return(sprintf("crash_%s", time()))
    })
    
    # Get df
    shiny::observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        r[[id]]$df(curbcut::update_df(
          tile = tile_choropleth(),
          zoom_string = rv_zoom_string()
        ))
      }
    )
    
    # Construct the left-hand UIs / servers automatically
    autovars <-
      curbcut::autovars_server(
        id = id,
        r = r,
        main_dropdown_title = main_dropdown_title,
        default_year = default_year
      )
    
    var_left <- shiny::reactive(autovars()$var)
    time <- shiny::reactive(if (is.null(autovars()$time)) "" else autovars()$time)
    
    # Heatmap checkbox
    heatmap <- curbcut::checkbox_server(
      id = id,
      r = r,
      label = shiny::reactive("Heatmap"))
    
    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = shiny::reactive(curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      # If there are no time in the page, use the latest census for date of
      # comparisons
      time = if (time() != "") time else shiny::reactive(2021)
    )
    
    # Update the `r[[id]]$vars` reactive
    curbcut::update_vars(
      id = id, r = r, var_left = var_left,
      var_right = var_right
    )
    
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
      region = current_region(),
      zoom_levels = current_zl()
    ))
    
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
      r = r
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
    dyk_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id,
      poi = r[[id]]$poi,
      region = current_region,
      zoom_levels = current_zl
    )
    
    # Update map in response to variable changes or zooming
    map_viewstate <- safety_map_js_server(
      id = id,
      r = r,
      tile = tile,
      select_id = r[[id]]$select_id,
      coords = r[[id]]$coords,
      zoom = r[[id]]$zoom,
      data_colours = data_colours,
      heatmap = heatmap,
      vars = r[[id]]$vars,
      stories = stories
    )
    
    # Craft the graph function reactives
    safety_graph_fun <- shiny::reactive({
      if (!heatmap())
        return(list(fun = curbcut::explore_graph,
                    args = list(r = r, data = data(), vars = r[[id]]$vars(), df = r[[id]]$df(),
                                select_id = r[[id]]$select_id(), region = current_region(), 
                                scales_as_DA = c("building", "street"), lang = r$lang())))
      
      return(list(fun = safety_graph,
                  args = list(vars = r[[id]]$vars(), year = time(), lang = r$lang())))
    })
    
    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = current_region,
      vars = r[[id]]$vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id,
      graph_fun = shiny::reactive(safety_graph_fun()$fun),
      graph_args = shiny::reactive(safety_graph_fun()$args)
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
      region = current_region,
      vars = r[[id]]$vars,
      data = data,
      zoom_levels = current_zl
    )
  })
}
