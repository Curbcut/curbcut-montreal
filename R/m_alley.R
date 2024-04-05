### GREEN ALLEY MODULE #########################################################

# Dropdown
var_left_dropdown <-
  list("Curbcut’s observational data" = "observation",
       "Borough summary" = "summary",
       "Green alleys per square kilometer" = "alley_sqkm",
       "Green alleys per 1,000 residents" = "alley_per1k")

# Alley special functions
borough_alley_info_table <- function(data, select_id, lang, mode) {
  
  if (mode == "borough") {
    if (is.na(select_id)) {
      participate <- nrow(data)
      return(curbcut::cc_t(lang = lang,
                           "{participate} out of 19 Montreal boroughs ",
                           "have a green alley program."))
    }
    
    row <- data[data$ID == select_id, ]
    
    # If not data
    if (nrow(row) == 0) {
      borough <- borough$name[borough$ID == select_id]
      return(curbcut::cc_t(lang = lang,
                           "{borough} does not have a green alley program."))
    }
    
    # Title
    title <- sprintf("<p><strong>%s</strong>", row$name)
    
    # Body
    body <- row[[paste0("text_", lang)]]
    
    # Return
    return(paste0(title, body))
  }
  
  # If individual alley mode
  if (is.na(select_id)) {
    
    alley_visited <- alleys[!is.na(alleys$type), ]
    green <- nrow(alley_visited[alley_visited$type == "green", ])
    community <- nrow(alley_visited[alley_visited$type == "community", ])
    mixed <- nrow(alley_visited[alley_visited$type == "mixed", ])
    unmaintained <- nrow(alley_visited[alley_visited$type == "unmaintained", ])
    
    green_per <- scales::percent(green/nrow(alley_visited))
    community_per <- scales::percent(community/nrow(alley_visited))
    mixed_per <- scales::percent(mixed/nrow(alley_visited))
    unmaintained_per <- scales::percent(unmaintained/nrow(alley_visited))
    
    visited <- nrow(alley_visited)
    all_nb <- nrow(alleys)
    return(curbcut::cc_t(lang = lang, "Our team visited {visited} of the ",
                         "{all_nb} green alleys in Montreal. We classified ",
                         "{green} ({green_per}) as 'green', {community} ",
                         "({community_per}) as 'community', {mixed} ",
                         "({mixed_per}) as 'mixed' green and community, and ",
                         "{unmaintained} ({unmaintained_per}) as 'unmaintained'."))
  }
  
  # If there is a selection
  data <- alleys[alleys$ID == select_id,]
  
  text_to_display <- data[[paste0("text_", lang)]]
  
  # Title
  title <- sprintf("<p><strong>%s</strong>", data$name)
  
  # If there is not photo, return the title and the text
  title_text <- sprintf("%s<p>%s", title, text_to_display)
  if (is.na(data$photo_ID)) return(title_text)
  
  img_piece <- tags$img(src = sprintf("alleys/%s", data$photo_ID),
                        id = "alley-alley_img",
                        style = "width:100%")
  span_piece <- sprintf("<p><span style = 'margin-bottom:20px; cursor:pointer;'>%s</span>",
                        img_piece)
  
  # Return the title text and the image
  return(sprintf("%s%s", title_text, span_piece))
}

# Grab when on borough summary
borough_alley_graph <- function(lang, mode, ...) {
  if (mode == "borough") {
    alleys <- alleys[!is.na(alleys$created), ]
    alleys$created <- s_extract("^\\d{4}", alleys$created)
    alleys$created <- as.numeric(alleys$created)
    alleys[!is.na(alleys$created),] |>
      ggplot2::ggplot(ggplot2::aes(x = created)) +
      ggplot2::geom_bar(fill = colours_dfs$left_5$fill[5]) +
      ggplot2::scale_y_continuous(name = NULL) +
      ggplot2::scale_x_continuous(name = curbcut::cc_t(lang = lang, "Green alley inaugurations")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                     legend.position = "none",
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank())
  } else {
    # Grab labels
    labels <- sapply(c("Green", "Community", "Mixed", "Unmaintained"),
                     curbcut::cc_t, lang = lang, USE.NAMES = FALSE)
    
    # Format correctly
    df <- alleys
    df <- df[!is.na(df$type),]
    new_order <- c("green", "community", "mixed", "unmaintained")
    df$type <- factor(df$type, levels = new_order)
    
    # Draw plot
    df |>
      ggplot2::ggplot(ggplot2::aes(as.factor(type))) +
      ggplot2::geom_bar(fill = c("#73AE80", "#FDE725FF", "#6C83B5", "#5B362A")) +
      ggplot2::scale_y_continuous(name = NULL) +
      ggplot2::scale_x_discrete(labels = labels,
                                name = curbcut::cc_t(lang = lang, "Visited green alleys type")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                     legend.position = "none",
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank())
  }
}

# Fill when on observational data
scale_fill_alley <- function(...) {
  cc.map::map_choropleth_fill_fun(
    df = tibble::tibble(group = c("green", "community", "mixed", "unmaintained"),
                        fill = c("#73AE80", "#FDE725", "#6C83B5", "#5B362A")),
    get_col = "type",
    fallback = "#B3B3BB")
}

# # line width when on observational data
# scale_lwd_alley <- function(select_id, ...) {
#   cc.map::map_choropleth_fill_fun(
#     df = tibble::tibble(group = c(select_id, "NA"),
#                         fill = c(6, 3)),
#     get_col = "ID",
#     fallback = "#B3B3BB")
# }

alley_legend <- function(lang, ...) {
  
  # Grab labels
  breaks <- sapply(c("Green", "Community", "Mixed", "Unmaintained"),
                   curbcut::cc_t, lang = lang, USE.NAMES = FALSE)
  
  # Construct the dataframe
  df <- tibble::tibble(group = 1:4, y = 1,
                       fill = c("#73AE80", "#FDE725FF", "#6C83B5", "#5B362A"))
  
  # Make the plot
  df |>
    ggplot2::ggplot(ggplot2::aes(
      xmin = group - 1, xmax = group, ymin = y - 1,
      ymax = y, fill = fill
    )) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = df$group - 0.5,
      labels = breaks
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      df$fill, df$fill
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                   legend.position = "none",
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = curbcut::cc_t(lang = lang, "Green alleys"),
                  y = NULL)
}


# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
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
  
  shiny::tagList(
    shiny::div(
      `data-theme` = theme_lowercased,
      # Sidebar
      curbcut::sidebar_UI(
        id = shiny::NS(id, id),
        curbcut::label_indicators(id = shiny::NS(id, "indicators_label")),
        curbcut::picker_UI(id = shiny::NS(id, id),
                           var_list = var_left_dropdown),
        shinyjs::hidden(shiny::tags$div(
          id = shiny::NS(id, "pre_compare_panel"),
          curbcut::compare_UI(
            id = shiny::NS(id, id),
            var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
          ))),
        shinyjs::hidden(shiny::div(
          id = shiny::NS(id, "scale_div"),
          shinyjs::hidden(geography_UI(shiny::NS(id, id), regions = regions,
                       avail_scale_combinations = avail_scale_combinations)),
          shiny::hr(),
          curbcut::zoom_UI(shiny::NS(id, id), zoom_levels = mzp))),
        bottom = shiny::tagList(
          curbcut::legend_UI(shiny::NS(id, id)),
        )
      ),
      
      # Map
      curbcut::map_js_UI(shiny::NS(id, id)),
      
      # Popup for photos
      popup_UI(id = shiny::NS(id, id)),
      
      # Right panel
      curbcut::right_panel(
        id = id,
        curbcut::explore_UI(shiny::NS(id, id)),
        curbcut::dyk_UI(shiny::NS(id, id))
      )
    )
  )
}


# server ------------------------------------------------------------------

alley_server <- function(id, r) {
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
    
    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_left = var_left,
      var_list = shiny::reactive(curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      zoom_levels = r[[id]]$zoom_levels,
      time = shiny::reactive(2021)
    )
    
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
    curbcut::update_select_id(id = id, r = r, data = data)
    
    # Choose tileset
    tile_1 <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      region = r[[id]]$region,
      zoom_levels = r[[id]]$zoom_levels
    )
    tile <- shiny::reactive({
      if (mode() == "borough") return("borough")
      if (mode() == "alleys") return("alleys")
      return(tile_1())
    })
    
    # Hide the zoom server when on var_left
    shiny::observeEvent(mode(), {
      shinyjs::toggle(id = "scale_div", condition = mode() == "choropleth")
    })
    
    # Get scale
    shiny::observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        r[[id]]$scale(update_scale(
          tile = tile(),
          zoom_string = rv_zoom_string()
        ))
      }
    )
    
    # Left variable
    var_left <- picker_server(
      id = id,
      r = r,
      var_list = shiny::reactive(var_left_dropdown),
      time = shiny::reactive(2023))
    
    # Hide the compare panel if there are no
    shiny::observeEvent(mode(), {
      shinyjs::toggle(id = "pre_compare_panel", condition = mode() == "choropleth")
    })
    
    # WHat is the active view?
    mode <- shiny::reactive({
      # Revert the selection to NA
      r[[id]]$select_id(NA)
      
      if (var_left() == "observation") return("alleys")
      if (var_left() == "summary") return("borough")
      return("choropleth")
    })
    
    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Data
    data <- shiny::reactive({
      if (mode() == "borough") return(alley_boroughs)
      if (mode() == "alleys") return(alleys)
      if (r[[id]]$scale() == "alleys") return(alleys)

      data_get(
        vars = r[[id]]$vars(),
        scale = r[[id]]$scale(),
        region = r[[id]]$region(),
        time = r[[id]]$time(),
        schemas = r[[id]]$schemas()
      )
    })
    
    # Data for tile coloring
    data_colours <- shiny::reactive({
      if (mode() != "choropleth") return(data.frame())

      data_get_colours(
        vars = r[[id]]$vars(),
        region = r[[id]]$region(),
        time = r[[id]]$time(),
        zoom_levels = r[[id]]$zoom_levels(),
        schemas = r[[id]]$schemas()
      )
    })
    
    shiny::observe({
      if (mode() != "choropleth") return(list())
      
      vr <- vars_build(var_left = var_left(), var_right = var_right(), 
                       scale = r[[id]]$scale(), time = 2023)
      update_rv(id, r, rv_name = "vars", new_val = shiny::reactive(vr$vars))
      update_rv(id, r, rv_name = "time", new_val = shiny::reactive(vr$time))
    })
    
    # Customized legen function
    legend <- shiny::reactive({
      if (mode() == "alleys") {
        return(list(fun = alley_legend,
                    args = list(lang = r$lang()), vars = r[[id]]$vars()))
      }
      return(list(fun = curbcut::legend_render,
                  args = list(vars = r[[id]]$vars(), lang = r$lang(), 
                              scale = r[[id]]$scale(), data = data(),
                              scales_as_DA = c("building", "street"))))
    })
    
    # Legend
    curbcut::legend_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      data = data,
      scale = r[[id]]$scale,
      time = r[[id]]$time,
      hide = shiny::reactive(mode() == "borough"),
      legend_fun = shiny::reactive(legend()$fun),
      legend_args = shiny::reactive(legend()$args)
    )
    
    # # Did-you-know panel
    # dyk_server(
    #   id = id,
    #   r = r,
    #   vars = r[[id]]$vars,
    #   scale = r[[id]]$scale,
    #   region = r[[id]]$region,
    #   select_id = r[[id]]$select_id,
    #   time = r[[id]]$time,
    #   poi = r[[id]]$poi,
    #   zoom_levels = r[[id]]$zoom_levels
    # )
    
    # Customized map functions
    alley_fill_fun <- shiny::reactive({
      if (mode() == "borough") return(\(...) hex8_to_rgba("#AAB6CF90"))
      if (mode() == "alleys") return(scale_fill_alley)
      cc.map::map_choropleth_fill_fun
    })
    # alley_lwd_fun <- shiny::reactive({
    #   if (mode() == "alleys") {
    #     return(list(fun = scale_lwd_alley,
    #                 args = list(select_id = r[[id]]$select_id())))
    #   }
    #
    #   return(list(fun = curbcut::map_scale_lwd,
    #               args = list(select_id = r[[id]]$select_id(), tile = tile(),
    #                           zoom = r[[id]]$zoom(),
    #                           zoom_levels = current_zl(), lwd = 1)))
    # })
    
    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_js_server(
      id = id,
      r = r,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      fill_fun = alley_fill_fun,
      outline_width = shiny::reactive(if (mode() == "alleys") 2 else 1),
      outline_color = shiny::reactive(if (mode() == "alleys") scale_fill_alley() else "transparent"),
      stories = stories,
      vars = r[[id]]$vars
    )

    # Explore tables
    alley_table_fun <- shiny::reactive({
      if (mode() == "choropleth" & !"alleys" %in% class(r[[id]]$vars())) {
        return(list(fun = curbcut::explore_text,
                    args = list(r = r, data = data(), vars = r[[id]]$vars(),
                                select_id = r[[id]]$select_id(), region = r[[id]]$region(),
                                scales_as_DA = c("building", "street"), scale = r[[id]]$scale(),
                                lang = r$lang(), time = r[[id]]$time(),
                                zoom_levels = r[[id]]$zoom_levels(),
                                schemas = r[[id]]$schemas())))
      }


      return(list(fun = borough_alley_info_table,
                  args = list(data = data(), select_id = r[[id]]$select_id(),
                              lang = r$lang(), mode = mode())))
    })

    alley_graph_fun <- shiny::reactive({
      if (mode() == "choropleth" & !"alleys" %in% class(r[[id]]$vars())) {
        return(list(fun = curbcut::explore_graph,
                    args = list(r = r, data = data(), vars = r[[id]]$vars(), scale = r[[id]]$scale(),
                                select_id = r[[id]]$select_id(), region = r[[id]]$region(),
                                scales_as_DA = c("building", "street"), lang = r$lang(),
                                time = r[[id]]$time(), schemas = NULL)))
      }

      return(list(fun = borough_alley_graph,
                  args = list(lang = r$lang(), mode = mode())))
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
      table_fun = shiny::reactive(alley_table_fun()$fun),
      table_args = shiny::reactive(alley_table_fun()$args),
      graph_fun = shiny::reactive(alley_graph_fun()$fun),
      graph_args = shiny::reactive(alley_graph_fun()$args)
    )

    # Hide the graph when an alley is selected
    shiny::observe({

      # If on alleys and there is a selection, hide the pannel
      cond <- (\(x) {
        if (is.na(r[[id]]$select_id())) return(TRUE)
        if (mode() != "alleys") return(TRUE)
        return(FALSE)
      })()

      shinyjs::toggle(id = shiny::NS(id, "explore_graph"), condition = cond)
    })

    content <- shiny::reactive({
      if (mode() != "alleys") return(NULL)
      if (is.na(r[[id]]$select_id())) return(NULL)

      title <- alleys$name[alleys$ID == r[[id]]$select_id()]
      src <- sprintf("alleys/%s", alleys$photo_ID[alleys$ID == r[[id]]$select_id()])

      shiny::div(style = "height:100%; overflow:auto; padding: var(--padding-h-md)",
                 shiny::h2(title),
                 shiny::img(src = src, width = "100%"))
    })

    # On a photo click, trigger the show_popup and make sure it creates a reaction
    # to TRUE
    show_popup <- shiny::reactiveVal(FALSE)
    shiny::observe({
      shinyjs::onclick("alley_img", {
        shiny::isolate({
          show_popup(FALSE)
          show_popup(TRUE)
        })
      })
    })
    shiny::observeEvent(r[[id]]$select_id(), show_popup(FALSE))

    popup_server(id = id,
                 content = content,
                 show_popup = show_popup)


    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )
    
  })
}
