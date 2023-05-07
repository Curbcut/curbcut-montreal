### GREEN ALLEY MODULE #########################################################

# Dropdown
var_left_dropdown <-
  list("Borough summary" = " ",
       "Green alleys per square kilometer" = "alley_sqkm",
       "Green alleys per 1,000 residents" = "alley_per1k")

# Alley special functions
borough_alley_info_table <- function(data, select_id, lang, mode) {
  
  if (mode == "borough") {
    if (is.na(select_id)) {
      return(curbcut::cc_t(lang = lang, 
                           "{nrow(data)} out of 19 Montreal boroughs ",
                           "have a green alley program."))
    }
    
    row <- data[data$ID == select_id, ]

    # If not data
    if (nrow(row) == 0) {
      borough <- city_CSD$name[city_CSD$ID == select_id]
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
    
    return(curbcut::cc_t(lang = lang, "Our team visited {nrow(alley_visited)} of the ",
                         "{nrow(alleys)} green alleys in Montreal. We classified ",
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
  
  img_piece <- paste0("<img src = 'alleys/", data$photo_ID,
                      "' id = 'alley-alley_img' style = 'width:100%'>")
  span_piece <- sprintf("<p><span style = 'margin-bottom:20px; cursor:pointer;'>%s</span>", 
                        img_piece)
  
  # Return the title text and the image
  return(sprintf("%s%s", title_text, span_piece))
}

# Grab when on borough summary
borough_alley_graph <- function(lang, mode) {
  if (mode == "borough") {
  alleys$created <- as.numeric(alleys$created)
  alleys[!is.na(alleys$created),] |> 
    ggplot2::ggplot(ggplot2::aes(x = created)) +
    ggplot2::geom_bar(fill = colours_dfs$left_5$fill[5]) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::scale_x_continuous(name = curbcut::cc_t(lang = lang, "Green alley inaugurations")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(family = "SourceSansPro", size = 12),
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
      ggplot2::theme(text = ggplot2::element_text(family = "SourceSansPro", size = 12),
                     legend.position = "none", 
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(), 
                     panel.grid.minor.y = ggplot2::element_blank())
  }
}

# Fill when on observational data
scale_fill_alley <- function(...) {
  rdeck::scale_color_category(
    col = !!as.name("type"), 
    palette = c("#73AE80", "#FDE725FF", "#6C83B5", "#5B362A"),
    unmapped_color = "#B3B3BB", 
    levels = c("green", "community", "mixed", "unmaintained"),
    legend = FALSE)
}

# line width when on observational data
scale_lwd_alley <- function(select_id, ...) {
  rdeck::scale_category(
    col = !!as.name("ID"), 
    range = c(6, 3), 
    unmapped_value = 3, 
    levels = c(select_id, "NA"), 
    legend = FALSE)
}

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
    ggplot2::theme(text = ggplot2::element_text(family = "SourceSansPro", size = 12),
                   legend.position = "none", 
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = curbcut::cc_t(lang = lang, "Green alleys"), 
                  y = NULL)
}


# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  default_region <- modules$regions[modules$id == id][[1]][1]
  mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
  
  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      id = shiny::NS(id, id),
      curbcut::checkbox_UI(id = shiny::NS(id, id), value = TRUE, 
                           label = "Curbcut’s observational data"),
      curbcut::picker_UI(id = shiny::NS(id, id), 
                         var_list = var_left_dropdown),
      bottom = shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        shinyjs::hidden(shiny::tags$div(
          id = shiny::NS(id, "zoom_div"),
          curbcut::zoom_UI(shiny::NS(id, id), zoom_levels = mzp)))
      )
    ),
    
    # Map
    curbcut::map_UI(shiny::NS(id, id)),
    
    # Right panel
    curbcut::right_panel(
      id = id,
      shinyjs::hidden(shiny::tags$div(
        id = shiny::NS(id, "pre_compare_panel"),
        curbcut::compare_UI(
          id = shiny::NS(id, id),
          var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
        ))),
      curbcut::explore_UI(shiny::NS(id, id)),
      curbcut::dyk_UI(shiny::NS(id, id))
    )
  )
}


# server ------------------------------------------------------------------

alley_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    default_region <- modules$regions[modules$id == id][[1]][1]
    mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
    
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
    tile_1 <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels
    )
    tile <- shiny::reactive({
      if (mode() == "borough") return("city_CSD")
      if (mode() == "alleys") return("alleys")
      return(tile_1())
    })
    
    # Hide the zoom server when on var_left
    shiny::observeEvent(mode(), {
      shinyjs::toggle(id = "zoom_div", condition = mode() == "choropleth")
    })
    
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
    
    # Left variable
    var_left <- picker_server(
      id = id, 
      r = r,
      var_list = shiny::reactive(var_left_dropdown),
      time = shiny::reactive(2023))
    
    # Hide the dropdown when on observational data
    shiny::observeEvent(mode(), {
      shinyjs::toggle(id = shiny::NS(id, "ccpicker_var"), condition = mode() != "alleys")
    })
    
    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = shiny::reactive(curbcut::dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      time = shiny::reactive(2021)
    )
    
    # Hide the compare panel if there are no 
    shiny::observeEvent(mode(), {
      shinyjs::toggle(id = "pre_compare_panel", condition = mode() == "choropleth")
    })
    
    # Checkbox
    cbox <- curbcut::checkbox_server(
      id = id,
      r = r,
      label = shiny::reactive(curbcut::cc_t(lang = r$lang(),
                                            "Curbcut’s observational data"))
    )
    
    # WHat is the active view?
    mode <- shiny::reactive({
      # Revert the selection to NA
      r[[id]]$select_id(NA)
      
      if (cbox()) return("alleys")
      if (var_left() == " ") return("borough")
      return("choropleth")
    })
    
    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Data
    data <- shiny::reactive({
      if (mode() == "borough") return(alley_boroughs)
      if (mode() == "alleys") return(alleys)
      
      curbcut::data_get(
        vars = vars(),
        df = r[[id]]$df()
      )})
    
    # Data for tile coloring
    data_colours <- shiny::reactive({
      if (mode() != "choropleth") return(data.frame())

      curbcut::data_get_colours(
        vars = vars(),
        region = zoom_levels()$region,
        zoom_levels = zoom_levels()$zoom_levels
      )})
    
    vars <- shiny::reactive({
      if (mode() != "choropleth") return(list())
      curbcut::vars_build(var_left(), var_right(), df = r[[id]]$df())
    })
    
    # Customized legen function
    legend <- shiny::reactive({
      if (mode() == "alleys") {
        return(list(fun = alley_legend,
                    args = list(lang = r$lang())))
      }
      return(list(fun = curbcut::legend_render,
                  args = list(vars = vars(), lang = r$lang(), df = r[[id]]$df(),
                              data = data(), breaks = NULL,
                              scales_as_DA = c("building", "street"))))
    })
    
    # Legend
    curbcut::legend_server(
      id = id,
      r = r,
      vars = vars,
      data = data,
      df = r[[id]]$df,
      hide = shiny::reactive(mode() == "borough"), 
      legend_fun = shiny::reactive(legend()$fun),
      legend_args = shiny::reactive(legend()$args)
    )
    
    alley_legend
    
    # Did-you-know panel
    curbcut::dyk_server(
      id = id,
      r = r,
      vars = vars,
      poi = r[[id]]$poi,
      df = r[[id]]$df
    )
    
    # Customized map functions
    alley_fill_fun <- shiny::reactive({
      if (mode() == "borough") return(\(...) "#FFFFFF00")
      if (mode() == "alleys") return(scale_fill_alley)
      curbcut::map_scale_fill
    })
    alley_colour_fun <- shiny::reactive({
      if (mode() == "borough") return(\(...) colours_dfs$left_5$fill[5])
      if (mode() == "alleys") return(scale_fill_alley)
      curbcut::map_scale_colour
    })
    alley_lwd_fun <- shiny::reactive({
      if (mode() == "alleys") {
        return(list(fun = scale_lwd_alley,
                    args = list(select_id = r[[id]]$select_id())))
      }
      
      return(list(fun = curbcut::map_scale_lwd,
                  args = list(select_id = r[[id]]$select_id(), tile = tile(), 
                              zoom = r[[id]]$zoom(), 
                              zoom_levels = zoom_levels()$zoom_levels, lwd = 1)))
    })
    
    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_server(
      id = id,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom_levels = shiny::reactive(zoom_levels()$zoom_levels),
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords, 
      fill_fun = alley_fill_fun,
      colour_fun = alley_colour_fun,
      lwd_fun = shiny::reactive(alley_lwd_fun()$fun),
      lwd_args = shiny::reactive(alley_lwd_fun()$args),
      extrude = shiny::reactive(if (mode() == "choropleth") TRUE else FALSE)
    )
    
    # Update map labels
    curbcut::label_server(
      id = id,
      tile = tile,
      zoom = r[[id]]$zoom,
      zoom_levels = shiny::reactive(zoom_levels()$zoom_levels),
      region = shiny::reactive(zoom_levels()$region),
      show = shiny::reactive(var_left() != " ")
    )
    
    # Explore tables
    alley_table_fun <- shiny::reactive({
      if (mode() == "choropleth")
        return(list(fun = curbcut::explore_text,
                    args = list(r = r, data = data(), vars = vars(), 
                                select_id = r[[id]]$select_id(), region = zoom_levels()$region, 
                                scales_as_DA = c("building", "street"), df = r[[id]]$df(), 
                                lang = r$lang())))
      
      return(list(fun = borough_alley_info_table,
                  args = list(data = data(), select_id = r[[id]]$select_id(),
                              lang = r$lang(), mode = mode())))
    })
    
    alley_graph_fun <- shiny::reactive({
      if (mode() == "choropleth")
        return(list(fun = curbcut::explore_graph,
                    args = list(r = r, data = data(), vars = vars(), df = r[[id]]$df(),
                                select_id = r[[id]]$select_id(), region = zoom_levels()$region, 
                                scales_as_DA = c("building", "street"), lang = r$lang())))
      
      return(list(fun = borough_alley_graph,
                  args = list(lang = r$lang(), mode = mode())))
    })
    
    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = shiny::reactive(zoom_levels()$region),
      vars = vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id, 
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
    
    # When a click on the image
    shinyjs::onclick("alley_img", {
      shiny::showModal(shiny::modalDialog(
        title = alleys$name[alleys$ID == r[[id]]$select_id()],
        shiny::HTML(paste0('<img src="alleys/',
                           alleys$photo_ID[alleys$ID == r[[id]]$select_id()],
                           '" width = 100%>')),
        easyClose = TRUE,
        size = "m",
        footer = NULL
      ))})
    
    # Bookmarking
    curbcut::bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )
    
  })
}






























# 
# 
# 
# # Dropdown menu -----------------------------------------------------------
# 
# var_list_left_alley <-
#   list("Borough summary" = " ",
#        "Per sq km" = "green_alley_sqkm",
#        "Per 1,000 residents" = "green_alley_per1k")
# 
# 
# # UI ----------------------------------------------------------------------
# 
# alley_UI <- function(id) {
#   id_map <- paste0(id, "-map")
# 
#   tagList(
#     
#     # Sidebar
#     sidebar_UI(
#       NS(id, id),
#       shiny::div(class = "sus-sidebar-widgets",
#         checkbox_UI(id = NS(id, id),
#                     label = curbcut::cc_t("Green alleys visited by our team")),
#         curbcut::picker_UI(NS(id, id), var_list = var_list_left_alley,
#                       label = curbcut::cc_t("Grouping"))),
#       bottom = div(class = "bottom_sidebar",
#                    tagList(legend_UI(NS(id, id)),
#                            zoom_UI(NS(id, id), map_zoom_levels_CMA)))),
#     
#     # Map
#     div(class = "map_div", rdeckOutput(NS(id, id_map), height = "100%")),
# 
#     # Right panel
#     right_panel(
#       id = id,
#       compare_UI(NS(id, id), dropdown_make(compare = TRUE)),
#       explore_UI(NS(id, id)),
#       dyk_UI(NS(id, id)))
# 
#   )
# }
# 
# 
# # Server ------------------------------------------------------------------
# 
# alley_server <- function(id, r) {
#   moduleServer(id, function(input, output, session) {
#     id_map <- paste0(id, "-map")
# 
#     # Initial reactives
#     zoom_string <- reactiveVal(zoom_get_string(map_zoom, map_zoom_levels_CMA))
#     poi <- reactiveVal(NULL)
#     
#     # Sidebar
#     sidebar_server(id = id, r = r, x = "alley")
# 
#     # Enter in choropleth() depending on var_left select_id
#     choropleth <- reactive(!(var_left() == " " || visited()))
# 
#     # Map
#     output[[id_map]] <- renderRdeck({
#       rdeck(map_style = map_base_style, 
#             initial_view_state = view_state(center = map_loc, 
#                                             zoom = isolate(r[[id]]$zoom()))) |> 
#         add_mvt_layer(id = "alley-alley",
#                       data = mvt_url("sus-mcgill.alley-alley"),
#                       pickable = FALSE,
#                       auto_highlight = FALSE,
#                       get_fill_color = paste0(colour_table$value[1], "90"),
#                       get_line_color = paste0(colour_table$value[1], "90"),
#                       line_width_units = "pixels",
#                       get_line_width = 2)
#     })
# 
#     # Zoom and POI reactives
#     observe({
#       r[[id]]$zoom(curbcut::zoom_get(get_view_state(id_map)$zoom))
#       new_poi <- observe_map(get_view_state(id_map))
#       if ((is.null(new_poi) && !is.null(poi())) ||
#           (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
#         poi(new_poi)
#     }) |> bindEvent(get_view_state(id_map))
#     
#     # Map zoom levels change depending on "city"
#     map_zoom_levels <- eventReactive(r$region(), {
#       out <- zoom_get_levels(default = "city", 
#                              geo = "city",
#                              var_left = isolate(var_left()))
#     })
#     
#     # Zoom string reactive
#     observe({
#       new_zoom_string <- zoom_get_string(r[[id]]$zoom(), map_zoom_levels()$zoom_levels,
#                                          "city")
#       if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
#     }) |> bindEvent(r[[id]]$zoom())
#     
#     # Click reactive
#     observe({
#       selection <- get_clicked_object(id_map)$ID
#       if (!is.na(r[[id]]$select_id()) && 
#           selection == r[[id]]$select_id()) {
#         r[[id]]$select_id(NA)
#       } else r[[id]]$select_id(selection)
#     }) |> bindEvent(get_clicked_object(id_map))
#     
#     # Default location
#     observe({
#       if (is.null(r$default_select_id())) return(NULL)
#       
#       new_id <- data()$ID[data()$ID %in% 
#                             r$default_select_id()[[gsub("_.*", "", r[[id]]$df())]]]
#       if (length(new_id) == 0) return(NULL)
#       
#       r[[id]]$select_id(new_id)
#     }) |> bindEvent(r$default_select_id(), r[[id]]$df())
# 
#     # Choose tileset
#     tile_choropleth <- zoom_server(
#       id = id, 
#       r = r,
#       zoom_string = zoom_string, 
#       zoom_levels = map_zoom_levels)
#     
#     tile <- reactive({
#       if (choropleth()) {
#         tile_choropleth()
#       } else if (!visited()) {
#         "borough_empty"
#       } else "alley"
#     })
#     
#     # Get df for explore/legend/etc
#     observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
#       bindEvent(tile(), zoom_string())
#     
#     # Focus on visited alleys
#     visited <- checkbox_server(id = id)
# 
#     # Time
#     time <- reactive("2016")
# 
#     # Left variable
#     var_left_1 <- select_var_server(id = id, r = r,
#                                     var_list = reactive(var_list_left_alley))
#     
#     var_left <- reactive(if (visited()) "alley_qual" else var_left_1())
# 
#     # Compare panel
#     var_right <- compare_server(
#       id = id,
#       r = r,
#       var_list = dropdown_make(compare = TRUE),
#       show_panel = choropleth,
#       time = time)
# 
#     # Data
#     data <- reactive(get_data(
#       df = r[[id]]$df(),
#       geo = "city",
#       var_left = var_left(), 
#       var_right = var_right()))
#     
#     # Data for tile coloring
#     data_color <- reactive({
#       if (!choropleth()) return(NULL)
#       get_data_color(
#         map_zoom_levels = map_zoom_levels()$zoom_levels,
#         geo = "city",
#         var_left = var_left(),
#         var_right = var_right())
#     })
#     
#     # Composite variable for map
#     map_var <- reactive({
#       if (choropleth()) {
#         str_remove(paste(var_left(), var_right(), sep = "_"), "_ $")
#       } else if (!visited()) {
#         ""
#       } else "type"
#     })
#     
#     # Legend
#     legend_server(
#       id = id,
#       r = r,
#       var_left = var_left,
#       var_right = var_right,
#       hide = reactive(tile() == "borough_empty"))
#         
#     # Choose explore graph
#     alley_graph <- reactive({
#       if (curbcut::is_scale_df(c("alley", "borough_empty"), r[[id]]$df())) {
#         explore_graph_alley
#       } else explore_graph
#     })
#     
#     # Choose explore graph
#     alley_table <- reactive({
#       if (curbcut::is_scale_df(c("alley", "borough_empty"), r[[id]]$df())) {
#         info_table_alley
#       } else info_table
#     })
#     
#     # Explore panel
#     explore_content <- explore_server(
#       id = id,
#       r = r,
#       data = data,
#       geo = reactive(map_zoom_levels()$region),
#       var_left = var_left,
#       var_right = var_right,
#       graph = alley_graph,
#       table = alley_table)
#     
#     # Popup the alley image if it's clicked on
#     onclick(
#       "alley_img", 
#       {showModal(modalDialog(
#         title = alley[alley$ID == r[[id]]$select_id(),]$name,
#         HTML(paste0('<img src="alleys/',
#                     alley[alley$ID == r[[id]]$select_id(),]$photo_ID,
#                     '" width = 100%>')),
#         easyClose = TRUE,
#         size = "m",
#         footer = NULL
#       ))})
#     
#     # Update map in response to variable changes or zooming
#     rdeck_server(
#       id = id,
#       r = r,
#       map_id = "map",
#       data_color = data_color,
#       tile = reactive(if (tile() %in% c("borough_empty", "alley")) 
#         paste(id, tile(), sep = "-") else tile()),
#       fill = scale_fill_alley,
#       fill_args = reactive(list(map_var(), tile(), data_color())),
#       colour = scale_colour_alley,
#       colour_args = reactive(list(map_var(), tile())),
#       lwd = scale_lwd_alley,
#       lwd_args = reactive(list(r[[id]]$select_id(), tile())))
#     
#     # Did-you-know panel
#     dyk_server(
#       id = id,
#       r = r,
#       var_left = var_left,
#       var_right = var_right,
#       poi = poi)
#     
#     # Toggle zoom
#     observeEvent({
#       choropleth()
#       visited()}, {
#         toggle("alley-zoom_auto", condition = choropleth() && !visited())
#         toggle("alley-zoom_slider", condition = choropleth() && !visited())
#         # If focus is clicked, toggle dropdown menu
#         toggle("alley-var", condition = !visited())
#       })
# 
#     # Bookmarking
#     bookmark_server(
#       id = id,
#       r = r,
#       s_id = r[[id]]$select_id,
#       df = r[[id]]$df,
#       map_viewstate = reactive(get_view_state(id_map)),
#       var_left = var_left,
#       var_right = var_right,
#       more_args = reactive(c("c-cbox" = visited()))
#     )
#     
#   })
# }
