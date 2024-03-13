
# Create the `basic` function
age_UI <- function(id) {
  page <- modules[modules$id == id, ]
  regions <- page$regions[[1]]
  if (is.null(regions)) {
    stop(sprintf(paste0(
      "Page `%s` does not have available regions. Please ",
      "check the `regions` column in the `modules` ",
      "dataframe."
    ), id))
  }
  avail_scale_combinations <- page$avail_scale_combinations[[1]]
  mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))
  stories <- get_from_globalenv("stories")
  
  # Grab the possible regions for the module
  possible_regions <- page$regions[[1]][1]
  
  # Necessary for the age module
  age_poss <- c(0:85)[0:85 %% 5 == 0]
  age_poss <- c(age_poss, "85+")
  
  # Data representation list
  dat_rep <- list("Data representation" = list("Percentage" = "pct", "Number" = "count"))
  
  shiny::tagList(
    # Sidebar
    shiny::div(
      `data-theme` = theme_lowercased,
      sidebar_UI(
        id = shiny::NS(id, id),
        label_indicators_UI(
          shiny::NS(id, id), 
          main_UIs = list(
            slider_text_UI(shiny::NS(id, id), label = NULL,
                           choices = age_poss, selected = c(0, 15))
          ),
          adv_UIs = list(
            picker_UI(shiny::NS(id, id), label = cc_t("Data representation"),
                      var_list = dat_rep))
        ),
        time_slider_UI(shiny::NS(id, id),
                       min = 1996, max = 2021, step = 5,
                       double_value = c(2016, 2021)),
        warnuser_UI(shiny::NS(id, id)),
        compare_UI(
          id = shiny::NS(id, id),
          var_list = dropdown_make(vars = " ", compare = TRUE)
        ),
        geography_UI(shiny::NS(id, id),
                     regions = regions,
                     avail_scale_combinations = avail_scale_combinations
        ),
        shiny::hr(),
        zoom_UI(shiny::NS(id, id), zoom_levels = mzp),
        bottom = shiny::tagList(
          legend_UI(shiny::NS(id, id))
        )
      ),
      
      # Map
      map_js_UI(shiny::NS(id, id)),
      
      # Tutorial
      tutorial_UI(id = shiny::NS(id, id)),
      
      # Change view (Map/Data/Place explorer)
      panel_view_UI(id = shiny::NS(id, id)),
      
      # Right panel
      right_panel(
        id = shiny::NS(id, id),
        explore_UI(shiny::NS(id, id)),
        dyk_UI(shiny::NS(id, id))
      )
    )
  )
}

# Create the basic server function
age_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    page <- modules[modules$id == id, ]
    regions <- page$regions[[1]]
    if (is.null(regions)) {
      stop(sprintf(paste0(
        "Page `%s` does not have available regions. Please ",
        "check the `regions` column in the `modules` ",
        "dataframe.", id
      )))
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
    
    # Necessary for the age module
    contains_age <- grepl("age_", variables$var_code)
    contains_age_agg <- grepl("age_agg_", variables$var_code)
    matches <- contains_age & !contains_age_agg
    
    age_vars <- variables$var_code[matches]
    age_vars <- age_vars[!age_vars %in% c("age_0_14", "age_15_64", "age_65_plus")]
    
    # Function to generate all sequences
    generate_sequences <- function(data) {
      all_sequences <- list()
      sequence_id <- 1
      
      for (i in 1:length(data)) {
        for (j in i:length(data)) {
          all_sequences[[sequence_id]] <- data[i:j]
          sequence_id <- sequence_id + 1
        }
      }
      
      return(all_sequences)
    }
    
    # Generate all sequences (variable name and all the variables it holds)
    age_agg_vars <- generate_sequences(age_vars)
    names(age_agg_vars) <- sapply(age_agg_vars, \(x) {
      if (length(x) == 1 && x == "age_0_4") return("age_agg_0_4")
      if (length(x) == 1 && x == "age_85") return("age_agg_85plus")
      start <- stringr::str_extract(x[[1]], "(?<=age_).*(?=_)")
      
      last <- x[[length(x)]]
      end <- if (last == "age_85") "85plus" else stringr::str_extract(last, "(?<=age_\\d{1,2}_).*")
      
      sprintf("age_agg_%s_%s", start, end)
    })
    
    age_graph_fun <- function(var_left, region, year, lang) {
      
      var_left <- gsub("_pct|_count", "", var_left)
      
      scale <- "DA"
      ids <- regions_dictionary$scales[regions_dictionary$region == region][[1]][[scale]]
      
      all_ages <- lapply(age_vars, \(x) qs::qread(sprintf("data/%s/%s.qs", scale, x)))
      
      age_data <- sapply(all_ages, \(x) {
        out <- x[grepl(sprintf("_%s", year), names(x))]
        
        out <- cbind(get_from_globalenv(scale)[c("ID", "population")], out)
        out <- out[out$ID %in% ids, ]
        
        sum(out[[2]] * out[[3]], na.rm = TRUE)
      })
      age_data <- data.frame(
        age_vars = age_vars,
        age_group = factor(c("0-4", "5-9", "10-14", "15-19", "20-24",
                             "25-29", "30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "60-64", "65-69", "70-74",
                             "75-79", "80-84", "85+"),
                           levels = c("0-4", "5-9", "10-14", "15-19", "20-24",
                                      "25-29", "30-34", "35-39", "40-44", "45-49",
                                      "50-54", "55-59", "60-64", "65-69", "70-74",
                                      "75-79", "80-84", "85+")),
        count = age_data
      )
      
      age_data$selected <- ifelse(age_data$age_vars %in% age_agg_vars[[var_left]], 1, 0)
      
      theme_default <- list(
        ggplot2::theme_minimal(),
        ggplot2::theme(
          text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
          legend.position = "none",
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
      )
      
      # Create the age pyramid plot
      ggplot2::ggplot(age_data, ggplot2::aes(x = age_group, y = count, alpha = selected)) +
        ggplot2::geom_bar(stat = "identity", fill = colours_dfs$left_5$fill[5]) +
        ggplot2::coord_flip() +
        theme_default +
        ggplot2::ylab(cc_t("Population", lang = lang)) +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_discrete(breaks = function(limits) {
          limits[seq(1, length(limits), by = 3)]
        }) +
        ggplot2::scale_alpha(range = c(0.3,1)) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
        explore_graph_scale(var = NULL, x_y = "y", data_vals = age_data$age_vars)
    }
    
    age_graph_selection_fun <- function(var_left, scale, select_id, year, lang) {
      
      var_left <- gsub("_pct|_count", "", var_left)
      
      all_ages <- lapply(age_vars, \(x) qs::qread(sprintf("data/%s/%s.qs", scale, x)))
      
      age_data <- sapply(all_ages, \(x) {
        out <- x[grepl(sprintf("_%s", year), names(x))]
        
        out <- cbind(get_from_globalenv(scale)[c("ID", "population")], out)
        out <- out[out$ID == select_id, ]
        
        sum(out[[2]] * out[[3]], na.rm = TRUE)
      })
      age_data <- data.frame(
        age_vars = age_vars,
        age_group = factor(c("0-4", "5-9", "10-14", "15-19", "20-24",
                             "25-29", "30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "60-64", "65-69", "70-74",
                             "75-79", "80-84", "85+"),
                           levels = c("0-4", "5-9", "10-14", "15-19", "20-24",
                                      "25-29", "30-34", "35-39", "40-44", "45-49",
                                      "50-54", "55-59", "60-64", "65-69", "70-74",
                                      "75-79", "80-84", "85+")),
        count = age_data
      )
      
      age_data$selected <- ifelse(age_data$age_vars %in% age_agg_vars[[var_left]], 1, 0)
      
      theme_default <- list(
        ggplot2::theme_minimal(),
        ggplot2::theme(
          text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
          legend.position = "none",
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
      )
      
      # Create the age pyramid plot
      ggplot2::ggplot(age_data, ggplot2::aes(x = age_group, y = count, alpha = selected)) +
        ggplot2::geom_bar(stat = "identity", fill = colours_dfs$left_5$fill[5]) +
        ggplot2::coord_flip() +
        theme_default +
        ggplot2::ylab(cc_t("Population", lang = lang)) +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_discrete(breaks = function(limits) {
          limits[seq(1, length(limits), by = 3)]
        }) +
        ggplot2::scale_alpha(range = c(0.3,1)) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
        explore_graph_scale(var = NULL, x_y = "y", data_vals = age_data$age_vars)
    }
    
    # Data representation list
    dat_rep <- list("Data representation" = list("Percentage" = "pct", "Number" = "count"))
    
    # Initiate the map.
    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[["lat"]],
        latitude = map_loc[["lon"]],
        zoom = map_zoom,
        map_style_id = map_base_style,
        inst_prefix = inst_prefix,
        stories = stories,
        stories_min_zoom = 13
      )
    })
    
    label_indicators_server(id = id, r = r)
    
    # Initial zoom string reactive value
    rv_zoom_string <- shiny::reactiveVal(
      zoom_get_string(
        zoom = map_zoom,
        zoom_levels = mzp
      )
    )
    
    # Zoom and POI reactives when the view state of the map changes.
    shiny::observeEvent(map_viewstate(),
                        {
                          r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
                          r[[id]]$poi(update_poi(
                            id = id, poi = r[[id]]$poi(),
                            map_viewstate = map_viewstate()
                          ))
                        },
                        ignoreInit = TRUE
    )
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = shiny::reactive(dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      # If there are no time in the page, use the latest census for date of
      # comparisons
      time = if (r[[id]]$time() != "") r[[id]]$time else shiny::reactive(2021)
    )
    
    # Region and zoom levels change depending on the geography widget
    zl <- geography_server(
      id = id,
      r = r,
      var_right = var_right,
      regions = regions,
      avail_scale_combinations = avail_scale_combinations
    )
    
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
    
    # Choose tileset
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      region = r[[id]]$region,
      zoom_levels = r[[id]]$zoom_levels,
      hide_if_one_zoom_level = shiny::reactive({
        # If there is one scale combinations that only includes ONE scale,
        # allow the mechanic to hide the zoom div if that scale combination
        # is active.
        sac <- curbcut:::single_scales_combination(avail_scale_combinations)
        all(!grepl("_", sac))
      })
    )
    
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
    
    # Hide the main dropdown, use a slider text instead
    vl <- slider_text_server(id, r = r)
    dr <- picker_server(id = id,
                        r = r,
                        var_list = shiny::reactive(dat_rep))
    var_left_1 <- shiny::reactive({
      vvll <- unique(vl())
      # If it's only 85+, return it
      if (length(vvll) == 1 && vvll == "85+") return("age_agg_85plus")
      if (length(vvll) == 1) {
        return(sprintf("age_agg_%s_%s", vvll, as.numeric(vvll) + 4))
      }
      
      last <- if (vvll[[2]] == "85+") "85plus" else as.numeric(vvll[[2]]) - 1
      sprintf("age_agg_%s_%s", vvll[[1]], last)
    })
    var_left <- shiny::reactive(sprintf("%s_%s", var_left_1(), dr()))
    widget_time <- time_slider_server(id = id, r = r)
    
    # Update the `r[[id]]$vars` reactive
    update_vars(
      id = id, r = r, var_left = var_left,
      var_right = var_right, widget_time = widget_time
    )
    
    # Sidebar
    sidebar_server(id = id, r = r)
    
    # Data
    data <- shiny::reactive(data_get(
      vars = r[[id]]$vars(),
      scale = r[[id]]$scale(),
      region = r[[id]]$region(),
      time = r[[id]]$time(),
      schemas = r[[id]]$schemas()
    ))
    
    # Data for tile coloring
    data_colours <- shiny::reactive(data_get_colours(
      vars = r[[id]]$vars(),
      region = r[[id]]$region(),
      time = r[[id]]$time(),
      zoom_levels = r[[id]]$zoom_levels(),
      schemas = r[[id]]$schemas()
    ))
    
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
    
    # Update map in response to variable changes or zooming
    map_viewstate <- map_js_server(
      id = id,
      r = r,
      tile = tile,
      select_id = r[[id]]$select_id,
      coords = r[[id]]$coords,
      zoom = r[[id]]$zoom,
      vars = r[[id]]$vars,
      # NDS: data_colours is going to need to be updated to work with `time`
      data_colours = data_colours,
      stories = stories
    )
    
    # Switch the graph to a static one when on grid q5
    explore_graph_fun_args <- shiny::reactive({
      if ("q5" %in% class(r[[id]]$vars())) {
        
        if (is.na(r[[id]]$select_id())) {
          return(list(fun = age_graph_fun,
                      args = list(var_left = r[[id]]$vars()$var_left,
                                  region = r[[id]]$region(),
                                  year = r[[id]]$time()$var_left,
                                  lang = r$lang())))          
        } else {
          return(list(fun = age_graph_selection_fun,
                      args = list(var_left = r[[id]]$vars()$var_left,
                                  scale = r[[id]]$scale(),
                                  select_id = r[[id]]$select_id(),
                                  year = r[[id]]$time()$var_left,
                                  lang = r$lang())))         
        }
        
      } else {
        return(list(fun = curbcut::explore_graph,
                    args = list(r = r,
                                data = data(),
                                vars = r[[id]]$vars(),
                                region = r[[id]]$region(),
                                scale = r[[id]]$scale(),
                                select_id = r[[id]]$select_id(),
                                time = r[[id]]$time(),
                                lang = r$lang(),
                                zoom_levels = r[[id]]$zoom_levels(),
                                schemas = r[[id]]$schemas())))
      }
    })
    
    # Explore panel
    explore_server(
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
      graph_fun = shiny::reactive(explore_graph_fun_args()$fun),
      graph_args = shiny::reactive(explore_graph_fun_args()$args),
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
