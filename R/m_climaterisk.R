# ### climate_risk PAGE ###########################################################
# 
# map_scale_fill_grid <- function(vars) {
#   var <- vars$var_left
#   
#   clr <- if (length(var) == 1) colours_dfs$left_5 else colours_dfs$delta
#   
#   if (length(var) == 2) {
#     var <- curbcut::var_remove_time(var)
#     var <- sprintf("%s_delta", var)
#   }
#   
#   cc.map::map_choropleth_fill_fun(df = clr[c("group", "fill")], 
#                                   get_col = var, 
#                                   fallback = "#B3B3BB")
# }
# 
# explore_graph_grid <- function(vars, lang, data, select_id) {
#   plot <- curbcut::explore_graph(vars = vars,
#                                  select_id = NA,
#                                  df = "grid_grid250",
#                                  data = data_get(vars, df = "grid_grid250"),
#                                  lang)
#   
#   if (!is.na(select_id)) {
#     if ("q5_ind" %in% class(vars)) {
#       plot <-
#         plot +
#         ggplot2::geom_vline(
#           xintercept = data$var_left[data$ID == select_id] + 1,
#           colour = "black", linewidth = 1.5)
#     }
# 
#     if ("delta_ind" %in% class(vars)) {
#       plot <-
#         plot +
#         ggplot2::geom_tile(data = data[data$ID == select_id, ],
#                            color = "white", fill = "transparent", size = 1.5)
#     }
#   }
#   
#   return(plot)
#   
# }
# 
# # GLOBAL ------------------------------------------------------------------
# 
# `climate_risk_default_region` <- unlist(modules$regions[modules$id == "climate_risk"])[1]
# `climate_risk_mzp` <-
#   eval(parse(text = paste0("map_zoom_levels_", `climate_risk_default_region`)))
# default_region <- modules$regions[modules$id == "climate_risk"][[1]][1]
# vars_right <- modules$var_right[modules$id == "climate_risk"][[1]]
# 
# 
# # UI ----------------------------------------------------------------------
# 
# `climate_risk_UI` <- function(id) {
#   page <- modules[modules$id == id, ]
#   theme_lowercased <- gsub(" .*", "", tolower(page$theme))
#   
#   shiny::tagList(
#     
#     # Add a piece of CSS to nensure we do not see the small grid ticks on
#     # the time sliders
#     shiny::tags$head(shiny::tags$style(
#       "#climate_risk-climate_risk-year_sliders .irs-grid-pol.small {
#     display: none;
#     }")),
# 
#     shiny::div(
#       `data-theme` = theme_lowercased,
#       # Sidebar
#     curbcut::sidebar_UI(
#       id = shiny::NS(id, id),
#       curbcut::autovars_UI(NS(id, id)),
#       curbcut::warnuser_UI(shiny::NS(id, id)),
#       curbcut::compare_UI(
#         id = NS(id, id),
#         var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
#       ),
#       shiny::hr(),
#       curbcut::zoom_UI(shiny::NS(id, id), `climate_risk_mzp`),
#       curbcut::checkbox_UI(id = NS(id, id), label = cc_t("View with grids"),
#                            value = TRUE),
#       bottom = shiny::tagList(
#         curbcut::legend_UI(shiny::NS(id, id)),
#       )
#     ),
# 
#     # Map
#     curbcut::map_js_UI(shiny::NS(id, id)),
#     
#     # Tutorial
#     curbcut::tutorial_UI(id = shiny::NS(id, id)),
# 
#     # Change view (Map/Data/Place explorer)
#     curbcut::panel_view_UI(id = NS(id, id)),
# 
#     # Right panel
#     curbcut::right_panel(
#       id = id,
#       curbcut::explore_UI(NS(id, id)),
#       curbcut::dyk_UI(NS(id, id))
#     )
#   )
#   )
# }
# 
# 
# # Server ------------------------------------------------------------------
# 
# `climate_risk_server` <- function(id, r) {
#   shiny::moduleServer(id, function(input, output, session) {
#     
#     output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
#       cc.map::map_input(
#         map_ID = shiny::NS(id, shiny::NS(id, "map")),
#         username = mapbox_username,
#         token = map_token,
#         longitude = map_loc[1],
#         latitude = map_loc[2],
#         zoom = map_zoom,
#         map_style_id = map_base_style,
#         tileset_prefix = tileset_prefix,
#         stories = stories,
#         stories_min_zoom = 13
#       )
#     })
# 
#     # Initial reactives
#     rv_zoom_string <- reactiveVal(
#       curbcut::zoom_get_string(
#         zoom = map_zoom,
#         zoom_levels = `climate_risk_mzp`,
#         region = default_region
#       )
#     )
#     
#     # Zoom and POI reactives when the view state of the map changes.
#     observeEvent(map_viewstate(), {
#       r[[id]]$zoom(curbcut::zoom_get(zoom = map_viewstate()$zoom))
#       r[[id]]$poi(curbcut::update_poi(
#         id = id, poi = r[[id]]$poi(),
#         map_viewstate = map_viewstate()
#       ))
#     })
#     
#     # If on grid, hide the slider and the auto-zoom
#     shiny::observe({
#       
#       # Add one namespace as these are inside other module servers
#       shinyjs::toggle(shiny::NS(id, "zoom_auto-cccheckbox_cbx"), condition = !grid())
#       # Follow the rest of the slidertext addition ID for the zoom slider
#       shinyjs::toggle(shiny::NS(id, "zoon_slider_div"), condition = !grid())
#       
#       # Get the zoom label
#       zoom_name <- rv_zoom_string()
#       names(zoom_name) <- zoom_name
#       zoom_name <- curbcut::zoom_get_label(zoom_name, lang = r$lang())
#       
#       if (grid()) {
#         shiny::removeUI("#climaterisk_grid_zn")
#         shiny::insertUI(selector = "#climate_risk-climate_risk-zoom_cbx_loc",
#                         where = "beforeEnd",
#                         ui = shiny::div(id = "climaterisk_grid_zn", zoom_name))
#       } else {
#         # Revert to default HTML
#         shiny::removeUI("#climaterisk_grid_zn")
#       }
#     })
#     
#     # Switch the region depending on inputs
#     region <- shiny::reactive({
#       if (grid()) return("grid")
#       r$region()
#     })
#     
#     # Map zoom levels change depending on r$region()
#     zoom_levels <-
#       shiny::reactive(curbcut::zoom_get_levels(id = id, region = region()))
#     current_region <- shiny::reactive(zoom_levels()$region)
#     current_zl <- shiny::reactive(zoom_levels()$zoom_levels)
# 
#     # Zoom string reactive
#     observe({
#       rv_zoom_string({
#         curbcut::zoom_get_string(
#           zoom = r[[id]]$zoom(),
#           zoom_levels = current_zl(),
#           region = current_region()
#         )
#       })
#     })
# 
#     # Update selected ID
#     update_select_id(id = id, r = r, data = data)
#     
#     
#     # Default to tileset values
#     grid_compare <- shiny::reactive(grid() && var_right()[1] != " ")
# 
#     # Choose tileset
#     tile_1 <- curbcut::zoom_server(
#       id = id,
#       r = r,
#       zoom_string = rv_zoom_string,
#       zoom_levels = zoom_levels
#     )
#     tile <- shiny::reactive(if (grid_compare()) "grid_grid250" else tile_1())
#     
#     # Get df
#     observeEvent(
#       {
#         tile()
#         rv_zoom_string()
#       },
#       {
#         if (grid_compare()) return(r[[id]]$df("grid_grid250"))
#         r[[id]]$df(curbcut::update_df(
#           tile = tile(),
#           zoom_string = rv_zoom_string()
#         ))
#       }
#     )
# 
#     # Construct the left-hand UIs / servers automatically
#     autovars <- 
#       curbcut::autovars_server(
#         id = id, 
#         r = r, 
#         main_dropdown_title = "Climate vulnerability indicator",
#         default_year = 2022)
#     
#     var_left <- shiny::reactive(autovars()$var)
#     time <- shiny::reactive(autovars()$time)
# 
#     # 250-m grid checkbox
#     grid <- curbcut::checkbox_server(
#       id = id,
#       r = r,
#       label = shiny::reactive("View with grids"))
# 
#     # Right variable / compare panel
#     var_right <- curbcut::compare_server(
#       id = id,
#       r = r,
#       var_list = shiny::reactive(curbcut::dropdown_make(
#         vars = vars_right,
#         compare = TRUE
#       )),
#       time = time
#     )
# 
#     # Update the `r[[id]]$vars` reactive
#     curbcut::update_vars(
#       id = id, 
#       r = r, 
#       var_left = var_left, 
#       # Force an empty var_right when on `grid`
#       var_right = var_right)
# 
#     # Sidebar
#     curbcut::sidebar_server(id = id, r = r)
# 
#     # Data
#     data <- reactive(curbcut::data_get(
#       vars = r[[id]]$vars(),
#       df = r[[id]]$df()
#     ))
#     
#     # Data for tile coloring
#     data_colours <- shiny::reactive({
#       # If the color shown is extracted from the tileset, do not calculate
#       if (grid() & !grid_compare()) return(data.frame())
#       zoom_levels <- if (grid_compare()) {
#         stats::setNames("grid250", "grid250") 
#       } else {
#         current_zl()
#       }
#       
#       curbcut::data_get_colours(
#         vars = r[[id]]$vars(),
#         region = current_region(),
#         zoom_levels = zoom_levels
#       )
#     })
# 
#     # Warn user
#     curbcut::warnuser_server(
#       id = id,
#       r = r,
#       vars = r[[id]]$vars,
#       time = time,
#       data = data
#     )
#     
#     # Tutorial
#     curbcut::tutorial_server(
#       id = id,
#       r = r,
#       skip_elements = shiny::reactive(if (grid()) "zoom_div" else NULL)
#     )
# 
#     # Legend
#     curbcut::legend_server(
#       id = id,
#       r = r,
#       vars = r[[id]]$vars,
#       data = data,
#       df = r[[id]]$df
#     )
# 
#     # Did-you-know panel
#     dyk_server(
#       id = id,
#       r = r,
#       vars = r[[id]]$vars,
#       df = r[[id]]$df,
#       select_id = r[[id]]$select_id,
#       poi = r[[id]]$poi,
#       region = current_region,
#       zoom_levels = current_zl
#     )
#     
#     # Switch the fill function of the map server when on grid
#     fill_fun_args <- shiny::reactive({
#       if (grid() & !grid_compare()) {
#         return(list(fun = map_scale_fill_grid,
#                     args = list(vars = r[[id]]$vars())))
#       } else {
#         return(list(fun = cc.map::map_choropleth_fill_fun,
#                     args = list(df = data_colours(), 
#                                 get_col = names(data_colours())[1], 
#                                 fallback = "#B3B3BB")))
#       }
#     })
#     
#     # Update map in response to variable changes or zooming
#     map_viewstate <- curbcut::map_js_server(
#       id = id,
#       r = r,
#       tile = tile,
#       select_id = r[[id]]$select_id,
#       coords = r[[id]]$coords,
#       zoom = r[[id]]$zoom,
#       data_colours = data_colours, 
#       fill_fun = shiny::reactive(fill_fun_args()$fun),
#       fill_fun_args = shiny::reactive(fill_fun_args()$args),
#       stories = stories
#     )
#     
#     # Switch the graph to a static one when on grid q5
#     explore_graph_fun_args <- shiny::reactive({
#       if (grid() & !grid_compare()) {
#         return(list(fun = explore_graph_grid,
#                     args = list(vars = r[[id]]$vars(), 
#                                 lang = r$lang(), 
#                                 data = shiny::isolate(data()), 
#                                 select_id = r[[id]]$select_id())))
#       } else {
#         return(list(fun = curbcut::explore_graph,
#                     args = list(r = r, 
#                                 data = data(), 
#                                 vars = r[[id]]$vars(), 
#                                 df = r[[id]]$df(),
#                                 select_id = r[[id]]$select_id(), 
#                                 region = current_region(), 
#                                 lang = r$lang())))
#       }
#     })
#     
#     # Update the selection when on grid() and the `df` changes (to redraw the graph)
#     shiny::observeEvent(r[[id]]$df(), {
#       if (grid() & !is.na(r[[id]]$select_id())) r[[id]]$select_id(NA)
#     })
# 
#     # Explore panel
#     curbcut::explore_server(
#       id = id,
#       r = r,
#       data = data,
#       region = current_region,
#       vars = r[[id]]$vars,
#       df = r[[id]]$df,
#       select_id = r[[id]]$select_id, 
#       graph = shiny::reactive(explore_graph_fun_args()$fun), 
#       graph_args = shiny::reactive(explore_graph_fun_args()$args)
#     )
# 
#     # Bookmarking
#     curbcut::bookmark_server(
#       id = id,
#       r = r,
#       select_id = r[[id]]$select_id,
#       map_viewstate = map_viewstate
#     )
# 
#     # Change view
#     curbcut::panel_view_server(
#       id = id,
#       r = r,
#       region = reactive(zoom_levels()$region),
#       vars = r[[id]]$vars,
#       data = data,
#       zoom_levels = current_zl
#     )
#   })
# }
