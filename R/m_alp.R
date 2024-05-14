# # Create the `basic` function
# alp_UI <- function(id) {
#   page <- modules[modules$id == id, ]
#   regions <- page$regions[[1]]
#   if (is.null(regions)) {
#     stop(sprintf(paste0(
#       "Page `%s` does not have available regions. Please ",
#       "check the `regions` column in the `modules` ",
#       "dataframe."
#     ), id))
#   }
#   avail_scale_combinations <- page$avail_scale_combinations[[1]]
#   mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
#   theme_lowercased <- gsub(" .*", "", tolower(page$theme))
#   # stories <- get_from_globalenv("stories")
#   
#   # Grab the possible regions for the module
#   possible_regions <- page$regions[[1]][1]
#   
#   shiny::tagList(
#     # Sidebar
#     shiny::div(
#       `data-theme` = theme_lowercased,
#       sidebar_UI(
#         id = shiny::NS(id, id),
#         autovars_UI(shiny::NS(id, id)),
#         warnuser_UI(shiny::NS(id, id)),
#         compare_UI(
#           id = shiny::NS(id, id),
#           var_list = dropdown_make(vars = " ", compare = TRUE)
#         ),
#         geography_UI(shiny::NS(id, id),
#                      regions = regions,
#                      avail_scale_combinations = avail_scale_combinations
#         ),
#         shiny::hr(),
#         zoom_UI(shiny::NS(id, id), zoom_levels = mzp),
#         bottom = shiny::tagList(
#           legend_UI(shiny::NS(id, id))
#         )
#       ),
#       
#       # Map
#       map_js_UI(shiny::NS(id, id)),
#       
#       # Tutorial
#       tutorial_UI(id = shiny::NS(id, id)),
#       
#       # Change view (Map/Data/Place explorer)
#       panel_view_UI(id = shiny::NS(id, id)),
#       
#       # Right panel
#       right_panel(
#         id = shiny::NS(id, id),
#         explore_UI(shiny::NS(id, id)),
#         dyk_UI(shiny::NS(id, id))
#       )
#     )
#   )
# }
# 
# # Create the basic server function
# alp_server <- function(id, r) {
#   shiny::moduleServer(id, function(input, output, session) {
#     page <- modules[modules$id == id, ]
#     regions <- page$regions[[1]]
#     if (is.null(regions)) {
#       stop(sprintf(paste0(
#         "Page `%s` does not have available regions. Please ",
#         "check the `regions` column in the `modules` ",
#         "dataframe.", id
#       )))
#     }
#     avail_scale_combinations <- page$avail_scale_combinations[[1]]
#     mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
#     
#     main_dropdown_title <- page$main_dropdown_title
#     default_year <- page$dates[[1]]
#     default_year <- if (is.null(default_year)) NULL else max(default_year)
#     vars_right <- page$var_right[[1]]
#     stories <- get_from_globalenv("stories", stop_if_missing = FALSE)
#     
#     map_zoom <- get_from_globalenv("map_zoom")
#     map_loc <- get_from_globalenv("map_loc")
#     inst_prefix <- get_from_globalenv("inst_prefix")
#     map_token <- get_from_globalenv("map_token")
#     map_base_style <- get_from_globalenv("map_base_style")
#     mapbox_username <- get_from_globalenv("mapbox_username")
#     
#     # Initiate the map.
#     output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
#       cc.map::map_input(
#         map_ID = shiny::NS(id, shiny::NS(id, "map")),
#         username = mapbox_username,
#         token = map_token,
#         longitude = map_loc[["lat"]],
#         latitude = map_loc[["lon"]],
#         zoom = map_zoom,
#         map_style_id = map_base_style,
#         inst_prefix = inst_prefix,
#         stories = stories,
#         stories_min_zoom = 13
#       )
#     })
#     
#     # Initial zoom string reactive value
#     rv_zoom_string <- shiny::reactiveVal(
#       zoom_get_string(
#         zoom = map_zoom,
#         zoom_levels = mzp
#       )
#     )
#     
#     # Zoom and POI reactives when the view state of the map changes.
#     shiny::observeEvent(map_viewstate(),
#                         {
#                           r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
#                           r[[id]]$poi(update_poi(
#                             id = id, poi = r[[id]]$poi(),
#                             map_viewstate = map_viewstate()
#                           ))
#                         },
#                         ignoreInit = TRUE
#     )
#     
#     # Right variable / compare panel
#     var_right <- compare_server(
#       id = id,
#       r = r,
#       var_left = r[[id]]$var_left,
#       var_list = shiny::reactive(dropdown_make(
#         vars = vars_right,
#         compare = TRUE
#       )),
#       zoom_levels = r[[id]]$zoom_levels,
#       # If there are no time in the page, use the latest census for date of
#       # comparisons
#       time = if (r[[id]]$time() != "") r[[id]]$time else shiny::reactive(2021)
#     )
#     
#     # Region and zoom levels change depending on the geography widget
#     zl <- geography_server(
#       id = id,
#       r = r,
#       var_right = var_right,
#       regions = regions,
#       avail_scale_combinations = avail_scale_combinations
#     )
#     
#     update_region(id = id, r = r, new_region = shiny::reactive(zl()$region))
#     update_zoom_levels(id = id, r = r, new_zl = shiny::reactive(zl()$zoom_levels))
#     
#     # Zoom string reactive
#     shiny::observe({
#       rv_zoom_string({
#         zoom_get_string(
#           zoom = r[[id]]$zoom(),
#           zoom_levels = r[[id]]$zoom_levels()
#         )
#       })
#     })
#     
#     # Update selected ID
#     update_select_id(id = id, r = r, data = data)
#     
#     # Choose tileset
#     tile <- zoom_server(
#       id = id,
#       r = r,
#       zoom_string = rv_zoom_string,
#       region = r[[id]]$region,
#       zoom_levels = r[[id]]$zoom_levels,
#       hide_if_one_zoom_level = shiny::reactive({
#         # If there is one scale combinations that only includes ONE scale,
#         # allow the mechanic to hide the zoom div if that scale combination
#         # is active.
#         sac <- single_scales_combination(avail_scale_combinations)
#         all(!grepl("_", sac))
#       })
#     )
#     
#     # Get scale
#     shiny::observeEvent(
#       {
#         tile()
#         rv_zoom_string()
#       },
#       {
#         r[[id]]$scale(update_scale(
#           tile = tile(),
#           zoom_string = rv_zoom_string()
#         ))
#       }
#     )
#     
#     # Construct the left-hand UIs / servers automatically
#     autovars <-
#       autovars_server(
#         id = id,
#         r = r,
#         main_dropdown_title = main_dropdown_title,
#         default_year = default_year
#       )
#     
#     update_rv(id, r, rv_name = "var_left", new_val = shiny::reactive(autovars()$var))
#     widget_time <- shiny::reactive(if (is.null(autovars()$time)) "" else autovars()$time)
#     
#     # Update the `r[[id]]$vars` reactive
#     update_vars(
#       id = id, r = r, var_left = r[[id]]$var_left,
#       var_right = var_right, scale = r[[id]]$scale, widget_time = widget_time
#     )
#     
#     # Sidebar
#     sidebar_server(id = id, r = r)
#     
#     # Data
#     data <- shiny::reactive(data_get(
#       vars = r[[id]]$vars(),
#       scale = r[[id]]$scale(),
#       region = r[[id]]$region(),
#       time = r[[id]]$time(),
#       schemas = r[[id]]$schemas(),
#       zoom_levels = r[[id]]$zoom_levels()
#     ))
#     
#     # Data for tile coloring
#     data_colours <- data_colours_server(
#       id = id, 
#       r = r, 
#       vars = r[[id]]$vars,
#       scale = r[[id]]$scale,
#       region = r[[id]]$region,
#       data = data,
#       time = r[[id]]$time,
#       zoom_levels = r[[id]]$zoom_levels,
#       schemas = r[[id]]$schemas,
#       map_viewstate = map_viewstate)
# 
#     # Warn user
#     warnuser_server(
#       id = id,
#       r = r,
#       vars = r[[id]]$vars,
#       time = r[[id]]$time,
#       widget_time = widget_time,
#       data = data
#     )
# 
#     # Tutorial
#     tutorial_server(
#       id = id,
#       r = r
#     )
# 
#     # Legend
#     legend_server(
#       id = id,
#       r = r,
#       vars = r[[id]]$vars,
#       data = data,
#       scale = r[[id]]$scale,
#       time = r[[id]]$time
#     )
# 
#     # Did-you-know panel
#     dyk_server(
#       id = id,
#       r = r,
#       vars = r[[id]]$vars,
#       scale = r[[id]]$scale,
#       region = r[[id]]$region,
#       select_id = r[[id]]$select_id,
#       time = r[[id]]$time,
#       poi = r[[id]]$poi,
#       zoom_levels = r[[id]]$zoom_levels
#     )
# 
#     # Update map in response to variable changes or zooming
#     map_viewstate <- map_js_server(
#       id = id,
#       r = r,
#       tile = tile,
#       select_id = r[[id]]$select_id,
#       coords = r[[id]]$coords,
#       zoom = r[[id]]$zoom,
#       vars = r[[id]]$vars,
#       data_colours = data_colours,
#       stories = stories
#     )
#     
#     # Explore panel
#     explore_server(
#       id = id,
#       r = r,
#       data = data,
#       region = r[[id]]$region,
#       vars = r[[id]]$vars,
#       scale = r[[id]]$scale,
#       select_id = r[[id]]$select_id,
#       time = r[[id]]$time,
#       zoom_levels = r[[id]]$zoom_levels,
#       schemas = r[[id]]$schemas
#     )
# 
#     # Bookmarking
#     bookmark_server(
#       id = id,
#       r = r,
#       select_id = r[[id]]$select_id,
#       map_viewstate = map_viewstate
#     )
# 
#     # Change view
#     panel_view_server(
#       id = id,
#       r = r,
#       region = r[[id]]$region,
#       scale = r[[id]]$scale,
#       vars = r[[id]]$vars,
#       data = data,
#       zoom_levels = r[[id]]$zoom_levels,
#       time = r[[id]]$time,
#       schemas = r[[id]]$schemas
#     )
#   })
# }