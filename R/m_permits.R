# ### PERMITS MODULE #############################################################
# 
# # UI ----------------------------------------------------------------------
# 
# permits_UI <- function(id) {
#   ns_id <- "permits"
#   
#   return(tagList(
#     # Sidebar
#     sidebar_UI(
#       NS(id, ns_id),
#       susSidebarWidgets(
#         select_var_UI(NS(id, ns_id), select_var_id = "d_1",
#                       var_list = var_left_list_1_permits,
#                       label = cc_t(r = r, "Grouping")),
#         select_var_UI(NS(id, ns_id), select_var_id = "d_2",
#                       var_list = var_left_list_2_permits,
#                       label = cc_t(r = r, "Type of permits")),
#         slider_UI(NS(id, ns_id), 
#                   slider_id = "slu",
#                   label = cc_t(r = r, "Select a year"),
#                   min = permits_slider$min,
#                   max = permits_slider$max,
#                   step = permits_slider$interval, sep = "",
#                   value = permits_slider$init,
#                   width = "95%"),
#         slider_UI(NS(id, ns_id), 
#                   slider_id = "slb",
#                   label = cc_t(r = r, "Select two dates"), 
#                   min = permits_slider$min,
#                   max = permits_slider$max, 
#                   step = permits_slider$interval, sep = "", 
#                   value = c("2000", "2021"),
#                   width = "95%"),
#         checkbox_UI(NS(id, ns_id),
#                     checkbox_id = "comp_d",
#                     label = cc_t(r = r, "Compare dates")),
#         hidden(checkbox_UI(NS(id, ns_id), 
#                            checkbox_id = "grid",
#                            label = cc_t(r = r, "250-metre grid"))),
#         year_disclaimer_UI(NS(id, ns_id))
#       ),
#       bottom = div(class = "bottom_sidebar", 
#                    tagList(legend_UI(NS(id, ns_id)),
#                            zoom_UI(NS(id, ns_id), map_zoom_levels)))),
#     
#     # Map
#     div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
#     
#     # Right panel
#     right_panel(
#       id = id, 
#       compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
#       explore_UI(NS(id, ns_id)), 
#       dyk_UI(NS(id, ns_id)))
#   ))
# }
# 
# 
# # Server ------------------------------------------------------------------
# 
# permits_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns_id <- "permits"
#     
#     # Initial reactives
#     zoom <- reactiveVal(get_zoom(map_zoom, c("heatmap" = 0, "point" = 12)))
#     selection <- reactiveVal(NA)
#     click_id <- reactiveVal(NULL)
#     
#     # Load heavy data used by this module
#       qload("data/permits.qsm")
#       borough <<- bind_cols(borough, permits_choropleth$borough)
#       CT <<- bind_cols(CT, permits_choropleth$CT)
#       DA <<- bind_cols(DA, permits_choropleth$DA)
#       grid <<- bind_cols(grid, permits_choropleth$grid)
#       permits <<- permits
# 
#     # Click reactive
#     observeEvent(input$map_polygon_click, {
#       click_id(get_click(input$map_polygon_click))})
#     
#     # Sidebar
#     sidebar_server(id = ns_id, x = "permits")
#     
#     # If COUNT isn't selected, choropleth is TRUE 
#     choropleth <- reactive(var_left_1() != "count")
# 
#     # Map
#     output$map <- renderMapdeck({mapdeck(
#       style = map_style, 
#       token = map_token, 
#       zoom = map_zoom, 
#       location = map_loc)})
#     
#     # Checkbox values
#     bi_time <- checkbox_server(id = ns_id, checkbox_id = "comp_d")
#     cbox_grid <- checkbox_server(id = ns_id, checkbox_id = "grid")
#     
#     # Enable or disable first and second slider
#     slider_uni <- slider_server(id = ns_id, slider_id = "slu")
#     slider_bi <- slider_server(id = ns_id, slider_id = "slb")
#     
#     observeEvent(bi_time(), {
#       toggle("permits-slb", condition = bi_time())
#       toggle("permits-slu", condition = !bi_time())})
#     
#     # If we aren't in choropleth, toggle off the zoom and grid checkbox
#     observeEvent(choropleth(), {
#       toggle("permits-grid", condition = choropleth())
#     })
#     observeEvent(cbox_grid(), {
#       toggle("permits-zoom_auto", condition = !cbox_grid())
#       toggle("permits-zoom_slider", condition = !cbox_grid())
#     })
#     
#     # Time variable depending on which slider
#     time <- reactive({if (!bi_time()) slider_uni() else slider_bi()})
#     
#     # Left variable servers
#     var_left_1 <- select_var_server(ns_id, select_var_id = "d_1",
#                                     var_list = reactive(var_left_list_1_permits))
#     var_left_2 <- select_var_server(ns_id, select_var_id = "d_2",
#                                     var_list = reactive(var_left_list_2_permits))
#     
#     # Construct left variable string
#     var_left <- reactive({
#       str_remove(paste("permits", 
#                        var_left_2(), 
#                        var_left_1(), 
#                        time(), sep = "_"), "_ ")
#     })
# 
#     # Zoom reactive
#     map_zoom_levels_permits <- reactive({
#       if (choropleth()) map_zoom_levels else c("heatmap" = 0, "point" = 12)
#     })
# 
#     observeEvent({input$map_view_change$zoom
#       map_zoom_levels_permits()}, {
#         actual_zoom <- if (is.null(input$map_view_change$zoom)) map_zoom else {
#           input$map_view_change$zoom
#         }
#         zoom(get_zoom(actual_zoom, map_zoom_levels_permits()))},
#       ignoreInit = TRUE)
# 
#     # Zoom level for data
#     df_choropleth <- zoom_server(
#       id = ns_id,
#       zoom = zoom,
#       zoom_levels = map_zoom_levels_permits)
# 
#     df <- reactive({if (cbox_grid()) "grid" else df_choropleth()})
#     
#     # Compare panel
#     var_right <- compare_server(
#       id = ns_id,
#       var_list = make_dropdown(compare = TRUE),
#       df = df,
#       time = time,
#       show_panel = choropleth)
# 
#     data <- reactive(get_data(df(), var_left(), var_right(), island = TRUE,
#                               point_df = "permits"))
# 
#     # Disclaimers and how to read the map
#     year_disclaimer_server(
#       id = ns_id,
#       data = data,
#       var_left = var_left,
#       var_right = var_right)
#     #   # time = time,
#     #   # pct_variation = choropleth,
#     #   # # If the same time is selected twice, other disclaimer
#     #   # more_condition = reactive(!choropleth() && all(is.na(pull(data(), ID)))),
#     #   # more_text = paste0(
#     #   #   "<p style='font-size:11px;'>",
#     #   #   "There is no '{var_left_title}' to report for ",
#     #   #   "{left_year}.</p>"))
#     # 
#     # Update map in response to variable changes or zooming
#     select_id <- map_change(
#       id = ns_id,
#       map_id = NS(id, "map"),
#       data = data,
#       df = df,
#       zoom = zoom,
#       click = click_id,
#     )
# 
#     # Explore select
#     # Update point on click
#     observeEvent(input$map_scatterplot_click, {
#       lst <- fromJSON(input$map_scatterplot_click)$index
#       if (lst == 0) selection(NA) else {
#         # This is a hack because of a mapdeck bug
#         selection(data()[lst + 1,]$ID)
#       }
#     })
# 
#     current_select <- reactive(if (choropleth()) select_id() else selection())
# 
#     # # Explore panel
#     # explore_content <- explore_server(
#     #   id = ns_id,
#     #   data = data,
#     #   var_left = var_left,
#     #   var_right = var_right,
#     #   df = df,
#     #   zoom = zoom,
#     #   select_id = select_id)
#     #   # custom_info = permits_info_table,
#     #   # custom_graph = permits_explore_graph)
# 
#     # # Legend
#     # legend_server(
#     #   id = ns_id, 
#     #   data = data,
#     #   var_left = var_left, 
#     #   var_right = var_right, 
#     #   df = df,
#     #   zoom = zoom)
#     #   # show_panel = choropleth)
# 
#     # # Did-you-know panel
#     dyk_server(
#       id = ns_id,
#       var_left = var_left,
#       var_right = var_right)
# 
#     # Clear selection on df change
#     observeEvent(df(), selection(NA), ignoreInit = TRUE)
# 
#     # Clear click status if prompted
#     observeEvent(input$`explore-clear_selection`, selection(NA))
# 
#     # Bi slider label explained
#     observe({
#       if (!choropleth()) {
#         updateSliderInput(session, inputId = "permits-slb",
#                           label = cc_t(r = r, "Total between two dates"))
#       } else if (choropleth()) {
#         updateSliderInput(session, inputId = "permits-slb",
#                           label = cc_t(r = r, "Compare two dates"))
#       }
#     })
#     
#     # Bookmarking
#     bookmark_server(
#       id = ns_id,
#       map_view_change = reactive(input$map_view_change),
#       var_left = var_left,
#       var_right = var_right,
#       select_id = select_id,
#       df = df,
#       map_id = NS(id, "map"),
#       more_args = reactive(c("c-comp_d" = str_extract(bi_time(), "^."),
#                              "c-grid" = str_extract(cbox_grid(), "^."),
#                              "s-slu" = slider_uni(),
#                              "s-slb" = paste(slider_bi(),
#                                              collapse = "-")))
#     )
#     
#     # Update click_id() on bookmark
#     observeEvent(r$sus_bookmark$active, {
#       # Delay of 2000 milliseconds more than the zoom update from bookmark.
#       # The map/df/data needs to be updated before we select an ID.
#       if (isTRUE(r$sus_bookmark$active)) {
#         delay(2000, {
#           if (!is.null(r$sus_bookmark$select_id)) {
#             if (r$sus_bookmark$select_id != "NA") click_id(r$sus_bookmark$select_id)
#           }
#         })
#       }
#       
#       # So that bookmarking gets triggered only ONCE
#       delay(1500, {r$sus_bookmark$active <- FALSE})      
#     }, priority = -2)
#     
#     # Update click_id() on module link
#     observeEvent(r$sus_link$activity, {
#       # Delay of 2000 milliseconds more than the zoom update from bookmark.
#       # The map/df/data needs to be updated before we select an ID.
#       delay(2000, {
#         if (!is.null(r$sus_link$select_id)) click_id(r$sus_link$select_id)
#       })
#     }, priority = -2)
#     
#   })
# }
