# ### GREEN SPACE MODULE ########################################################
# 
# # UI ----------------------------------------------------------------------
# 
# green_space_UI <- function(id) {
#   ns_id <- "green_space"
#   
#   return(tagList(
#     # Side bar
#     sidebar_UI(
#       NS(id, ns_id),
#       susSidebarWidgets(
#         select_var_UI(NS(id, ns_id), select_var_id = "d_1",
#                       var_list = var_left_list_1_green_space,
#                       label = cc_t(r = r, "Grouping")),
#         select_var_UI(NS(id, ns_id), select_var_id = "d_2",
#                       var_list = var_left_list_2_green_space,
#                       label = cc_t(r = r, "Type of green space"))
#       ),
#       bottom = div(class = "bottom_sidebar",
#                    tagList(legend_UI(NS(id, ns_id)),
#                            zoom_UI(NS(id, ns_id), map_zoom_levels_cma)))),
#     
#     # Map
#     div(class = "mapdeck_div", 
#         mapdeckOutput(NS(id, "map"), height = "100%")),
#     
#     # Right panel
#     right_panel(id, compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
#                 explore_UI(NS(id, ns_id)), 
#                 dyk_UI(NS(id, ns_id)))
#   ))
# }
# 
# # Server ------------------------------------------------------------------
# 
# green_space_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns_id <- "green_space"
#     
#     # Initial reactives
#     zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels_cma))
#     click_id <- reactiveVal(NULL)
# 
#     # Sidebar
#     sidebar_server(ns_id, x = "green_space")
#     
#     # If green space isn't selected, choropleth is TRUE 
#     choropleth <- reactive(var_left_groupings() != " ")
#     
#     # Map
#     output$map <- renderMapdeck({mapdeck(
#       style = map_style, 
#       token = map_token, 
#       zoom = map_zoom, 
#       location = map_loc)})
#     
#     # Zoom reactive
#     observeEvent(input$map_view_change$zoom, {
#       zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels_cma))})
#     
#     # Click reactive
#     observeEvent(input$map_polygon_click, {
#       click_id(get_click(input$map_polygon_click))})
#     
#     # Zoom level for data
#     df <- zoom_server(
#       id = ns_id, 
#       zoom = zoom, 
#       zoom_levels = reactive(map_zoom_levels_cma))
#     
#     # Left variable servers
#     var_left_groupings <- select_var_server(ns_id, select_var_id = "d_1",
#                                             var_list = reactive(var_left_list_1_green_space))
#     var_left_type <- select_var_server(ns_id, select_var_id = "d_2",
#                                        var_list = reactive(var_left_list_2_green_space))
#     
#     # Construct left variable string
#     var_left <- reactive(str_remove(paste("green_space", var_left_type(), 
#                                           var_left_groupings(), sep = "_"), 
#                                     "_ "))
# 
#     # Compare panel
#     var_right <- compare_server(
#       id = ns_id,
#       var_list = make_dropdown(compare = TRUE),
#       df = df,
#       time = reactive("2016"),
#       show_panel = choropleth)
# 
#     # # Data
#     data <- reactive({
#       if (choropleth()) {
#         get_data(df(), var_left(), var_right(), island = TRUE)
#       } else {
#         green_space %>%
#           {if (var_left_type() != "total")
#             filter(., type_1 == var_left_type()) else .}
#       }
#     })
# 
#     # # Update map in response to variable changes or zooming
#     select_id <- map_change(
#       id = ns_id,
#       map_id = NS(id, "map"),
#       data = data,
#       df = df,
#       zoom = zoom,
#       click = click_id,
#       standard_width = choropleth
#     )
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
#     #   # standard = choropleth,
#     #   # custom_info = green_space_info_table,
#     #   # custom_graph = green_space_explore_graph)
#     # 
#     # # Legend
#     # legend_server(
#     #   id = ns_id,
#     #   data = data,
#     #   var_left = var_left,
#     #   var_right = var_right,
#     #   df = df,
#     #   zoom = zoom)
#       # show_panel = choropleth)
# 
#     # # Did-you-know panel
#     # dyk_server(
#     #   id = ns_id,
#     #   var_left = var_left,
#     #   var_right = var_right)
#     
#     # Bookmarking
#     bookmark_server(
#       id = ns_id,
#       map_view_change = reactive(input$map_view_change),
#       var_left = var_left,
#       var_right = var_right,
#       select_id = select_id,
#       df = df,
#       map_id = NS(id, "map")
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
#     # Update click_id() on modulke link
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
