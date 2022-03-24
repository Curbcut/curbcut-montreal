# ### NATURAL INFRASTRUCTURE MODULE #########################################
# 
# # UI ----------------------------------------------------------------------
# 
# natural_infrastructure_UI <- function(id) {
#   ns_id <- "natural_infrastructure"
#   
#   tagList(
#     # Sidebar
#     sidebar_UI(
#       NS(id, ns_id), 
#       susSidebarWidgets(
#         select_var_UI(NS(id, ns_id), var_list = vars_natural_infrastructure_left),
#         checkbox_UI(NS(id, ns_id),
#                     label = sus_translate("Focus on prioritization")),
#         hidden(slider_UI(NS(id, ns_id),
#                          label = sus_translate("Slide to visualize more/less importance"),
#                          min = round(min(natural_infrastructure$habitat_quality, na.rm = TRUE), 
#                                      digits = 2),
#                          max = round(max(natural_infrastructure$habitat_quality, na.rm = TRUE), 
#                                      digits = 2), 
#                          step = NULL,
#                          value = round(min(natural_infrastructure$habitat_quality, na.rm = TRUE), 
#                                        digits = 2))),
#         htmlOutput(NS(id, "explainer"))),
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
#       compare_UI(NS(id, ns_id), make_dropdown(compare_default = TRUE)),
#       explore_UI(NS(id, ns_id)), 
#       dyk_UI(NS(id, ns_id)))
#   )
# }
# 
# 
# # Server ------------------------------------------------------------------
# 
# natural_infrastructure_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns_id <- "natural_infrastructure"
#     
#     # Initial reactives
#     zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
#     click_id <- reactiveVal(NULL)
#     poi <- reactiveVal(NULL)
#     
#     # Map
#     output$map <- renderMapdeck(mapdeck(
#       style = map_style, 
#       token = map_token, 
#       zoom = map_zoom, 
#       location = map_location))
#     
#     # Zoom and POI reactives
#     observeEvent(input$map_view_change, {
#       zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))
#       poi(observe_map(input$map_view_change))
#     })
#     
#     # Click reactive
#     observeEvent(input$map_polygon_click, {
#       click_id(get_click(input$map_polygon_click))})
#     
#     # Zoom level for data
#     df <- zoom_server(
#       id = ns_id, 
#       zoom = zoom, 
#       zoom_levels = reactive(map_zoom_levels))
#     
#     # Checkbox value
#     focus <- checkbox_server(id = ns_id)
#     
#     # Enable or disable first and second slider
#     observeEvent(focus(), toggle(NS(id, "slider"), condition = focus()))
#     
#     # Left variable
#     var_left <- select_var_server(
#       id = ns_id, 
#       var_list = reactive(vars_natural_infrastructure_left))
# 
#     slider <- slider_server(id = ns_id,
#                             value = reactive(min(natural_infrastructure[[var_left()]], 
#                                                  na.rm = TRUE) |> 
#                                                round(digits = 2)),
#                             min = reactive(min(natural_infrastructure[[var_left()]], 
#                                                na.rm = TRUE) |> 
#                                              round(digits = 2)),
#                             max = reactive(max(natural_infrastructure[[var_left()]], 
#                                                na.rm = TRUE) |> 
#                                              round(digits = 2)))
# 
#     # Right variable / compare panel
#     var_right <- compare_server(
#       id = ns_id,
#       var_list = make_dropdown(compare_default = TRUE),
#       df = df)
#     
#     # Sidebar
#     sidebar_server(id = ns_id, x = "natural_infrastructure")
#     
#     # Data
#     data <- eventReactive({var_left()
#       focus()
#       slider()}, {
#       
#       var_left <- var_left()
#       left_q3 <- paste0(var_left, "_q3")
#       left_q5 <- paste0(var_left, "_q5")
#       
#       out <- 
#         natural_infrastructure |> 
#         st_drop_geometry() |> 
#         select(ID, name, name_2, any_of(c("DAUID", "CTUID", "CSDUID")), 
#                population, var_left = all_of(var_left), 
#                var_left_q3 = all_of(left_q3),
#                var_left_q5 = all_of(left_q5)) |>
#         filter(!is.na(var_left)) |> 
#         (\(x) if (focus() && slider() <= max(x$var_left)) {
#           x |> 
#             filter(var_left >= slider()) |>
#             mutate(fill = colour_left_5$fill[[6]])
#         } else {
#           x |> 
#             mutate(group = coalesce(as.character(var_left_q5), "NA"),
#                    .after = var_left_q5) |> 
#             left_join(colour_left_5, by = "group") |> 
#             relocate(fill, .after = group)
#         })()
#       
#       right_join(select(natural_infrastructure, ID),
#                  out, by = "ID")
#       
#     })
#     
#     # Disclaimers and how to read the map
#     output$explainer <- renderText({
#       
#       "COLORS OF VALUES MUST BE CHANGED. A ~VIRIDIS COLOR PALETTE. LOW VALUES
#       ARE GOOD, HIGH VALUES ARE BAD."
#       
#     })
#     
#     # Legend
#     # legend <- legend_server(
#     #   id = ns_id, 
#     #   data = data,
#     #   var_left = var_left, 
#     #   var_right = var_right, 
#     #   df = df,
#     #   zoom = zoom)
#     
#     # Did-you-know panel
#     dyk_server(
#       id = ns_id, 
#       var_left = var_left,
#       var_right = var_right,
#       poi = poi)
#     
#     # Update map in response to variable changes or zooming
#     select_id <- map_change(
#       id = ns_id,
#       map_id = NS(id, "map"), 
#       data = data, 
#       df = df, 
#       zoom = zoom,
#       click = click_id,
#       standard_width = reactive(FALSE)
#     )
#     
#     # Explore panel
#     # explore_content <- explore_server(
#     #   id = ns_id, 
#     #   data = data, 
#     #   var_left = var_left,
#     #   var_right = var_right, 
#     #   df = df, 
#     #   zoom = zoom,
#     #   select_id = select_id)
#     
#     # Data export TKTK should this become a non-reactive function?
#     data_export <- data_export_server(
#       id = ns_id,
#       df = data,
#       var_left = var_left,
#       var_right = var_right)
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
#     )
#     
#     # Update click_id() on bookmark
#     observeEvent(sus_bookmark$active, {
#       # Delay of 2000 milliseconds more than the zoom update from bookmark.
#       # The map/df/data needs to be updated before we select an ID.
#       if (isTRUE(sus_bookmark$active)) {
#         delay(2000, {
#           if (!is.null(sus_bookmark$select_id)) {
#             if (sus_bookmark$select_id != "NA") click_id(sus_bookmark$select_id)
#           }
#         })
#       }
#       
#       # So that bookmarking gets triggered only ONCE
#       delay(1500, {sus_bookmark$active <- FALSE})      
#     }, priority = -2)
#     
#     # Update click_id() on module link
#     observeEvent(sus_link$activity, {
#       # Delay of 2000 milliseconds more than the zoom update from bookmark.
#       # The map/df/data needs to be updated before we select an ID.
#       delay(2000, {
#         if (!is.null(sus_link$select_id)) click_id(sus_link$select_id)
#       })
#     }, priority = -2)
#     
#   })
# }
