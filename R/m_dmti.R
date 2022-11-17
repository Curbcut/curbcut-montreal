# ### dmti MODULE ###############################################################
# 
# # UI ----------------------------------------------------------------------
# 
# dmti_UI <- function(id) {
#   return(div(class = "mapdeck_div", 
#                mapdeckOutput(NS(id, "map"), height = "100%")),
#           title_UI(NS(id, "title"),
#                    select_var_UI(NS(id, "left_1"), var_list = var_list_left_dmti_1), 
#                    select_var_UI(NS(id, "left_2"), var_list = var_list_left_dmti_2), 
#                    select_var_UI(NS(id, "left_3"), var_list = var_list_left_dmti_3), 
#                    slider_UI(NS(id, "left"), dmti_slider$min, dmti_slider$max, 
#                              dmti_slider$interval, dmti_slider$init)
#           ),
#           right_panel(id, 
#                       compare_UI(NS(id, "dmti"), var_list_right_dmti),
#                       # explore_UI(NS(id, "explore")),
#                       # dyk_UI(NS(id, "dyk"))
#           ),
#           legend_bivar_UI(NS(id, "dmti")))
# }
# 
# 
# # Server ------------------------------------------------------------------
# 
# dmti_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     
#     # Title bar
#     title_server("title", "dmti")
#     
#     # Map
#     output$map <- renderMapdeck({
#       mapdeck(
#         style = map_style, token = token_dmti,
#         zoom = map_zoom, location = map_loc) %>%
#         add_polygon(data = borough %>%
#                       mutate(group = paste(dmti_food_healthy_2016_q3, "- 1")) %>%
#                       left_join(colour_bivar_borough, by = "group"),
#                     stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
#                     update_view = FALSE, id = "ID", auto_highlight = TRUE,
#                     highlight_colour = "#FFFFFF90")
#     })
#     
#     # Zoom level
#     observeEvent(input$map_view_change$zoom, {
#       rv_dmti$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "DA_2",
#                                  input$map_view_change$zoom >= 12 ~ "DA",
#                                  input$map_view_change$zoom >= 10.5 ~ "CT",
#                                  TRUE ~ "CSD")
#     })
#     
#     # Compare panel
#     var_right_dmti <- compare_server("dmti", var_list_right_dmti,
#                                       reactive(rv_dmti$zoom))
#     
#     # Left variable servers
#     var_left_dmti_1 <- select_var_server("left_1", var_list = reactive(var_list_left_dmti_1))
#     
#     left_2_update <- reactive({
#       output <- list()
#      if (var_left_dmti_1() == "food")  output <- var_list_left_dmti_2_food
#      if (var_left_dmti_1() == "city")  output <- var_list_left_dmti_2_city
#      if (var_left_dmti_1() == "street")  output <- var_list_left_dmti_2_street
#      if (var_left_dmti_1() == "access")  output <- var_list_left_dmti_2_access
#      if (var_left_dmti_1() == "exposure")  output <- var_list_left_dmti_2_exposure
#       output
#     })
#     
#     left_3_update <- reactive({
#       output <- list()
#       if (var_left_dmti_1() == "food") output <- var_list_left_dmti_3_food
#       if (var_left_dmti_1() == "city") output <- var_list_left_dmti_3_city
#       if (var_left_dmti_1() == "street") output <- var_list_left_dmti_3_street
#       if (var_left_dmti_1() == "access") output <- var_list_left_dmti_3_access
#       if (var_left_dmti_1() == "exposure") output <- var_list_left_dmti_3_exposure
#       output
#     })
#     
#     var_left_dmti_2 <- select_var_server("left_2", var_list = left_2_update)
#     var_left_dmti_3 <- select_var_server("left_3", var_list = left_3_update)
#     
#     # Get time from slider
#     time <- slider_server("left")
#     
#     # Construct left variable string
#     var_left_dmti <- reactive(
#       str_remove(paste(
#         "dmti", 
#         var_left_dmti_1(), 
#         var_left_dmti_2(), 
#         time(), 
#         var_left_dmti_3(),
#         sep = "_"), "_ $")
#     )
#     
#     # Data 
#     data_dmti <- data_server("dmti", var_left_dmti, var_right_dmti, 
#                               reactive(rv_dmti$zoom))
#     
#     # # Explore panel
#     # explore_server("explore", data_canale, reactive("canale_ind"),
#     #                var_right_canale, reactive(rv_canale$poly_selected),
#     #                reactive(rv_canale$zoom), reactive("CanALE index"))
#     
#     # Did-you-know panel
#     # dyk_server("dyk", reactive("alley_ind"), var_right_alley)
#     
#     # # Left map
#     # small_map_server("left", reactive(paste0(
#     #   "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
#     
#     # Bivariate legend
#     legend_bivar_server("dmti", var_right_dmti)
#     
#     # # Update map in response to variable changes or zooming
#     observeEvent({
#       var_left_dmti()
#       var_right_dmti()
#       rv_dmti$zoom}, {
#         width <- switch(rv_dmti$zoom, "CSD" = 100, "CT" = 10, 2)
#         mapdeck_update(map_id = NS(id, "map")) %>%
#           add_polygon(
#             data = data_dmti(), stroke_width = width,
#             stroke_colour = "#FFFFFF", fill_colour = "fill",
#             update_view = FALSE, id = "ID", auto_highlight = TRUE,
#             highlight_colour = "#FFFFFF90")
#       })
#     
#     # Update poly_selected on click
#     observeEvent(input$map_polygon_click, {
#       lst <- fromJSON(input$map_polygon_click)
#       if (is.null(lst$object$properties$id)) {
#         rv_dmti$poly_selected <- NA
#       } else rv_dmti$poly_selected <- lst$object$properties$id
#     })
#     
#     # # Clear poly_selected on zoom
#     observeEvent(rv_dmti$zoom, {rv_dmti$poly_selected <- NA},
#                  ignoreInit = TRUE)
#     
#     # Update map in response to poly_selected change
#     observeEvent(rv_dmti$poly_selected, {
#       if (!is.na(rv_dmti$poly_selected)) {
#         width <- switch(rv_dmti$zoom, "CSD" = 100, "CT" = 10, 2)
#         data_to_add <-
#           data_dmti() %>%
#           filter(ID == rv_dmti$poly_selected)
#         
#         mapdeck_update(map_id = NS(id, "map")) %>%
#           add_polygon(
#             data = data_to_add, stroke_width = width, stroke_colour = "#000000",
#             fill_colour = "fill", update_view = FALSE,
#             layer_id = "poly_highlight", auto_highlight = TRUE,
#             highlight_colour = "#FFFFFF90")
#       } else {
#         mapdeck_update(map_id = NS(id, "map")) %>%
#           clear_polygon(layer_id = "poly_highlight")
#       }
#     })
#     
#     # # Clear click status if prompted
#     # # (Namespacing hardwired to explore module; could make it return a reactive)
#     # observeEvent(input$`explore-clear_selection`, {
#     #   rv_dmti$poly_selected <- NA})
#     
#   })
# }
