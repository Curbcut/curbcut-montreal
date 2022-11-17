# #### PEDESTRIAN MODULE #########################################################
# 
# # UI ----------------------------------------------------------------------
# 
# ped_UI <- function(id) {
#   return(tagList(
#   div(class = "mapdeck_div", 
#                mapdeckOutput(NS(id, "map"), height = "100%")),
#           title_UI(NS(id, "title")),
#           right_panel(id,
#           #             compare_UI(NS(id, "ped"), var_list_ped),
#           #             explore_UI(NS(id, "explore")),
#                       dyk_UI(NS(id, "dyk")))#,
#           # legend_bivar_UI(NS(id, "climate_risk")))
#   ))
#   }
# 
# 
# # Server ------------------------------------------------------------------
# 
# ped_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     
#     # Title bar
#     title_server("title", "ped")
#     
#     # Map
#     output$map <- renderMapdeck({
#       mapdeck(style = "mapbox://styles/skohn90/ckgjqwg1w00bv1bmorr5oad7q",
#               token = token_ped, zoom = map_zoom, location = map_loc)
#     })
#     
#     # Zoom level
#     observeEvent(input$map_view_change$zoom, {
#       rv_ped$zoom <- case_when(
#         input$map_view_change$zoom >= 14 ~ "FINAL",
#         input$map_view_change$zoom >= 10.5 ~ "IN",
#         TRUE ~ "OUT")
#     })
#     
#     # Translate drop-down lists
#     observe({
#       updateSelectInput(session = session, inputId = "var_right",
#                         choices = cc_t(r = r, var_list_ped))
#     })
#     
#     observe({
#       updateSelectInput(session = session,
#                         inputId = "var_slider",
#                         choices = cc_t(r = r, var_list_ped_slider))
#     })
#     
#     
#     
#   })
# }
