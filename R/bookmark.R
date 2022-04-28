# # Bookmarking
# onBookmark(function(state) {
#   
#   state$values$zoom_val <- zoom_val()
#   state$values$numeric_zoom <- input$map_view_change$zoom
#   state$values$location <- c(input$map_view_change$longitude,
#                              input$map_view_change$latitude)
#   state$values$poly_selected <- rv_climate_risk$poly_selected
#   state$values$var_right <- var_right()
#   state$values$var_left <- var_left()
# })
# 
# onRestored(function(state) {
#   restored_numeric_zoom <- state$values$numeric_zoom
#   restored_map_location <- state$values$location
#   zoom_val(state$values$zoom_val)
#   
#   output$map <- renderMapdeck({
#     mapdeck(
#       style = map_style, token = token_climate_risk,
#       zoom = restored_numeric_zoom, location = restored_map_location)
#   })
#   
#   updatePickerInput(
#     session = session,
#     inputId = NS(id, "compare-var"),
#     choices = sus_translate(make_dropdown(compare = TRUE)),
#     selected = state$values$var_right
#   )
#   
#   # Not working, no idea why?
#   updatePickerInput(
#     session = session,
#     inputId = NS(id, "left-var"),
#     choices = sus_translate(var_list_climate_risk),
#     selected = state$values$var_left
#   )
#   
#   if (input$grid) {map_change(NS(id, "map"), df = data, zoom = reactive("grid"))}
#   
#   rv_climate_risk$poly_selected <- state$values$poly_selected
# })
