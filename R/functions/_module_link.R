#### LINKS BETWEEN MODULES #####################################################

module_link <- function(module, zoom = NULL, location = map_location, 
                        select_id = NULL, var_right = NULL, df = NULL, 
                        zoom_auto = NULL, more_args = NULL) {
      
      if (sus_rv$active_tab() == module) {
        sus_link$mod_ns <- module
        # Tweak map namespace
        sus_link$map_id <- "map"
      } else {
        sus_link$mod_ns <- paste(module, module, sep = "-")
        # Open the link to the linked moule
        sus_rv$link <- reactive(module)
        # Tweak map namespace
        sus_link$map_id <- paste(module, "map", sep = "-")
      }
      
    # Send other reactives 
      sus_link$activity <- 
        if (is.null(sus_link$activity)) 0 else sus_link$activity + 1
      sus_link$zoom <- zoom
      sus_link$location <- location
      sus_link$var_right <- var_right
      sus_link$df <- df
      sus_link$zoom_auto <- zoom_auto
      sus_link$more_args <- more_args
      
      sus_link$select_id <- select_id

}
