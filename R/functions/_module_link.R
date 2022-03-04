#### LINKS BETWEEN MODULES #####################################################

module_link <- function(module, zoom = NULL, location = map_location, 
                        select_id = NULL, var_left = NULL,
                        var_right = NULL, df = NULL, 
                        zoom_auto = NULL, more_args = NULL) {
  
  sus_link$mod_ns <- paste(module, module, sep = "-")
  
  # Open the link to the linked moule
  sus_rv$link <- reactive(module)
  # Tweak map namespace
  sus_link$map_id <- paste(module, "map", sep = "-")
  
  # Other values 
  sus_link$activity <- 
    if (is.null(sus_link$activity)) 0 else sus_link$activity + 1
  sus_link$zoom <- zoom
  sus_link$location <- location
  sus_link$var_left <- var_left
  sus_link$var_right <- var_right
  sus_link$df <- df
  sus_link$zoom_auto <- zoom_auto
  sus_link$more_args <- more_args
  
  sus_link$select_id <- select_id
  
}


# EXAMPLE ON HOW TO MAKE A LINK:

# UI
# actionLink(NS(id, "module_link"), "CLICK HERE for a CanALE + tenant-occupied bivariate comparison"),

# SERVER
# observeEvent(input$module_link, {
#   module_link("canale", zoom = 9.5, location = c(-74.037371, 45.570801),
#               select_id = "24740044", var_right = "housing_tenant_pct",
#               zoom_auto = FALSE, df = "DA")
# }, ignoreInit = TRUE, ignoreNULL = TRUE)