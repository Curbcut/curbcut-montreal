#### LINKS BETWEEN MODULES #####################################################

module_link <- function(module, zoom = NULL, location = map_loc, 
                        select_id = NULL, var_left = NULL,
                        var_right = NULL, df = NULL, 
                        zoom_auto = NULL, more_args = NULL, 
                        update_view = TRUE) {
  
  sus_link$mod_ns <- paste(module, module, sep = "-")
  
  # Open the link to the linked moule
  sus_rv$link <- reactive(module)
  # Tweak map namespace
  sus_link$map_id <- paste(module, "map", sep = "-")
  
  # Update view
  sus_link$zoom <- NULL
  sus_link$location <- NULL
  
  if (update_view) {
    if (!is.null(df) && !is.null(select_id)) {
      sus_link$zoom <- 
        if (df == "borough") map_zoom else map_zoom_levels[[df]] + 0.75
      
      data <- st_set_agr(get(df), "constant")
      sus_link$location <- 
        data[data$ID == select_id, ] |> 
        st_centroid() |> 
        st_coordinates()
    }
  }
  
  # Other values 
  sus_link$activity <- 
    if (is.null(sus_link$activity)) 0 else sus_link$activity + 1
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