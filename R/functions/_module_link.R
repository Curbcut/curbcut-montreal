#### LINKS BETWEEN MODULES #####################################################

module_link <- function(r, module, zoom = NULL, location = map_loc, 
                        select_id = NULL, var_left = NULL,
                        var_right = NULL, df = NULL, 
                        zoom_auto = NULL, more_args = NULL, 
                        update_view = TRUE) {
  
  r$sus_link$mod_ns <- paste(module, module, sep = "-")
  
  # Open the link to the linked module
  r$link <- module
  # Tweak map namespace
  r$sus_link$map_id <- paste(module, module, "map", sep = "-")
  
  # Update view
  r$sus_link$zoom <- NULL
  r$sus_link$location <- NULL
  
  if (update_view) {
    if (!is.null(df) && !is.null(select_id)) {
      r$sus_link$zoom <- 
        if (df == "borough") map_zoom else map_zoom_levels[[df]] + 0.75
      
      r$sus_link$location <- if (df == "grid") {
        sapply(
          as.numeric(dbGetQuery(db, paste0("SELECT centroid_lat, centroid_lon ",
                                           "FROM grid WHERE ID = ", select_id))),
          round, digits = 2)
      } else {
        data <- get(df)
        sapply(unlist(data[data$ID == select_id, ]$centroid),
               round, digits = 2)
      }
    }
  }
  
  # Other values 
  r$sus_link$activity <- 
    if (is.null(r$sus_link$activity)) 0 else r$sus_link$activity + 1
  r$sus_link$var_left <- var_left
  r$sus_link$var_right <- var_right
  r$sus_link$df <- df
  r$sus_link$zoom_auto <- zoom_auto
  r$sus_link$more_args <- more_args
  
  # Update destination select_id() and df()
  r[[module]]$df <- reactiveVal(df)
  r[[module]]$select_id <- reactiveVal(select_id)
  
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
