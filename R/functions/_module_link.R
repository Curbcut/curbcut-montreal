#### LINKS BETWEEN MODULES #####################################################

module_link <- function(r, module, zoom = NULL, location = map_loc, 
                        select_id = NA, var_left = NULL,
                        var_right = NULL, df = NULL, 
                        zoom_auto = NULL, more_args = NULL, 
                        update_view = if (is.na(select_id)) FALSE else TRUE) {
  
  r$sus_link$id <- module
  
  # Open the link to the linked module
  r$link <- module
  
  # Update view
  r$sus_link$zoom <- NULL
  r$sus_link$location <- NULL
  
  if (update_view) {
    if (!is.null(df) && !is.null(select_id)) {
      r$sus_link$zoom <- 
        get(paste("map_zoom_levels", r$geo(), sep = "_"))[[
          gsub(".*_", "", df)]] + 0.75
      
      r$sus_link$location <- if (is.na(select_id)) location else {
        sapply(do.call("dbGetQuery", list(rlang::sym(paste0(df, "_conn")),
                                          paste0("SELECT lat, lon FROM centroid ",
                                                 "WHERE ID = ", select_id))) |> 
                 unlist(),
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
  if (!is.null(df)) {
    r$sus_link$df <- df
    r[[module]]$df(df)
  }
  if (!is.null(select_id)) {
    r$sus_link$select_id <- select_id
    r[[module]]$select_id(select_id)
  }
  
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
