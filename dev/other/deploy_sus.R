deploy_sus <- function(app_name = "sus-mssi", ...) {
  
  stopifnot(app_name %in% c("sus-mssi", "sus-dev", "sus-dev2", "sus-dev3"))
  
  # Temporarily update server.R
  server_to_update <- readLines("server.R")
  which_line_s <- stringr::str_which(server_to_update, 
                                     'updateQueryString\\("\\?"\\)')
  server_to_update[which_line_s] <- r"(    # updateQueryString("?"))"
  writeLines(server_to_update, "server.R")
  
  # Set on.exit to restore server.R
  on.exit({
    server_to_update[which_line_s] <- r"(    updateQueryString("?"))"
    writeLines(server_to_update, "server.R")
  })
  
  if (app_name != "sus-mssi") { 
    
    # Temporarily update m_bookmark.R
    bookmark_to_update <- readLines("R/m_bookmark.R")
    which_line_b <- stringr::str_which(bookmark_to_update, 
                                       'updateQueryString\\(url\\)')
    if (app_name == "sus-dev") {
      bookmark_to_update[which_line_b] <- 
        r"(      updateQueryString(paste0("/sus-dev", url)))" 
    } else if (app_name == "sus-dev2") {
      bookmark_to_update[which_line_b] <- 
        r"(      updateQueryString(paste0("/sus-dev2", url)))" 
    } else if (app_name == "sus-dev3") {
      bookmark_to_update[which_line_b] <- 
        r"(      updateQueryString(paste0("/sus-dev3", url)))" 
    }
    writeLines(bookmark_to_update, "R/m_bookmark.R")
    
    # Set on.exit to restore server.R and m_bookmark.R
    on.exit({
      server_to_update[which_line_s] <- r"(    updateQueryString("?"))"
      writeLines(server_to_update, "server.R")
      bookmark_to_update[which_line_b] <- r"(      updateQueryString(url))"
      writeLines(bookmark_to_update, "R/m_bookmark.R")  
    })
  }
  
  
  # Deploy app
  rsconnect::deployApp(appName = app_name, forceUpdate = TRUE, ...)
  
}
