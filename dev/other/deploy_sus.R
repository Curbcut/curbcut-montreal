deploy_sus <- function(app_name = "sus-mssi", ...) {
  
  stopifnot(app_name %in% c("sus-mssi", "sus-dev", "sus-dev2"))
  
  # Temporarily update server.R
  server_to_update <- readLines("server.R")
  which_line_s <- stringr::str_which(server_to_update, 'updateQueryString\\("\\?"\\)')
  server_to_update[which_line_s] <- r"(    # updateQueryString("?"))"
  writeLines(server_to_update, "server.R")
  
  # Temporarily update m_bookmark.R
  bookmark_to_update <- readLines("R/m_bookmark.R")
  which_line_b <- stringr::str_which(bookmark_to_update, 'updateQueryString\\(url\\)')
  if (app_name == "sus-mssi") {
    bookmark_to_update[which_line_b] <- 
      r"(      updateQueryString(paste0("/sus-mssi", url)))"   
  } else if (app_name == "sus-dev") {
    bookmark_to_update[which_line_b] <- 
      r"(      updateQueryString(paste0("/sus-dev", url)))" 
  } else if (app_name == "sus-dev2") {
    bookmark_to_update[which_line_b] <- 
      r"(      updateQueryString(paste0("/sus-dev2", url)))" 
  }
  writeLines(bookmark_to_update, "R/m_bookmark.R")
  
  # Deploy app
  tryCatch(rsconnect::deployApp(appName = app_name, forceUpdate = TRUE, ...),
           error = function(e) cat("DEPLOY ERROR"))
  
  # Restore server.R and m_bookmark.R
  server_to_update[which_line_s] <- r"(    updateQueryString("?"))"
  writeLines(server_to_update, "server.R")
  bookmark_to_update[which_line_b] <- r"(      updateQueryString(url))"
  writeLines(bookmark_to_update, "R/m_bookmark.R")
  
}
