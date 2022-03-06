deploy_sus <- function() {
  
  # Temporarily update server.R
  server_to_update <- readLines("server.R")
  which_line_s <- str_which(server_to_update, 'updateQueryString\\("\\?"\\)')
  server_to_update[which_line_s] <- r"(    # updateQueryString("?"))"
  writeLines(server_to_update, "server.R")
  
  # Temporarily update m_bookmark.R
  bookmark_to_update <- readLines("R/m_bookmark.R")
  which_line_b <- str_which(bookmark_to_update, 'updateQueryString\\(url\\)')
  bookmark_to_update[which_line_b] <- 
    r"(      updateQueryString(paste0("/sus-mssi", url)))"
  writeLines(bookmark_to_update, "R/m_bookmark.R")
  
  # Deploy app
  rsconnect::deployApp(appName = "sus-mssi", forceUpdate = TRUE)
  
  # Restore server.R and m_bookmark.R
  server_to_update[which_line_s] <- r"(    updateQueryString("?"))"
  writeLines(server_to_update, "server.R")
  bookmark_to_update[which_line_b] <- r"(      updateQueryString(url))"
  writeLines(bookmark_to_update, "R/m_bookmark.R")
  
}
