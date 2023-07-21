#### UI FUNCTIONS ##############################################################

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  # Alphabetical order
  mods_rdy <- mods_rdy[names(mods_rdy)[order(names(mods_rdy))]]
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(theme,
        lapply(names(mods_rdy[[theme]]), function(module) {
          name <- curbcut::cc_t(module)
          key <- unname(mods_rdy[[theme]][module])
          tabPanel(name,
                   do.call(paste0(key, "_UI"), list(key)),
                   value = key)
        })
      )
    })
  
  # navbarMenu creation
  out <- lapply(list_args, \(x) do.call(navbarMenu, x)) 
  
  # Translate and return
  lapply(out, function(x) {
    x$title <- curbcut::cc_t(x$title)
    x$menuName <- curbcut::cc_t(x$menuName)
    x
  })
}
