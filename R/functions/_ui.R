#### UI FUNCTIONS ##############################################################

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(theme,
        lapply(names(mods_rdy[[theme]]), function(module) {
          name <- cc_t(r = r, module)
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
    x$title <- cc_t(r = r, x$title)
    x$menuName <- cc_t(r = r, x$menuName)
    x
  })
}

# ------------------------------------------------------------------------------

ready_modules_home <- function(mods_rdy) {
  
  mods_rdy_img <- sapply(mods_rdy, \(x) sapply(x, \(y) {
    if (y %in% c("afford", "tenure", "dw_types", "demographics"))
      "centraide_logo/centraide_sm.png" else NULL
  }))
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(list(name = cc_t(r = r, theme)),
        lapply(names(mods_rdy[[theme]]), function(module) {
          list(name = cc_t(r = r, module), 
               onclick = paste0(
                 "openTab('", unname(mods_rdy[[theme]][module]), "')"),
               img = mods_rdy_img[[theme]][[module]])
        })
      )
    })
  
  lapply(list_args, \(x) do.call(linkListGroup, x))
  
}
