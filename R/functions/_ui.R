#### UI FUNCTIONS ##############################################################

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(theme,
        lapply(names(mods_rdy[[theme]]), function(module) {
          name <- sus_translate(r = r, module)
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
    x$title <- sus_translate(r = r, x$title)
    x$menuName <- sus_translate(r = r, x$menuName)
    x
  })
}

# ------------------------------------------------------------------------------

ready_modules_home <- function(mods_rdy) {
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(list(name = sus_translate(r = r, theme)),
        lapply(names(mods_rdy[[theme]]), function(module) {
          list(name = sus_translate(r = r, module), 
               onclick = paste0(
                 "openTab('", unname(mods_rdy[[theme]][module]), "')"))
        })
      )
    })
  
  lapply(list_args, \(x) do.call(linkListGroup, x))
  
}
