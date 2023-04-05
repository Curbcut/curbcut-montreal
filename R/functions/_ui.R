#### UI FUNCTIONS ##############################################################

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  mods_rdy <- mods_rdy[names(mods_rdy) != "Place explorer"]
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

# ------------------------------------------------------------------------------

ready_modules_home <- function(mods_rdy) {
  
  mods_rdy_img <- sapply(mods_rdy, \(x) sapply(x, \(y) {
    if (y %in% c("afford", "tenure", "dw_types", "demographics"))
      "centraide_logo/centraide_sm.png" else NULL
  }))
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(list(name = curbcut::cc_t(theme)),
        lapply(names(mods_rdy[[theme]]), function(module) {
          list(name = curbcut::cc_t(module), 
               onclick = paste0(
                 "openTab('", unname(mods_rdy[[theme]][module]), "')"),
               img = mods_rdy_img[[theme]][[module]])
        })
      )
    })
  
  lapply(list_args, \(x) do.call(linkListGroup, x))
  
}


# Stories -----------------------------------------------------------------


stories_dropdown_ui <- function(stories) {
  
  ind_stories <- lapply(stories$ID, \(x) {
    bslib::nav_item(tags$a(curbcut::cc_t(
                                stories$short_title[stories$ID == x]), 
                           onclick = paste0("openTab('stories');",
                                            "Shiny.setInputValue(`",
                                            "stories-select_nav`, '", x, "');"),
                           style = "cursor: pointer"))
  })
  
  do.call(bslib::nav_menu, c(
    # The title
    list(title = curbcut::cc_t(stories_page)),
    # The first element, see map
    list(tabPanel(curbcut::cc_t("SEE MAP"), 
                  stories_UI("stories"),
             value = "stories")),
    # Divider and header
    list(HTML("<li class='divider' style = 'padding:0px;margin:0px'></li>")),
    list(HTML(paste0("<li class='dropdown-header'>", 
                     curbcut::cc_t("Stories"), "</li>"))),
    # Individual stories
    ind_stories
  ))
}
