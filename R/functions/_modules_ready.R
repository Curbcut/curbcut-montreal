##### MODULES READY FUNCTION ###################################################


# ui.R version ------------------------------------------------------------

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  list(
    tabPanel("Home", home_UI("home"), value = "home"),
    
    map(1:length(mods_rdy), function(higher_theme) {
      navbarMenu(
        names(mods_rdy[higher_theme]),
        map(1:length(mods_rdy[[higher_theme]]), function(lower_theme) {
          name <- names(mods_rdy[[higher_theme]][lower_theme])
          key <- unname(mods_rdy[[higher_theme]][lower_theme])
          tabPanel(name,
                   eval(parse(text = paste0(key, "_UI('", key, "')"))),
                   value = key)
        })
      )
    }),
    
    map(1:length(stand_alone_tabs), function(tab) {
      name <- names(stand_alone_tabs[tab])
      key <- unname(stand_alone_tabs[tab])
      tabPanel(name, 
               eval(parse(text = paste0(key, '_UI("', key, '")'))),
               value = key)
    })
  )
  
}


# m_home.R version --------------------------------------------------------

