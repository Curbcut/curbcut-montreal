#### UI FUNCTIONS ##############################################################

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  list_args <-
    lapply(1:length(mods_rdy), function(higher_theme) {
      c(names(mods_rdy[higher_theme]),
        lapply(1:length(mods_rdy[[higher_theme]]), function(lower_theme) {
          name <- sus_translate(names(mods_rdy[[higher_theme]][lower_theme]))
          key <- unname(mods_rdy[[higher_theme]][lower_theme])
          tabPanel(name,
                   eval(parse(text = paste0(key, "_UI('", key, "')"))),
                   value = key)
        })
      )
    })
  
  map(list_args, ~{do.call(navbarMenu, .x)}) |> 
    map(~{
      .x$title <- sus_translate(.x$title)
      .x$menuName <- sus_translate(.x$menuName)
      .x
    })
  
}
