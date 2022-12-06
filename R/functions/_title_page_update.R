### UPDATE TITLE PAGE DEPENDING ON LANGUAGE AND TAB VISITED ####################

title_page_update <- function(r, session, sus_page) {
  added_title <- if (sus_page %in% unlist(mods_rdy)) {
    unlist(unname(mods_rdy))[sus_page == unlist(mods_rdy)] |> 
      names()
  } else if (sus_page %in% c("about_sus", "how_to_use", "authors",
                                   "place_explorer", "stories")) {
    switch(sus_page,  
           "about_sus" = "About Curbcut",
           "how_to_use" = "How to use",
           "authors" = "Authors",
           "place_explorer" = "Place explorer",
           "stories" = "Montréal stories")
  }
  
  construct_title <- 
    paste0(site_name, 
           if (!is.null(added_title)) {
             paste0(" - ", cc_t(r = r, added_title))})
  
  session$sendCustomMessage("changetitle", construct_title)
}
