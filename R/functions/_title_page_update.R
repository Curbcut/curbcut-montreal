### UPDATE TITLE PAGE DEPENDING ON LANGUAGE AND TAB VISITED ####################

title_page_update <- function(r, session, cc_page) {
  added_title <- if (cc_page %in% unlist(mods_rdy)) {
    unlist(unname(mods_rdy))[cc_page == unlist(mods_rdy)] |> 
      names()
  } else if (cc_page %in% c("about_sus", "how_to_use", "authors",
                                   "place_explorer", "stories")) {
    switch(cc_page,  
           "about_sus" = "About Curbcut",
           "how_to_use" = "How to use",
           "authors" = "Authors",
           "place_explorer" = "Place explorer",
           "stories" = "Montréal stories")
  }
  
  construct_title <- 
    paste0(site_name, 
           if (!is.null(added_title)) {
             paste0(" - ", curbcut::cc_t(lang = r$lang(), added_title))})
  
  session$sendCustomMessage("changetitle", construct_title)
}
