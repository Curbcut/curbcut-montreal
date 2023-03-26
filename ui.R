##### SUS UI SCRIPT ############################################################

ui <- function(request) {
  tagList(
    
    # Import packages dependencies -----------------------------------------------
    
    useShinyjs(),
    useSever(),
    

    # Styling objects ------------------------------------------------------------
    
    tags$head(tags$link(rel = "icon", href = "favicon.ico")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "bootstrap.min.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "stevens.greenblue.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.palette.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.icons.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.navbar.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.footer.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.button.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = "sus.carousel.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.pages.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.banner.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.linklist.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.home.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.maps.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.authors.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.stories.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "sus.news.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = "sus.place_explorer.css")),
    tags$head(tags$script(src = "sus.js")),
    tags$head(tags$script(src = "shinybrowser.js")),
    tags$head(tags$script(js_links_between_modules)),
    tags$head(tags$style(HTML(styler))),
    
    # Curbcut scripts
    curbcut::use_curbcut_cookie(),
    curbcut::use_curbcut_js(),
    curbcut::use_curbcut_css(),
    
    tags$head(tags$style("span.text {display: block !important;}")),
    
    # Google analytics
    tags$head(includeHTML("www/google_analytics.html")),

    # Language switching ---------------------------------------------------------
    
    # Add a class to the body, to toggle between languages
    tags$body(class = "user-lang-fr"),
    # Add visible and hidden to classes, to switch between active language
    tags$head(tags$style(HTML(lang_classes))),
    
    
    # Sharing card ---------------------------------------------------------------
    
    meta() |> 
      meta_social(
        title = paste0("Welcome | ", site_name, " | Towards a sustainable city"),
        description = paste0(site_name, " is a platform for exploring ",
                             "urban sustainability across ",
                             "multiple spatial and temporal scales."),
        url = "https://montreal.curbcut.ca",
        image = "share.jpg",
        image_alt = paste0("A photo of a winding footpath through a verdant ",
                           "Montreal alley."),
        twitter_creator = "@susmontreal",
        twitter_card_type = "summary",
        twitter_site = "@susmontreal"
      ),
    
    # Navigation bar -------------------------------------------------------------
    
    do.call(
      navbarPageWithInputs, 
      c(list(id = "cc_page", 
             windowTitle = site_name, 
             title = actionLink("title", "Curbcut"),
             tabPanel(curbcut::cc_t("Home"), home_UI("home"), value = "home")),
        ready_modules_ui(mods_rdy),
        list(stories_dropdown_ui(stories),
             tabPanel(curbcut::cc_t("Place explorer"), 
                      place_explorer_UI("place_explorer"),
                      value = "place_explorer"),
             navbarMenu(curbcut::cc_t("About"),
                        tabPanel(curbcut::cc_t("About Curbcut"), 
                                 about_sus_UI("about_sus"), value = "about_sus"),
                        tabPanel(curbcut::cc_t("How to use"), how_to_use_UI("how_to_use"), 
                                 value = "how_to_use"),
                        tabPanel(curbcut::cc_t("Authors"), authors_UI("authors"), value = "authors")),
             collapsible = TRUE,
             inputs = list(
               # Language toggle
               curbcut::language_UI(),
               # Actions dropdown
               curbcut::settings_UI()
             )
        ))
    )
  )
}
