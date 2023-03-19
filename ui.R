##### SUS UI SCRIPT ############################################################

ui <- function(request) {
  
  
  tagList(
    
    # Import packages dependencies -----------------------------------------------
    
    useShinyjs(),
    useSever(),
    

    # Styling objects ------------------------------------------------------------
    
    tags$head(tags$link(rel = "icon", href = "favicon.ico")),    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "bootstrap.min.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stevens.greenblue.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.palette.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.floatingcontrols.css")), #ADDED STYLING FOR FLOATING CONTROLS #
    tags$head(tags$link(rel = "stylesheet", type = "text/css",href = "sus.scrollbars.css")),        #SCROLL BARS STYLING
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.icons.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.navbar.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.footer.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.button.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.carousel.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.pages.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.banner.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.linklist.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.home.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.maps.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.authors.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.stories.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.news.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.place_explorer.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = paste0("https://fonts.googleapis.com/icon?", "family=Material+Icons"))),
    tags$head(tags$script(src = "sus.js")),
    tags$head(tags$script(src = "shinybrowser.js")),
    tags$head(tags$script(src = "movemenu")),
    # Cookie js script
    curbcut::use_curbcut_cookie(),
    tags$head(tags$script(js_links_between_modules)),
    curbcut::use_curbcut_js(),
    # tags$head(tags$script(curbcut::js_copy_current_url())),
    tags$head(tags$style(HTML(styler))),
    # # To allow screenshot
    # tags$script(src = paste0("https://cdn.jsdelivr.net/npm/html2canvas@1.0.0-",
    #                          "rc.5/dist/html2canvas.min.js")),
    # extendShinyjs(text = screenshot_js, functions = "takeShot"),
    # tags$script(HTML('HTMLCanvasElement.prototype.getContext = function(origFn) {
    #   return function(type, attribs) {
    #     attribs = attribs || {};
    #     attribs.preserveDrawingBuffer = true;
    #     return origFn.call(this, type, attribs);
    #   };
    # }(HTMLCanvasElement.prototype.getContext);')),
    
    # change page title JS function
    tags$script(HTML('Shiny.addCustomMessageHandler("changetitle", function(x) 
                   {document.title=x});')),
    
    # To allow div elements with title attribute, for dropdown text on hover
    tags$script(
      "var myDefaultWhiteList = $.fn.selectpicker.Constructor.DEFAULTS.whiteList;
    myDefaultWhiteList.div = ['title'];"
    ),
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
    ),
    # Floating controls Giovanni
    shiny::HTML('<div class="floating-panel">
        <div class="hidden-icons">
          <div class="icon help">
            <img class="icon" src="magnifying-glass-icon.png">
            <span class="help-text">Map</span>
          </div>
          <div class="icon help">
            <img class="icon" src="map-icon.png">
            <span class="help-text">Data</span>
          </div>
          <div class="icon help">
            <img class="icon" src="export-icon.png">
            <span class="help-text">Export</span>
          </div>
        </div>
      </div>
     ')
  )
}
