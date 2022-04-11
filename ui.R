##### SUS UI SCRIPT ############################################################

ui <- function(request) {
  tagList(
  # Styling objects
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stevens.greenblue.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.palette.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.navbar.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.footer.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.button.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.carousel.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.pages.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.banner.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.linklist.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.home.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.maps.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/icon?family=Material+Icons")),
  tags$head(tags$script(src = "sus.js")),
  tags$head(tags$script(src = "cookie.js")),
  tags$script(
    src = paste0(
      "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
      "dist/js.cookie.min.js"
    )
  ),
  tags$head(tags$script(js_links_between_modules)),
  tags$head(tags$script(bookmark_url)),
  tags$head(tags$style(HTML(styler))),
  
  # Add a class to the body, to toggle between languages.
  tags$body(class = "user-lang-fr"),
  # Add visible and hidden to classes, to switch between active language
  tags$head(tags$style(HTML(lang_classes))),
  # JS function to change body class when button is clicked
  extendShinyjs(text = set_ui_lang, functions = "setLanguage"),
  
  meta() |> 
    meta_social(
      title = "Welcome | SUS | Towards a sustainable city",
      description = paste0("SUS is an online platform for integrating, ",
                           "exploring, and analyzing urban sustainability ",
                           "data sources for the Montreal region."),
      url = "https://e978-174-91-206-108.ngrok.io/",
      image = "https://e978-174-91-206-108.ngrok.io/share.jpg",
      image_alt = "A photo of a winding footpath through a verdant Montreal alley.",
      twitter_creator = "@sus_montreal",
      twitter_card_type = "summary",
      twitter_site = "@sus_montreal"
    ),
  
  # Navigation bar
  do.call(
    navbarPageWithInputs, 
    c(list(id = "sus_page", 
           windowTitle = "SUS", 
           title = actionLink("title", "SUS"),
           tabPanel(sus_translate("Home"), home_UI("home"), value = "home")),
      ready_modules_ui(mods_rdy),
      list(tabPanel(sus_translate("Montréal stories"), stories_UI("stories"),
                    value = "stories"),
           tabPanel(sus_translate("Place explorer"), place_explorer_UI("place_explorer"),
           value = "place_explorer"),
           navbarMenu(sus_translate("About"),
                      tabPanel(sus_translate("About Sus"), why_dash_UI("why_dash"), value = "why_dash"),
           tabPanel(sus_translate("How to use"), how_to_use_UI("how_to_use"), value = "how_to_use")),
           collapsible = TRUE,
           inputs = list(
             # Language toggle
             actionLink(
               inputId = "language_button",
               style = "min-width: 112px;",
               label = span(span(class = "material-icons", "language"), 
                            span("English"))),
             
             # Actions dropdown
             materialIconButton(
               dropdownButton(inputId = "settings",
                              a(id = "bookmark",
                                class = "action-button shiny-bound-input",
                                role = "menuitem",
                                href = "#",
                                icon("link"), sus_translate("Bookmark"), 
                                onclick = "copyUrl()"),
                              # actionLink(inputId = "download_data", 
                              #            label = sus_translate(
                              #              "Data explanation and export"), 
                              #            icon("download")),
                              # downloadLink("create_report", 
                              #              label = div(icon("file-pdf"), 
                              #                           sus_translate(
                              #                            "Generate a report")))
                                               actionLink(inputId = "contact", label = "Contact/feedback", icon("comment"))
             ), "summarize")
           )
      ))
  )
)
}