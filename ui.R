##### SUS UI SCRIPT ############################################################

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$div(class = "navbar-fixed", inputs)
  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form)
  navbar
}

materialIconButton <- function(tag, icon) {
  tag <- tagSetChildren(tag, .cssSelector = "button", span(class="material-icons", icon))
  tag
}

ui <- function(request) {
  tagList(
  # Styling objects
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "sus.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/icon?family=Material+Icons")),
  tags$head(tags$style(HTML(styler))),
  tags$head(tags$style(HTML(navbar_js))),
  usei18n(i18n),
  
  meta() %>%
    meta_social(
      title = "Welcome | MSSI SUS | Towards a sustainable city",
      description = "SUS is an online platform for integrating, exploring, and analyzing urban sustainability data sources for the Montreal region.",
      url = "https://e978-174-91-206-108.ngrok.io/",
      image = "https://e978-174-91-206-108.ngrok.io/share.jpg",
      image_alt = "A photo of a winding footpath through a verdant Montreal alley.",
      twitter_creator = "@McGillMSSI",
      twitter_card_type = "summary",
      twitter_site = "@McGillMSSI"
    ),
  
  # Navigation bar
  navbarPageWithInputs(id = "sus_page", windowTitle = "SUS", 
             title = actionLink("title", "SUS"),
             
             tabPanel("Home", home_UI("home"), value = "home"),
             
             navbarMenu(
               "Climate",
               tabPanel("Climate risk", climate_risk_UI("climate_risk"),
                        value = "climate_risk")),

             navbarMenu(
               "Covid",
               tabPanel("Covid interventions", covid_UI("covid"),
                        value = "covid")),
             
             navbarMenu(
               "Housing",
               tabPanel("Housing system", housing_UI("housing"), 
                        value = "housing"),
               tabPanel("Gentrification", gentrification_UI("gentrification"),
                        value = "gentrification"),
               tabPanel("Permits", permits_UI("permits"),
                        value = "permits"),
               tabPanel("Marketed Sustainability", 
                        marketed_sustainability_UI("marketed_sustainability"),
                        value = "marketed_sustainability")),
             
             navbarMenu(
               "Policy",
               tabPanel("Montréal climate plans", mcp_UI("mcp"),
                        value = "mcp")),

             navbarMenu(
               "Transport",
               tabPanel("Accessibility", access_UI("access"), value = "access"),
               tabPanel("Road safety", crash_UI("crash"), value = "crash")),
             
             navbarMenu(
               "Urban life",
               tabPanel("Active living potential", canale_UI("canale"),
                        value = "canale"),
               tabPanel("Green alleys", alley_UI("alley"), value = "alley"),
               tabPanel("Green space", green_space_UI("green_space"), 
                        value = "green_space")),
             
             tabPanel("Montréal stories", stories_UI("stories"),
                      value = "stories"),
             
             tabPanel("Place explorer", place_explorer_UI("place_explorer"),
                      value = "place_explorer"),
             
             tabPanel("About", why_dash_UI("why_dash"), value = "why_dash"),
             
  collapsible = TRUE,
  inputs = list(
      # Language toggle
      actionLink(
        inputId = "language_button",
        style="min-width: 108px;",
        label = span(span(class="material-icons", "language"), "English")),
      
      # Actions dropdown
      materialIconButton(dropdownButton(inputId = "settings",
        actionLink(inputId = "._bookmark_", label = "Bookmark", icon = icon("link")),
        actionLink(inputId = "download_data", label = "Data explanation and export", icon("download")),
        downloadLink("create_report", label = div(icon("file-pdf"), "Generate a report"))
        # actionLink(inputId = "contact", label = "Contact/feedback", icon("comment"))
      ), "summarize")
    )
  )
)
}