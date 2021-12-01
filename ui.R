##### SUS UI SCRIPT ############################################################

ui <- function(request) {
  tagList(
  # Styling objects
  
  shinyjs::useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),
  tags$head(tags$style(HTML(styler))),
  tags$head(tags$style(HTML(navbar_js))),
  usei18n(i18n),
  
  # Settings button
  fixedPanel(
    id = "settings_button",
    style = "z-index: 1001; border-color: #FFFFFF00; background-color: #FFFFFF00;",
    top = 7, right = 150, width = 0,
      dropdownButton(inputId = "settings", icon = icon("tools"), circle = T, size = "sm",
        actionLink(inputId = "language_button", label = "EN/FR", icon = icon("globe")),
        actionLink(inputId = "._bookmark_", label = "Bookmark", icon = icon("link")),
        downloadLink("download_data", label = div(icon("download"), "Download data")),
        actionLink(inputId = "create_report", label = "Create a report", icon("file-pdf"))
        )),
  
  # Others
  shinyjs::useShinyjs(),
  waiter::use_waiter(),
  waiter::waiter_show_on_load(
    html = shiny::tagList(img(src = "Sus_logo_LoadPage.png", 
                              style = "height:50vh; max-height:600px;"), 
                          spin_folding_cube()), 
    color = "#D8F5FF"),
  
  # Navigation bar
  navbarPage(id = "sus_page", windowTitle = "SUS", 
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
                        value = "gentrification")),

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
               tabPanel("Green alleys", alley_UI("alley"), value = "alley")),
             
             tabPanel("Montréal stories", stories_UI("stories"),
                      value = "stories"),
             
             # tabPanel("Place explorer", "More to come!"),
             
             tabPanel("About", why_dash_UI("why_dash"), value = "why_dash")
             
  , collapsible = TRUE)
)
}