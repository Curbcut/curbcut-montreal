##### SUS UI SCRIPT ############################################################

ui <- tagList(
  # Styling objects
  
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML(styler))),
  tags$head(tags$style(HTML(navbar_js))),
  
  # Language button
  fixedPanel(
    id = "language_button",
    style = "z-index: 9998; border-color: #FFFFFF00; background-color: #FFFFFF00;",
    top = 7, right = 150, width = 0,
    tagList(usei18n(i18n), actionButton(
      "language_button", label = "EN/FR",
      style = "color: #3C3C3B; background-color: #0096C940;
        border-color: #FFFFFF;border-radius: 50px;
        border-width: 1px;  padding:7px; font-size:100%"))),
  
  # Others
  shinyjs::useShinyjs(),
  waiter::use_waiter(),
  waiter::waiter_show_on_load(
    html = shiny::tagList(img(src = "Sus_logo_LoadPage.png", 
                              style = "height:50vh; max-height:600px;"), 
                          spin_folding_cube()), 
    color = "#D8F5FF"),
  
  # Navigation bar
  navbarPage(id = "sus_page",
             title = actionLink("title", "Sus"),
             
             tabPanel("Home", home_UI("home")),
             
             navbarMenu("Climate",
                        tabPanel("Climate change risk", climate_risk_UI("climate_risk"))),
             
             navbarMenu("Covid",
                        tabPanel("Covid interventions", covid_UI("covid"))),
                        # tabPanel("Pedestrian realms", "More to come!")),
             
             # navbarMenu("Ecology",
                        # tabPanel("Biodiversity", "More to come!")),
             
             # navbarMenu("Health",
             #            tabPanel("Health", "More to come")),
                        # tabPanel("Healthy urban features", "More to come!")),
             
             navbarMenu("Housing",
                        tabPanel("Housing system", housing_UI("housing"))),
             
             navbarMenu("Policy",
                        tabPanel("Montréal climate plans", mcp_UI("mcp"))),
             
             navbarMenu("Transport",
                        tabPanel("Accessibility", access_UI("access")),
                        # tabPanel("Commuter mode switching", "More to come!"),
                        tabPanel("Road safety", crash_UI("crash"))),
             
             navbarMenu("Urban life",
                        tabPanel("Active living potential", canale_UI("canale")),
                        tabPanel("Green alleys", alley_UI("alley"))),
             
             tabPanel("Montreal stories", stories_UI("stories")),
             
             # tabPanel("Places explorer", "More to come!"),
             
             tabPanel("Why a dashboard", why_dash_UI("why_dash"))#,
             
             # tabPanel("Change language")
             
  , collapsible = T)
)