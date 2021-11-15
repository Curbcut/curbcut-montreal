home_tab <- 
  tabItem(tabName = "home", fluidPage(
  id = 'home', tags$style('#home {background-color: #FFFFFF;}'),
  fluidRow(
    column(width = 4, img(src = "logo_large.png", 
                          style = "height:35vh; max-height:600px;"), 
           align = "center"),
    column(width = 8, 
           HTML("<h1><b><center>SUS</center></b></h1>"), 
           HTML("<h2><b><center>Towards a sustainable city</center></b></h2>"), 
           HTML(paste0(
             "<h3><b><center>",
             "SUS is a platform for integrating, exploring, and analyzing a ", 
             "wide range of urban sustainability data sources for the Montreal ",
             "region across multiple spatial and temporal scales. SUS offers ",
             "a robust set of tools for scenario modelling and analysis which ",
             "will be useful for researchers, policymakers, communities, and ",
             "individuals.</center></b></h3>")))),
  fluidRow(hr()),
  fluidRow(HTML(paste0(
    "<h3>SUS embraces an inclusive vision of urban sustainability, allowing ",
    "users to contextualize questions into larger frameworks of equity and ",
    "accessibility. It serves as both a data-exploration tool and a knowledge ",
    "and information-sharing resource, designed to encourage greater ",
    "reflection on different urban sustainability issues, and on the ", 
    "communities which are most impacted by them.</h3>"))),
  fluidRow(HTML(paste0(
    "<h3>The majority of the data used are publicly available and aggregated ",
    "into thematic and place-based modules to allow a range of stakeholders ",
    "greater accessibility to answering sustainability questions. SUS ", 
    "further mobilizes both qualitative and quantitative research to bring ",
    "questions without clear datasets into the discussion.</h3>"))),
  fluidRow(HTML(paste0(
    "<h3>SUS aims to engage Montrealers to harness the momentum surrounding ",
    "technologically-based approaches to sustainability for public good with ",
    "a vision towards making the City more socially inclusive and less ",
    "environmentally impactful.</h3>"))),
  fluidRow(hr()),
  fluidRow(
    img(src = "mssi_logo.png", style = "height:10vh; max-height:70px"), 
    align = "center"),
  fluidRow(
    HTML(paste0(
      "<h5>An initiative of the <a href = 'https://www.mcgill.ca/mssi/'>McGill ",
      "Sustainability Systems Initiative</a></h5>")), align = "center")
))