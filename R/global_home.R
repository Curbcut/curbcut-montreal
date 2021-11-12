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
             "SUS is a comprehensive and inclusive urban sustainability dashboard ",
             "providing a platform for integrating, exploring, and analyzing the ", 
             "widest possible range of urban sustainability data sources pertaining ",
             "to the Montreal region. SUS offers a robust set of tools for scenario ", 
             "modelling and analysis across spatial and temporal scales, which can ", 
             "be of significant use to researchers, policymakers, communities and individuals.",
             "</center></b></h3>")))),
  fluidRow(hr()),
  fluidRow(h3("Relying on a unified data infrastructure, SUS additionally incorporates the necessary contextualization for users to place their personal or organisational questions into a larger framework of socioeconomic equity and accessibility. It therefore serves as both a data-exploration tool, as well as a knowledge and information-sharing resource, designed to encourage greater reflection on different urban sustainability issues, and which communities are most impacted by them.")),
  fluidRow(h3("The majority of the data used are publicly available and reproducible, aggregated into thematic and place-based modules to allow a range of users and stakeholders greater accessibility to answering sustainability questions. SUS further mobilizes both qualitative and quantitative research to bring questions without clear datasets into the discussion.")),
  fluidRow(h3("SUS aims to more accurately represent and better engage urban residents in order to harness the momentum surrounding technologically-based approaches to sustainability for public good.")),
  fluidRow(hr()),
  fluidRow(
    img(src = "mssi_logo.png", style = "height:10vh; max-height:70px"), 
    align = "center"),
  fluidRow(
    HTML(paste0(
      "<h5>An initiative of the <a href = 'https://www.mcgill.ca/mssi/'>McGill ",
      "Sustainability Systems Initiative</a></h5>")), align = "center")
))