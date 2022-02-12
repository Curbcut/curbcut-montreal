### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

home_UI <- function(id) {
  susPage(class="sus-page-home", header=susBanner(), footer=susFooter(),
    susPageSectionFeature(
      tags$div(class="sus-carousel",
        tags$div(class="sus-carousel-slide",
          tags$h2(tags$em("Towards a sustainable city")),
          tags$p("SUS is a platform for integrating, exploring, and analyzing a wide range of urban sustainability data sources for the Montreal region across multiple spatial and temporal scales. SUS offers a robust set of tools for scenario modelling and analysis which will be useful for researchers, policymakers, communities, and individuals."),
          tags$div(class="sus-button-group",
            tags$a(class="sus-button sus-icon-button sus-button-secondary", href="#learn-more", span("Learn More"), span(class="material-icons", "auto_stories")),
            tags$a(class="sus-button sus-icon-button sus-button-primary", href="#start-exploring", span("Start Exploring Maps"), span(class="material-icons", "travel_explore"))
          )
        )
      )
    ),
    susPageSection(
      tags$h2("Statement", id="learn-more"),
      tags$p("SUS embraces an inclusive vision of urban sustainability, allowing ",
             "users to contextualize questions into larger frameworks of equity and ",
             "accessibility. It serves as both a data-exploration tool and a knowledge ",
             "and information-sharing resource, designed to encourage greater ",
             "reflection on different urban sustainability issues, and on the ",
             "communities which are most impacted by them."),
      tags$p("The majority of the data used are publicly available and aggregated ",
             "into thematic and place-based modules to allow a range of stakeholders ",
             "greater accessibility to answering sustainability questions. SUS ",
             "further mobilizes both qualitative and quantitative research to bring ",
             "questions without clear datasets into the discussion."),
      tags$p("SUS aims to engage Montrealers to harness the momentum surrounding ",
             "technologically-based approaches to sustainability for public good with ",
             "a vision towards making the City more socially inclusive and less ",
             "environmentally impactful."),
      tags$p(class="text-center", tags$em("An initiative of the ",
        HTML("<a href='https://www.mcgill.ca/mssi/'>McGill Sustainability Systems Initiative</a>.")
      ))
    ),
    susPageSection(
      tags$h2("Maps", id="start-exploring"),
      linkList(
        linkListGroup(name="Housing",
         list(name="Housing system", url="#"),
         list(name="Gentrification", url="#"),
         list(name="Permits", url="#"),
         list(name="Marketed sustainability", url="#")
        ),
        linkListGroup(name="Urban Life",
         list(name="Active living potential", url="#"),
         list(name="Green alleys", url="#"),
         list(name="Green spaces", url="#")
        ),
        linkListGroup(name="Transport",
         list(name="Accessibility", url="#"),
         list(name="Road safety", url="#"),
         list(name="Green spaces", url="#")
        ),
        linkListGroup(name="Climate",
         list(name="Climate risk", url="#")
        ),
        linkListGroup(name="Covid",
         list(name="Covid interventions", url="#")
        ),
        linkListGroup(name="Policy",
         list(name="Montreal climate plans", url="#")
        ),
        linkListGroup(name="More",
         list(name="Montreal stories", url="#"),
         list(name="Place explorer", url="#")
        )
      )
    ), tags$div(style="width: 250px; height: 50px;", hidden="", susLegend()),
  )
  
  # fixedPage( 
  #           # tags$style('#home {background-color: #FFFFFF;}'),
  #           fluidRow(
  #             column(width = 4, img(src = "logo_large.png", 
  #                                   style = "height:30vh; max-height:600px; vertical-align:middle;"), 
  #                    align = "center"),
  #             column(width = 8, 
  #                    htmlOutput(NS(id, "first")))),
  #           fluidRow(hr()),
  #           fluidRow(htmlOutput(NS(id, "second"))),
  #           fluidRow(htmlOutput(NS(id, "third"))),
  #           fluidRow(htmlOutput(NS(id, "fourth"))),
  #           fluidRow(hr()),
  #           fluidRow(
  #             img(src = "mssi_logo.png", style = "height:10vh; max-height:70px"),
  #             align = "center"),
  #           fluidRow(htmlOutput(NS(id, "fifth")), align = "center")
  #         )
}


# Server ------------------------------------------------------------------

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$first <- renderText({
      paste0(
        sus_translate("<h1><b><center>SUS</center></b></h1>"),
        sus_translate("<h2><b><center>Towards a sustainable city</center></b></h2>"),
        sus_translate("<h3><b><center>",
          "SUS is a platform for integrating, exploring, and analyzing a ", 
          "wide range of urban sustainability data sources for the Montreal ",
          "region across multiple spatial and temporal scales. SUS offers ",
          "a robust set of tools for scenario modelling and analysis which ",
          "will be useful for researchers, policymakers, communities, and ",
          "individuals.</center></b></h3>"))
    })
    
    output$second <- renderText({
      HTML(sus_translate(
        "<h3>SUS embraces an inclusive vision of urban sustainability, allowing ",
        "users to contextualize questions into larger frameworks of equity and ",
        "accessibility. It serves as both a data-exploration tool and a knowledge ",
        "and information-sharing resource, designed to encourage greater ",
        "reflection on different urban sustainability issues, and on the ",
        "communities which are most impacted by them.</h3>"))
      })
    
    output$third <- renderText({
      HTML(sus_translate(
        "<h3>The majority of the data used are publicly available and aggregated ",
        "into thematic and place-based modules to allow a range of stakeholders ",
        "greater accessibility to answering sustainability questions. SUS ",
        "further mobilizes both qualitative and quantitative research to bring ",
        "questions without clear datasets into the discussion.</h3>"))
    })
    
    output$fourth <- renderText({
      HTML(sus_translate(
        "<h3>SUS aims to engage Montrealers to harness the momentum surrounding ",
        "technologically-based approaches to sustainability for public good with ",
        "a vision towards making the City more socially inclusive and less ",
        "environmentally impactful.</h3>"))
    })
    
    output$fifth <- renderText({
      sus_translate(
        "<h5>An initiative of the <a href = 'https://www.mcgill.ca/mssi/'>McGill ",
        "Sustainability Systems Initiative</a></h5>")
    })

  })
}
