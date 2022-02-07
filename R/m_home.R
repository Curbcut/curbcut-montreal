### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

home_UI <- function(id) {
  tags$div(class="sus-page sus-page-home",
    tags$div(class="sus-banner noselect",
      tags$div(class="sus-banner-bg sus-bg-img-map"),
      tags$div(class="sus-banner-bg sus-bg-img-skyline"),
      tags$h1(class="sus-brand sus-banner-text", "SUS")
    ),
    tags$div(class="sus-page-content",
      tags$div(class="sus-page-content-section",
        tags$div(class="sus-carousel",
          tags$div(class="sus-carousel-slide",
            tags$h2(tags$em("Towards a sustainable city")),
            tags$p("SUS is a platform for integrating, exploring, and analyzing a wide range of urban sustainability data sources for the Montreal region across multiple spatial and temporal scales. SUS offers a robust set of tools for scenario modelling and analysis which will be useful for researchers, policymakers, communities, and individuals."),
            tags$div(class="sus-button-group",
              tags$button(class="sus-button sus-icon-button sus-button-secondary", span("Learn More"), span(class="material-icons", "auto_stories")),
              tags$button(class="sus-button sus-icon-button sus-button-primary", span("Start Exploring Maps"), span(class="material-icons", "travel_explore"))
            )
          )
        )
      ),
      tags$div(class="sus-page-content-section",
        tags$h2("Statement"),
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
      tags$div(class="sus-page-content-section",
        tags$h2("Maps"),
        tags$div(class="sus-maps-list",
          tags$div(class="sus-maps-list-group",
            tags$h3("Climate"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Climate risk"))
            )
          ),
          tags$div(class="sus-maps-list-group",
            tags$h3("Covid"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Covid interventions"))
            )
          ),
          tags$div(class="sus-maps-list-group",
            tags$h3("Housing"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Housing system")),
              tags$li(tags$a(class="noselect", "Gentrification")),
              tags$li(tags$a(class="noselect", "Permits")),
              tags$li(tags$a(class="noselect", "Marketed sustainability"))
            )
          ),
          tags$div(class="sus-maps-list-group",
            tags$h3("Policy"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Montreal climate plans"))
            )
          ),
          tags$div(class="sus-maps-list-group",
            tags$h3("Transport"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Accessibility")),
              tags$li(tags$a(class="noselect", "Road safety"))
            )
          ),
          tags$div(class="sus-maps-list-group",
            tags$h3("Urban Life"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Active greening potential")),
              tags$li(tags$a(class="noselect", "Green alleys")),
              tags$li(tags$a(class="noselect", "Green spaces"))
            )
          ),
          tags$div(class="sus-maps-list-group",
            tags$h3("More"),
            tags$ul(class="sus-map-link-group",
              tags$li(tags$a(class="noselect", "Active greening potential")),
              tags$li(tags$a(class="noselect", "Green alleys")),
              tags$li(tags$a(class="noselect", "Green spaces"))
            )
          ),
        )
      )
    ),
    tags$div(class="sus-page-footer",
      tags$div(class="sus-page-footer-content",
        tags$div(class="sus-page-footer-logos",
          tags$img(class="sus-page-footer-logo", src="mcgill-mssi-logo-final.png")
        ),
        tags$div(class="sus-page-footer-links",
          tags$ul(
            tags$li(tags$a(href="", "About")),
            tags$li(tags$a(href="", "Terms & Conditions")),
            tags$li(tags$a(href="", "Privacy Policy")),
            tags$li(tags$a(href="", "Contact"))
          )
        )
      )
    )
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
