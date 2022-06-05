### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

home_UI <- function(id) {
  susPage(class = "sus-page-home", header = susBanner(), footer = susFooter(),
    susPageSectionFeature(
      susCarousel(
        susCarouselSlide(
          title = tags$em(nowrap(sus_translate(r = r, "Towards a")), 
                          nowrap(sus_translate(r = r, "sustainable city"))),
          preview = "Welcome",
          tags$p(sus_translate(r = r, "Sus is a platform for exploring urban ",
                              "sustainability in the Montreal region across ",
                              "multiple spatial and temporal scales. Sus ",
                              "offers a justice- and inclusivity-focused ",
                              "approach to sustainability which integrates ",
                              "the widest possible range of data sources to ",
                              "help researchers, policymakers, communities, ",
                              "and individuals.")),
          tags$div(class = "sus-button-group",
                  tags$a(class = "sus-button sus-icon-button sus-button-secondary", 
                         href = "#learn-more", sus_translate(r = r, "Learn more"), 
                         span(class = "material-icons", "auto_stories")),
                  tags$a(class = "sus-button sus-icon-button sus-button-primary", 
                         href = "#start-exploring", 
                         sus_translate(r = r, "Start Exploring Maps"), 
                         span(class = "material-icons", "travel_explore"))
          )
        ),
        susCarouselSlide(
          title = tags$em(nowrap(sus_translate(r = r, "Another slide!"))),
          # preview = "Another slide",
          tags$p(sus_translate(r = r, "Sus is a platform for inventing urban ",
                               "sustainability in the Montreal region across ",
                               "multiple spatial and temporal scales. Sus ",
                               "offers a justice- and inclusivity-focused ",
                               "approach to sustainability which integrates ",
                               "the widest possible range of data sources to ",
                               "help researchers, policymakers, communities, ",
                               "and individuals.")),
          tags$div(class = "sus-button-group",
                   tags$a(class = "sus-button sus-icon-button sus-button-secondary",
                          href = "#learn-more", sus_translate(r = r, "Learn less"),
                          span(class = "material-icons", "auto_stories")),
                   tags$a(class = "sus-button sus-icon-button sus-button-primary",
                          href = "#start-exploring",
                          sus_translate(r = r, "Start Imagining Maps"),
                          span(class = "material-icons", "travel_explore"))
          )
        )
      )
    ),
    susPageSection(
      tags$h2(sus_translate(r = r, "About Sus"), 
              scrollAnchor(id = "learn-more")),
      tags$p(sus_translate(
        r = r, "Sus embraces an inclusive vision of urban ",
        "sustainability, allowing users to pose questions ",
        "about environmental issues and contextualize them ",
        "within larger frameworks of equity and ",
        "accessibility. It serves as both a data-exploration ",
        "tool and a knowledge and information-sharing ",
        "resource, designed to encourage greater reflection ",
        "on urban sustainability challenges, and on the ",
        "communities which are most affected by them.")),
      tags$p(sus_translate(
        r = r, "Sus is organized into thematic and place-based ",
        "“modules”, each of which takes a narrow slice of ",
        "our data and presents it in a way designed to ",
        "answer existing questions and provoke new ones. ",
        "What is the relationship between heat risk and ",
        "housing tenure? Does my neighbourhood have better ",
        "or worse active transport options than the rest ",
        "of the city? What is the history of environmental ",
        "gentrification in Little Burgundy? The majority ",
        "of the data is publicly available, and over time ",
        "we will be adding more tools for users to export ",
        "the data and use it themselves.")),
      tags$p(HTML(paste0(
        sus_translate(r = r, "See the "),
        "<a style ='cursor:pointer;' onclick = openTab('how_to_use')>",
        sus_translate(r = r, "“How to use”"),"</a>",
        sus_translate(r = r, "page for more information on ",
        "how Sus works. And see the "),
        "<a style ='cursor:pointer;' onclick = openTab('authors')>",
        sus_translate(r = r, "“Authors”"), "</a>", 
        sus_translate(r = r, " page to learn more about our team.")))),
      tags$p(class = "text-center", tags$em(
        sus_translate(r = r, "An initiative of the "),
        HTML(paste0("<a href = 'https://www.mcgill.ca/mssi/'>",
                    sus_translate(r = r, 
                                  "McGill Sustainability Systems Initiative"), 
                    "</a>."))
      ))
    ),
    susPageSection(
      tags$h2(sus_translate(r = r, "Maps"), 
              scrollAnchor(id = "start-exploring")),
      tags$div(
        class = "text-width", 
        do.call(linkList, c(
          ready_modules_home(mods_rdy), 
          list(linkListGroup(
            name = sus_translate(r = r, "More"), 
            list(name = sus_translate(r = r, "Montréal stories"), 
                 onclick = "openTab('stories')"),
            list(name = sus_translate(r = r, "Place explorer"), 
                 onclick = "openTab('place_explorer')"))))
      ))
    ), tags$div(style = "width: 250px; height: 50px;", hidden = "", susLegend())
  )
}


# Server ------------------------------------------------------------------

home_server <- function(id, session, r) {
  moduleServer(id, function(input, output, session) {
    
    bookmark_server(id = "home", r = r)

  })
}
