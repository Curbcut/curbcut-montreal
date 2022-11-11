### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

home_UI <- function(id) {
  susPage(class = "sus-page-home", header = susBanner(), footer = susFooter(),
    susPageSectionFeature(
      susCarousel(
        susCarouselSlide(
          title = tags$em(nowrap("Centraide x"), 
                          nowrap("Curbcut")),
          preview = cc_t(r = r, "Centraide"),
          tags$p(cc_t(r = r, "In a novel collaboration, Centraide of Greater ",
                      "Montreal is partnering with Curbcut on a series of ",
                      "housing maps. Centraide is using its social expertise ",
                      "and data to help target and interpret housing issues, ",
                      "a decisive factor in poverty and social exclusion.")),
          tags$div(class = "sus-button-group",
                   tags$a(class = "sus-button sus-icon-button sus-button-secondary", 
                          href = "#", onclick = "openTab('afford')",
                          cc_t(r = r, "Housing affordability"), 
                          span(class = "material-icons", "location_city")),
                   tags$a(class = "sus-button sus-icon-button sus-button-primary", 
                          href = "https://www.centraide-mtl.org/", 
                          cc_t(r = r, "Centraide"), target = "_blank",
                          img(src = "centraide_logo/centraide_sm.png",
                              style = paste0("display:inline; height:20px; ",
                                             "float:right; margin-top:auto; ",
                                             "margin-bottom:auto; margin-left:10px")))
          )
        ),
        susCarouselSlide(
          title = tags$em(nowrap(cc_t(r = r, "Towards a")), 
                          nowrap(cc_t(r = r, "sustainable city"))),
          preview = cc_t(r = r, "Welcome"),
          tags$p(cc_t(r = r, "Curbcut is a platform for exploring urban ",
                              "sustainability in the Montreal region across ",
                              "multiple spatial and temporal scales. Curbcut ",
                              "offers a justice- and inclusivity-focused ",
                              "approach to sustainability which integrates ",
                              "the widest possible range of data sources to ",
                              "help researchers, policymakers, communities, ",
                              "and individuals.")),
          tags$div(class = "sus-button-group",
                  tags$a(class = "sus-button sus-icon-button sus-button-secondary", 
                         href = "#learn-more", cc_t(r = r, "Learn more"), 
                         span(class = "material-icons", "auto_stories")),
                  tags$a(class = "sus-button sus-icon-button sus-button-primary", 
                         href = "#start-exploring", 
                         cc_t(r = r, "Start Exploring Maps"), 
                         span(class = "material-icons", "travel_explore"))
          )
        ), susCarouselSlide(
          title = tags$em(tags$em(nowrap(cc_t(r = r, "Get notified about")), 
                                  nowrap(cc_t(r = r, "the 2021 Census")))),
          preview = cc_t(r = r, "2021 Census data"),
          tags$p(cc_t(r = r, "Many of the topics that can be explored on Curbcut draw on data from ",
                      "the Canadian Census. The Census of Population is conducted ",
                      "every five years and provides statistical information ",
                      "about demographic, social and economic characteristics.")),
          tags$p(cc_t(r = r, "Sign up to our newsletter to get notified when ",
                      "the 2021 Census data is available on Curbcut!")),
          tags$div(class = "sus-button-group",
                   tags$a(class = "sus-button sus-icon-button sus-button-primary",
                          style = "cursor: pointer;",
                          id = "sign_up_from_carousel", 
                          cc_t(r = r, "Sign up!"),
                          span(class = "material-icons", "email"))
          )
        )
      )
    ),
    susPageSection(
      tags$h2(cc_t(r = r, "About Curbcut"), 
              scrollAnchor(id = "learn-more")),
      tags$p(cc_t(
        r = r, "Curbcut embraces an inclusive vision of urban ",
        "sustainability, allowing users to pose questions ",
        "about environmental issues and contextualize them ",
        "within larger frameworks of equity and ",
        "accessibility. It serves as both a data-exploration ",
        "tool and a knowledge and information-sharing ",
        "resource, designed to encourage greater reflection ",
        "on urban sustainability challenges, and on the ",
        "communities which are most affected by them.")),
      tags$p(cc_t(
        r = r, "Curbcut is organized into thematic and place-based ",
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
        cc_t(r = r, "See the "),
        "<a style ='cursor:pointer;' onclick = openTab('how_to_use')>",
        cc_t(r = r, "“How to use”"),"</a>",
        cc_t(r = r, "page for more information on ",
        "how Curbcut works. And see the "),
        "<a style ='cursor:pointer;' onclick = openTab('authors')>",
        cc_t(r = r, "“Authors”"), "</a>", 
        cc_t(r = r, " page to learn more about our team.")))),
      tags$p(class = "text-center", tags$em(
        cc_t(r = r, "An initiative of the "),
        HTML(paste0("<a href = 'https://www.mcgill.ca/mssi/'>",
                    cc_t(r = r, "McGill Sustainability Systems Initiative"), 
                    "</a>."))
      ))
    ),
    susPageSection(
      tags$h2(cc_t(r = r, "Maps"), 
              scrollAnchor(id = "start-exploring")),
      tags$div(
        class = "text-width", 
        do.call(linkList, c(
          ready_modules_home(mods_rdy), 
          list(linkListGroup(
            name = cc_t(r = r, "More"), 
            list(name = cc_t(r = r, "Montréal stories"), 
                 onclick = "openTab('stories')"),
            list(name = cc_t(r = r, "Place explorer"), 
                 onclick = "openTab('place_explorer')"))))
      ))
    ), tags$div(style = "width: 250px; height: 50px;", hidden = "", susLegend())
  )
}


# Server ------------------------------------------------------------------

home_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    bookmark_server(id = "home", r = r)

  })
}
