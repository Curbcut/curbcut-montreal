### CANALE MODULE ##############################################################

ready_modules_home <- function(mods_rdy) {
  
  list_args <- 
  map(1:length(mods_rdy), function(higher_theme) {
    c(list(name = sus_translate(names(mods_rdy[higher_theme]))),
      map(1:length(mods_rdy[[higher_theme]]), function(lower_theme) {
        list(name = sus_translate(names(mods_rdy[[higher_theme]][lower_theme])), 
             onclick = paste0("openTab('", mods_rdy[[higher_theme]][lower_theme], "')"))
      }))
  })
  
  map(list_args, ~{do.call(linkListGroup, .x)})
  
}

# UI ----------------------------------------------------------------------

home_UI <- function(id) {
  susPage(class = "sus-page-home", header = susBanner(), footer = susFooter(),
    susPageSectionFeature(
      tags$div(class = "sus-carousel",
        tags$div(class = "sus-carousel-slide",
          tags$h2(tags$em(nowrap(sus_translate("Towards a")), nowrap(sus_translate("sustainable city")))),
          
          actionLink(NS(id, "module_link"), "CLICK HERE for a CanALE + tenant-occupied bivariate comparison"),
          
          tags$p(sus_translate("SUS is a platform for integrating, exploring, ",
                               "and analyzing a wide range of urban sustainability ",
                               "data sources for the Montreal region across ",
                               "multiple spatial and temporal scales. SUS ",
                               "offers a robust set of tools for scenario ",
                               "modelling and analysis which will be useful ",
                               "for researchers, policymakers, communities, ",
                               "and individuals.")),
          tags$div(class = "sus-button-group",
            tags$a(class = "sus-button sus-icon-button sus-button-secondary", 
                   href = "#learn-more", sus_translate("Learn more"), 
                   span(class = "material-icons", "auto_stories")),
            tags$a(class = "sus-button sus-icon-button sus-button-primary", 
                   href = "#start-exploring", sus_translate("Start Exploring Maps"), 
                   span(class = "material-icons", "travel_explore"))
          )
        )
      )
    ),
    susPageSection(
      tags$h2(sus_translate("Statement"), scrollAnchor(id = "learn-more")),
      tags$p(sus_translate("SUS embraces an inclusive vision of urban sustainability, allowing ",
             "users to contextualize questions into larger frameworks of equity and ",
             "accessibility. It serves as both a data-exploration tool and a knowledge ",
             "and information-sharing resource, designed to encourage greater ",
             "reflection on different urban sustainability issues, and on the ",
             "communities which are most impacted by them.")),
      tags$p(sus_translate("The majority of the data used are publicly available and aggregated ",
             "into thematic and place-based modules to allow a range of stakeholders ",
             "greater accessibility to answering sustainability questions. SUS ",
             "further mobilizes both qualitative and quantitative research to bring ",
             "questions without clear datasets into the discussion.")),
      tags$p(sus_translate("SUS aims to engage Montrealers to harness the momentum surrounding ",
             "technologically-based approaches to sustainability for public good with ",
             "a vision towards making the City more socially inclusive and less ",
             "environmentally impactful.")),
      tags$p(class = "text-center", tags$em(sus_translate("An initiative of the "),
        HTML(paste0("<a href = 'https://www.mcgill.ca/mssi/'>",
        sus_translate("McGill Sustainability Systems Initiative"), "</a>."))
      ))
    ),
    susPageSection(
      tags$h2(sus_translate("Maps"), scrollAnchor(id = "start-exploring")),
      do.call(linkList, c(ready_modules_home(mods_rdy),
                          list(linkListGroup(name = sus_translate("More"),
                                             list(name = sus_translate("Montreal stories"), 
                                                  onclick = "openTab('stories')"),
                                             list(name = sus_translate("Place explorer"), 
                                                  onclick = "openTab('place_explorer')"))))
      )
    ), tags$div(style = "width: 250px; height: 50px;", hidden = "", susLegend())
  )
}


# Server ------------------------------------------------------------------

home_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$module_link, {
      module_link("canale", zoom = 9.5, location = c(-74.037371, 45.570801),
                  select_id = "24740044", var_right = "housing_tenant_pct",
                  zoom_auto = FALSE, df = "DA")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    bookmark_server(id = "home")

  })
}
