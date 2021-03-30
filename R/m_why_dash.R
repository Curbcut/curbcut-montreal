### WHY A DASHBOARD MODULE #####################################################

why_dash_UI <- function(id) {

  tabItem(
    
    tabName = "why_dash",

    tags$head(tags$style(HTML(
      '#why_title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}'))
      ),

    absolutePanel(
      id = "why_title_bar", class = "panel panel-default", draggable = FALSE,
      top = 70, left = 270, width = "50%",
      h2(i18n$t("Why a dashboard? The science behind Sus")),
      img(src = "glamour_shot.png", height = 300),
      p(i18n$t(paste0("Dashboards offer a tool for communicating ",
                      "sustainability data in a visually based digital ",
                      "platform. We see a gap in current dashboards ",
                      "going beyond the visualization of pre-existing ",
                      "data at static scales, leaving room for a more ",
                      "future-oriented, scalable, and interactive model."))),
      p(i18n$t(paste0("Existing data-driven approaches to urban ",
                      "sustainability are characterized by static data, ",
                      "limited user interaction, and the ",
                      "oversimplification of complex urban issues. ",
                      "They provide little opportunity for user ",
                      "engagement and exploration of questions ",
                      "connecting different data and issues."))),
      p(i18n$t(paste0("Some of the limitations of existing dashboards ",
                      "include a bias towards quantifiable, measurable ",
                      "components of sustainability, and a reliance on ",
                      "data with potential bias. Furthermore, they often ",
                      "attempt to play the role of a neutral force to ",
                      "communicate “objective” information on cities."))),
      p(i18n$t(paste0("Sustainability dashboards should build upon best ",
                      "practices to provide useful tools for individuals ",
                      "and cities alike to examine the many facets of ",
                      "urban sustainability and question existing ",
                      "assumptions."))),
      p(i18n$t(paste0("Maintaining transparency with data and ",
                      "methodologies, ensuring public participation and ",
                      "accurate representation of underprivileged ",
                      "communities, and using engaging and accessible ",
                      "tools contribute to the success of a dashboard."))),
      p(i18n$t(paste0("Sus aims to more accurately represent and better ",
                      "engage urban residents in order to harness the ",
                      "momentum surrounding technologically-based ",
                      "approaches to sustainability for public good."))),
      br(),
      p(i18n$t("Further resources:")),
      HTML(paste0("<ul><li><a href= ''>Robin Basalaev-Binder ",
                  "and David Wachsmuth. 2020. 'Progress in ",
                  "data-driven urban sustainability'. ",
                  "Working paper.</a> <b>(MSSI research)</b></ul>"))
      )
    )
}


why_dash_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
  }
