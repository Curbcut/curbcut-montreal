#### BIVARIATE LEGEND MODULE ###################################################

legend_bivar_UI <- function(id) {
  uiOutput(NS(id, "legend"))
}

legend_bivar_server <- function(id, var_right) {
  stopifnot(is.reactive(var_right))
  
  moduleServer(id, function(input, output, session) {
    output$legend <- 
      renderUI({
        if (var_right() != " ") {
          absolutePanel(
            style = paste0("z-index:500; background-color: rgba(0,0,255,0); ",
                           "border-width: 0px; margin:0px"),
            bottom = 25, fixed = TRUE,
            img(src = "bivariate_legend_2.png", width = 200, height = 177))    
        }
    })
  })
}
