#' Did You Know Server and UI Module
#'
#' This function provides a server module for the "Did You Know" (DYK) section
#' of the a Curbcut map page. It retrieves and displays interesting facts, based
#' on `vars` and `poi` (point of interests). The server module retrieves DYKs and
#' handles the click events on the links.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param poi <`reactive`> (Optional) Point of interests. The output of
#' \code{\link{update_df}}. Default is NULL.
#'
#' @return A Shiny module server function for the DYK module.
#' @export
dyk_server <- function(id, r, vars, df, poi = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(poi))
  
  shiny::moduleServer(id, function(input, output, session) {
    # Get the DYKs
    dyk <- shiny::reactive(dyk_get(id, vars(), df(), poi(), lang = r$lang()))
    
    # Hide the panel if there are no DYK
    shiny::observe({
      shinyjs::toggle(id = "dyk_panel", condition = !is.null(dyk()))
    })
    
    # Observe for clicks
    shiny::observeEvent(input$dyk_1, do.call(
      link, c(session = session, r = list(r), attr(dyk(), "links")[[1]])
    ))
    shiny::observeEvent(input$dyk_2, do.call(
      link, c(session = session, r = list(r), attr(dyk(), "links")[[2]])
    ))
    
    # Only show contents if dyk_output isn't empty
    output$dyk_contents <- shiny::renderUI({
      if (!is.null(dyk())) {
        shiny::tagList(dyk())
      }
    })
  })
}

#' @describeIn dyk_server Create the UI for the legend module
#' @export
dyk_UI <- function(id) {
  shiny::tagList(
    shinyjs::hidden(shiny::div(
      id = shiny::NS(id, "dyk_panel"),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          width = 7,
          shiny::h4(
            icon_material_title("info"),
            cc_t("Did you know?")
          )
        )
      ),
      shiny::div(
        shiny::uiOutput(shiny::NS(id, "dyk_contents"))
      )
    ))
  )
}
