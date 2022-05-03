### MOBILE WARNING, BETTER TO USE A COMPUTER ###################################

mobile_warning <- function(r, session) {
  session <- shiny::getDefaultReactiveDomain()$rootScope()
  shiny::req(session$input$.shinybrowser)
  if (session$input$.shinybrowser$device != "Desktop") {
    shinyjs::info(
      sus_translate(r = r, 
                    "Sus does not currently support mobile phones. ",
                    "Please visit from a computer."))
  }
}