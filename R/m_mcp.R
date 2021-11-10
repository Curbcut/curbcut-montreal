### MCP MODULE #################################################################

# UI ----------------------------------------------------------------------

mcp_UI <- function(id) {
  tabItem(tabName = "mcp", 
          select_var_UI(NS(id, "left"), var_list_left_mcp),
          htmlOutput(NS(id, "mcp_output")))
}


# Server ------------------------------------------------------------------

mcp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    left_var_mcp <- select_var_server("left", reactive(var_list_left_mcp))
    
    
    output$mcp_output <- renderUI(
      
      if (sus_reactive_variables$active_language() == "en") {
        includeHTML(paste0("www/mcp/", left_var_mcp(), "_en.html"))
        
      } else if (sus_reactive_variables$active_language() == "fr") {
        includeHTML(paste0("www/mcp/", left_var_mcp(), "_fr.html"))
      }
      
      )
    
  })
  }
