### MCP MODULE #################################################################

# UI ----------------------------------------------------------------------

mcp_UI <- function(id) {

  tagList(
          susPage(class = "sus-page-mcp", footer = susFooter(),
            susPageControls(select_var_UI(NS(id, "left"), var_list = var_list_left_mcp, width = "300px", inline = T)),
            susPageSection(
              htmlOutput(NS(id, "mcp_output"))
            )
          )
        )
}


# Server ------------------------------------------------------------------

mcp_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    left_var_mcp <- select_var_server(
      id = "left", 
      r = r,
      var_list = reactive(var_list_left_mcp))
    
    
    output$mcp_output <- renderUI(
      HTML('<div class="mcp_module">',
           includeHTML(paste0("www/mcp/", left_var_mcp(), "_",
                              r$lang(), 
                              ".html")),
           '</div>'))
    
  })
}
