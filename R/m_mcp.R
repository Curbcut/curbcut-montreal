### MCP MODULE #################################################################

# UI ----------------------------------------------------------------------

mcp_UI <- function(id) {
  # tagList(
  #   select_var_UI(NS(id, "left"), var_list = var_list_left_mcp, width = "300px"),
  #   susPage(class="sus-page-about", footer = susFooter(),susPageSection(
  #     htmlOutput(NS(id, "mcp_output"))
  #   ))
  # )
  tagList(
          #Only change the size of photos in this module
          tags$head(tags$style(
            HTML('
      .mcp_module img {
                  margin:10px;
                  margin-top:0px
      }'))),
          select_var_UI(NS(id, "left"), var_list = var_list_left_mcp, width = "300px",
                        inline = T),
          susPage(class="sus-page-about", footer = susFooter(),susPageSection(
            htmlOutput(NS(id, "mcp_output"))
          )))
}


# Server ------------------------------------------------------------------

mcp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    left_var_mcp <- select_var_server(
      id = "left", 
      var_list = reactive(var_list_left_mcp))
    
    
    output$mcp_output <- renderUI(
      HTML('<div class="mcp_module">',
           includeHTML(paste0("www/mcp/", left_var_mcp(), "_",
                              sus_rv$lang(), 
                              ".html")),
           '</div>'))
    
  })
}
