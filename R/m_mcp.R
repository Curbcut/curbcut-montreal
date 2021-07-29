### MCP MODULE #################################################################

# UI ----------------------------------------------------------------------

mcp_UI <- function(id) {
  tabItem(tabName = "mcp", includeHTML("www/mcp/test.html"))
}


# Server ------------------------------------------------------------------

mcp_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
  }
