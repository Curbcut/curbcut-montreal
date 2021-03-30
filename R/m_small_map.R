#### SMALL MAP MODULE ##########################################################

small_map_UI <- function(id) {
  div(imageOutput(NS(id, "small_map"), inline = TRUE), 
      style = "text-align: center")
}

# `string` should be, e.g. "left_borough_ale_index"
small_map_server <- function(id, string) {
  stopifnot(is.reactive(string))
  
  moduleServer(id, function(input, output, session) {
    output$small_map <- renderImage({
      
      paste(string())
      
      list(src = paste0("www/maps/", string(), ".png"), filetype = "image/png", 
           height = 215, width = 215)
      }, deleteFile = FALSE)
  })
}
