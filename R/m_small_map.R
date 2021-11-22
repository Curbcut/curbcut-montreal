#### SMALL MAP MODULE ##########################################################

small_map_UI <- function(id) {
  div(class = "small_map", style = "text-align: center;",
      imageOutput(NS(id, "small_map"), inline = TRUE))
}

# `string` should be, e.g. "left_borough_ale_index"
small_map_server <- function(id, string) {
  stopifnot(is.reactive(string))
  
  moduleServer(id, function(input, output, session) {
    output$small_map <- renderImage({
      
      list(src = paste0("www/maps/", string(), ".png"), filetype = "image/png", 
           height = 175, width = 175)
      }, deleteFile = FALSE)
  })
}
