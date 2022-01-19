#### MARKETED SUSTAINABILITY INFO TABLE MODULE #################################

marketed_sustainability_info_table <- function(id, x, select_id, ...) {
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      x <- x()
      
      if (is.na(select_id())) {
        
        min_val <- min(x$sustainability_prop) |> scales::percent(accuracy = 0.1)
        max_val <- max(x$sustainability_prop) |> scales::percent(accuracy = 0.1)
        mean_val <- mean(x$sustainability_prop) |> scales::percent(accuracy = 0.1)
        
        HTML(str_glue(sus_translate(
          paste0("At the scale of the Island of Montreal, the proportion of ",
                 "words, used on website marketing, that are related to ",
                 "sustainability varies from {min_val} to {max_val}, with an ",
                 "average value of {mean_val}.")
        )))
      }
    })
  })
}
