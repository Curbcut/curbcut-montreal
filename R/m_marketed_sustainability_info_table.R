#### MARKETED SUSTAINABILITY INFO TABLE MODULE #################################

marketed_sustainability_info_table <- function(id, x, select_id, ...) {
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      if (is.na(select_id)) {
        
        min_val <- min(x$sustainability_prop) |> scales::percent(accuracy = 0.1)
        max_val <- max(x$sustainability_prop) |> scales::percent(accuracy = 0.1)
        mean_val <- mean(x$sustainability_prop) |> scales::percent(accuracy = 0.1)
        
        HTML(cc_t(r = r, 
          "At the scale of the Island of Montreal, the proportion of ",
          "words, used on website marketing, that are related to ",
          "sustainability varies from {min_val} to {max_val}, with an ",
          "average value of {mean_val}."))
        
      } else {
        
        z <- x[x$ID == select_id, ]
        
        title <- cc_t(r = r, 
          "{z$Name} development from {z$dev_company}")
        
        civic <- cc_t(r = r, 
          "{paste(unique(z$civic_start, z$civic_end), sep = '-')} ",
          "{z$street_name} in {z$borough}")
        
        area <- cc_t(r = r, 
          "{z$surface_area} m^2 and {z$number_units} units")
        
        score <- scales::percent(z$sustainability_prop, accuracy = 0.01)
        
        HTML(
          cc_t(r = r, "<p><b>{title}</b></p>",
                        "At the {civic}, development {z$Name} built in ",
                        "{z$year_construction} has a score of {score} regarding ",
                        "the proportion of words flagged as being ",
                        "`sustainability-related."))
        
      }
      
    })
  })
}
