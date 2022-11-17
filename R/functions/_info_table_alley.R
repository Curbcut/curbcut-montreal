#### GREEN ALLEY EXPLORE INFO TABLE GRAPH ######################################

info_table_alley <- function(r = r, data, var_type, var_left, var_right, df, 
                             select_id, geo, build_str_as_DA = TRUE) {
  
  if (is_scale_in_df("borough_empty", df)) {
    
    if (is.na(select_id) || !select_id %in% alley_text$ID) {
      
      participating_boroughs <- nrow(alley_text)
      nb_alleys <- {nrow(alley)}
      
      cc_t(r = r, "{participating_boroughs} out of 19 Montreal boroughs ",
                    "have a green alley program. They collectively have ",
                    "{nb_alleys} green alleys.")
    } else {
      
      data <- alley_text[alley_text$ID == select_id,]
      
      text_to_display <- list()
      text_to_display$title <- paste0("<p><b>", cc_t(r = r, "Borough"), " ", 
                                      data$name, "</b></p>")
      text_to_display$intro <-
        paste0("<p>",
               cc_t(r = r, "The first green alley inauguration was in "),
               data$first_alley, if (!is.na(data$green_alley_sqm)) 
                 cc_t(r = r, " and there are {data$green_alley_sqm} square",
                               " meters of green alley in the borough.") else ".",
               "</p>")
      text_to_display$text <- 
        paste0("<p>",
               if (!is.na(data$app_process)) cc_t(r = r, data$app_process), " ",
               if (!is.na(data$management)) cc_t(r = r, data$management), " ",
               if (!is.na(data$budget)) cc_t(r = r, data$budget),
               "</p>")
      
      text_to_display$guide <- 
        paste0(glue("<p><a href = {data$guide}>"),
               cc_t(r = r, "The green alley guide of {data$name}"),
               "</a></p>")
      
      text_to_display$contact <- 
        if (!is.na(data$contact))
          glue("<p>Contact: <a href = 'mailto:{data$contact}'>",
               "{data$contact}</a></p>")
      
      HTML(unlist(text_to_display))
    }
    
  } else if (is_scale_in_df("alley", df)) {
    
    if (is.na(select_id) || !select_id %in% alley$ID) {
      
      alley_visited <- alley[alley$visited, ]
      green <- nrow(alley_visited[alley_visited$type == "green", ])
      community <- nrow(alley_visited[alley_visited$type == "community", ])
      mixed <- nrow(alley_visited[alley_visited$type == "mixed", ])
      none <- nrow(alley_visited[alley_visited$type == "none", ])
      
      green_per <- scales::percent(green/nrow(alley_visited))
      community_per <- scales::percent(community/nrow(alley_visited))
      mixed_per <- scales::percent(mixed/nrow(alley_visited))
      none_per <- scales::percent(none/nrow(alley_visited))
      
      cc_t(r = r, "Our team visited {nrow(alley_visited)} of the ",
                    "{nrow(alley)} green alleys in Montreal. We classified ",
                    "{green} ({green_per}) as 'green', {community} ",
                    "({community_per}) as 'community', {mixed} ",
                    "({mixed_per}) as 'mixed' green and community, and ",
                    "{none} ({none_per}) as 'unmaintained'.")
      
    } else {
      
      data <- alley[alley$ID == select_id,]
      
      text_to_display <- list()
      text_to_display$title <- paste0("<p><b>", data$name, " (", data$name_2, 
                                      ")", "</b></p>")
      if (!is.na(data$created)) 
        text_to_display$inauguration <- 
        paste0("<p>",
               cc_t(r = r, "Inauguration date: "), 
               data$created, "</p>")
      text_to_display$text <- 
        if (is.na(data$description)) {
          paste0("<p>",
                 cc_t(r = r, "We do not have information ",
                               "on this green alley."),
                 "</p>")
        } else {
          paste0("<p>", cc_t(r = r, data$description), "</p>",
                 if (!is.na(data$circulation)) {
                   paste0("<p>Circulation: ", cc_t(r = r, data$circulation),
                          "</p>")
                 })
        }
    
    list(HTML(unlist(text_to_display)),
         if (!is.na(data$photo_ID)) {
           div(style = "margin-bottom:20px; cursor:pointer;",
               HTML(paste0("<img src = 'alleys/", data$photo_ID, 
                           "' id = 'alley-alley_img' style = 'width:100%'>")))
         })
    
    }
  }
}

  
