#### ALLEY INFO TABLE MODULE ###################################################

alley_info_table <- function(id, select_id, ...) {
  
  moduleServer(id, function(input, output, session) {
    reactive({

      if (select_id() %in% alley_text$ID) {
        
        text_to_display <- 
          alley_text |>
          filter(ID == select_id()) |> 
          select(-ID) |> 
          select_if(~sum(!is.na(.)) > 0) %>% 
          {if (nrow(.) > 0) as.list(.) else NULL}
        
        text_to_display <- alley_borough_text(text_to_display)
        
        if (exists("text_to_display") && !is.null(text_to_display)) {
          HTML(unlist(text_to_display[1:(length(text_to_display) - 1)]))
        }
        
      } else if (select_id() %in% alleys[alleys$visited,]$ID) {
        
        text_to_display <- 
          alleys |>
          st_drop_geometry() |> 
          filter(ID == select_id()) |> 
          mutate(name = str_glue(sus_translate(
            "<p><b>{str_to_title(name)} in ",
            "{name_2}</b></p>"))) |> 
          select(-ID, -CSDUID, -visited, -name_2, -fill) |> 
          select_if(~sum(!is.na(.)) > 0) %>% 
          {if (nrow(.) > 0) as.list(.) else NULL}
        
        text_to_display <- alley_alleys_text(text_to_display)
        
        output$alley_img <- renderImage({
          list(src = paste0("www/", text_to_display$photo_ID),
               alt = "Photo of the selected green alley",
               width = "100%")},
          deleteFile = FALSE)
        
        if (exists("text_to_display") && !is.null(text_to_display)) {
          list(HTML(unlist(text_to_display[1:(length(text_to_display) - 1)])),
               if (!is.null(text_to_display$photo_ID)) {
                 div(style = "margin-bottom:20px; cursor:pointer;", 
                     imageOutput(session$ns("alley_img"), height = "100%"))
               })}}
      
    })
  })
}
