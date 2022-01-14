#### GREEN SPACE INFO TABLE MODULE ############################################

green_space_info_table <- function(id, x, select_id, ...) {
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      pnr <- function(x) prettyNum(round(x/1e+6, digits = 2), big.mark = ",")
      
      if (is.na(select_id())) {
        type <- if (nrow(x()) == nrow(green_space)) {
          str_glue(sus_translate("a total of {nrow(x())} green spaces"))
        } else {
          str_glue(sus_translate("{nrow(x())} `{unique(x()$type)}`"))
        }
        
        HTML(str_glue(sus_translate(
          paste0("At the scale of the City of Montreal, there are {type}, ",
                 "combining {pnr(sum(x()$area))} km^2. Their area range from ",
                 "{pnr(min(x()$area))} and {pnr(max(x()$area))} km^2, with ",
                 "an average of {pnr(mean(x()$area))} km^2."))))
      } else {
        x <-
          x() |>
          mutate(total_rank = rank(-area)) |>
          group_by(CSDUID) |>
          mutate(borough_rank = rank(-area))
        
        type <- if (nrow(x()) == nrow(green_space)) {
          str_glue(sus_translate("green space"))
        } else {
          str_glue(sus_translate("`{unique(x()$type)}`"))
        }
        
        z <- x[x$ID == select_id(), ]
        total_rank <- pull(z, total_rank)
        borough_rank <- pull(z, borough_rank)
        borough_name <- filter(borough, ID == z$CSDUID)$name
        
        ordinal_form <- function(x) {
          # English ordinal form
          if (sus_rv$lang() == "en") {
            if (x > 20) {
              if (x %% 100 %in% c(11 , 12, 13)) {
                form <- "th "
              } else {
                form <- switch(as.character(x %% 10), "1" = "st ", "2" = "nd ",
                               "3" = "rd ", "th ")
              }
              paste0(x, form)
            } else {
              switch(as.character(x), "1" = "", "2" = "second ",
                     "3" = "third ", "4" = "fourth ", "5" = "fifth ", 
                     "6" = "sixth ",  "7" = "seventh ", "8" = "eighth ", 
                     "9" = "ninth ", "10" = "tenth ",
                     paste0(as.character(x), "th "))
            }
          } else {
            # French ordinal form
            switch(as.character(x), "1" = "", "2" = "deuxième ",
                   "3" = "troisième ", paste0(as.character(x), "ième "))
          }
        }
        
        total_rank <- ordinal_form(total_rank)
        borough_rank <- ordinal_form(borough_rank)
        
        HTML(str_glue(sus_translate(
          paste0("<p><b>{z$name}</b><p>",
                 "<p>The green space {z$name} is a `{z$type}` of ",
                 "{prettyNum(z$area, big.mark = ',')} m^2. It is ",
                 "categorized as a `{z$type_2}`",
                 "and is of `{z$property}` property. Its ",
                 "management entity is `{z$management}`.</p>",
                 "<p>It is the {total_rank}biggest {type} in the ",
                 "City, and the {borough_rank} largest in {borough_name}.</p>"))))

      }
    })
  })
}
