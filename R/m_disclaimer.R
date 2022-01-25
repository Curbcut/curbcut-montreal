#### DISCLAIMER MODULE #########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. 
#' @param time A reactive dataframe coming from data_server.
#' @param pct_variation A logical indicating when "How to read the map" must tell
#' it's a percent variation (TRUE), or an aggregate (FALSE).
#' @return A reactive expression containing a data frame with the following
#' fields


year_disclaimer_UI <- function(id) {
  tagList(htmlOutput(NS(id, "year_disclaimer")))
}

year_disclaimer_server <- function(id, data, var_left, var_right, time,
                                   pct_variation = reactive(TRUE),
                                   more_condition = reactive(FALSE), 
                                   more_text = NULL) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(time))
  stopifnot(is.reactive(pct_variation))
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    disclaimer_text <- reactive({
      
      # Unique vars
      var_left <- unique(var_left())
      var_right <- unique(var_right())
      
      # Unique years
      left_year <- unique(str_extract(var_left(), "\\d{4}$"))
      right_year <- unique(str_extract(var_right(), "\\d{4}$"))
      
      # Unique var_code
      left_var_code <- unique(str_remove(var_left(), "_\\d{4}$"))
      right_var_code <- unique(str_remove(var_right(), "_\\d{4}$"))
      
      # Vars title
      var_left_title <- variables[
        variables$var_code == str_remove(left_var_code, "_\\d{4}$"),]$var_title
      var_left_title <- sus_translate(var_left_title)
      
      var_right_title <- variables[
        variables$var_code == str_remove(right_var_code, "_\\d{4}$"),]$var_title
      var_right_title <- sus_translate(var_right_title)
      
      # Prepare out list
      out <- list()
      
      # If comparison mode, but same year selected.
      if (length(time()) != length(var_left)) {
        out <- c(out, list(str_glue(sus_translate(paste0(
          "<p>No comparison shown, but static data from {left_year}.</p>")))
        ))
      }
      

    # `data()` filled with NAs ------------------------------------------------
      if ("var_left" %in% names(data())) {
        is_values <- 
          data() |> 
          st_drop_geometry() |> 
          select(`var_left`) |> 
          pull() |> 
          is.na() |> 
          all()
        
        if (is_values) {
          out <- c(out, list(str_glue(sus_translate(paste0(
            "<p style='font-size:11px;'>",
            "There is no data for '{var_left_title}' to report for ",
            "{left_year}.</p>")))
          ))
        }
      }


    # Year displayed != year chosen -------------------------------------------

      # Year displayed LEFT
      if (length(var_left) == 1) {
        if (left_year != unique(time())) {
          out <- c(out, list(str_glue(sus_translate(paste0(
            "<p style='font-size:11px; font-style:italic;'>",
            "Displayed data for <b>{var_left_title}</b> is for the ",
            "closest available year <b>({left_year})</b>.</p>")))
          ))
        }
      }

      # Year displayed RIGHT
      if (length(var_right) == 1) {
        if (var_right != " " && right_year != unique(time())) {
          out <- c(out, list(str_glue(sus_translate(paste0(
            "<p style='font-size:11px; font-style:italic;'>",
            "Displayed data for <b>{var_right_title}</b> is for the ",
            "closest available year <b>({right_year})</b>.</p>")))
          ))
        }
      }


    # More condition for more disclaimers -------------------------------------
      
      if (more_condition()) {
        out <- c(out, list(str_glue(sus_translate(more_text))))
      }
      
      out
      
    })
    
    # Output the texts 
    output$year_disclaimer <- renderText(paste0(disclaimer_text()))
  })
}
