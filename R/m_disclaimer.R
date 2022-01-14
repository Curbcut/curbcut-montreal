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
  tagList(
    htmlOutput(NS(id, "year_disclaimers"))
  )
}

year_disclaimer_server <- function(id, data, var_left, var_right, time,
                                   pct_variation = reactive(T),
                                   more_condition = reactive(F), 
                                   more_text = NULL) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(time))
  stopifnot(is.reactive(pct_variation))
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    disclaimer_texts <- reactive({
      
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
      var_left_title <- variables[variables$var_code == str_remove(left_var_code, "_\\d{4}$"),]$var_title
      var_left_title <- sus_translate(var_left_title)
      
      var_right_title <- variables[variables$var_code == str_remove(right_var_code, "_\\d{4}$"),]$var_title
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


    # How to read the map -----------------------------------------------------

      # How to read map
      if (length(var_left) == 2) {
        if (var_right[1] == " ") {
          if (pct_variation()) {
                      out <- c(out, list(str_glue(
            sus_translate(paste0("<b>How to read the map</b><br>",
                                 "The map displays the percent variation in ",
                                 "'{var_left_title}' between {time()[1]} and {time()[2]}. ",
                                 "Blue means an increase in '{var_left_title}', and red ",
                                 "means a decrease.")))
                      ))
          } else {
            out <- c(out, list(str_glue(
              sus_translate(paste0("<b>How to read the map</b><br>",
                                   "The map displays a data aggregate of ",
                                   "'{var_left_title}' between {time()[1]} and {time()[2]}.")))
            ))
          }

        } else {

          if (length(right_year) == 2) {
            if (left_year[1] == right_year[1] && left_year[2] == right_year[2]) {
              # Maybe unnecessary?
              out <- c(out, list(str_glue(sus_translate(paste0(
                "<b>How to read the map</b><br>",
                "The map displays the comparison of two percent variations ",
                "between {left_year[1]} and {left_year[2]}. ",
                "A darker green means a relative increase ",
                "in '{var_left_title}', and a darker blue means a relative increase ",
                "in '{var_right_title}'. You can find the comparison legend at ",
                "the bottom left of the page.")))
              ))
            } else {
              out <- c(out, list(str_glue(sus_translate(paste0(
                "<b>How to read the map</b><br>",
                "The map displays the comparison of two percent variations. ",
                "In green, the percent variation in ",
                "'{var_left_title}' between {left_year[1]} and {left_year[2]}. ",
                "A darker green means a relative increase in '{var_left_title}'. ",
                "In blue, the percent variation of '{var_right_title}' between ",
                "{right_year[1]} and {right_year[2]}. ",
                "A darker blue means a relative increase in '{var_right_title}'. ",
                "You can find the comparison legend at ",
                "the bottom left of the page.")))
              ))
            }

          } else out <- c(out, list(str_glue(sus_translate(paste0(
            # This is used for a module like crash, where var_left and var_right
            # often have different years to display. (yearly crash nb vs census data)
            "<b>How to read the map</b><br>",
            "The map displays the comparison of a percent variation ",
            "with a census variable. In green, the percent variation ",
            "in '{var_left_title}' between {left_year[1]} and {left_year[2]}. ",
            "A darker green means a relative increase in '{var_left_title}'. ",
            "Displayed in blue is '{var_right_title}' numbers in {right_year}. ",
            "A darker blue means a relatively higher number of '{var_right_title}'. ",
            "You can find the comparison legend at the bottom left of the page.")))
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
    output$year_disclaimers <- renderText({paste0(disclaimer_texts())})
  })
}
