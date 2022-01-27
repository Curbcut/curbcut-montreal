#### GET DISCLAIMER ############################################################

get_disclaimer <- function(data, var_left, var_right, more, more_text) {
  
  out <- list()
  
  # Same year selected
  if (length(var_left) == 2 && var_left[1] == var_left[2]) {
    out <- c(out, list(str_glue(sus_translate(paste0(
      "<p><i>Comparison requires two different dates.</i></p>")))))
  }
  
  return(out)
  
  
  # # `data` filled with NAs ------------------------------------------------
  # 
  # if ("var_left" %in% names(data())) {
  #   is_values <- 
  #     data() |> 
  #     st_drop_geometry() |> 
  #     select(`var_left`) |> 
  #     pull() |> 
  #     is.na() |> 
  #     all()
  #   
  #   if (is_values) {
  #     out <- c(out, list(str_glue(sus_translate(paste0(
  #       "<p style='font-size:11px;'>",
  #       "There is no data for '{var_left_title}' to report for ",
  #       "{left_year}.</p>")))
  #     ))
  #   }
  # }
  # 
  # 
  # 
  # 
  # disclaimer_text <- reactive({
  #   
  #   # Unique vars
  #   var_left <- unique(var_left())
  #   var_right <- unique(var_right())
  #   
  #   # Unique years
  #   left_year <- unique(str_extract(var_left(), "\\d{4}$"))
  #   right_year <- unique(str_extract(var_right(), "\\d{4}$"))
  #   
  #   # Unique var_code
  #   left_var_code <- unique(str_remove(var_left(), "_\\d{4}$"))
  #   right_var_code <- unique(str_remove(var_right(), "_\\d{4}$"))
  #   
  #   # Vars title
  #   var_left_title <- variables[
  #     variables$var_code == str_remove(left_var_code, "_\\d{4}$"),]$var_title
  #   var_left_title <- sus_translate(var_left_title)
  #   
  #   var_right_title <- variables[
  #     variables$var_code == str_remove(right_var_code, "_\\d{4}$"),]$var_title
  #   var_right_title <- sus_translate(var_right_title)
  #   
  #   
  #   
  #   
  #   
  #   
  #   # Year displayed != year chosen -------------------------------------------
  #   
  #   # Year displayed LEFT
  #   if (length(var_left) == 1) {
  #     if (left_year != unique(time())) {
  #       out <- c(out, list(str_glue(sus_translate(paste0(
  #         "<p style='font-size:11px; font-style:italic;'>",
  #         "Displayed data for <b>{var_left_title}</b> is for the ",
  #         "closest available year <b>({left_year})</b>.</p>")))
  #       ))
  #     }
  #   }
  #   
  #   # Year displayed RIGHT
  #   if (length(var_right) == 1) {
  #     if (var_right != " " && right_year != unique(time())) {
  #       out <- c(out, list(str_glue(sus_translate(paste0(
  #         "<p style='font-size:11px; font-style:italic;'>",
  #         "Displayed data for <b>{var_right_title}</b> is for the ",
  #         "closest available year <b>({right_year})</b>.</p>")))
  #       ))
  #     }
  #   }
  #   
  #   
  #   # More condition for more disclaimers -------------------------------------
  #   
  #   if (more()) out <- c(out, list(str_glue(sus_translate(more_text))))
  #   
  #   out
  #   
  # })
  
  
  
}