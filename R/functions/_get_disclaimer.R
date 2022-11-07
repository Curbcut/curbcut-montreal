#### GET DISCLAIMER ############################################################

get_disclaimer <- function(r = r, data, var_left, var_right, time, more, more_text) {
  
  
  # Data prep ---------------------------------------------------------------
  
  out <- list()
  
  # Unique years
  left_year <- str_extract(var_left, "(?<=_)\\d{4}$")
  right_year <- str_extract(var_right, "(?<=_)\\d{4}$")
  
  # Unique var_code
  left_var_code <- unique(str_remove(var_left, "_\\d{4}$"))
  right_var_code <- unique(str_remove(var_right, "_\\d{4}$"))
  
  # Vars title
  var_left_title <- variables[
    variables$var_code == str_remove(left_var_code, "_\\d{4}$"),]$var_title
  var_left_title <- cc_t(r = r, var_left_title)
  
  var_right_title <- variables[
    variables$var_code == str_remove(right_var_code, "_\\d{4}$"),]$var_title
  var_right_title <- cc_t(r = r, var_right_title)
  
  
  # Same year selected ------------------------------------------------------
  
  if (length(left_year) == 2 && left_year[1] == left_year[2]) {
    out <- c(out, list(cc_t(r = r, 
      "Comparison requires two different dates.")))
  }
  
  
  # `data` filled with NAs ------------------------------------------------
  
  # if ("var_left" %in% names(data)) {
  #   is_values <-
  #     data |>
  #     select(`var_left`) |>
  #     pull() |>
  #     is.na() |>
  #     all()
  #   
  #   if (is_values) {
  #     out <- c(out, list(str_glue(cc_t(r = r, 
  #       "There is no data for '{var_left_title}' to report for ",
  #       "{left_year}."))
  #     ))
  #   }
  # }
  
  
  # Year displayed != year chosen -------------------------------------------
  
  # Year displayed LEFT
  if (length(left_year) == 1) {
    if (left_year != unique(time)) {
      out <- c(out, list(cc_t(r = r, 
        "Displayed data for <b>{var_left_title}</b> is for the ",
        "closest available year <b>({left_year})</b>.")
      ))
    }
  }
  
  # Year displayed RIGHT
  if (length(right_year) == 1) {
    if (var_right != " " && right_year != unique(time)) {
      out <- c(out, list(cc_t(r = r, 
        "Displayed data for <b>{var_right_title}</b> is for the ",
        "closest available year <b>({right_year})</b>.")
      ))
    }
  }
  
  
  # More condition for more disclaimers -------------------------------------
  
  if (more) out <- c(out, list(cc_t(r = r, more_text)))
  
  
  # Return ------------------------------------------------------------------
  
  out <- paste0("<p style='font-size:12px'><i>", out, "</p></i>", collapse = "")
  
  return(out)
  
}
