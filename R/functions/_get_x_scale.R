#### GET EXPLORE GRAPH X SCALE #################################################

get_x_scale <- function(graph_type, data, var_type, var_left, var_right, df, 
                        geo) {
  
  ## Get labels ----------------------------------------------------------------
  
  breaks_q5_left <- variables$breaks_q5[[
    which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
  
  if (length(breaks_q5_left) > 0) breaks_q5_left <- 
      breaks_q5_left[breaks_q5_left$scale == gsub(".*_", "", df) &
                       breaks_q5_left$geo == geo, ]

  if (suppressWarnings(!is.null(breaks_q5_left$var_name)) && 
      sum(!is.na(breaks_q5_left$var_name)) > 0) {
    var_left_label <- breaks_q5_left$var_name_short
  } else var_left_label <- NULL
  
  
  ## Additional info -----------------------------------------------------------
  
  bivar_unmatched_years <- 
    length(var_left) == 2 && length(unique(var_right)) == 1

  
  ## Get scale type ------------------------------------------------------------
  
  scale_type <- 
    if (graph_type == "date") {
      "date"
    } else if (graph_type %in% c("deltabivar", "NAdeltabivar") && 
               bivar_unmatched_years) {
      if (str_detect(var_right[1], "_pct")) {
        "cont_pct"
      } else if (str_detect(var_right[1], "_dollar")) {
        "cont_dollar"
      } else "cont_comma"
    } else if (graph_type %in% c("deltabivar", "NAdeltabivar")) {
      "deltabivar"
    } else if (graph_type %in% c("delta", "NAdelta") && 
               str_detect(var_left[1], "_pct")) {
      "delta_pct"
    } else if (graph_type %in% c("delta", "NAdelta") && 
               str_detect(var_left[1], "_dollar")) {
      "delta_dollar"
    } else if (graph_type %in% c("delta", "NAdelta")) {
      "delta"
    } else if (!is.null(var_left_label) && graph_type %in% c("bar", "box")) {
      "discrete"
    } else if (!is.null(var_left_label) && graph_type == "hist") {
      "cont_labels"
    } else if (graph_type == "scatter" && str_detect(var_right[1], "_pct")) {
      "cont_pct"
    } else if (graph_type == "scatter" && str_detect(var_right[1], "_dollar")) {
      "cont_dollar"
    } else if (graph_type == "scatter") {
      "cont_comma"
    } else if (str_detect(var_left[1], "_pct")) {
      "cont_pct"
    } else if (str_detect(var_left[1], "_dollar")) {
      "cont_dollar"
    } else "cont_comma"
  
  
  ## Compress dollar values ----------------------------------------------------
  
  if (str_detect(scale_type, "dollar") && !bivar_unmatched_years) {
    
    if (str_detect(var_type, "NA")) lab_dl <- scales::label_dollar() else {
      
      if (str_detect(scale_type, "delta_")) {
        min_dig <- data$var_left_1
      } else {
        min_dig <- if (str_detect(var_right[1], "_dollar")) data$var_right else
          data$var_left
      }
      
      min_dig <- 
        min_dig |> 
        setdiff(0) |> 
        abs() |> 
        min(na.rm = TRUE) |> 
        log10() |> 
        ceiling()
      
      if (min_dig >= 10) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+09, suffix = "B")  
      } else if (min_dig >= 7) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+06, suffix = "M")  
      } else if (min_dig >= 4) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+03, suffix = "K")  
      } else lab_dl <- scales::label_dollar()
      
    }
  }
  
  if (str_detect(scale_type, "dollar") && bivar_unmatched_years) {
    
    if (str_detect(var_type, "NA")) lab_dl <- scales::label_dollar() else {
      
      if (str_detect(scale_type, "delta_")) {
        min_dig <- data$var_right_1
      } else {
        min_dig <- if (str_detect(var_left[1], "_dollar")) data$var_left else
          data$var_right
      }
      
      min_dig <- 
        min_dig |> 
        setdiff(0) |> 
        abs() |> 
        min(na.rm = TRUE) |> 
        log10() |> 
        ceiling()
      
      if (min_dig >= 10) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+09, suffix = "B")  
      } else if (min_dig >= 7) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+06, suffix = "M")  
      } else if (min_dig >= 4) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+03, suffix = "K")  
      } else lab_dl <- scales::label_dollar()
      
    }
  }
  
  
  ## Get scale -----------------------------------------------------------------
  
  if (scale_type == "date") out <- list(scale_x_date())
  
  if (scale_type == "deltabivar") out <- 
    list(scale_x_continuous(labels = scales::percent))
  
  if (scale_type == "delta") out <- 
    list(scale_x_continuous(labels = scales::percent))
  
  if (scale_type == "delta_pct") out <- 
    list(scale_x_continuous(labels = scales::percent))
  
  if (scale_type == "delta_dollar") out <- 
    list(scale_x_continuous(labels = lab_dl))
  
  if (scale_type == "discrete") out <- 
    list(scale_x_discrete(labels = var_left_label))
  
  if (scale_type == "cont_labels") out <- list(scale_x_continuous(
    limits = c(min(0, as.numeric(names(var_left_label))),
               max(0, as.numeric(names(var_left_label)))),
    breaks = as.numeric(names(var_left_label)),
    labels = var_left_label))
  
  if (scale_type == "cont_pct") out <- 
    list(scale_x_continuous(labels = scales::percent))
  
  if (scale_type == "cont_dollar") out <- 
    list(scale_x_continuous(labels = lab_dl))
  
  if (scale_type == "cont_comma") out <- 
    list(scale_x_continuous(labels = scales::comma))
  
  return(out)
  
}
