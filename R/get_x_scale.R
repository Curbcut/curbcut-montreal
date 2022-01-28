#### GET EXPLORE GRAPH X SCALE #################################################

get_x_scale <- function(graph_type, data, var_left, var_right, df) {
  
  ## Get labels ----------------------------------------------------------------
  
  breaks_q5_left <- 
    variables |> 
    filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
    pull(breaks_q5)
  
  if (length(breaks_q5_left) > 0) breaks_q5_left <- 
      breaks_q5_left |> 
      pluck(1) |> 
      filter(scale == df)
  
  if (suppressWarnings(!is.null(breaks_q5_left$var_name)) && 
      sum(!is.na(breaks_q5_left$var_name)) > 0) {
    var_left_label <- breaks_q5_left$var_name_short
  } else var_left_label <- NULL
  
  
  ## Get scale type ------------------------------------------------------------
  
  scale_type <- case_when(
    graph_type == "date" ~ "date",
    graph_type == "deltabi" ~ "deltabi",
    graph_type == "delta" & str_detect(var_left[1], "_pct") ~
      "delta_pct",
    graph_type == "delta" & str_detect(var_left[1], "_dollar") ~
      "delta_dollar",
    graph_type == "delta" ~ "delta",
    !is.null(var_left_label) & graph_type %in% c("bar", "box") ~ "discrete",
    !is.null(var_left_label) & graph_type == "hist" ~ "cont_labels",
    graph_type == "scatter" & str_detect(var_right[1], "_pct") ~ 
      "cont_pct",
    graph_type == "scatter" & str_detect(var_right[1], "_dollar") ~ 
      "cont_dollar",
    graph_type == "scatter" ~ "cont_comma",
    str_detect(var_left[1], "_pct") ~ "cont_pct",
    str_detect(var_left[1], "_dollar") ~ "cont_dollar",
    TRUE ~ "cont_comma"
  )
  
  scale_type <- unique(scale_type)
  
  ## Compress dollar values ----------------------------------------------------
  
  if (str_detect(scale_type, "dollar")) {
    
    min_dig <- if (str_detect(var_right[1], "_dollar")) data$var_right else
      data$var_left
    
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
  
  
  
  ## Get scale -----------------------------------------------------------------
  
  if (scale_type == "date") out <- list(scale_x_date())
  
  if (scale_type == "deltabi") out <- 
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
