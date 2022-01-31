# GET EXPLORE VARIABLE TYPE ####################################################

get_var_type <- function(data, var_left, var_right, df, select_id) {
  
  ## Identify NA tables --------------------------------------------------------
  
  if (length(var_right) == 2 && var_right[1] == var_right[2]) 
    return("NA_delta_bivar")
  if (length(var_left) == 2 && var_left[1] == var_left[2]) 
    return("NA_delta")
  
  
  ## Handle multiple dates then strip dates ------------------------------------
  
  delta <- length(var_left) == 2 || length(var_right) == 2
  var_left <- unique(str_remove(var_left, "_\\d{4}$"))
  var_right <- unique(str_remove(var_right, "_\\d{4}$"))
  
  
  ## Selections ----------------------------------------------------------------
  
  select_df <- if (df == "building") building else data
  selection <- filter(select_df, ID == select_id)
  active_left <- nrow(filter(selection, !is.na(var_left)))
  active_right <- active_left
  if (length(var_right) != 1 || var_right != " ") active_right <- 
    nrow(filter(selection, !is.na(var_left), !is.na(var_right)))
  
  
  ## Create var_left_label and var_right_label ---------------------------------
  
  breaks_q5_left <- 
    variables |> 
    filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
    pull(breaks_q5) |> 
    pluck(1) |> 
    filter(scale == df)
  
  var_left_label <- suppressWarnings(breaks_q5_left$var_name)
  if (all(is.na(var_left_label))) var_left_label <- NULL
  
  if (var_right != " ") {
    
    breaks_q5_right <- 
      variables |> 
      filter(var_code == var_right) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df)
    
    var_right_label <- suppressWarnings(breaks_q5_right$var_name)
    if (all(is.na(var_right_label))) var_right_label <- NULL
    
  } else var_right_label <- NULL
  
  
  ## Decide on table type ------------------------------------------------------
  
  comp_type <- case_when(var_right == " " ~ "uni", TRUE ~ "bi")
  
  var_type <- case_when(
    comp_type == "uni" & is.null(var_left_label) ~ "quant",
    comp_type == "bi" & is.null(var_left_label) & is.null(var_right_label) ~ 
      "quantxy",
    comp_type == "bi" & is.null(var_left_label) & !is.null(var_right_label) ~ 
      "quantx",
    comp_type == "bi" & !is.null(var_left_label) & is.null(var_right_label) ~ 
      "quanty",
    TRUE ~ "qual")
  
  select_type <- unique(case_when(
    is.na(select_id) ~ "all", 
    comp_type == "uni" & active_left == 0 ~ "na",
    active_right == 0 ~ "na",
    TRUE ~ "select"))
  
  table_type <- paste(comp_type, var_type, select_type, sep = "_")
  if (select_type == "na") table_type <- paste0(comp_type, "_na")
  if (delta) table_type <- paste0(table_type, "_delta")
  table_type <- unique(table_type)
  return(table_type)
  
}
