# GET EXPLORE VARIABLE TYPE ####################################################

get_var_type <- function(data, var_left, var_right, df, select_id, 
                         build_str_as_DA = TRUE) {
  
  ## Invalidate if non-standard df() -------------------------------------------
  
  if (!df %in% c("borough", "CT", "DA", "building", "grid",
                 "street", "heatmap", "point", "centraide")) return(df)
  
  
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
  
  select_df <- if (build_str_as_DA && df == "building") {
    if (is.na(select_id)) DA else {
      dbGetQuery(db, paste0("SELECT * FROM building WHERE ID = ", 
                                                        select_id))
    }
  } else data
  selection <- if (is.na(select_id)) select_df[0,] else 
    select_df[select_df$ID == select_id,]
  active_left <- if (build_str_as_DA && df == "building") {
    sum(!is.na(data$var_left[data$ID == selection$DAUID]))
  } else sum(!is.na(selection$var_left))
  active_right <- active_left
  if (length(var_right) != 1 || var_right != " ") 
    active_right <- if (build_str_as_DA && df == "building") {
      sum(!is.na(data$var_left[data$ID == selection$DAUID]) &
            !is.na(data$var_right[data$ID == selection$DAUID])) 
    } else sum(!is.na(selection$var_left) & !is.na(selection$var_right))
  
  
  ## Create var_left_label and var_right_label ---------------------------------
  
  if (build_str_as_DA && df == "building") df <- "DA"
  
  breaks_q5_left <- variables$breaks_q5[[
    which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
  
  if (length(breaks_q5_left) > 0) breaks_q5_left <- 
    breaks_q5_left[breaks_q5_left$scale == df,]
  
  var_left_label <- suppressWarnings(breaks_q5_left$var_name)
  if (all(is.na(var_left_label))) var_left_label <- NULL
  
  if (var_right != " ") {
    
    breaks_q5_right <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_right)))]]
    
    if (length(breaks_q5_right) > 0) breaks_q5_right <- 
        breaks_q5_right[breaks_q5_right$scale == df,]
    
    var_right_label <- suppressWarnings(breaks_q5_right$var_name)
    if (all(is.na(var_right_label))) var_right_label <- NULL
    
  } else var_right_label <- NULL
  
  
  ## Is select_id() not NA but not part of data() ------------------------------
  
  absent_id <- !is.na(select_id) && !select_id %in% data$ID
  
  
  ## Decide on table type ------------------------------------------------------
  
  comp_type <- if (var_right == " ") "uni" else "bi"
  
  var_type <- 
    if (comp_type == "uni" && all(is.null(var_left_label))) "quant" else
      if (comp_type == "bi" && all(is.null(var_left_label)) && 
          all(is.null(var_right_label))) "quantxy" else
            if (comp_type == "bi" && all(is.null(var_left_label)) && 
                !all(is.null(var_right_label))) "quantx" else
                  if (comp_type == "bi" && !all(is.null(var_left_label)) && 
                      all(is.null(var_right_label))) "quanty" else "qual"
  
  select_type <- 
    if (is.na(select_id) || absent_id) "all" else
      if (comp_type == "uni" && active_left == 0) "na" else
        if (active_right == 0) "na" else "select"
  
  table_type <- paste(comp_type, var_type, select_type, sep = "_")
  if (select_type == "na") table_type <- paste0(comp_type, "_na")
  if (delta) table_type <- paste0(table_type, "_delta")
  table_type <- unique(table_type)
  return(table_type)
  
}
