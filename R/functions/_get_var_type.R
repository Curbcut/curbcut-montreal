# GET EXPLORE VARIABLE TYPE ####################################################

get_var_type <- function(data, geo, var_left, var_right, df, select_id, 
                         build_str_as_DA = TRUE) {
  
  ## Invalidate if non-standard df() -------------------------------------------
  
  if (is.null(df) || !is_scale_in_df(c(all_choropleth, "grid"), df)) return(df)
  
  
  ## Identify NA tables --------------------------------------------------------
  
  if (length(var_right) == 2 && var_right[1] == var_right[2]) 
    return("NA_delta_bivar")
  if (length(var_left) == 2 && var_left[1] == var_left[2]) 
    return("NA_delta")
  
  
  ## Handle multiple dates then strip dates ------------------------------------
  
  delta <- 
    length(var_left) == 2 && (length(var_right) == 2 || var_right[1] == " ")
  deltax <- 
    length(var_left) == 2 && (length(var_right) == 1 && var_right[1] != " ")
  
  var_left <- unique(str_remove(var_left, "_\\d{4}$"))
  var_right <- unique(str_remove(var_right, "_\\d{4}$"))
  
  
  ## Selections ----------------------------------------------------------------
  
  select_df <- if (build_str_as_DA && is_scale_in_df("building", df)) {
    if (is.na(select_id)) get(paste(geo, "DA", sep = "_")) else {
      dbGetQuery(building_conn, 
                 paste0("SELECT * FROM ", paste(geo, "building", sep = "_"), 
                        " WHERE ID = '", select_id, "'"))
    }
  } else data
  if (nrow(select_df) == 0) select_id <- NA
  selection <- if (is.na(select_id)) select_df[0,] else 
    select_df[select_df$ID == select_id,]
  active_left <- if (build_str_as_DA && is_scale_in_df("building", df)) {
    sum(!is.na(data$var_left[data$ID == selection$DA_ID]))
  } else sum(!is.na(selection$var_left))
  active_right <- active_left
  if (length(var_right) != 1 || var_right != " ") 
    active_right <- if (build_str_as_DA && is_scale_in_df("building", df)) {
      sum(!is.na(data$var_left[data$ID == selection$DA_ID]) &
            !is.na(data$var_right[data$ID == selection$DA_ID])) 
    } else sum(!is.na(selection$var_left) & !is.na(selection$var_right))
  
  
  ## Is select_id() not NA but not part of data() ------------------------------
  
  absent_id <- !is.na(select_id) && !select_id %in% data$ID && !is_scale_in_df("building", df)
  
  
  ## Create var_left_label and var_right_label ---------------------------------
  
  built_df <- if (build_str_as_DA && is_scale_in_df("building", df)) 
    paste0(geo, "_DA") else df
  
  breaks_q5_left <- variables$breaks_q5[[
    which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
  
  if (length(breaks_q5_left) > 0) breaks_q5_left <- 
    breaks_q5_left[breaks_q5_left$scale == built_df,]
  
  var_left_label <- suppressWarnings(breaks_q5_left$var_name)
  if (all(is.na(var_left_label))) var_left_label <- NULL
  
  if (var_right != " ") {
    
    breaks_q5_right <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_right)))]]
    
    if (length(breaks_q5_right) > 0) breaks_q5_right <- 
        breaks_q5_right[breaks_q5_right$scale == built_df,]
    
    var_right_label <- suppressWarnings(breaks_q5_right$var_name)
    if (all(is.na(var_right_label))) var_right_label <- NULL
    
  } else var_right_label <- NULL
  
  
  ## Decide on table type ------------------------------------------------------
  
  comp_type <- if (var_right[1] == " ") "uni" else "bi"
  
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
  if (deltax) table_type <- paste0(table_type, "_deltax")
  

  ## Deal with NAs -------------------------------------------------------------

  if (all(is.na(data$var_left))) table_type <- "all_na"
  if (var_right != " " && all(is.na(data$var_right))) table_type <- "all_na"
  
  table_type <- unique(table_type)
  return(table_type)
  
}
