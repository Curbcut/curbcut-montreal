# Function to decide explore_graph and info_table details

explore_var_type <- function(id, x, var_left, var_right, select, 
                             var_left_label, var_right_label) {
  
  reactive({
    
    ## Handle multiple dates then strip dates ----------------------------------
    
    multi <- length(var_left()) == 2 || length(var_right()) == 2
    var_left <- str_remove(var_left(), "_\\d{4}$")
    var_right <- str_remove(var_right(), "_\\d{4}$")
    
    
    ## Titles and explanations -------------------------------------------------
    
    exp_left <- var_exp[var_exp$var_code == var_left,]$explanation
    exp_right <- var_exp[var_exp$var_code == var_right,]$explanation
    if (length(exp_left) == 0) warning("No var_exp: ", var_left, call. = FALSE)
    if ((length(var_right) != 1 || var_right != " ") && 
        length(exp_right) == 0) warning(
          "No var_exp: ", var_right, call. = FALSE)
    
    
    ## Selections --------------------------------------------------------------
    
    selection <- x() %>% filter(ID == select())
    active_left <- nrow(filter(selection, !is.na(left_var)))
    active_right <- active_left
    if (length(var_right()) != 1 || var_right() != " ") active_right <- 
      nrow(filter(selection, !is.na(left_var), !is.na(right_var)))
    
    
    ## Decide on table type ----------------------------------------------------
    
    comp_type <- case_when(
      var_right() == " " ~ "uni",
      TRUE ~ "bi")
    
    var_type <- case_when(
      comp_type == "uni" & is.null(var_left_label) ~ "quant",
      comp_type == "bi" & is.null(var_left_label) & is.null(var_right_label) ~ 
        "quantxy",
      comp_type == "bi" & is.null(var_left_label) & !is.null(var_right_label) ~ 
        "quantx",
      comp_type == "bi" & !is.null(var_left_label) & is.null(var_right_label) ~ 
        "quanty",
      TRUE ~ "qual")
    
    select_type <- unique(case_when(is.na(select()) ~ "all", 
                                    comp_type == "uni" & active_left == 0 ~ "na",
                                    active_right == 0 ~ "na",
                                    TRUE ~ "select"))
    
    table_type <- paste(comp_type, var_type, select_type, sep = "_")
    if (select_type == "na") table_type <- paste0(comp_type, "_na")
    if (multi) table_type <- paste0(table_type, "_multi")
    
    table_type
  })

}