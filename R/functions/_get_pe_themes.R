#### GET PLACE EXPLORER THEMES #################################################

get_pe_themes <- function(df, select_id) {
  
  themes <- 
    do.call("dbGetQuery", list(rlang::sym("pe_theme_order_conn"),
                               paste0("SELECT * FROM '", 
                                      paste(df, select_id, sep = "_"), "'")))
  
  mapply(c, themes$theme, themes$standout, SIMPLIFY = FALSE)
  }