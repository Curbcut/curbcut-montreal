#### GET TITLE CARD ############################################################

get_title_card <- function(r = r, geo, df, select_id) {

  ## Setup ---------------------------------------------------------------------
  
  # Choose indicators based on data availability
  indicators_table <- title_card_index

  
  ## Generate output grid ------------------------------------------------------
  
  to_grid <- lapply(seq_along(indicators_table$name), \(x) {
    
    z <- prep_title_card(r = r, 
                         geo = geo, 
                         df = df, 
                         select_id = select_id, 
                         ind = indicators_table$name[x],
                         percent = indicators_table$percent[x], 
                         high_is_good = indicators_table$high_is_good[x], 
                         val_digits = indicators_table$val_digits[x],
                         link_module = indicators_table$link_module[x], 
                         link_var_left = indicators_table$link_var_left[x], 
                         link_outside = indicators_table$link_outside[x])
    
    if (is.null(z)) return(NULL)
    
    if (indicators_table$name[x] == "air_quality_no2") higher_than_threshold <-
        if (z$pretty_data_var > 53) {
          curbcut::cc_t(lang = r$lang(), translation = translation, 
            "Its value is higher than the WHO's guideline value of 53. ")
        } else ""
    
    list(row_title = indicators_table$title[x],
         percentile = z$percentile,
         text = if (is.na(z$pretty_data_var)) curbcut::cc_t(lang = r$lang(), translation = translation, "No data.") else 
           curbcut::cc_t(lang = r$lang(), translation = translation, indicators_table$text[x]),
         link = z$link,
         link_module = z$link_module,
         link_var_left = z$link_var_left,
         hex_cat = z$hex_cat)
  })

  names(to_grid) <- indicators_table$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid

}
