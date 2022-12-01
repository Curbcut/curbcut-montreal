#### GET TITLE CARD ############################################################

get_title_card <- function(r = r, df, select_id) {

  ## Setup ---------------------------------------------------------------------
  
  on_island <- select_id %in% c(island_DA$DA_ID, island_CT$ID, island_CSD$ID)

  # Choose indicators based on data availability
  indicators_table <-
    if (on_island) title_card_index else {
      title_card_index[title_card_index$island_only == FALSE, ]
    }

  # Get scale names
  geo_area <- switch(gsub(".*_", "", df), 
                     "CSD" = "borough/city",
                     "CT" = "census tract",
                     "DA" = "dissemination area",
                     "centraide" = "centraide zone",
                     "zone")
  geo_areas <-  switch(gsub(".*_", "", df), 
                       "CSD" = "boroughs or cities",
                       "CT" = "census tracts", 
                       "DA" = "dissemination areas",
                       "centraide" = "centraide zones",
                       "zones")

  
  ## Generate output grid ------------------------------------------------------
  
  to_grid <- lapply(seq_along(indicators_table$name), \(x) {
    
    z <- prep_title_card(r = r, df, select_id, 
                         ind = indicators_table$name[x],
                         percent = indicators_table$percent[x], 
                         high_is_good = indicators_table$high_is_good[x], 
                         val_digits = indicators_table$val_digits[x],
                         link_module = indicators_table$link_module[x], 
                         link_var_left = indicators_table$link_var_left[x], 
                         link_outside = indicators_table$link_outside[x],
                         island = on_island, 
                         geo_area = geo_area,
                         geo_areas = geo_areas)
    
    if (is.null(z)) return(NULL)
    
    if (indicators_table$name[x] == "air_quality_no2") higher_than_threshold <-
        if (z$pretty_data_var > 5) {
          cc_t(r = r, 
            "Its value is higher than the WHO's guideline value of 5. ")
        } else ""
    
    list(row_title = indicators_table$title[x],
         percentile = z$percentile,
         graph = z$plot,
         text = if (is.na(z$pretty_data_var)) cc_t(r = r, "No data.") else 
           cc_t(r = r, indicators_table$text[x]),
         link = z$link,
         link_module = z$link_module,
         link_var_left = z$link_var_left)
  })

  names(to_grid) <- indicators_table$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid

}
