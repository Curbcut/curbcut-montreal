#### GET TITLE CARD ############################################################

#' @param island_or_region A character string, either "region" or "island".

get_title_card <- function(df, select_id, island_or_region) {

  ## Setup ---------------------------------------------------------------------

  # Test if comparison will be only on island
  on_island <- if (island_or_region == "island") TRUE else FALSE

  # Choose indicators based on data availability
  indicators_table <-
    if (on_island) title_card_index else {
      title_card_index[title_card_index$island_only == FALSE, ]
    }

  # Get scale names
  geo_area <- switch(df, "borough" = "borough/city", "CT" = "census tract",
                     "DA" = "dissemination area")
  geo_areas <- switch(df, "borough" = "boroughs or cities",
                      "CT" = "census tracts", "DA" = "dissemination areas")

  
  ## Generate output grid ------------------------------------------------------
  
  to_grid <- lapply(seq_along(indicators_table$name), \(x) {
    
    z <- prep_title_card(df, select_id, 
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
          sus_translate(
            "Its value is higher than the WHO's guideline value of 5. ")
        } else ""
    
    list(row_title = indicators_table$title[x],
         percentile = z$percentile,
         graph = z$plot,
         text = if (is.na(z$pretty_data_var)) sus_translate("No data.") else 
           sus_translate(indicators_table$text[x]),
         link = z$link,
         link_module = z$link_module,
         link_var_left = z$link_var_left)
  })

  names(to_grid) <- indicators_table$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid

}
