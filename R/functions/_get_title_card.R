#### GET TITLE CARD ############################################################

get_title_card <- function(df, select_id, island_only_comparison) {
  
  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  
  if (on_island && island_only_comparison == "region") on_island <- FALSE
  
  indicators_table <- if (!filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID) {
    title_card_index[title_card_index$island_only == FALSE, ]
  } else title_card_index
  
  geo_area <- switch(df, "borough" = "borough/city",
                     "CT" = "census tract",
                     "DA" = "dessimination area")
  geo_areas <- switch(df, "borough" = "boroughs or cities",
                      "CT" = "census tracts",
                      "DA" = "dessimination areas")
  
  to_grid <- 
    map(set_names(indicators_table$name), ~{
      
      iteration_result <- 
        if (.x == "transit_walk_cycle_share") {
          
          z <- prep_title_card(df, select_id, ind =  "transit_walk_cycle_share",
                               island = on_island, geo_area = geo_area,
                               geo_areas = geo_areas)
          
          text <- 
            if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
              sus_translate("{z$pretty_data_var} of residents ",
                            "use public transit, walk or ",
                            "bicycle to get to work. {z$data_rank}. ",
                            "(Data from {z$data_date})")
            }
          
          list(row_title = "Sus. transport",
               percentile = z$percentile,
               graph = z$plot,
               text = text)
          
        } else if (.x == "single_detached") {
          
          z <- prep_title_card(df, select_id, ind = "single_detached",
                               island = on_island, high_is_good = FALSE, 
                               geo_area = geo_area, geo_areas = geo_areas)
          
          text <- 
            if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
              sus_translate("{z$pretty_data_var} of occupied dwellings are ",
                            "single-detached houses. {z$data_rank}. ",
                            "(Data from {z$data_date})")
            }
          
          
          list(row_title = "Housing",
               percentile = z$percentile,
               graph = z$plot,
               text = text)
          
        } else if (.x == "total_crash_per1k") {
          
          z <- prep_title_card(df, select_id, ind = "total_crash_per1k",
                               percent = FALSE,
                               island = on_island, high_is_good = FALSE, 
                               geo_area = geo_area, geo_areas = geo_areas)
          
          text <- 
            if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
              sus_translate("There were {z$pretty_data_var} total crashes ",
                            "per 1,000 residents in {z$data_date}. ", 
                            "{z$data_rank}. ")
            }
          
          list(row_title = "Road safety",
               percentile = z$percentile,
               graph = z$plot,
               text = text)
          
        } else if (.x == "air_quality_no2") {
          
          z <- prep_title_card(df, select_id, ind = "air_quality_no2",
                               percent = FALSE, val_digits = 1,
                               island = on_island, high_is_good = FALSE, 
                               geo_area = geo_area, geo_areas = geo_areas)
          
          higher_than_threshold <- 
            if (z$pretty_data_var > 5) {
              sus_translate("Its value is higher than WHO's guideline value of 5. ")
            } else ""
          
          list(row_title = "Air pollution",
               percentile = z$percentile,
               graph = z$plot,
               text = sus_translate("{z$data_rank} in terms of level of NO2 ",
                                    "pollution. {higher_than_threshold}(NO2 = ",
                                    "{z$pretty_data_var}, data from {z$data_date})"))
          
        } else if (.x == "green_space_ndvi") {
          
          z <- prep_title_card(df, select_id, ind = "green_space_ndvi",
                               percent = TRUE,
                               island = on_island, high_is_good = TRUE, 
                               geo_area = geo_area, geo_areas = geo_areas)
          
          text <- 
            if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
              sus_translate("{z$data_rank} in terms of greenery. (NDVI ", 
                            "= {z$pretty_data_var}, data from ", 
                            "{z$data_date})")
            }
          
          list(row_title = "Greenery",
               percentile = z$percentile,
               graph = z$plot,
               text = text)
          
          
        } else if (.x == "canale_index") {
          
          z <- prep_title_card(df, select_id, ind = "canale_index",
                               percent = FALSE,
                               island = on_island, high_is_good = TRUE, 
                               geo_area = geo_area, geo_areas = geo_areas)
          
          text <- 
            if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
              sus_translate("{z$data_rank} in terms of active living. ",
                            "(Data from {z$data_date})")
            }
          
          list(row_title = "Active living",
               percentile = z$percentile,
               graph = z$plot,
               text = text)
        }
      
      iteration_result
      
    })
  
  to_grid[sapply(to_grid, is.null)] <- NULL
  
  to_grid
  
}
