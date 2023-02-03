#### PREPARE TITLE CARD ROW ####################################################

prep_title_card <- function(r = r, geo, df, select_id, ind, percent = TRUE,
                            high_is_good = TRUE, val_digits = 0,
                            link_module = NULL, link_var_left = NULL,
                            link_outside = NULL) {
  
  # Setup ----------------------------------------------------------------------
  
  geo_area <- cc_t(r = r, "The ", scales_dictionary$sing[scales_dictionary$scale == df])
  geo_areas <- cc_t(r = r, scales_dictionary$plur[scales_dictionary$scale == df])
  
  # To what it compares
  to_compare <- 
    cc_t(r = r, regions_dictionary$to_compare[regions_dictionary$geo == geo])
  
  # Prepare list to store all data
  info <- list()
  
  data <- title_card_indicators[[ind]][[paste(geo, df, sep = "_")]]
  data_var <- data$var[data$ID == select_id]
  if (length(data_var) == 0) return(NULL)
  
  
  # pretty_data_var ------------------------------------------------------------
  
  info$pretty_data_var <- if (percent) {
    scales::percent(data_var)
  } else round(data_var, digits = val_digits)
  
  
  # Data date ------------------------------------------------------------------
  
  info$data_date <- title_card_index$date[title_card_index$name == ind]
  
  
  # Data rank ------------------------------------------------------------------
  
  data_rank <- data[data$ID == select_id, ]$percentile
  
  if (nrow(get(paste(geo, df, sep = "_"))) < 40) {
    
    rank <- data[data$ID == select_id, ]$rank
    df_row <- sum(!is.na(data$var))
    
    # If high is good, then last rank means 1st. Inverse!
    data_CSD_rank <- if (high_is_good) df_row - rank + 1 else rank
    
    text_data_rank <- 
      if (data_CSD_rank > 2 / 3 * df_row) {
        cc_t(r = r, "relatively low at {ordinal_form(r = r, data_CSD_rank)}")
      } else if (data_CSD_rank > 1 / 3 * df_row) {
        ordinal_form(r = r, data_CSD_rank)
      } else {
        if (r$lang() == "fr" && {ordinal_form(r = r, data_CSD_rank)} == "") {
          "premier"
        } else cc_t(r = r, "{ordinal_form(r = r, data_CSD_rank)} best")
      }
    
    info$data_rank <- 
      cc_t(r = r, "It ranks {text_data_rank} {to_compare}")
    
  } else {
    
    info$data_rank <-
      if (is.na(data_rank)) {
        
        ""
        
      } else if (data_rank > 0.75) {
        
        paste0(cc_t(r = r, "{geo_area} ranks in the top "),
               if (abs(data_rank - 1) < 0.01) "1%" else 
                 scales::percent(abs(data_rank - 1)))
        
      } else if (data_rank < 0.25) {
        
        paste0(
          cc_t(r = r, "{geo_area} ranks in the bottom "),
          if (data_rank == 0) "1%" else scales::percent(data_rank))
        
      } else {
        
        scale_percent_data_rank <- scales::percent(data_rank)
        
        if (ind == "air_quality_no2") {
          cc_t(r = r, "Its value is worse than {scale_percent_data_rank} ",
                        "of {geo_areas} {to_compare}")
        } else {
          cc_t(r = r, "Its value is higher than {scale_percent_data_rank} ",
                        "of {geo_areas} {to_compare}")
        }
        
      }
  }
  
  
  # Colour ---------------------------------------------------------------------
  
  colours_which <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  if (!high_is_good) colours_which <- rev(colours_which)
  
  info$hex_cat <- which.min(abs(
    colours_which - data[data$ID == select_id, ]$percentile))
  
  # In case it's higher than the threshold of 5 for Air Quality
  if (ind == "air_quality_no2" && data_var >= 53) info$hex_cat <- 1
  
  
  # Percentile -----------------------------------------------------------------

  info$percentile <- 
    if (is.na(data_rank)) {
      paste0("")
    } else if (data_rank > 0.50) {
      per <- scales::percent(abs(data_rank - 1))
      if (per == "0%") per <- "1%"
      paste0(cc_t(r = r, "Top {per}"))
    } else {
      per <- scales::percent(abs(data_rank))
      if (per == "0%") per <- "1%"
      paste0(cc_t(r = r, "Bottom {per}"))
    }
  
  return(info)
  
}
