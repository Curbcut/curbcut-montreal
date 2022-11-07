#### PREPARE TITLE CARD ROW ####################################################

prep_title_card <- function(r = r, df, select_id, ind, percent = TRUE,
                            high_is_good = TRUE, val_digits = 0,
                            link_module = NULL, link_var_left = NULL,
                            link_outside = NULL, island = TRUE,
                            geo_area = geo_area, geo_areas = geo_areas) {
  
  # Setup ----------------------------------------------------------------------
  
  scale <- if (island) "island" else "region"
  
  geo_area <- cc_t(r = r, "The ", geo_area)
  geo_areas <- cc_t(r = r, geo_areas)
  
  # Prepare list to store all data
  info <- list()
  
  data <- title_card_indicators[[ind]][[df]]
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
  
  if (is_scale_in_df("CSD", df)) {
    
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
    
    text_island_region <- if (island) cc_t(r = r, " on the island") else 
      cc_t(r = r, " in the region")
    
    info$data_rank <- 
      cc_t(r = r, "It ranks {text_data_rank} {text_island_region}")
    
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
        text_island_region <- if (island) cc_t(r = r, "on the island") else 
          cc_t(r = r, "in the region")
        
        if (ind == "air_quality_no2") {
          cc_t(r = r, "Its value is worse than {scale_percent_data_rank} ",
                        "of {geo_areas} {text_island_region}")
        } else {
          cc_t(r = r, "Its value is higher than {scale_percent_data_rank} ",
                        "of {geo_areas} {text_island_region}")
        }
        
      }
  }
  
  
  # Module link ----------------------------------------------------------------
  
  if (!is.na(link_module) && !is.null(link_module) && 
      # Is the link module active?
      link_module %in% unlist(mods_rdy)) {
    
    info$link <- paste0(" <a id='place_explorer-title_card_", ind, "' href='#'", 
                        " class='action-button shiny-bound-input'>",
                        cc_t(r = r, "[LEARN MORE]"), "</a>")
    info$link_module <- link_module
    info$link_var_left <- link_var_left
    
  } else if (!is.na(link_outside) && !is.null(link_outside)) {
    
    info$link <- paste0(" <a href='", link_outside, "' target='_blank'>",
                        cc_t(r = r, "[LEARN MORE]"), "</a>")
    info$link_module <- NULL
    info$link_var_left <- NULL
    
  } else {
    
    info$link <- NULL
    info$link_module <- NULL
    info$link_var_left <- NULL
    
  }
  
  
  # Colour ---------------------------------------------------------------------
  
  colours_which <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  if (!high_is_good) colours_which <- rev(colours_which)
  
  hex_to_plot <- col_pe[which.min(abs(
    colours_which - data[data$ID == select_id, ]$percentile))]
  
  # In case it's higher than the threshold of 5
  if (ind == "air_quality_no2" && data_var >= 5) hex_to_plot <- col_pe[1]
  
  
  # Percentile -----------------------------------------------------------------

  info$percentile <- 
    if (is.na(data_rank)) {
      paste0("<p style = 'font-size: small; margin:auto; text-align:center;",
             "color:", "#999999","'></p>")
    } else if (data_rank > 0.50) {
      per <- scales::percent(abs(data_rank - 1))
      if (per == "0%") per <- "1%"
      paste0("<p style = 'font-size: small; margin:auto; text-align:center;",
             "color:", hex_to_plot,"'>", cc_t(r = r, "Top {per}"), "</p>")
    } else {
      per <- scales::percent(abs(data_rank))
      if (per == "0%") per <- "1%"
      paste0("<p style = 'font-size: small; margin:auto; text-align:center;",
             "color:", hex_to_plot,"'>", cc_t(r = r, "Bottom {per}"), "</p>")
    }
  
  
  # Plot -----------------------------------------------------------------------
  
  info$plot <-
    if (length(hex_to_plot) > 0) {
      
      dat <- data[!is.na(data$var),]
      outliers <- find_outliers(dat$var)
      if (length(outliers) > 0 && !data_var %in% dat$var[outliers]) {
          dat <- dat[-outliers, ]
      }
      
      dat |> 
        ggplot() +
        geom_density(aes(x = var), size = 1, color = hex_to_plot) +
        geom_vline(aes(xintercept = data_var), color = "#000000", size = 1) +
        theme_void() +
        theme(panel.background = element_rect(fill = "#fbfbfb", colour = NA),
              plot.background = element_rect(fill = "#fbfbfb", colour = NA))
      
    } else ggplot() +
    theme(panel.background = element_rect(fill = "#fbfbfb", colour = NA),
          plot.background = element_rect(fill = "#fbfbfb", colour = NA))
  
  
  return(info)
  
}
