#### GET PLACE EXPLORER BLOCK PLOT #############################################

get_pe_block_plot <- function(df, theme, select_id, island_or_region,
                              data_order) {
  
  if (df == "CT" && theme == "Transport") data_order <- unique(data_order)
  
  raw_data_var <- pe_var_hierarchy[[df]][
    names(pe_var_hierarchy[[df]]) %in% data_order$var_code]
  
  # Plot
  plots <- lapply(data_order$var_code, \(var_code) {
    
    hex_to_plot <- "#A9A9A9"
    
    data <- raw_data_var[[var_code]]
    data_var <- data$var[data$ID == select_id]
    
    if (!is.na(data_var)) {
      
      dat <- data[!is.na(data$var),]
      outliers <- find_outliers(dat$var)
      if (length(outliers) > 0 && data_var %in% dat$var[outliers]) {
        dat <- dat[-outliers,]
      }
      
      ggplot(dat) +
        geom_density(aes(x = var), size = 1, color = hex_to_plot) +
        geom_vline(aes(xintercept = data_var), color = "#000000", size = 1,
                   alpha = 1) +
        theme_void()
    } else ggplot()
    
  })
  
}
