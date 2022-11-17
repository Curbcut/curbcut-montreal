#### GET DATA ##################################################################

get_data_color <- function(map_zoom_levels, geo, var_left, var_right,
                           build_str_as_DA = TRUE) {

  dfs <- names(map_zoom_levels)
  dfs <- dfs[!dfs %in% c("building")]
  if (var_right[1] != " ") dfs <- dfs[!dfs %in% c("DB")]
  
  dfs <- paste(geo, dfs, sep = "_")
  
  # Get data type
  data_type <- get_data_type(dfs[1], var_left, var_right, 
                             build_str_as_DA)
  

  ## Get data table ------------------------------------------------------------
  
  datas <- lapply(dfs, get_data_table, geo, var_left, var_right, data_type)
  
  keep_cols <- sapply(datas, names) |> 
    unlist() |>  
    table() |> 
    (\(x) x[x == max(x)])() |> 
    names()
  
  datas <- lapply(datas, \(x) x[, keep_cols])
  
  data <- Reduce(rbind, datas)
  
  ## Create coloring table -----------------------------------------------------
  
  data_color <- 
    if (data_type == "q5") {
      out <- merge(data, colour_table, by.x = "var_left_q5", 
                   by.y = "group")[, c(group = "ID", value = "value")]
      names(out) <- c("group", "value")
      out
    } else if (grepl("bivar", data_type)) {
      out <- merge(data, colour_bivar, by.x = "group", 
                   by.y = "group")[, c("ID", "fill")]
      names(out) <- c("group", "value")
      out
    } else if (grepl("delta", data_type)) {
      
      val_delta <- rep("#0571B0", length(data$var_left))
      val_delta[data$var_left < 0.1] <- "#92C5DE"
      val_delta[data$var_left < 0.02] <- "#E8E8E8"
      val_delta[data$var_left < -0.02] <- "#F4A582"
      val_delta[data$var_left < -0.1] <- "#CA0020"
      val_delta[is.na(data$var_left)] <- "#B3B3BB"
      
      data.frame(group = data$ID, value = val_delta)
      
    }
  
  ## Return output -------------------------------------------------------------

  return(data_color)
  
}
