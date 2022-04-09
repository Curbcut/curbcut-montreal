#### GET PLACE EXPLORER DATA ORDER #############################################

get_pe_data_order <- function(df, theme, select_id, island_or_region) {
  data_order <- pe_variable_order[[df]]
  data_order <- data_order[[island_or_region]]
  data_order <- data_order[data_order$theme == theme,]
  data_order <- data_order[data_order$ID == select_id, "var_code"]
  data_order
}