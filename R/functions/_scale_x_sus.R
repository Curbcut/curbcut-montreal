#### SUS SCALES FOR MAPS #######################################################

scale_fill_sus <- function(var, alpha = NULL) {
  scale_color_category(
    col = !!var, 
    palette = paste0(colour_table$value, alpha),
    unmapped_color = paste0(col_NA, alpha), 
    levels = colour_table$group,
    legend = FALSE
  )
}

scale_line_width_sus <- function(select_id) {
  scale_category(
    col = ID, 
    range = c(3, 1),
    unmapped_value = 1, 
    levels = c(select_id, "NA"),
    legend = FALSE
  )
}
