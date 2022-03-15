#### SUS FILL SCALE FOR MAPS ###################################################

scale_fill_sus <- function(var, alpha = NULL) {
  scale_color_category(
    col = !!var, 
    palette = paste0(colour_table$value, alpha),
    unmapped_color = paste0(col_NA, alpha), 
    levels = colour_table$group,
    legend = FALSE
  )
}
