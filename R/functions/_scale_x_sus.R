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
      range = c(5, 1),
      unmapped_value = 1,
      levels = c(select_id, "NA"),
      legend = FALSE
    )
}

scale_line_width_street_sus <- function() {
    scale_category(
      col = street_type,
      range = c(3, 3, 3, 2, 2, 1, 1, 1, 1, 0.5),
      unmapped_value = 0,
      levels = c("motorway", "trunk", "primary", "secondary", "tertiary",
                 "motorway_link", "trunk_link", "primary_link", "residential", 
                 "service"),
      legend = FALSE
    )
}