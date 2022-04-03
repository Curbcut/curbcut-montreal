#### SUS SCALES FOR MAPS #######################################################

# Default scales ----------------------------------------------------------

# Default fill
scale_fill_sus <- function(var) {
  scale_color_category(
    col = !!rlang::sym(var), 
    palette = colour_table$value,
    unmapped_color = colour_table$value[1], 
    levels = colour_table$group,
    legend = FALSE)
}

# Default line colour
scale_colour_sus <- function(...) "#FFFFFF"

# Default line width
scale_lwd_sus <- function(select_id) {
  scale_category(
    col = ID,
    range = c(5, 1),
    unmapped_value = 1,
    levels = c(select_id, "NA"),
    legend = FALSE) 
}


# Module-specific scales --------------------------------------------------

# Climate_risk lwd
scale_lwd_climate_risk <- function(select_id, tile) {
  
  scale_category(
    col = ID,
    range = c(5, if (tile == "grid") 0.3 else 1),
    unmapped_value = if (tile == "grid") 0.3 else 1,
    levels = c(select_id, "NA"),
    legend = FALSE) 
  
}

# Alley fill
scale_fill_alley <- function(var, tile) {
  if (tile == "borough_empty") return("#FFFFFF00")

  scale_color_category(
    col = !!var, 
    palette = colour_table$value,
    unmapped_color = colour_table$value[1], 
    levels = colour_table$group,
    legend = FALSE
  )
}

# Alley colour
scale_colour_alley <- function(var, tile) {
  if (tile == "borough_empty") {
    "#008100"
  } else if (tile == "alley") {
    scale_color_category(
      col = !!rlang::sym(var), 
      palette = colour_table$value,
      unmapped_color = colour_table$value[1], 
      levels = colour_table$group,
      legend = FALSE)
  } else "#FFFFFF"
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



scale_line_width_alley <- function(select_id) {
    scale_category(
      col = ID,
      range = c(4, 2.5),
      unmapped_value = 2.5,
      levels = c(select_id, "NA"),
      legend = FALSE)
}