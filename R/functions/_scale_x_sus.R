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
  if (tile == "borough_empty") {
    "#FFFFFF00"
  } else scale_fill_sus(var)
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

# Alley lwd
scale_lwd_alley <- function(select_id, tile) {
  if (tile == "borough_empty") {
    scale_category(
      col = ID,
      range = c(5, 1),
      unmapped_value = 1,
      levels = c(select_id, "NA"),
      legend = FALSE) 
  } else if (tile == "alley") {
    scale_category(
      col = ID,
      range = c(6, 3),
      unmapped_value = 3,
      levels = c(select_id, "NA"),
      legend = FALSE) 
  } else scale_lwd_sus(select_id)
}

# Access fill
scale_fill_access <- function(var, tile, access_colors) {
  if (!is.null(access_colors)) {
    colour_table <- access_colors
    unmapped_color <- "#B3B3BB"
  } else {
    unmapped_color <- colour_table$value[1]
  }
  
  scale_color_category(
    col = !!rlang::sym(var), 
    palette = colour_table$value,
    unmapped_color = unmapped_color, 
    levels = colour_table$group,
    legend = FALSE)
}

