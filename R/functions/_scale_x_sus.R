#### SUS SCALES FOR MAPS #######################################################

# Default scales ----------------------------------------------------------

# Default fill
scale_fill_sus <- function(module_colors, id = "ID_color") {
  scale_color_category(
    col = !!rlang::sym(id), 
    palette = module_colors$value,
    unmapped_color = "#B3B3BB", 
    levels = module_colors$group,
    legend = FALSE)
}

# Default line colour
scale_colour_sus <- function(...) "#FFFFFF"

# Default line width
scale_lwd_sus <- function(select_id, tile = NULL, zoom = NULL, 
                          zoom_levels = NULL) {
  
  if (!is.null(tile) && !is.null(zoom) && !grepl("auto_zoom", tile) &&
      !is.null(zoom_levels)) {
    if (is_scale_in_df(names(zoom_levels), tile)) {
      normal_zoom <- zoom_levels[sapply(names(zoom_levels), is_scale_in_df, tile)]
      # print(normal_zoom)
      if (zoom < (normal_zoom - 1)) late_range <- 0 else late_range <- 1
    } else {
      late_range <- 1
    }
  } else {
    late_range <- 1
  }
  
  scale_category(
    col = !!rlang::sym("ID_color"),
    range = c(5, late_range),
    unmapped_value = late_range,
    levels = c(select_id, "NA"),
    legend = FALSE) 
}

# Module-specific scales --------------------------------------------------

# Climate_risk lwd
scale_lwd_climate_risk <- function(select_id, tile) {
  
  scale_category(
    col = ID_color,
    range = c(5, if (is_scale_in_df("grid", tile)) 0.3 else 1),
    unmapped_value = if (is_scale_in_df("grid", tile)) 0.3 else 1,
    levels = c(select_id, "NA"),
    legend = FALSE) 
  
}

# Alley fill
scale_fill_alley <- function(var, tile, data_color) {
  if (tile == "borough_empty") {
    "#FFFFFF00"
  } else if (tile == "alley") {
    scale_color_category(
      col = !!rlang::sym(var), 
      palette = colour_table$value,
      unmapped_color = colour_table$value[1], 
      levels = colour_table$group,
      legend = FALSE)
  } else scale_fill_sus(data_color, id = "ID")
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

# Place explorer fill
scale_fill_pe <- function(select_id) {
  scale_color_category(
    col = ID,
    palette = c("#BAE4B3BB", "#BAE4B300"),
    unmapped_color = "#BAE4B300",
    levels = c(select_id, "NA"),
    legend = FALSE) 
}

# Natural infrastructure lws
scale_lwd_natural_inf <- function() 0

# Natural infrastructure fill
scale_fill_natural_inf <- function(var, tile, natural_inf_colours) {
    
    if (!is.null(natural_inf_colours)) colour_table <- natural_inf_colours

    scale_color_category(
      col = !!rlang::sym(var), 
      palette = colour_table$value,
      unmapped_color = "#FFFFFF00", 
      levels = colour_table$group,
      legend = FALSE)
  
}
