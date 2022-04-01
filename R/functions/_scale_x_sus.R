#### SUS SCALES FOR MAPS #######################################################

scale_fill_sus <- function(var, id, tile, access_colors = NULL) {
  if (id == "alley" && tile == "empty_borough") return("#FFFFFF00")
  if (!is.null(access_colors)) colour_table <- access_colors
  
  scale_color_category(
    col = !!var, 
    palette = colour_table$value,
    unmapped_color = colour_table$value[1], 
    levels = colour_table$group,
    legend = FALSE
  )
}

scale_line_width_sus <- function(id, select_id, tile) {
  
  if (id == "alley" && tile %in% c("empty_borough", "alley")) {
    scale_category(
      col = ID,
      range = c(4, 2.5),
      unmapped_value = 2.5,
      levels = c(select_id, "NA"),
      legend = FALSE)
  } else {
    scale_category(
      col = ID,
      range = c(5, if (tile == "grid") 0.3 else 1),
      unmapped_value = if (tile == "grid") 0.3 else 1,
      levels = c(select_id, "NA"),
      legend = FALSE) 
  }
    
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

scale_line_color_alley <- function(var, tile) {
  if (tile == "empty_borough") return("#008100")
  if (tile == "alley") return(scale_color_category(
    col = !!rlang::sym(var), 
    palette = colour_table$value,
    unmapped_color = colour_table$value[1], 
    levels = colour_table$group,
    legend = FALSE))
  
  "#FFFFFF"
}