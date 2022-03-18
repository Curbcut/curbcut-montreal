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

# scale_line_width_sus <- function(select_id) {
#   scale_category(
#     col = ID,
#     range = c(3, 1),
#     unmapped_value = 1,
#     levels = c(select_id, "NA"),
#     legend = FALSE
#   )
# }

scale_line_width_sus <- function(select_id, zoom) {
  if (zoom == "DA") {
    scale_category(
      col = ID,
      range = c(5, 0),
      unmapped_value = 0,
      levels = c(select_id, "NA"),
      legend = FALSE
    )
  } else {
    scale_category(
      col = ID,
      range = c(3, 1),
      unmapped_value = 1,
      levels = c(select_id, "NA"),
      legend = FALSE
    )
  }
}

# scale_line_width_texture_sus <- function(select_id, zoom) {
#   
#   if (zoom %in% c("DA") && !is.na(select_id)) {
#     scale_category(
#       col = street_type, 
#       range = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#       unmapped_value = 0, 
#       levels = c("motorway", "trunk", "primary",
#                  "secondary", "tertiary",
#                  "motorway_link", "trunk_link",
#                  "primary_link", "residential", "service"),
#       legend = FALSE
#     )
#   } else {
#     scale_category(
#       col = street_type, 
#       range = switch(zoom,
#                      "borough" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#                      "CT" = c(3, 3, 3, 0, 0, 0, 0, 0, 0, 0),
#                      "DA" = c(4, 4, 4, 2.5, 2.5, 1, 1, 1, 1, 0.5),
#                      "building" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
#       unmapped_value = 0, 
#       levels = c("motorway", "trunk", "primary",
#                  "secondary", "tertiary",
#                  "motorway_link", "trunk_link",
#                  "primary_link", "residential", "service"),
#       legend = FALSE
#     )
#   }
# }


scale_line_width_texture_sus <- function(select_id, zoom) {

  if (zoom %in% c("DA") && !is.na(select_id)) {
    scale_category(
      col = street_type,
      range = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      unmapped_value = 0,
      levels = c("motorway", "trunk", "primary",
                 "secondary", "tertiary",
                 "motorway_link", "trunk_link",
                 "primary_link", "residential", "service"),
      legend = FALSE
    )
  } else {
    scale_category(
      col = street_type,
      range = c(3.5, 3.5, 3.5, 2.5, 2.5, 1, 1, 1, 1, 0.5),
      unmapped_value = 0,
      levels = c("motorway", "trunk", "primary",
                 "secondary", "tertiary",
                 "motorway_link", "trunk_link",
                 "primary_link", "residential", "service"),
      legend = FALSE
    )
  }
}