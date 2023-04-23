### STORIES GLOBAL #############################################################

# Temporary non-translated stories:
available_stories <- list.files("www/stories", full.names = TRUE) |> 
  str_subset(".html")



# Cycling infrastructure map, legend and source ---------------------------

cycling_infrastructure_map <- function(slider) {
  rdeck_proxy("stories_custom_map") |>
    add_mvt_layer(
      id = "stories_custom_map",
      data = mvt_url("sus-mcgill.stories-cycling_infrastructure"),
      visible = TRUE,
      get_line_width = 2,
      get_line_color = !!rlang::sym(paste0("fill_", slider)),
      line_width_units = "pixels",
      get_fill_color = !!rlang::sym(paste0("fill_", slider)),
      get_point_radius = 10)
}

cycling_infrastructure_legend <- function(year, ...) {
  
  types_df <- 
    data.frame(year = c(1986, 1991, 1996, 2001, 2006, 2011, 2016, 2022),
               types = I(list(c("Bicycle path"), 
                              c("New bicycle path", "Removed path", "Old path"),
                              c("New bicycle path", "Removed path", "Old path"),
                              c("New bicycle path", "Removed path", "Old path"),
                              c("New bicycle path", "Removed path", "Old path"),
                              c("New bicycle path", "Removed path", "Old path"),
                              c("New bicycle path", "Removed path", "Old path", "Bixi station"),
                              c("Bicycle path", "Bixi station"))))
  
  types <- 
    unlist(types_df$types[types_df$year == year])
  
  type_fill <- 
    data.frame(type = c("Bicycle path", "New bicycle path", "Removed path", 
                        "Old path", "Bixi station"),
               fill = c("#73AE80", "#73AE80", "#CA0020", "#2E4633", "#000000"))
  
  label <- type_fill$type[type_fill$type %in% types]
  
  legend <- 
    data.frame(x = seq_along(label),
               y = 1,
               fill = 
                 unique(type_fill[type_fill$type %in% types, ]$fill))
  
  legend |> 
    ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
               fill = fill)) +
    geom_rect() + 
    scale_x_continuous(breaks = seq_along(label) - 0.5, labels = label) +
    scale_y_continuous(labels = NULL) +
    scale_fill_manual(values = setNames(
      legend$fill, legend$fill)) +
    theme_minimal() +
    theme(text = element_text(family = "SourceSansPro", size = 12),
          legend.position = "none", 
          panel.grid = element_blank())
}

cycling_infrastructure_source <- function(year, r) {
  if (year %in% c(1986:2016)) {
    paste0("See Note 1. Data provided by: Houde, M., Apparicio, P., & Ségu",
           "in, A.-M. (2018). A ride for whom: Has cycling network expansi",
           "on reduced inequities in accessibility in Montreal, Canada? Jo",
           "urnal of Transport Geography, 68, 9–21. https://doi.org/10.101",
           "6/j.jtrangeo.2018.02.005")
  } else {
    paste0("See Note 2. Data provided by: Winters et al. The Canadian Bi",
           "keway Comfort and Safety Metrics (Can-BICS): Measuring the b",
           "icycling environment in all communities in Canada – for subm",
           "ission to Health Reports (forthcoming).")
  }
}

cycling_infrastructure_choices <- function() {
  c(1986, 1991, 1996, 2001, 2006, 2011, 2016, 2022)
}

