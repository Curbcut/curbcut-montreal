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

# Metro evolution map and legend ------------------------------------------

metro_evolution_map <- function(year) {
  rdeck_proxy("stories_custom_map") |>
    add_mvt_layer(
      id = "stories_custom_map",
      data = mvt_url("sus-mcgill.stories-metro_evolution"),
      visible = TRUE,
      get_line_width = 5,
      get_line_color = !!rlang::sym(paste0("fill_", year)),
      line_width_units = "pixels")
}

metro_evolution_legend <- function(date, lang) {
  
  metro_evolution <- 
    data.frame(type_en = c('Blue line', 'Blue line', 'Blue line \nextension', 
                           'Green line', 'Green line', 'Green line', 'Green line', 
                           'Green line', 'Orange line', 'Orange line', 'Orange line', 
                           'Orange line', 'Orange line', 'Proposed blue line', 
                           'Proposed \nblue line \nextension', 'Proposed extension', 
                           'Proposed extension', 'Proposed extension', 
                           'Proposed extensions', 'Proposed \ngreen line \nextension', 
                           'Proposed line', 'Proposed line', 'Proposed \nline 10', 
                           'Proposed \nline 11', 'Proposed \nline 6', 'Proposed \nline 7', 
                           'Proposed \nline 8', 'Proposed lines', 
                           'Proposed \norange and \ngreen lines', 
                           'Proposed \norange line \nextension', 
                           'Proposed \norange line \nextension', 
                           'Proposed \norange line \nextension', 'Proposed \npink line', 
                           'Proposed \nred line', 'Proposed \nyellow line \nextension', 
                           'Yellow line', 'Yellow line', 'Yellow line', 'Yellow line', 
                           'Yellow line'),
               type_fr = c('Ligne bleue', 'Ligne bleue', 
                           'Prolongement de la \nligne bleue', 'Ligne verte', 
                           'Ligne verte', 'Ligne verte', 'Ligne verte', 'Ligne verte', 
                           'Ligne orange', 'Ligne orange', 'Ligne orange', 
                           'Ligne orange', 'Ligne orange', 'Ligne bleue proposée', 
                           'Prolongement \nde la ligne \nbleue proposé', 
                           'Prolongements proposé', 'Prolongements proposé', 
                           'Prolongements proposé', 'Prolongements proposés', 
                           'Prolongement de la \nligne verte proposé', 'Ligne proposée', 
                           'Ligne proposée', 'Ligne 10 \nproposée', 'Ligne 11 \nproposée', 
                           'Ligne 6 \nproposée', 'Ligne 7 \nproposée', 'Ligne 8 \nproposée', 
                           'Lignes proposées', 'Lignes orange and verte proposées', 
                           'Prolongement \nde la ligne \norange proposé', 
                           'Prolongement \nde la ligne \norange proposé', 
                           'Prolongement \nde la ligne \norange proposé', 
                           'Ligne rose proposée', 'Ligne rouge proposée', 
                           'Prolongement \nde la ligne \njaune proposé', 'Ligne jaune', 
                           'Ligne jaune', 'Ligne jaune', 'Ligne jaune', 'Ligne jaune'),
               date = c('1970', '2000', '2000', '1967', '1970', '1978', '1981', 
                        '2000', '1967', '1970', '1978', '1981', '2000', '1981', 
                        '1970', '1910', '1910', '1944', '1961', '1981', '1910', 
                        '1944', '1970', '1970', '1970', '1970', '1970', '1953', 
                        '1961', '1970', '1981', '2000', '2000', '1961', '2000', 
                        '1967', '1970', '1978', '1981', '2000'),
               fill = c('#1082CD', '#1082CD', '#9CC0F0', '#00A650', '#00A650', 
                        '#00A650', '#00A650', '#00A650', '#F47216', '#F47216', 
                        '#F47216', '#F47216', '#F47216', '#9CC0F0', '#9CC0F0', 
                        '#1263A6', '#1263A6', '#1263A6', '#1263A6', '#76C274', 
                        '#000000', '#000000', '#000000', '#454545', '#696868', 
                        '#949292', '#B5B1B1', '#000000', '#000000', '#f5D7A4', 
                        '#f5D7A4', '#f5D7A4', '#E60E70', '#8B0000', '#FAF093', 
                        '#FCD300', '#FCD300', '#FCD300', '#FCD300', '#FCD300'))
  
  values <- 
    metro_evolution[metro_evolution$date == date, ][[paste0("type_", lang)]] |> 
    unique()

  legend <- 
    data.frame(x = seq_along(values),
               y = 1,
               fill = 
                 unique(metro_evolution[metro_evolution[[
                   paste0("type_", lang)]] %in% values, ]$fill))
  
  legend |> 
    ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
               fill = fill)) +
    geom_rect() + 
    scale_x_continuous(breaks = seq_along(values) - 0.5, labels = values) +
    scale_y_continuous(labels = NULL) +
    scale_fill_manual(values = setNames(
      legend$fill, legend$fill)) +
    theme_minimal() +
    theme(text = element_text(family = "SourceSansPro", size = 12),
          legend.position = "none", 
          panel.grid = element_blank())
  
}

metro_evolution_source <- function(year, r) {
  if (year %in% c(1910, 1944, 1953)) {
    cc_t(r = r, "The proposed {year} metro network.")
  } else if (year == 1961) {
    cc_t(r = r, "The proposed 1962 metro network consisting of three lines: ",
                  "orange, green, and red.")
  } else if (year == 1967) {
    cc_t(r = r, "The first complete iteration of the Montreal metro.")
  } else if (year == 1970) {
    cc_t(r = r, "Some extensive proposed underground and surface metro ",
                  "expansions of the early 1970s that never materialized.")
  } else if (year == 1978) {
    cc_t(r = r, "The 1978 iteration of the metro network.")
  } else if (year == 1981) {
    cc_t(r = r, "The metro expansion plans made following the 1970 ",
                  "re-election of Jean Drapeau.")
  } else if (year == 2000) {
    cc_t(r = r, "The current network along with the proposed extensions over ",
                  "the years, including the confirmed blue line extension to ",
                  "Anjou.")
  }
}

metro_evolution_choices <- function() {
  c(1910, 1944, 1953, 1961, 1967, 1970, 1978, 1981, 2000)
}

