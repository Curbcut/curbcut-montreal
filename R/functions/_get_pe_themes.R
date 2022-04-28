#### GET PLACE EXPLORER THEMES #################################################

get_pe_themes <- function(df, select_id, island_or_region) {
  themes <- pe_theme_order[[df]][[island_or_region]]
  themes <- themes[themes$ID == select_id, ]
  mapply(c, themes$theme, themes$standout, SIMPLIFY = FALSE)
  }