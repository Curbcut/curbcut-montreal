#### GET PLACE EXPLORER THEMES #################################################

get_pe_themes <- function(df, select_id) {
  themes <- pe_theme_order[[df]]
  themes <- themes[[as.character(select_id)]]
  mapply(c, themes$theme, themes$standout, SIMPLIFY = FALSE)
  }