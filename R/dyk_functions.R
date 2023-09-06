#### DYK FUNCTIONS #############################################################

dyk_poi <- function(poi) {
  
  # Get POIs; currently just Stories. Return nothing if the `stories` df is
  # missing.
  stories <- get0("stories", envir = .GlobalEnv)
  if (is.null(stories)) return(NULL)
  pois <- stories[c("ID", "name_id", "preview_en", "preview_fr")]
  
  # Grab two stories
  out <- pois[pois$name_id %in% poi, ]
  out <- out[min(1, nrow(out)):min(2, nrow(out)), ]
  
  # Construct the preview column
  preview_lang <- if (is.null(lang)) "en" else lang
  preview_col <- sprintf("preview_%s", preview_lang)
  
  # Make the a tag links as if they were action buttons
  previews_links <- lapply(seq_along(out$name_id), \(x) {
    button_id <- ns_doubled(page_id = id, element = sprintf("dyk_%s", x))
    
    shiny::tags$li(
      # Grab the preview column, in the correct language
      out[[preview_col]][x],
      shiny::tags$a(
        id = button_id,
        href = "#",
        class = "action-button shiny-bound-input",
        curbcut::cc_t("[LEARN MORE]", lang = lang)
      )
    )
  })
  
  # Arguments necessary for the `link` function (except `r` which is added
  # subsequently)
  link_attrs <- lapply(
    seq_along(out$name_id),
    \(x) list(page = "stories", select_id = out$ID[x])
  )
  
  # Construct the HTML list
  previews_links <- shiny::tags$ul(previews_links)
  
  # Flag that these are links
  attr(previews_links, "links") <- link_attrs
  
  # Return
  return(previews_links)
  
}


dyk_text <- function(vars, df) {
  
  # Parse vars and df
  var_left <- vars$var_left
  var_right <- vars$var_right
  region <- sub("_.*", "", df)
  scale <- sub(".*_", "", df)
  
  # Subset dyk
  dyk_df <- dyk[dyk$region == region & dyk$scale == scale,]
  
  # Get the best DYK from each category
  dyk_high <- dyk_df[dyk_df$dyk_type %in% c("highest", "lowest"),]
  
  return(NULL)

}