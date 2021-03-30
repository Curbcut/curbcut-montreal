#### Translation functions #####################################################

# Basic list French translation -------------------------------------------

sus_translate_list <- function(x) {
  
  # translate name of lists
  names(x) <-
    purrr::map_chr(names(x), ~{
      if (is.null(.x)) NULL else {
        out <- 
          translation_fr %>%
          filter(en == .x) %>%
          pull()
        
        if (length(out) == 0) {
          warning("No translation text found for `", .x, "`.", call. = FALSE)
          out <- .x
        }
        
        out
      }})
  
  # Re-iterate in list depth to translate every name
  if (purrr::vec_depth(x) > 2) x <- purrr::map(x, ~{
    if (purrr::vec_depth(.x) > 1) sus_translate_list(.x) else (.x)
    })
  
  x
  
}


# Reactive translation function for text, lists and png -------------------

sus_translate <- function(x) {
  # English
  if (sus_reactive_variables$active_language() == "en") {
    x
    
  # French
  } else if (sus_reactive_variables$active_language() == "fr") {
    
    # List
    if (is.list(x)) {
      sus_translate_list(x)
    
    # png
    } else if (any(stringr::str_detect(x, "_en.png"))) {
      stringr::str_replace(x, "_en.png", "_fr.png")
      
    # Character
    } else if (is.character(x)) {
      
      # In some cases, there are multiple different strings to translate (e.g. 
      # m_dyk and the list created there). This loop will take care of it.
      translated <- vector("character", length(x))
      
      for (i in seq_along(x)) {
        out <- 
          translation_fr %>%
          filter(en == x[[i]]) %>%
          pull()
        
        if (length(out) == 0) {
          warning("No translation text found for `", x, "`.", call. = FALSE)
          out <- x[[i]]
        }
        
        translated[i] <- out
      }
      
      translated
  }
  }
}
