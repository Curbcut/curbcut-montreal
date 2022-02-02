#### Translation functions #####################################################


# Automatic translation function ------------------------------------------

translate_fun <- function(x) deeplr::toFrench2(x, auth_key = .deepl_key)


# Basic list French translation -------------------------------------------

sus_translate_list <- function(x) {
  
  # translate name of lists
  names(x) <-
    map_chr(names(x), ~{
      if (is.null(.x)) NULL else {
        out <- 
          translation_fr %>%
          filter(en == .x) %>%
          pull()
        
        if (length(out) == 0) {
          # warning("No translation text found for `", .x, "`.", call. = FALSE)
          out <- .x
        }
        
        out
      }})
  
  # Re-iterate in list depth to translate every name
  if (vec_depth(x) > 2) {
    x <- 
      map(x, ~{
      if (vec_depth(.x) > 1) sus_translate_list(.x) else (.x)
    })
  }
  
  x
  
}


# Reactive translation function for text, lists and png -------------------

sus_translate <- function(...) {
  
  # Error if we provide lists + character vectors unintentionally
  args <- list(...)
  error_check <- map_lgl(args, inherits, "list")
  stopifnot(length(error_check) == sum(error_check) || sum(error_check) == 0)
  
  x <- c(...)
  if (!is.list(x)) x <- paste0(..., collapse = "")
  
  
  # Return input if we're not in a Shiny context
  if (!shiny::isRunning()) return(x) 
  
  # If not in a reactive shiny context, return 2 spans.
  if (is.null(getDefaultReactiveDomain())) return(
    tagList(tags$span(class = "lang-en", x),
            tags$span(class = "lang-fr", 
                      translation_fr[translation_fr$en == x, ]$fr))
  )
  
  # English
  if (sus_rv$lang() == "en") return(x)
  
  # If not english, so french
  # List
  if (is.list(x)) return(sus_translate_list(x))
  
  # # PNG
  # if (str_detect(x, "_en.png")) return(str_replace(x, "_en.png", "_fr.png"))
  
  # Character
  translated <- translation_fr[translation_fr$en == x, ]$fr
  # In case there is no translations:
  if (length(translated) == 0) return(x)
  
  # For vectors with names (such as used for x axis of some modules' graph)
  if (!is.null(names(x))) names(translated) <- names(x)
  
  translated
}
