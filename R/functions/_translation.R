#### Translation functions #####################################################


# Automatic translation function ------------------------------------------

translate_fun <- function(x) deeplr::toFrench2(x, auth_key = .deepl_key)


# Basic list French translation -------------------------------------------

sus_translate_list <- function(x) {
  
  # translate name of lists
  names(x) <- sapply(names(x), \(y) {
    if (is.null(y)) NULL else {
      out <- translation_fr$fr[translation_fr$en == y]
      
      if (length(out) == 0 || is.na(out)) {
        warning("No translation text found for `", y, "`.", call. = FALSE)
        out <- y
      }
      
      out
    }})
  
  # Re-iterate in list depth to translate every name
  if (vec_dep(x) > 2) {
    x <- lapply(x, \(y) if (vec_dep(y) > 1) sus_translate_list(y) else (y))
  }
  
  x
  
}


# Reactive translation function for text, lists and png -------------------

sus_translate <- function(..., .envir = parent.frame(), r) {
  
  sus_glue <- function(x) {
    glue(x, .na = character(1), .null = character(1), .envir = .envir)
  }
  
  # Error if we provide lists + character vectors unintentionally
  args <- list(...)
  error_check <- sapply(args, inherits, "list")
  stopifnot(length(error_check) == sum(error_check) || sum(error_check) == 0)
  
  x <- c(...)
  if (!is.list(x)) x <- paste0(..., collapse = "")
  
  # Return input if we're not in a Shiny context
  if (!shiny::isRunning()) return({
    if (is.list(x)) return(x)
    x <- sub("<<.>>", "", x)
    sus_glue(x)})
  
  # If not in a reactive shiny context, return 2 spans.
  if (is.null(getDefaultReactiveDomain())) return(
    tagList(tags$span(class = "lang-en", x),
            tags$span(class = "lang-fr", {
              translated <- translation_fr[translation_fr$en == x, ]$fr
              if (length(translated) != 0 && !is.na(translated)) translated else {
                warning("No translation text found for `", x, "`.",
                call. = FALSE)
                x
              }
            }))
  )
  
  if (all(x == "")) return("")
  
  # English
  if (r$lang == "en") return({
    if (is.list(x)) return(x)
    x <- sub("<<.>>", "", x)
    sus_glue(x)})
  
  # French
  if (is.list(x)) return(sus_translate_list(x))
  
  # Character
  translated <- translation_fr[translation_fr$en == x, ]$fr
  # In case there is no translations:
  if (length(translated) == 0 || is.na(translated)) return({
    warning("No translation text found for `", x, "`.", call. = FALSE)
    x <- sub("<<.>>", "", x)
    sus_glue(x)})
  
  # For vectors with names (such as used for x axis of some modules' graph)
  if (!is.null(names(x))) names(translated) <- names(x)
  
  sus_glue(translated)
}
