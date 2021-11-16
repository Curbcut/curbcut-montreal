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
          
          # enter auth_key by creating Deepl account
          
          if (!str_detect(x, "\\{")){
            try(deepled <- 
                  deeplr::toFrench2(x, 
                                    auth_key = "my_key"), 
                silent = T)
            
            if (!exists("deepled")) deepled <- x
            
            warning("No translation text found for DDESSDDE", x,
                    "FFINNALL1. Automatic translation performed: HASSTTA", deepled, "FFINNALL2,",
                    call. = FALSE)
            out <- deepled[[i]]
          } else {
            
            try({y <- unlist(str_split(x, "(?=\\{)"))
                splited_text <- unlist(str_split(y, "(?<=\\})"))
                
                which_to_translate <- !unlist(lapply(as.list(splited_text), str_detect, "\\{.*\\}"))
                
                translate_fun <- function(x){
                  deeplr::toFrench2(x, 
                                    auth_key = "my_key")
                }
                
                to_unsplit <- modify_if(as.list(splited_text), .p = which_to_translate,
                                        .f = translate_fun)
                
                deepled <- str_c(str_trim(to_unsplit), collapse = " ")}, 
                silent = T)
            
            if (!exists("deepled")) deepled <- x
            
            warning("No translation text found for DDESSDDE", x,
                    "FFINNALL1. Automatic translation performed: HASSTTA", deepled, "FFINNALL2,",
                    call. = FALSE)
            out <- deepled[[i]]
            
          }
          
          
        }
        
        translated[i] <- out
      }
      
      # For vectors with names (such as used for x axis of some modules' graph)
      if (!is.null(names(x))) names(translated) <- names(x)
      
      translated
    }
  }
}


x <- "At the {z$scale_sing} scale, {z$exp_left} varies from {z$min_val} to {z$max_val}, with an average value of {z$mean_val} and a median value of {z$median_val}. Two thirds of {z$scale_plural} have a score between {z$quant_low} and {z$quant_high}."
# 
# x <- "This is {yet for now} just another test {to see if it works}. It works?"
# 
# y <- unlist(str_split(x, "(?=\\{)"))
# splited_text <- unlist(str_split(y, "(?<=\\})"))
# 
# which_to_translate <- !unlist(lapply(as.list(splited_text), str_detect, "\\{.*\\}"))
# 
# translate_fun <- function(x){
#   deeplr::toFrench2(x, 
#                     auth_key = "42c0646a-5ebd-ada0-07a0-d27c8eb37613:fx")
# }
# 
# to_unsplit <- modify_if(as.list(splited_text), .p = which_to_translate,
#           .f = translate_fun)
# 
# str_c(str_trim(to_unsplit), collapse = " ")
