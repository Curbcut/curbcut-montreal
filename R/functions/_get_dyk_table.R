#### GET DYK TABLE #############################################################

get_dyk_table <- function(var_left, var_right, poi = NULL) {
  
  # Prepare variables ----------------------------------------------------------
  
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  
  vars <- c(str_remove(var_left, "_\\d{4}$"), str_remove(var_right, "_\\d{4}$"))
  vars <- vars[vars != " "]
  
  themes <- variables |> 
    filter(var_code %in% vars) |> 
    pull(theme) |> 
    unique()
  
  
  # Find special matches -------------------------------------------------------
  
  if (!is.null(poi)) {
    return({
      
      out <- 
        stories |> 
        filter(name %in% poi) |> 
        slice(1:2)
      
      links <- map_chr(seq_along(out$name), ~{
        paste0("<a id='dyk_", .x, 
               "home-module_link' href='#' ",
               "class='action-button shiny-bound-input'>[LEARN MORE]</a>")
        })
      
      out <- paste(out$preview, links)
      out <- paste("<li> ", out, collapse = "")
      out <- paste0("<ul>", out, "</ul>")
      HTML(out)
      
    })
  }
  
  
  # Score rows -----------------------------------------------------------------
  
  dyk_scored <- 
    dyk |> 
    rowwise() |> 
    mutate(score = sum(vars %in% variable) * 3 + sum(themes %in% theme) * 2) |> 
    ungroup() |> 
    arrange(-score) |> 
    filter(score > 0)


  # Choose rows ----------------------------------------------------------------
  
  out <- dyk_scored[0,]

  while (nrow(out) < 2 && nrow(dyk_scored) > 0) {
   
    to_add <- 
      dyk_scored |> 
      filter(score == max(score))
    
    if (nrow(to_add) > 2 - nrow(out)) {
      to_add <- slice_sample(to_add, n = 2 - nrow(out))
    }
    
    out <- bind_rows(out, to_add)
    
    dyk_scored <- 
      dyk_scored |> 
      filter(score != max(score))
    
  }
  
  
  # Warn if <2 DYKs ------------------------------------------------------------
  
  if (nrow(out) < 2) {
    warning("No DYK matches for variable(s): ", 
            paste(vars, collapse = ", "), call. = FALSE)
  }
  

  # Return output --------------------------------------------------------------
  
  if (nrow(out) > 0) {
    map_chr(out$text, sus_translate) %>%
      paste("<li> ", ., collapse = "") %>%
      paste0("<ul>", ., "</ul>") %>%
      HTML()
  } else NULL
  
}