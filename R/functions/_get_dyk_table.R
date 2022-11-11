#### GET DYK TABLE #############################################################

get_dyk_table <- function(id, r, var_left, var_right, df, poi = NULL) {
  
  # Prepare variables ----------------------------------------------------------
  
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  vars <- c(str_remove(var_left, "_\\d{4}$"), str_remove(var_right, "_\\d{4}$"))
  vars <- vars[vars != " "]
  themes <- unique(variables$theme[variables$var_code %in% vars])
  
  
  # Find special matches -------------------------------------------------------
  
  if (!is.null(poi)) {
    return({
      
      out <- stories[stories$name %in% poi,]
      out <- out[min(1, nrow(out)):min(2, nrow(out)),]

      links <- sapply(seq_along(out$name), \(x) {
        paste0("<a id='", id, "-", id, "-dyk_", x, "' href='#' ",
               "class='action-button shiny-bound-input'>", 
               cc_t(r = r, "[LEARN MORE]"), "</a>")})
      
      link_attrs <- lapply(seq_along(out$name), 
                           \(x) list(module = "stories", select_id = out$ID[x]))
      
      previews <- sapply(out$preview, cc_t, r = r, USE.NAMES = FALSE)
      out <- paste(previews, links)
      out <- paste("<li> ", out, collapse = "")
      out <- paste0("<ul>", out, "</ul>")
      out <- HTML(out)
      attr(out, "links") <- link_attrs
      out
      
    })
  }
  
  
  # Score rows -----------------------------------------------------------------
  
  dyk_scored <- dyk
  dyk_scored$score <- sapply(dyk$variable, \(x) sum(vars %in% x) * 3) +
    sapply(dyk$theme, \(x) sum(themes %in% x) * 2)
  dyk_scored <- dyk_scored[dyk_scored$score > 0,]
  dyk_scored <- dyk_scored[order(dyk_scored$score, decreasing = TRUE),]
  

  # Choose rows ----------------------------------------------------------------
  
  out <- dyk_scored[0,]

  while (nrow(out) < 2 && nrow(dyk_scored) > 0) {
   
    to_add <- dyk_scored[dyk_scored$score == max(dyk_scored$score),]
    if (nrow(to_add) > 2 - nrow(out)) {
      to_add <- to_add[sample.int(nrow(to_add), min(2, nrow(to_add))),]
    }
    out <- rbind(out, to_add)
    dyk_scored <- dyk_scored[dyk_scored$score != max(dyk_scored$score),]

  }
  
  
  # Warn if <2 DYKs ------------------------------------------------------------
  
  # if (nrow(out) < 2) {
  #   warning("No DYK matches for variable(s): ", 
  #           paste(vars, collapse = ", "), call. = FALSE)
  # }
  
  
  # Process links --------------------------------------------------------------
  
  if (nrow(out) > 0) {
    links <- out[!is.na(out$module),]
    
    links <- sapply(seq_along(out$module), \(x) {
      # Don't display a link if the variables are the same
      if (is.na(out$module[x]) || out$module[x] != "canale" ||
          all(out$variable[[x]] %in% str_remove(c(var_left, var_right), 
                                                "_\\d{4}$"))) "" else {
        paste0(" <a id='", id, "-", id, "-dyk_", x, "' href='#' ",
               "class='action-button shiny-bound-input'>", 
               cc_t(r = r, "[LEARN MORE]"), "</a>")
      }
    })
    
    link_attrs <- lapply(seq_len(nrow(out)), \(x) {
      if (is.na(out$module[x]) || out$module[x] != "canale") list() else {
        list(module = out$module[x], 
             var_left = out$variable[[x]][1],
             var_right = if (length(out$variable[[x]]) == 1) " " else 
               out$variable[[x]][2],
             df = if (!is.na(out$df[x])) out$df[x] else df)
      }
    })
    
  }

  # Return output --------------------------------------------------------------
  
  if (nrow(out) > 0) {
    out <- 
      sapply(out$text, cc_t, r = r) |> 
      paste0(links) |> 
      (\(x) paste("<li> ", x, collapse = ""))() |> 
      (\(x) paste0("<ul>", x, "</ul>"))() |> 
      HTML()
    attr(out, "links") <- link_attrs
    out
  } else NULL
  
}
