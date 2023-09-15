#### DYK FUNCTIONS #############################################################

#' Create an HTML list element with a "Learn More" link and link attributes
#'
#' This function generates an HTML list item (`<li>`) containing the provided text
#' and an action link labeled "Learn More". The link appears as an action button.
#' The function also attaches attributes that specify that these are links.
#'
#' @param id <`character`> The ID of the page in which the DYK will appear,
#' e.g. `alp`.
#' @param element_id <`numeric`> This is used to create the HTML `<a>` tag of
#' the `[LEARN MORE]` button. It must be numeric, either 1 or 2. A click on 
#' `dyk_1` and `dyk_2` only will trigger a reaction.
#' @param text <`character`> Text to be displayed in the DYK.
#' @param page <`character`> The target page for the link's action. Defaults
#' to `id`, stay on the same page.
#' @param lang <`character`> A character string specifying the language to
#' translate the content. Defaults to NULL for no translation.
#' @param ... Additional attributes to pass to the `link` function. Any of
#' `select_id`, `var_right`, `date`, ...
#'
#' @return An HTML list item (`<li>`) with an embedded action link. The list item
#' will also have attributes specifying that these are links.
#' @export
dyk_link <- function(id, element_id, page = id, text, lang = NULL, ...) {
  if (!element_id %in% c(1, 2)) stop("`element_id` must be either 1 or 2.")
  
  # Make the a tag links as if they were action buttons
  button_id <- curbcut:::ns_doubled(page_id = id, element = sprintf("dyk_%s", element_id))
  
  text_link <- shiny::tags$li(
    # Grab the preview column, in the correct language
    text,
    shiny::tags$a(
      id = button_id,
      href = "#",
      class = "action-button shiny-bound-input",
      curbcut::cc_t("[LEARN MORE]", lang = lang)
    )
  )
  
  # Convert as character to add attrs (can't be added to shiny.tag)
  text_link <- as.character(text_link)
  
  # Arguments necessary for the `link` function (except `r` which is added
  # subsequently)
  link_attrs <- list(page = page, ...)
  
  # Flag that these are links
  attr(text_link, "links") <- link_attrs
  
  return(text_link)
}



# dyk_text ----------------------------------------------------------------

#' Generate DYK text for the given variables and region
#'
#' This function dispatches to the appropriate text-generating function based on
#' the type of `vars` and returns the resulting text.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param region_df <`character`> String specifying the name of the region.
#' Usually equivalent to `r$region()`.
#' #' @param scale_df <`character`> String specifying the name of the scale.
#' Usually equivalent to `r$scale()`.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
dyk_text <- function(vars, df, select_id, lang, region, zoom_levels, scales_as_DA, ...) {
  UseMethod("dyk_text", vars)
}

#' @rdname dyk_text
#' @export
#'
dyk_text.default <- function(vars, df, select_id, lang, region, zoom_levels, scales_as_DA, ...) {
  
  return(NULL)
  
}

#' @rdname dyk_text
#' @export
#'
dyk_text.q5 <- function(vars, df, select_id, lang, region, zoom_levels, scales_as_DA, ...) {
  
  # Parse vars and df
  var_left <- vars$var_left
  var <- var_remove_time(var_left)
  date <- var_get_time(var_left)
  
  # Switch the scales_as_DA to df = *_DA if scale is eg. building or street
  df <- curbcut::treat_to_DA(scales_as_DA, df)

  # If there's a selection, scale should match it, otherwise default to CSD
  # select_id can sometimes be at the wrong scale (click and zoom in does not de-select,
  # for it the user zooms back).
  scale <- sub(".*_", "", df)
  scale_for_highest <- if (is.na(select_id) || !select_id %in% get_from_globalenv(df)$ID) {
    names(zoom_levels)[[1]]
  } else {
    scale
  }
  
  # Subset dyk
  dyk_df <- dyk[dyk$region == region & dyk$var_left == var,]
  
  # Get the best DYK from each category
  dyk_high <- dyk_df[dyk_df$dyk_type %in% c("highest", "lowest") &
                       dyk_df$scale == scale_for_highest & dyk_df$date == date,]
  dyk_high <- dyk_high[round(runif(1, 1, 2)),]
  dyk_change <- dyk_df[dyk_df$dyk_type == "change",]
  dyk_compare <- dyk_df[dyk_df$dyk_type == "compare" & dyk_df$scale == scale & 
                          dyk_df$date == date,]
  if (nrow(dyk_compare) > 0) dyk_compare <- dyk_compare[
    sample(length(dyk_compare$dyk_value), 1, prob = dyk_compare$dyk_value ^ 2),]
  
  # Randomly choose one
  dyk_out <- rbind(dyk_high, dyk_change, dyk_compare)
  out <- dyk_out[sample(seq_along(dyk_out$dyk_text), 1), ]
  
  if (nrow(out) > 1) stop("DYK links expect 1 dyk")
  out <- if (out$dyk_type %in% c("highest", "lowest")) {
    dyk_link(id = out$module, element_id = 1, text = out$dyk_text, lang = lang, 
             df = sprintf("%s_%s", out$region, out$scale), select_id = out$select_ID,
             # Feed zoom_levels to the link. The zoom will be adjusted using exactly
             # the ones specified (sometimes, map_zoom_levels_* may undergo transformation
             # in some pages. Better to have the current zoom_levels follow)..
             zoom_levels = zoom_levels)
  } else if (out$dyk_type == "change") {
    dyk_link(id = out$module, element_id = 1, text = out$dyk_text, lang = lang, 
             date = out$date[[1]])
  } else if (out$dyk_type == "compare") {
    dyk_link(id = out$module, element_id = 1, text = out$dyk_text, lang = lang, 
             var_right = out$var_right)
  }
  
  # Return output
  return(list(out))
  
}


# dyk_text.delta <- function(vars, df, select_id, lang, region, zoom_levels, ...) {
# 
#   # Parse vars and df
#   var_left <- vars$var_left
#   var <- curbcut::var_remove_time(var_left)
#   date <- var_get_time(var_left)
#   
#   # If there's a selection, scale should match it, otherwise default to CSD
#   scale <- if(is.na(select_id)) "CSD" else sub(".*_", "", df)
#   # Overwrite the building scale with DA
#   if (scale == "building") scale <- "DA"
# 
#   # Subset dyk
#   dyk_df <- dyk[dyk$region == region & dyk$var_left == var,]
# 
#   # Get the best DYK from each category
#   dyk_high <- dyk_df[dyk_df$dyk_type %in% c("highest", "lowest") &
#                        dyk_df$scale == scale & dyk_df$date == date,]
#   dyk_high <- dyk_high[round(runif(1, 1, 2)),]
#   dyk_change <- dyk_df[dyk_df$dyk_type == "change",]
#   dyk_compare <- dyk_df[dyk_df$dyk_type == "compare" & dyk_df$scale == scale &
#                           dyk_df$date == date,]
#   dyk_compare <- dyk_compare[
#     sample(length(dyk_compare$dyk_value), 1, prob = dyk_compare$dyk_value ^ 2),]
# 
#   # Randomly choose one
#   dyk_out <- rbind(dyk_high, dyk_change, dyk_compare)
#   out <- dyk_out$dyk_text[sample(1:3, 1)]
#   out <- shiny::tags$ul(shiny::tags$li(out))
# 
#   # Return output
#   return(out)
# 
# }

# dyk_text.bivar <- function(vars, df, select_id, lang, region, zoom_levels, ...) {
#   
#   # Parse vars and df
#   var_left <- vars$var_left
#   var_right <- vars$var_right
#   var_1 <- sub("_\\d{4}", "", var_left)
#   var_2 <- sub("_\\d{4}", "", var_right)
#   date_1 <- regmatches(var_left, regexpr("\\d{4}", var_left))
#   date_2 <- regmatches(var_right, regexpr("\\d{4}", var_right))
#   region <- sub("_.*", "", df)
#   # If there's a selection, scale should match it, otherwise default to CSD
#   scale <- if(is.na(select_id)) "CSD" else sub(".*_", "", df)
#   # Overwrite the building scale with DA
#   if (scale == "building") scale <- "DA"
#   
#   # Subset dyk
#   dyk_df <- dyk[dyk$region == region & dyk$var_left == var,]
#   
#   # Get the best DYK from each category
#   dyk_high <- dyk_df[dyk_df$dyk_type %in% c("highest", "lowest") &
#                        dyk_df$scale == scale & dyk_df$date == date,]
#   dyk_high <- dyk_high[round(runif(1, 1, 2)),]
#   dyk_change <- dyk_df[dyk_df$dyk_type == "change",]
#   dyk_compare <- dyk_df[dyk_df$dyk_type == "compare" & dyk_df$scale == scale & 
#                           dyk_df$date == date,]
#   dyk_compare <- dyk_compare[
#     sample(length(dyk_compare$dyk_value), 1, prob = dyk_compare$dyk_value ^ 2),]
#   
#   # Randomly choose one
#   dyk_out <- rbind(dyk_high, dyk_change, dyk_compare)
#   out <- dyk_out$dyk_text[sample(1:3, 1)]
#   out <- shiny::tags$ul(shiny::tags$li(out))
#   
#   # Return output
#   return(out)
#   
# }

