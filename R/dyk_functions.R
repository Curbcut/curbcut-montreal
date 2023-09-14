#### DYK FUNCTIONS #############################################################

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
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
dyk_text <- function(vars, df, select_id, lang, ...) {
  UseMethod("dyk_text", vars)
}

#' @rdname dyk_text
#' @export
#'
dyk_text.default <- function(vars, df, select_id, lang, ...) {
  
  return(NULL)
  
}

#' @rdname dyk_text
#' @export
#'
dyk_text.q5 <- function(vars, df, select_id, lang, ...) {
  
  # Parse vars and df
  var_left <- vars$var_left
  var <- sub("_\\d{4}", "", var_left)
  date <- regmatches(var_left, regexpr("\\d{4}", var_left))
  region <- sub("_.*", "", df)
  # If there's a selection, scale should match it, otherwise default to CSD
  # TKTK use get_from_global_env to get map_zoom_levels_<<region>> and then take first name
  scale <- if(is.na(select_id)) "CSD" else sub(".*_", "", df)
  # Overwrite the building scale with DA TKTK treat_to_DA(scales_as_DA = c("building", "street"), df = df)
  if (scale == "building") scale <- "DA"
  
  # Subset dyk
  dyk_df <- dyk[dyk$region == region & dyk$var_left == var,]
  
  # Get the best DYK from each category
  dyk_high <- dyk_df[dyk_df$dyk_type %in% c("highest", "lowest") &
                       dyk_df$scale == scale & dyk_df$date == date,]
  dyk_high <- dyk_high[round(runif(1, 1, 2)),]
  dyk_change <- dyk_df[dyk_df$dyk_type == "change",]
  dyk_compare <- dyk_df[dyk_df$dyk_type == "compare" & dyk_df$scale == scale & 
                          dyk_df$date == date,]
  if (nrow(dyk_compare) > 0) dyk_compare <- dyk_compare[
    sample(length(dyk_compare$dyk_value), 1, prob = dyk_compare$dyk_value ^ 2),]
  
  # Randomly choose one
  dyk_out <- rbind(dyk_high, dyk_change, dyk_compare)
  out <- dyk_out$dyk_text[sample(seq_along(dyk_out$dyk_text), 1)]
  out <- shiny::tags$ul(shiny::tags$li(out))
  
  # Return output
  return(out)
  
}


# dyk_text.delta <- function(vars, df, select_id, lang, ...) {
#   
#   # Parse vars and df
#   var_left <- vars$var_left
#   var <- sub("_\\d{4}", "", var_left)
#   date <- regmatches(var_left, regexpr("\\d{4}", var_left))
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

# dyk_text.bivar <- function(vars, df, select_id, lang, ...) {
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

