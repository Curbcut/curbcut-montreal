#' Get Did You Know Contents
#'
#' This function retrieves the content for the "Did You Know" (DYK) section of
#' a map page.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param poi <`reactive`> (Optional) Point of interests. The output of
#' \code{\link{update_df}}. Default is NULL.
#' @param lang <`character`> A character string specifying the language to
#' translate the content. Defaults to NULL for no translation.
#'
#' @return An HTML list of DYK content with the "links" attribute containing the
#' necessary arguments for the link function (except r, which is added subsequently).
#' @export
dyk_get <- function(id, vars, df, poi, lang = NULL) {
  
  # Start with a NULL output
  dyk_out <- NULL
  
  # If there are POIs, take them
  if (!is.null(poi)) dyk_out <- dyk_poi(poi)
  
  # Otherwise take standard DYKs
  if (is.null(dyk_out)) dyk_out <- dyk_text(vars, df)
  
  return(dyk_out)
  
}
