#### GET CLICK #################################################################

get_click <- function(click, type = "polygon_click", retrieve = "id") {
  
  if (type == "polygon_click") {
    select_id <- tryCatch(
      click$ID,
      error = function(e) NULL)
    if (is.null(select_id)) select_id <- NA
    
    return(select_id)
  } else if (type == "point_click") {
    warning("Point click not supported by `_get_click.R` function yet")
      # select_id <- tryCatch(
      #   fromJSON(selection(x()[lst + 1,]$ID))$object$properties$id,
      #   error = function(e) NULL)
      # if (is.null(select_id)) select_id <- NA
      # return(select_id)
  }
}