#### GET LEGEND AXIS LABELS ####################################################

get_legend_labels <- function(var_left, var_right, data_type, breaks = NULL) {
  
  ## If breaks has non-NULL name, use it ---------------------------------------
  
  if (!is.null(breaks) && !is.null(attr(breaks, "label"))) {
    labs_xy <- list(labs(x = attr(breaks, "label"), y = NULL))
    return(labs_xy)
  }
  
  ## Get basic titles ----------------------------------------------------------
  
  title_left <- 
    variables[variables$var_code == unique(sub("_\\d{4}$", "", var_left)),]
  
  title_left_short <- sus_translate(title_left$var_short)
  title_left <- sus_translate(title_left$var_title)

  title_right <- 
    variables[variables$var_code == unique(sub("_\\d{4}$", "", var_right)),]
  
  if (data_type %in% c("bivar", "delta_bivar")) {
    title_right_short <- sus_translate(title_right$var_short)
    title_right <- sus_translate(title_right$var_title)
    # If axis title is too long, take the short version
    if (nchar(title_right) > 35) title_right <- title_right_short  
    if (nchar(title_left) > 35) title_left <- title_left_short
  } else if (nchar(title_left) > 35) title_left <- title_left_short
  
  
  ## Construct labels ----------------------------------------------------------
  
  # Return NULL if no data_type matches
  labs_xy <- NULL
  
  # q5 version
  if (data_type == "q5") {
    labs_xy <- list(labs(x = title_left, y = NULL))
  }
  
  # q100 version
  if (data_type == "q100") {
    labs_xy <- list(labs(x = title_left, y = NULL))
  }
  
  # qual version
  if (data_type == "qual") {
    labs_xy <- list(labs(x = title_left, y = NULL))
  }
  
  # Delta version
  if (data_type == "delta") {
    date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
    date_left <- paste(date_left, collapse = " - ")
    title_left <- paste0(title_left, " (", date_left, ")")
    labs_xy <- list(labs(x = title_left, y = NULL))
  }
  
  # Bivar version
  if (data_type == "bivar") {
    labs_xy <- list(labs(x = title_right, y = title_left), 
                    x_short = title_right_short, y_short = title_left_short)
  }
  
  
  if (data_type == "delta_bivar") {
    date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
    date_left <- paste(date_left, collapse = " - ")
    if (nchar(title_left) > 16) title_left <- title_left_short
    title_left <- paste0(title_left, " (\u0394 ", date_left, ")")
    date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
    date_right <- paste(date_right, collapse = " - ")
    if (nchar(title_right) > 16) title_right <- title_right_short
    title_right <- paste0(title_right, " (\u0394 ", date_right, ")")
    labs_xy <- list(labs(x = title_right, y = title_left), 
                    x_short = title_right_short, y_short = title_left_short)
  }

  return(labs_xy)
  
}