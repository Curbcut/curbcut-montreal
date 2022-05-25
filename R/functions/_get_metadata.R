### GET METADATA FUNCTION ######################################################

get_metadata <- function(export_data, r, about_data,
                         var, variables_row) {
  
  # Time
  time <- str_extract(export_data[[var]], "\\d{4}$")
  
  variables_row$explanation <- sus_translate(r = r, variables_row$explanation)
  variables_row$var_title <- sus_translate(r = r, variables_row$var_title)
  
  time_text <- 
    if (!is.na(time[1])) 
      if (length(time) == 1) sus_translate(r = r, " for the year {time}") else 
        sus_translate(r = r, " for the years {time[1]} and {time[2]}")

  about_data[[var]]$details_1 <-
    if (!variables_row$private) {
      # As there's not preview or data to download, don't talk about columns
      sus_translate(r = r, "The column(s) `<b>{paste0(export_data[[var]], ",
                    "collapse = '</b>` and `<b>')}</b>` contain(s) data on ",
                    "{variables_row$explanation} ({variables_row$var_title})",
                    "{time_text}.")
    } else {
      sus_translate(r = r, "The variable `<b>{variables_row$var_title}</b>` ",
                    "contain(s) data on {variables_row$explanation} ",
                    "({variables_row$var_title}){time_text}.")
    }
  
  # Data type (Qualitative, Quantitative?)
  var_right_quant <- if (str_detect(variables_row$var_code, "_qual$")) FALSE else TRUE
  
  # Quantitative (Range, mean, sd)
  # Skip for natural_inf. Everything is already pre-computed, there is no possibility
  # to get min/max/mean/sd, except if we add them to the pre-computation step
  if (var_right_quant && export_data$id != "natural_inf") {
    
    dat <- if (length(export_data[[var]]) == 1) {
      export_data$data[[export_data[[var]]]]
    } else {
      export_data$data[[paste0(export_data[[paste0(var, "_code")]], 
                               "_variation")]]
    }
    
    quant_info <- list()
    quant_info$min <- min(dat, na.rm = TRUE)
    quant_info$max <- max(dat, na.rm = TRUE)
    quant_info$mean <- mean(dat, na.rm = TRUE)
    quant_info$sd <- sd(dat, na.rm = TRUE)
    
    quant_info <- lapply(quant_info, convert_unit, 
                         var_name = export_data[[paste0(var, "_code")]])
    
    about_data[[var]]$details_2 <-
      sus_translate(r = r,
                    "The data range from <b>{quant_info$min}</b> to ",
                    "<b>{quant_info$max}</b>. The mean is <b>{quant_info$mean}",
                    "</b> and the standard deviation is <b>{quant_info$sd}</b>.")
  }
  
  # Climate risk special cases (1 = Insignificant, 2 = ...)
  if (str_starts(variables_row$var_code, "climate_")) {
    
    ranks <- variables_row$breaks_q5[[1]][
      !is.na(variables_row$breaks_q5[[1]]$var_name), ]
    
    about_data[[var]]$details_2 <- 
      paste0(about_data[[var]]$details_2, " (",
             paste(ranks$var,
                   sapply(ranks$var_name, sus_translate, r = r, 
                          USE.NAMES = FALSE),
                   sep = " = ", collapse = ", "), ")")
  }
  
  
  about_data[[var]]$details <- 
    glue("<p = style = 'font-size: 1.45rem;'>{about_data[[var]]$details_1}",
         " {about_data[[var]]$details_2}<p>", .null = character(1))
  about_data[[var]]$details_1 <- NULL
  about_data[[var]]$details_2 <- NULL
  
  
  # Source with possibly more information.
  about_data[[var]]$source <- 
    if (variables_row$source == "Canadian census") {
      
      census_details <- 
        map(export_data[[var]], function(var_code_year) {
          census_variables_row <- 
            census_variables[census_variables$var_code == var_code_year, ]
          
          vector_definition <- 
            paste0("<b>", unlist(census_variables_row$vec), "</b> (", 
                   lapply(unlist(census_variables_row$vec_label), sus_translate, 
                          r = r), ")",
                   collapse = ", ")
          
          parent_vector_definition <- 
            paste0("<b>", unlist(census_variables_row$parent_vec), "</b> (", 
                   lapply(unlist(census_variables_row$parent_vec_label), sus_translate, 
                          r = r), ")",
                   collapse = ", ")
          
          if (str_detect(export_data[[paste0(var, "_code")]], "_pct$")) {
            
            # Singular nominator
            if (length(unlist(census_variables_row$vec)) == 1) {
              # Singular denominator
              if (length(unlist(census_variables_row$parent_vec)) == 1) {
                sus_translate(r = r,
                              "The nominator of the percentage is {vector_definition},",
                              " and the denominator is {parent_vector_definition}.")
                # Plural denominator
              } else {
                sus_translate(r = r,
                              "The nominator of the percentage is {vector_definition},",
                              " and the summed denominators are {parent_vector_definition}.")
              }
              # Plural nominator
            } else {
              # Singular denominator
              if (length(unlist(census_variables_row$parent_vec)) == 1) {
                sus_translate(r = r,
                              "The percentage has been done with the addition of ",
                              "the following vectors, forming the nominator: ",
                              "{vector_definition}. The denominator is ",
                              "{parent_vector_definition}.")
                # Plural denominator
              } else {
                sus_translate(r = r,
                              "The percentage has been done with the addition of ",
                              "the following vectors, forming the nominator: ",
                              "{vector_definition}. The summed denominators are ",
                              "{parent_vector_definition}.")
              }
            }
            
          } else if (str_detect(export_data[[paste0(var, "_code")]], "_dollar$")) {
            # If average
            if (str_detect(export_data[[paste0(var, "_code")]], "_avg")) {
              sus_translate(r = r,
                            "The census vector is ",
                            "{vector_definition}. It is the average of ",
                            "{parent_vector_definition}.")
              # If a median
            } else if (str_detect(export_data[[paste0(var, "_code")]], "_median")) {
              sus_translate(r = r,
                            "The census vector is ",
                            "{vector_definition}. It is the median of ",
                            "{parent_vector_definition}.")
            }
            
          }
        })
      
      time_text <- 
        if (!is.na(time[1])) 
          if (length(time) == 1) glue("{time}") else 
            sus_translate(r = r, "{time[1]} and {time[2]}")
      
      if (length(time) == 2)
        census_details <- paste0(
          sus_translate(r = r, "For {time}: {census_details}"),
          collapse = " ")
      
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r,
                           "The data comes from the {time_text} ",
                           "canadian census(es). {census_details}",
                           "</p>"))
        
      
    } else {
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r,
                           "The data comes from {variables_row$source}.</p>"),
             "</p>")
    }
  
  # Has data been interpolated from another geometry?
  interpolated_dfs <-
    unlist(variables_row$interpolated)[
      -str_which(unlist(variables_row$interpolated), "FALSE")]
  
  interpolated <- 
    if (!is.null(export_data$df)) export_data$df %in% names(interpolated_dfs) else FALSE
  
  if (interpolated) {
    # TKTKTKTKTK SPECIAL CASE FOR CANADIAN CENSUS AT BOROUGHS
    df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
    from <- interpolated_dfs[[export_data$df]]
    
    about_data[[var]]$interpolated <- 
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r, "`{variables_row$var_title}` at the {df} scale is ",
                           "spatially interpolated from {from}s."),
             "</p>")
  }
  # For census data, for the CSD of Montreal, data comes from DA
  if (variables_row$source == "Canadian census" && 
      export_data$df == "borough") {
    about_data[[var]]$interpolated <- 
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r, "`{variables_row$var_title}` at the {df} scale, ",
                           "only for the City of Montreal, is ",
                           "spatially interpolated from dissemination areas."),
             "</p>")
  }
  
  # Is the data represented different from the underlying data?
  if (!is.null(export_data$df))
    if (export_data$data_origin != export_data$df) {
      df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
      data_origin <- 
        str_to_lower(sus_translate(r = r, get_zoom_name(export_data$data_origin)))
      
      about_data[[var]]$diff_representation <- 
        paste0("<p style = 'font-size: 1.45rem;'>",
               sus_translate(r = r, "The data is represented as {df}s, but the ",
                             "underlying dataset is spatially organised as ",
                             "{data_origin}s."),
               "</p>")
    }
  
  about_data
  
}