### GET METADATA FUNCTION ######################################################

get_metadata <- function(export_data, r, about_data,
                         var, variables_row) {
  
  # Time
  time <- str_extract(export_data[[var]], "\\d{4}$")
  
  variables_row$explanation <- cc_t(r = r, variables_row$explanation)
  variables_row$var_title <- cc_t(r = r, variables_row$var_title)
  
  time_text <- 
    if (!is.na(time[1])) {
      paste0(" ",
             if (length(time) == 1) cc_t(r = r, "for the year {time}") else 
               cc_t(r = r, "for the years {time[1]} and {time[2]}"))
    } else ""
  
  # If private
  if (variables_row$private)
    about_data[[var]]$private <- 
    paste0("<p = style = 'font-size: 1.45rem;'>",
           cc_t(r = r, "We do not have permission to make the ", 
                "variable <b>'{variables_row$var_title}'</b> ",
                "available for public download."),
           "<p>")
  
  
  # How many columns
  single_col <- if (export_data$data |> 
                    names() |> 
                    str_subset(variables_row$var_code) |> 
                    length() == 1) TRUE else FALSE
  
  about_data[[var]]$details_1 <-
    if (!variables_row$private) {
      if (single_col) {
        cc_t(r = r, "The column `<b>{paste0(export_data[[var]], ",
             "collapse = '</b>` and `<b>')}</b>` contains data on ",
             "{variables_row$explanation} ('{variables_row$var_title}')",
             "{time_text}.")        
      } else {
        cc_t(r = r, "The columns `<b>{paste0(export_data[[var]], ",
             "collapse = '</b>` and `<b>')}</b>` contain data on ",
             "{variables_row$explanation} ('{variables_row$var_title}')",
             "{time_text}.")        
      }
    } else {
      # As there's not preview or data to download, don't talk about columns
      cc_t(r = r, "The variable `<b>{variables_row$var_title}</b>` ",
           "contain(s) data on {variables_row$explanation} ",
           "('{variables_row$var_title}'){time_text}.")
    }
  
  # Data type (Qualitative, Quantitative?)
  var_quant <- 
    if (str_detect(variables_row$var_code, "_qual$")) FALSE else TRUE
  
  # Quantitative (Range, mean, sd)
  # Skip for natural_inf. Everything is already pre-computed, there is no 
  # possibility to get min/max/mean/sd, except if we add them to the 
  # pre-computation step
  if (var_quant && export_data$id != "natural_inf") {
    
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
      cc_t(r = r, "Values range from <b>{quant_info$min}</b> to ",
           "<b>{quant_info$max}</b>, with a mean of <b>{quant_info$mean}",
           "</b> and a standard deviation is <b>{quant_info$sd}</b>.")
  }
  
  # Climate risk special cases (1 = Insignificant, 2 = ...)
  if (str_starts(variables_row$var_code, "climate_")) {
    
    ranks <- variables_row$breaks_q5[[1]][
      !is.na(variables_row$breaks_q5[[1]]$var_name), ]
    
    ranks <- ranks[ranks$scale == export_data$df, ]
    
    about_data[[var]]$details_2 <- 
      paste0(about_data[[var]]$details_2, " (",
             paste(ranks$var,
                   sapply(ranks$var_name, cc_t, r = r, 
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
        lapply(export_data[[var]], function(var_code_year) {
          census_variables_row <- 
            census_variables[census_variables$var_code == var_code_year, ]
          
          vector_definition <- 
            paste0("<b>", unlist(census_variables_row$vec), "</b> ('", 
                   lapply(unlist(census_variables_row$vec_label), cc_t, 
                          r = r), "')",
                   collapse = ", ")
          
          parent_vector_definition <- 
            paste0("<b>", unlist(census_variables_row$parent_vec), "</b> ('", 
                   lapply(unlist(census_variables_row$parent_vec_label), cc_t, 
                          r = r), "')",
                   collapse = ", ")
          
          if ("pct" %in% variables_row$type) {
            
            # Singular numerator
            if (length(unlist(census_variables_row$vec)) == 1) {
              # Singular denominator
              if (length(unlist(census_variables_row$parent_vec)) == 1) {
                cc_t(r = r, "The numerator of the percentage is {vector_definition},",
                     " and the denominator is {parent_vector_definition}.")
                # Plural denominator
              } else {
                cc_t(r = r, "The numerator of the percentage is {vector_definition},",
                     " and the summed denominators are {parent_vector_definition}.")
              }
              # Plural numerator
            } else {
              # Singular denominator
              if (length(unlist(census_variables_row$parent_vec)) == 1) {
                cc_t(r = r, "The percentage has been done with the addition of ",
                     "the following vectors, forming the numerator: ",
                     "{vector_definition}. The denominator is ",
                     "{parent_vector_definition}.")
                # Plural denominator
              } else {
                cc_t(r = r, "The percentage has been done with the addition of ",
                     "the following vectors, forming the numerator: ",
                     "{vector_definition}. The summed denominators are ",
                     "{parent_vector_definition}.")
              }
            }
            
          } else if ("dollar" %in% variables_row$type) {
            # If average
            if (str_detect(export_data[[paste0(var, "_code")]], "_avg")) {
              cc_t(r = r, "The Canadian Census vector is ",
                   "{vector_definition}. It is the average of ",
                   "{parent_vector_definition}.")
              # If a median
            } else if ("median" %in% variables_row$type) {
              cc_t(r = r, "The Canadian Census vector is ",
                   "{vector_definition}. It is the median of ",
                   "{parent_vector_definition}.")
            }
            
          }
        })
      
      time_text <- 
        if (!is.na(time[1])) 
          if (length(time) == 1) glue("{time}") else 
            cc_t(r = r, "{time[1]} and {time[2]}")
      
      if (length(time) == 2)
        census_details <- paste0(
          cc_t(r = r, "For {time}: {census_details}"),
          collapse = " ")
      
      single_year <- if (length(time_text) == 1) TRUE else FALSE
      
      out <- 
        paste0("<p style = 'font-size: 1.45rem;'>",
               cc_t(r = r,
                    "The data comes from the {time_text} ",
                    "Canadian Census. {census_details}"),
               "</p>") 
      
      out
      
    } else {
      source <- cc_t(r = r, variables_row$source)
      paste0("<p style = 'font-size: 1.45rem;'>",
             cc_t(r = r, "The data comes from {source}."),
             "</p>")
    }
  
  # Has data been interpolated from another geometry?
  interpolated_dfs <-
    unlist(variables_row$interpolated)[
      -str_which(unlist(variables_row$interpolated), "FALSE")]
  
  interpolated <- 
    if (!is.null(export_data$df)) 
      is_scale_in_df(names(interpolated_dfs), export_data$df) else FALSE
  
  if (interpolated && length(interpolated_dfs) > 0) {
    
    from <- cc_t(r = r, interpolated_dfs[[export_data$df]])
    
    about_data[[var]]$interpolated <- 
      # Special case for the boroughs at the census scale!
      if (is_scale_in_df("CSD", export_data$df) && 
          variables_row$source == "Canadian census") {
        paste0("<p style = 'font-size: 1.45rem;'>",
               cc_t(r = r, "For the City of Montreal's boroughs, ",
                    "`{variables_row$var_title}` is ",
                    "spatially interpolated from {from}s."),
               "</p>")
      } else {
        df <- str_to_lower(cc_t(r = r, get_zoom_name(export_data$df)))
        paste0("<p style = 'font-size: 1.45rem;'>",
               cc_t(r = r, "`{variables_row$var_title}` at the {df} scale is ",
                    "spatially interpolated from {from}s."),
               "</p>")
      }
    
  }
  
  # Is the data represented different from the underlying data? ex. buildings,
  # we show DAs
  if (!is.null(export_data$df))
    if (export_data$data_origin != export_data$df) {
      df <- str_to_lower(cc_t(r = r, get_zoom_name(export_data$df)))
      data_origin <- 
        str_to_lower(cc_t(r = r, get_zoom_name(export_data$data_origin)))
      
      about_data[[var]]$diff_representation <- 
        paste0("<p style = 'font-size: 1.45rem;'>",
               cc_t(r = r, "The data is represented as {df}s, but the ",
                    "underlying dataset is spatially organised as ",
                    "{data_origin}s."),
               "</p>")
    }
  
  about_data
  
}
