### GET METADATA FUNCTION ######################################################

get_metadata <- function(export_data, about_data,
                         var, variables_row) {
  
  # Time
  # Year
  if (str_detect(export_data[[var]], "_\\d{4}$"))
    time <- str_extract(export_data[[var]], "\\d{4}$")
  
  variables_row$explanation <- sus_translate(r = r, variables_row$explanation)
  variables_row$var_title <- sus_translate(r = r, variables_row$var_title)
  
  about_data$right_parts$details_1 <-
    sus_translate(r = r, "The column `<b>{export_data[[var]]}</b>` contains data on ",
                  "{variables_row$explanation} ({variables_row$var_title}) for the year ",
                  "{time}.")
  
  # Data type (Qualitative, Quantitative?)
  var_right_quant <- if (str_detect(variables_row$var_code, "_qual$")) FALSE else TRUE
  
  # Quantitative (Range, mean, sd)
  if (var_right_quant) {
    quant_info <- list()
    quant_info$min <-
      min(export_data$data[[export_data[[var]]]], na.rm = TRUE)
    quant_info$max <-
      max(export_data$data[[export_data[[var]]]], na.rm = TRUE)
    quant_info$mean <-
      mean(export_data$data[[export_data[[var]]]], na.rm = TRUE)
    quant_info$sd <-
      sd(export_data$data[[export_data[[var]]]], na.rm = TRUE)
    
    quant_info <- lapply(quant_info, convert_unit, 
                         var_name = export_data[[paste0(var, "_code")]])
    
    about_data$right_parts$details_2 <-
      sus_translate(r = r,
                    "The data range from <b>{quant_info$min}</b> to ",
                    "<b>{quant_info$max}</b>. The mean is <b>{quant_info$mean}",
                    "</b> and the standard deviation is <b>{quant_info$sd}</b>.")
  }
  
  about_data$right_parts$details <- 
    glue("<p = style = 'font-size: 1.45rem;'>{about_data$right_parts$details_1}",
         " {about_data$right_parts$details_2}<p>")
  about_data$right_parts$details_1 <- NULL
  about_data$right_parts$details_2 <- NULL
  
  # Qualitative (Possible values)
  
  ## TKTK
  
  # Source with possibly more information.
  about_data$right_parts$source <- 
    if (variables_row$source == "Canadian census") {
      
      census_variables_row <- 
        census_variables[census_variables$var_code == export_data[[var]], ]
      
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
      
      census_details <- 
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
      
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r,
                           "The data comes from the {time} ",
                           "Canadian census. {census_details}",
                           "</p>"))
    } else {
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r,
                           "The data comes from {variables_row$source}.</p>"),
             "</p>")
    }
  
  # Has data been interpolated from another geometry?
  right_interpolated_dfs <-
    unlist(variables_row$interpolated)[
      -str_which(unlist(variables_row$interpolated), "FALSE")]
  
  right_interpolated <- export_data$df %in% names(right_interpolated_dfs)
  
  if (right_interpolated) {
    # TKTKTKTKTK SPECIAL CASE FOR CANADIAN CENSUS AT BOROUGHS
    df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
    from <- right_interpolated_dfs[[export_data$df]]
    
    about_data$right_parts$interpolated <- 
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r, "{variables_row$var_title} at the {df} scale is ",
                           "spatially interpolated from {from}s."),
             "</p>")
  }
  
  # Is the data represented different from the underlying data?
  if (export_data$data_origin != export_data$df) {
    df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
    data_origin <- 
      str_to_lower(sus_translate(r = r, get_zoom_name(export_data$data_origin)))
    
    about_data$right_parts$diff_representation <- 
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r, "The data is represented as {df}s, but the ",
                           "underlying dataset is spatially organised as ",
                           "{data_origin}s."),
             "</p>")
  }
  
  about_data
  
}