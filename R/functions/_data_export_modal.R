### DATA EXPORT MODAL FUNCTION #################################################

data_export_modal <- function(r = r, export_data) {
  
  
  # About module data -------------------------------------------------------
  
  module <- modules[modules$id == export_data$id, ]
  
  # Check for lack of data
  if (sum(modules$id == export_data$id) != 1) 
    warning(paste0("No entry in `variables` for `", export_data$id, 
                   "` module."))
  if (length(module$dataset_info) == 0)
    warning(paste0("No `datataset_info` entry in `modules` for `", 
                   export_data$id, "` module."))
  
  about_module <- list()
  
  about_module$title <- 
    paste0("<h3>", sus_translate(r = r, "About module data"), "</h3>")
  
  about_module$text_linked <- 
    paste0("<a href = '", module$link, "' target = '_blank'>", 
           str_replace_all(
             sus_translate(r = r, module$dataset_info), 
             "<p>", "<p style = 'font-size: 1.45rem; cursor: help;'>"),
           "</a>")
  
  
  # About the data ----------------------------------------------------------
  
  about_data <- list()
  
  about_data$title <- 
    paste0("<h3>", sus_translate(r = r, "About exportable data"), "</h3>")
  
  # Spatial organization of data
  data_organization <- 
    str_to_lower(sus_translate(r = r, get_zoom_name(export_data$data_origin)))
  
  about_data$general_detail <- 
    paste0("<p style = 'font-size: 1.45rem; cursor: help;'>",
           sus_translate(r = r, "The data is spatially organized as ",
                         "{data_organization}."),
           "</p>")
  
  
  exist_var_right <- 
    !is.null(export_data$var_right) && export_data$var_right != " "
  
  ## Left variable ----------------------------------------------------------
  
  var_left <- variables[variables$var_code == export_data$var_left_code, ]
  
  var_left_private <- var_left$private
  
  about_data$left_parts <- list()
  
  if (exist_var_right)
    about_data$left_parts$title <- 
    paste0("<h4>", sus_translate(r = r, "About left variable"), "</h3>")
  
  # Time
  # Year
  if (str_detect(export_data$var_left, "_\\d{4}$"))
    left_time <- str_extract(export_data$var_left, "\\d{4}$")
  
  var_left$explanation <- sus_translate(r = r, var_left$explanation)
  var_left$var_title <- sus_translate(r = r, var_left$var_title)
  
  about_data$left_parts$details_1 <-
    sus_translate(r = r, "The column `<b>{export_data$var_left}</b>` contains data on ",
                  "{var_left$explanation} ({var_left$var_title}) for the year ",
                  "{left_time}.")
  
  # Data type (Qualitative, Quantitative?)
  var_left_quant <- if (str_detect(var_left$var_code, "_qual$")) FALSE else TRUE
  
  # Quantitative (Range, mean, sd)
  if (var_left_quant) {
    quant_info <- list()
    quant_info$min <-
      min(export_data$data[[export_data$var_left]], na.rm = TRUE)
    quant_info$max <-
      max(export_data$data[[export_data$var_left]], na.rm = TRUE)
    quant_info$mean <-
      mean(export_data$data[[export_data$var_left]], na.rm = TRUE)
    quant_info$sd <-
      sd(export_data$data[[export_data$var_left]], na.rm = TRUE)
    
    quant_info <- lapply(quant_info, convert_unit, 
                         var_name = export_data$var_left_code)
    
    about_data$left_parts$details_2 <-
      sus_translate(r = r,
                    "The data range from <b>{quant_info$min}</b> to ",
                    "<b>{quant_info$max}</b>. The mean is <b>{quant_info$mean}",
                    "</b> and the standard deviation is <b>{quant_info$sd}</b>.")
  }
  
  about_data$left_parts$details <- 
    glue("<p = style = 'font-size: 1.45rem;'>{about_data$left_parts$details_1}",
         " {about_data$left_parts$details_2}<p>")
  about_data$left_parts$details_1 <- NULL
  about_data$left_parts$details_2 <- NULL
  
  # Qualitative (Possible values)
  
  ## TKTK
  
  # Source with possibly more information.
  about_data$left_parts$source <- 
    if (var_left$source == "Canadian census") {
      
      census_variables_row <- 
        census_variables[census_variables$var_code == export_data$var_left, ]
      
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
        if (str_detect(export_data$var_left_code, "_pct$")) {
          
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
          
        } else if (str_detect(export_data$var_left_code, "_dollar$")) {
          # If average
          if (str_detect(export_data$var_left_code, "_avg")) {
            sus_translate(r = r,
                          "The census vector is ",
                          "{vector_definition}. It is the average of ",
                          "{parent_vector_definition}.")
            # If a median
          } else if (str_detect(export_data$var_left_code, "_median")) {
            sus_translate(r = r,
                          "The census vector is ",
                          "{vector_definition}. It is the median of ",
                          "{parent_vector_definition}.")
          }
          
        }
      
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r,
                           "The data comes from the {left_time} ",
                           "Canadian census. {census_details}",
                           "</p>"))
    } else {
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r,
                           "The data comes from {var_left$source}.</p>"),
             "</p>")
    }
  
  # Has data been interpolated from another geometry?
  left_interpolated_dfs <-
    unlist(var_left$interpolated)[
      -str_which(unlist(var_left$interpolated), "FALSE")]
  
  left_interpolated <- export_data$df %in% names(left_interpolated_dfs)
  
  if (left_interpolated) {
    df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
    from <- left_interpolated_dfs[[export_data$df]]
    
    about_data$left_parts$interpolated <- 
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r, "{var_left$var_title} at the {df} scale is ",
                           "spatially interpolated from {from}s."),
             "</p>")
  }
  
  # Is the data represented different from the underlying data?
  if (export_data$data_origin != export_data$df) {
    df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
    data_origin <- 
      str_to_lower(sus_translate(r = r, get_zoom_name(export_data$data_origin)))
    
    about_data$left_parts$diff_representation <- 
      paste0("<p style = 'font-size: 1.45rem;'>",
             sus_translate(r = r, "The data is represented as {df}s, but the ",
                           "underlying dataset is spatially organised as ",
                           "{data_origin}s."),
             "</p>")
  }
  
  ## Right variable ---------------------------------------------------------
  
  var_right_private <- FALSE
  
  if (exist_var_right) {
    
    ## Right variable ----------------------------------------------------------
    
    var_right <- variables[variables$var_code == export_data$var_right_code, ]
    
    var_right_private <- var_right$private
    
    about_data$right_parts <- list()
    
    about_data$right_parts$title <- 
      paste0("<h4>", sus_translate(r = r, "About right variable"), "</h3>")
    
    # Time
    # Year
    if (str_detect(export_data$var_right, "_\\d{4}$"))
      right_time <- str_extract(export_data$var_right, "\\d{4}$")
    
    var_right$explanation <- sus_translate(r = r, var_right$explanation)
    var_right$var_title <- sus_translate(r = r, var_right$var_title)
    
    about_data$right_parts$details_1 <-
      sus_translate(r = r, "The column `<b>{export_data$var_right}</b>` contains data on ",
                    "{var_right$explanation} ({var_right$var_title}) for the year ",
                    "{right_time}.")
    
    # Data type (Qualitative, Quantitative?)
    var_right_quant <- if (str_detect(var_right$var_code, "_qual$")) FALSE else TRUE
    
    # Quantitative (Range, mean, sd)
    if (var_right_quant) {
      quant_info <- list()
      quant_info$min <-
        min(export_data$data[[export_data$var_right]], na.rm = TRUE)
      quant_info$max <-
        max(export_data$data[[export_data$var_right]], na.rm = TRUE)
      quant_info$mean <-
        mean(export_data$data[[export_data$var_right]], na.rm = TRUE)
      quant_info$sd <-
        sd(export_data$data[[export_data$var_right]], na.rm = TRUE)
      
      quant_info <- lapply(quant_info, convert_unit, 
                           var_name = export_data$var_right_code)
      
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
      if (var_right$source == "Canadian census") {
        
        census_variables_row <- 
          census_variables[census_variables$var_code == export_data$var_right, ]
        
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
          if (str_detect(export_data$var_right_code, "_pct$")) {
            
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
            
          } else if (str_detect(export_data$var_right_code, "_dollar$")) {
            # If average
            if (str_detect(export_data$var_right_code, "_avg")) {
              sus_translate(r = r,
                            "The census vector is ",
                            "{vector_definition}. It is the average of ",
                            "{parent_vector_definition}.")
              # If a median
            } else if (str_detect(export_data$var_right_code, "_median")) {
              sus_translate(r = r,
                            "The census vector is ",
                            "{vector_definition}. It is the median of ",
                            "{parent_vector_definition}.")
            }
            
          }
        
        paste0("<p style = 'font-size: 1.45rem;'>",
               sus_translate(r = r,
                             "The data comes from the {right_time} ",
                             "Canadian census. {census_details}",
                             "</p>"))
      } else {
        paste0("<p style = 'font-size: 1.45rem;'>",
               sus_translate(r = r,
                             "The data comes from {var_right$source}.</p>"),
               "</p>")
      }
    
    # Has data been interpolated from another geometry?
    right_interpolated_dfs <-
      unlist(var_right$interpolated)[
        -str_which(unlist(var_right$interpolated), "FALSE")]
    
    right_interpolated <- export_data$df %in% names(right_interpolated_dfs)
    
    if (right_interpolated) {
      # TKTKTKTKTK SPECIAL CASE FOR CANADIAN CENSUS AT BOROUGHS
      df <- str_to_lower(sus_translate(r = r, get_zoom_name(export_data$df)))
      from <- right_interpolated_dfs[[export_data$df]]
      
      about_data$right_parts$interpolated <- 
        paste0("<p style = 'font-size: 1.45rem;'>",
               sus_translate(r = r, "{var_right$var_title} at the {df} scale is ",
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
    
  }
  
  
  # Hide private data -------------------------------------------------------
  
  bivariate <- exist_var_right
  
  # If univariate and data is private, no data to export.
  if (!bivariate && var_left_private) export_data$data <- NULL
  
  # If bivariate ...
  #  ... and both data are private, no data to export.
  if (bivariate && var_left_private && var_right_private) 
    export_data$data <- NULL
  
  # ... and only one data is private, hide that column.
  if (bivariate && sum(var_left_private, var_right_private) == 1) {
    if (var_left_private) {
      export_data$data <- 
        export_data$data[, names(export_data$data) != export_data$var_left]
    }
    if (var_right_private) {
      export_data$data <- 
        export_data$data[, names(export_data$data) != export_data$var_left]
    }
  }
  
  button_style <- 
    if (!is.data.frame(export_data$data)) {
      "border: 1px solid #999999; pointer-events: none; opacity: 0.5;"
    } else NULL
  
  modal_title <- 
    sus_translate(r = r, names(unlist(unname(mods_rdy))[
      export_data$id == unlist(mods_rdy)]))
  
  modal <- 
    modalDialog(
      title = 
        sus_translate(r = r, "Data explanation and export on `{modal_title}`"),
      
      # About module data
      HTML(unlist(about_module)),
      
      # About exportable data
      HTML(unlist(about_data)),
      
      # Preview table 
      if (is.data.frame(export_data$data))
        h3(sus_translate(r = r, "Data preview (first 10 rows)")),
      if (is.data.frame(export_data$data))
        div(style = "width: 100%; overflow-x: auto; height:300px; overflow-y:auto;",
            tableHTML::tableHTML(export_data$data[1:10, ], collapse = 'separate_shiny', 
                                 round = 2, rownames = FALSE)),
      
      footer = tagList(
        modalButton(sus_translate(r = r, "Dismiss")),
        downloadButton("download_csv", 
                       style = button_style,
                       sus_translate(r = r, "Download CSV")),
        downloadButton("download_shp",
                       style = button_style,
                       sus_translate(r = r, "Download SHP"))
      ))
  
  
  return(list(data = export_data$data, modal = modal))
}