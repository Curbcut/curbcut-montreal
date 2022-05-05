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
    paste0("<h4>", sus_translate(r = r, "About module data"), "</h4>")
  
  about_module$text_linked <- 
    paste0("<a href = '", module$link, "' target = '_blank'>", 
           str_replace_all(
             sus_translate(r = r, module$dataset_info), 
             "<p>", "<p style = 'font-size: 1.45rem; cursor: help;'>"),
           "</a>")
  
  
  # About the data ----------------------------------------------------------
  
  about_data <- list()
  
  about_data$left_parts$title <- 
    paste0("<h4>", sus_translate(r = r, "About exportable data"), "</h4>")
  
  ## Left variable ----------------------------------------------------------
  
  var_left <- variables[variables$var_code == export_data$var_left_code, ]
  
  var_left_private <- var_left$private
  
  about_data$left_parts <- list()
  
  about_data$left_parts$title <- 
    paste0("<h5>", sus_translate(r = r, "About left variable"), "</h4>")
  
  # Time
    # Year
    if (str_detect(export_data$var_left, "_\\d{4}$")) 
      about_data$left_parts$time <- str_extract(export_data$var_left, "\\d{4}$")
  
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
    
    quant_info <- lapply(quant_info, round, digit = 2)
    
    about_data$left_parts$quant_details <- 
      sus_translate(r = r, 
                    "The data range from <b>{quant_info$min}</b> to ",
                    "<b>{quant_info$max}</b>. The mean is <b>{quant_info$mean}",
                    "</b> and the standard deviation is <b>{quant_info$sd}</b>.")
  }
  
  # Qualitative (Possible values)
  
  # Source with possibly more information.
  about_data$left_parts$source
  if (var_right$source == "Canadian census") {
    
    
    sus_translate(r = r,
                  "The data comes from the {about_data$left_parts$time} ",
                  "Canadian census. ")
  } else {
    sus_translate(r = r,
                  "The data comes from {var_left$source}.")
  }
  
  # Has data been interpolated from another geometry?
  
  
  ## Right variable ---------------------------------------------------------
  
  var_right_private <- FALSE
  
  if (!is.null(export_data$var_right) && export_data$var_right != " ") {
    
    var_right <- variables[variables$var_code == export_data$var_right_code, ]
    
    var_right_private <- var_right$private
    
    about_data$right_parts <- list()
    
    about_data$right_parts$title <- 
      paste0("<h5>", sus_translate(r = r, "About right variable"), "</h4>")
    
  }
  
  
  # Hide private data -------------------------------------------------------
  
  bivariate <- {!is.null(export_data$var_right) && export_data$var_right != " "}
  
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
  
  # # Prepare the list of text
  # left_parts <- list()
  # 
  # left_parts$details <- 
  #   paste0(sus_translate(r = r, "The column `"), 
  #          export_data$var_left, 
  #          sus_translate(r = r, "` contains data on "), 
  #          sus_translate(r = r, var_left$explanation), 
  #          " (<b>", sus_translate(r = r, var_left$var_title), "</b>).")
  # 
  # if (!is.null(var_right)) {
  #   left_parts$details <- 
  #     paste0(left_parts$details, " ",
  #            paste0(sus_translate(r = r, "The column `"), 
  #                   export_data$var_right, 
  #                   sus_translate(r = r, "` contains data on "), 
  #                   sus_translate(r = r, var_right$explanation), 
  #                   " (<b>", sus_translate(r = r, var_right$var_title), "</b>)."))
  # }
  # 
  # left_parts$data_origin <- 
  #   str_to_lower(sus_translate(r = r, get_zoom_name(export_data$data_origin)))
  # left_parts$data_origin <- 
  #   sus_translate(r = r, "Data is organized by {left_parts$data_origin}.")
  #                            
  # left_parts$public <- 
  #   if (var_left$private) sus_translate(r = r, "The data is private.") else 
  #     sus_translate(r = r, "The data is public.")
  # 
  # left_parts$source <- sus_translate(r = r, var_left$source)
  # left_parts$source <- sus_translate(r = r, "Source: {left_parts$source}")
  
  button_style <- 
    if (!is.data.frame(export_data$data)) {
      "border: 1px solid #999999; pointer-events: none; opacity: 0.5;"
    } else NULL
  
  modal <- 
    modalDialog(
      title = paste0(sus_translate(r = r, "Data explanation and export on `"),
                     sus_translate(r = r, names(unlist(unname(mods_rdy))[
                       export_data$id == unlist(mods_rdy)])),
                     "`"),
      
      # About module data
      HTML(unlist(about_module)),
      
      # About exportable data
      # HTML(paste0("<p style = 'font-size: 1.45rem;'>", 
      #             paste0(about_data, 
      #                    collapse = "<p style = 'font-size: 1.45rem;'></p>"), 
      #             "</p>")),
      
      # Preview table 
      if (is.data.frame(export_data$data))
        h4(sus_translate(r = r, "Data preview (first 10 rows, numeric rounded)")),
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