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
  
  about_data <- get_metadata(export_data = export_data, 
                             about_data = about_data,
                             var = "var_left",
                             variables_row = var_left)
  
  ## Right variable ---------------------------------------------------------
  
  var_right_private <- FALSE
  
  if (exist_var_right) {

    var_right <- variables[variables$var_code == export_data[[paste0(var, "_code")]], ]
    
    var_right_private <- var_right$private
    
    about_data$right_parts <- list()
    
    about_data$right_parts$title <- 
      paste0("<h4>", sus_translate(r = r, "About right variable"), "</h3>")
    
    export_data <- get_metadata(export_data = export_data, var = "var_right",
                                variables_row = var_right)
    
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