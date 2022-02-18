update_module <- function(mod_ns = NULL, session, zoom, location, 
                          map_id = "map", df, zoom_auto, var_right, more_args) {
  
  # Drop down menus should be delayed, as updating other widgets could 
  # have a reset power on them (e.g. housing)
  delayupdatePickerInput <- function(...) delay(100, updatePickerInput(...))
  construct_namespace <- function(inputId) {
    paste(mod_ns, inputId, sep = "-") |> str_remove("^-")
  }
    
    # Update mapdeck_view
    if (!all(map_lgl(c(zoom, location), is.null))) {
      if (!is.null(map_id)) {
        mapdeck_update(map_id = map_id) |>
          mapdeck_view(
            location = location,
            zoom = zoom,
            duration = 500,
            transition = "fly",
          )
      }}
    
    # Update df()
    if (!is.null(df)) {
      if (isFALSE(zoom_auto)) {
        updateCheckboxInput(
          session = session,
          inputId = construct_namespace("zoom_auto"),
          value = FALSE
        )
        updateSliderTextInput(
          session = session,
          inputId = construct_namespace("zoom_slider"),
          selected = get_zoom_name(df)
        )
      }
    }
    
    # PARSE more_args from the URL
    if (!is.null(more_args)) {
      additional <- str_split(more_args, ";")[[1]]
      
      walk(additional, function(arg) {
        type_inputId <- str_split(arg, ":")[[1]][1]
        widget_type <- str_split(type_inputId, "-")[[1]][1]
        inputId <- str_split(type_inputId, "-")[[1]][2]
        value <- str_split(arg, ":")[[1]][2] |> 
          str_split("-")
        value <- value[[1]]
        
        if (widget_type == "c") {
          updateCheckboxInput(
            session = session,
            inputId = construct_namespace(inputId),
            value = if (str_detect(value, "^\\d$")) value else as.logical(value)
          )
        } else if (widget_type == "s") {
          updateSliderInput(
            session = session,
            inputId = construct_namespace(inputId),
            value = value
          )
        } else if (widget_type == "d") {
          
          # Does it follow a code from get_variables_rowid ?
          selected_value <- if (str_detect(value, "^\\d*$")) {
            get_variables_rowid(value)} else value
          
          delayupdatePickerInput(
            session = session,
            inputId = construct_namespace(inputId),
            selected = selected_value
          )
        }
      })
    }
    
    # Update var_right
    if (!is.null(var_right)) {
      selected_var <- if (str_detect(var_right, "^\\d*$")) {
        get_variables_rowid(var_right)} else var_right
      delayupdatePickerInput(
        session = session,
        inputId = construct_namespace("compare-var"),
        selected = selected_var
      )
    }
  
}