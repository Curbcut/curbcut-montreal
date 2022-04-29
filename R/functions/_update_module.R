#' @param mod_ns A character string representing the module namespace, for 
#' links between modules. Ex. to call a change in the canale module from a link
#' in housing (from a housing namespace to a canale). In that case, the link 
#' would be present in housing and the mod_ns would be "canale". NOT to use for 
#' bookmarking.
#' @param id A character string representing the module id, used for 
#' bookmarking AND dyk. It is used to correctly identify how to collect and 
#' recreate var_left. 

update_module <- function(r = r, mod_ns = NULL, id = mod_ns, session, zoom, location, 
                          map_id = "map", df, zoom_auto, var_left, var_right, 
                          more_args) {
  
  # Drop down menus should be delayed, as updating other widgets could 
  # have a reset power on them (e.g. housing)
  delayupdatePickerInput <- function(...) delay(100, updatePickerInput(...))
  construct_namespace <- function(inputId) {
    paste(mod_ns, inputId, sep = "-") |> str_remove("^-")
  }

  # Update mapdeck_view
  if (!all(vapply(c(zoom, location), is.null, TRUE))) {
    if (!is.null(map_id)) {
      rdeck_proxy(id = map_id,
                  initial_view_state = 
                    view_state(center = location, zoom = zoom)
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
    }
    updateSliderTextInput(
      session = session,
      inputId = construct_namespace("zoom_slider"),
      selected = sus_translate(r = r, get_zoom_name(df))
    )
  }

  if (str_detect(id, "-$")) id <- str_extract(id, ".*(?=-)")

    # Update var_left
  if (!is.null(id)) {
    if (id %in% c("canale", "marketed_sustainability")) {
      
    } else if (id %in% c("climate_risk", "housing", "gentrification",
                         "alley")) {
      if (!is.null(var_left)) {
        selected_var <- if (str_detect(var_left, "^\\d*$")) {
          get_variables_rowid(var_left)} else var_left
        updatePickerInput(
          session = session,
          inputId = construct_namespace("var"),
          selected = selected_var
        )
      }
    } else if (id %in% c("permits", "access", "crash", "green_space")) {
      if (!is.null(var_left)) {
        
        selected_var <- if (str_detect(var_left, "^\\d*$")) {
          get_variables_rowid(var_left)} else var_left
        
        var_left_list_1 <- get(paste0("var_left_list_1_", id))
        var_left_list_2 <- get(paste0("var_left_list_2_", id))
        
        selected_var_1 <-
          str_extract(selected_var, paste0(var_left_list_1, collapse = "|")) |> 
          na.omit()
        
        selected_var_2 <-
          str_extract(selected_var, paste0(var_left_list_2, collapse = "|")) |> 
          na.omit()
        
        updatePickerInput(
          session = session,
          inputId = construct_namespace("d_1"),
          selected = selected_var_1
        )
        updatePickerInput(
          session = session,
          inputId = construct_namespace("d_2"),
          selected = selected_var_2
        )
      }
    }
  }
  
  # PARSE more_args from the URL
  if (!is.null(more_args)) {
    additional <- str_split(more_args, ";")[[1]]
    
    lapply(additional, function(arg) {
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
