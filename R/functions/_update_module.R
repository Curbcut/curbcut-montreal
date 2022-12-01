#' @param mod_ns A character string representing the module namespace, for 
#' links between modules. By default it will take id (ex. `canale`) and create
#' a namespace to tweak its widgets (ex. `canale-canale`)
#' @param id A character string representing the module id.

update_module <- function(r, id, mod_ns = paste(id, id, sep = "-"), 
                          session, 
                          zoom = NULL, location, 
                          map_id = NULL, 
                          zoom_auto, var_left, 
                          var_right, select_id = NA, df = NULL, more_args) {
  
  # Drop down menus should be delayed, as updating other widgets could 
  # have a reset power on them (e.g. housing)
  delayupdatePickerInput <- function(time = 100, ...) 
    delay(time, updatePickerInput(...))
  construct_namespace <- function(inputId) {
    paste(mod_ns, inputId, sep = "-") |> str_remove("^-")
  }


  ## Update mapview ----------------------------------------------------------
  if (!all(sapply(c(zoom, location), is.null)))
    if (!is.null(map_id))
      if (length(location) == 2)
        rdeck_proxy(id = paste(id, id, map_id, sep = "-"),
                    initial_view_state = 
                      view_state(center = location, zoom = zoom))


  ## Update df ---------------------------------------------------------------
  
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
      selected = cc_t(r = r, get_zoom_name(df))
    )
    r[[id]]$df(df)
  }

  if (str_detect(id, "-$")) id <- str_extract(id, ".*(?=-)")
  
  ## Update select_id --------------------------------------------------------
  
  if (!is.na(select_id)) r[[id]]$select_id(select_id)
  
  ## Parse and update more_args from the URL ---------------------------------
  
  if (!is.null(more_args)) {
    additional <- str_split(more_args, ";")[[1]]
    
    lapply(additional, function(arg) {
      type_inputId <- str_split(arg, ":")[[1]][1]
      widget_type <- str_split(type_inputId, "-")[[1]][1]
      inputId <- str_split(type_inputId, "-")[[1]][2]
      value <- str_split(arg, ":")[[1]][2] |> 
        str_split("-")
      value <- value[[1]]
      
      # Checkboxes
      if (widget_type == "c") {
        updateCheckboxInput(
          session = session,
          inputId = construct_namespace(inputId),
          value = if (str_detect(value, "^\\d$")) value else as.logical(value)
        )
      
        # Sliders
      } else if (widget_type == "s") {
        updateSliderInput(
          session = session,
          inputId = construct_namespace(inputId),
          value = value
        )
        
        # Dropdowns
      } else if (widget_type == "d") {
        
        # Does it follow a code from get_variables_rowid ?
        selected_value <- if (str_detect(value, "^\\d*$")) {
          get_variables_rowid(value)} else value
        
        delayupdatePickerInput(
          session = session,
          inputId = construct_namespace(inputId),
          selected = selected_value
        )
        
        # Other custom cases
      } else if (widget_type == "o") {
        # Previously normalized?, for Centraide modules
        if (inputId == "p_n") r[[id]]$prev_norm(as.logical(value))
      }
    })
  }
  

  ## Update var_left ---------------------------------------------------------
  
  if (!is.null(id)) {
    if (id %in% c("canale")) {
      
    } else if (id %in% c("climate_risk", "housing",
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
    } else if (id %in% c("access", "crash", "tenure", "dw_types",
                         "afford", "demographics", "city_amenities",
                         "vac_rate")) {
      if (!is.null(var_left)) {
        
        selected_var <- if (str_detect(var_left, "^\\d*$")) {
          get_variables_rowid(var_left)} else var_left
        
        # How many var_left dropdowns
        var_left_drops_length <- 
          length(str_subset(ls(envir = environment_created_in_R), 
                            paste0("var_left_list_\\d*_", id)))
        
        lapply(seq_len(var_left_drops_length), function(num) {
          
          var_left_list <- unlist(get(paste0("var_left_list_", num, "_", id)))
          
          # If total, don't include it ~yet
          if ("total" %in% var_left_list) {
            var_left_list_no_total <- 
              var_left_list[-which(var_left_list == "total")]
            
            selected_var_num <-
              str_extract(selected_var, 
                          paste0(var_left_list_no_total, collapse = "|")) |> 
              na.omit()
            
            if (length(selected_var_num) == 0) selected_var_num <- "total"
            
          } else {
            selected_var_num <-
              str_extract(selected_var, 
                          paste0(var_left_list, collapse = "|")) |> 
              na.omit()
          }
          
          updatePickerInput(
            session = session,
            inputId = construct_namespace(paste0("d_", num)),
            selected = selected_var_num
          )
          
        })
        
      }
    } else if (id == "natural_inf") {
      
      selected_var <- if (str_detect(var_left, "^\\d*$")) {
        get_variables_rowid(var_left)} else var_left
      
      var_left_list_1 <- get(paste0("var_left_list_1_", id))
      var_left_list_2 <- get(paste0("var_left_list_2_", id))
      
      selected_var_1 <- 
        if (selected_var %in% var_left_list_1) selected_var else {
          unlisted <- unlist(var_left_list_2)
          list_name <- names(unlisted[unlisted == selected_var])
          str_extract(list_name, "^.*(?=\\.)")
        }

      updatePickerInput(
        session = session,
        inputId = construct_namespace("d_1"),
        selected = selected_var_1
      )
      
      if (!selected_var %in% var_left_list_1) {
        delayupdatePickerInput(
          session = session,
          inputId = construct_namespace("d_2"),
          selected = selected_var
        )
      }
    } else if (id == "amenities") {
      
      selected_var <- if (str_detect(var_left, "^\\d*$")) {
        get_variables_rowid(var_left)} else var_left
      
      group <- variables[variables$var_code == selected_var, ]$group_name
      # Same process that's in make_dropdown() to decide which variable is
      # the one to select in the compare-var dropdown.
      cat_vecs <- 
        variables[!is.na(variables$group_name) & variables$group_name == group, ]
      higher_level_var <- choose_first_data_compare_group(cat_vecs)
      
      # The compare-var picker update
      updatePickerInput(
        session = session,
        inputId = construct_namespace("auto_var-var"),
        selected = higher_level_var
      )
      
      # Other dropdowns update
      update_drops <- 
        variables$group_diff[variables$var_code == selected_var][[1]]
      
      # If the choices are numeric, change to numeric
      update_drops <- change_inlist_numeric(update_drops)
      
      lapply(seq_along(update_drops), \(x) {
        var_named <- update_drops[x]
        id_s <- create_id_s(var_named)$key

        if (is.numeric(var_named[[1]])) {
          delay(1000, {
          updateSliderInput(session = session,
                            inputId = construct_namespace(paste0(id_s, "-slider")),
                            value = var_named[[1]])})
        } else {
          delayupdatePickerInput(
            time = 1000,
            session = session,
            inputId = construct_namespace(paste0(id_s, "-var")),
            selected = var_named)
        }})
    }
  }
  

  ## Update var_right --------------------------------------------------------
  
  if (!is.null(var_right)) {
    selected_var <- if (str_detect(var_right, "^\\d*$")) {
      get_variables_rowid(var_right)} else var_right
    
    if (!selected_var %in% variables$var_code[!is.na(variables$group_name)]) {
      delayupdatePickerInput(
        session = session,
        inputId = construct_namespace("compare-auto_var-var"),
        selected = selected_var
      )
      # In the case the var_right is part of a bigger group
    } else {
      
      group <- variables[variables$var_code == selected_var, ]$group_name
      # Same process that's in make_dropdown() to decide which variable is
      # the one to select in the compare-var dropdown.
      cat_vecs <- 
        variables[!is.na(variables$group_name) & variables$group_name == group, ]
      higher_level_var <- choose_first_data_compare_group(cat_vecs)
      
      # The compare-var picker update
      delayupdatePickerInput(
        session = session,
        inputId = construct_namespace("compare-auto_var-var"),
        selected = higher_level_var
      )
      
      # Other dropdowns update
      update_drops <- 
        variables$group_diff[variables$var_code == selected_var][[1]]
      
      # If the choices are numeric, change to numeric
      update_drops <- change_inlist_numeric(update_drops)
      
      lapply(seq_along(update_drops), \(x) {
        var_named <- update_drops[x]
        id_s <- create_id_s(var_named)$key

        if (is.numeric(var_named[[1]])) {
          delay(1000, {
            updateSliderInput(session = session,
                              inputId = construct_namespace(paste0("compare-", id_s, "-slider")),
                              value = var_named[[1]])})
        } else {
          delayupdatePickerInput(
            time = 1000,
            session = session,
            inputId = construct_namespace(paste0("compare-", id_s, "-var")),
            selected = var_named)
        }})
      
    }
  }
  
}


