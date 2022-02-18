#### BOOKMARK MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param df A reactive which resolves to a character string representing the
#' underlying data set to be loaded. Currently available options are 
#' `c("borough", "building", "CT", "DA", "grid", "street")`.
#' @return A reactive expression containing a data frame with the following
#' fields (If the `var_right` input is " ", the right_var_full and right_var 
#' fields will be omitted.):
#' - ID, name, name_2, population: The unmodified variables of the same names
#' from the input data frame.
#' - var_left, var_left_q3, var_left_q5: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_left`.
#' - var_right, var_right_q3, var_right_q5: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_right`.
#' - geometry: The unmodified geometry variable from the input data frame.
#' - group: A character field of form "X - Y", where X and Y are the quantile
#' values from var_left and var_right respectively.
#' - fill: A character vector of hex colour values to be passed to mapdeck for
#' colouring choropleths drawn from the data frame.
#' 
#'       id = "canale",

bookmark_server <- function(id, map_view_change, var_right, select_id, zoom_auto,
                            df, map_id, more_args = reactive(NULL)) {
  
  stopifnot(is.reactive(map_view_change))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select_id))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(more_args))
  
  moduleServer(id, function(input, output, session) {
    
    # Update URL
    observe({
      
      # Map arguments
      if (!is.null(map_view_change())) {
        zoom <- floor(map_view_change()$zoom * 100) / 100
        longitude <- round(as.numeric(map_view_change()$longitude), digits = 6)
        latitude <- round(as.numeric(map_view_change()$latitude), digits = 6)
      }
      
      zoom <- if (!exists("zoom")) map_zoom else zoom
      latitude <- if (!exists("latitude")) map_location[2] else latitude
      longitude <- if (!exists("longitude")) map_location[1] else longitude
      
      # More arguments
      widget_type <- names(more_args())
      widget_value <- more_args()
      more_arguments <- 
        paste(widget_type, widget_value, sep = ":", collapse = ";")
      
      # Update the URL
      updateQueryString(paste0("/?tb=", sus_rv$active_tab(),
                               "&lng=", sus_rv$lang(),
                               "&zm=", zoom,
                               "&lat=", latitude,
                               "&lon=", longitude,
                               "&v_r=", get_dropdown_list_nb(unique(str_remove(var_right(), 
                                                                "_\\d{4}$"))),
                               "&id=", select_id(),
                               "&zm_a=", str_extract(input$zoom_auto, "^."),
                               "&df=", df(),
                               "&more=", more_arguments
      ))
    })

    # Update from bookmark
    observeEvent(sus_bookmark$active, {
      
      # Drop down menus should be delayed, as updating other widgets could 
      # have a reset power on them (e.g. housing)
      delayupdatePickerInput <- function(...) delay(100, updatePickerInput(...))
      
      if (isTRUE(sus_bookmark$active)) {
        
        # Update mapdeck_view
        if (!all(map_lgl(c(sus_bookmark$zoom, sus_bookmark$location), is.null))) {
          mapdeck_update(map_id = map_id) |>
            mapdeck_view(
              location = sus_bookmark$location,
              zoom = sus_bookmark$zoom,
              duration = 1000,
              transition = "fly",
            )
        }
        
        # Update df()
        if (!is.null(sus_bookmark$df)) {
          if (isFALSE(sus_bookmark$zoom_auto)) {
            updateCheckboxInput(
              session = session,
              inputId = "zoom_auto",
              value = FALSE
            )
            updateSliderTextInput(
              session = session,
              inputId = "zoom_slider",
              selected = get_zoom_name(sus_bookmark$df)
            )
          }
        }
        
        # PARSE more_args from the URL
        if (!is.null(sus_bookmark$more_args)) {
          additional <- str_split(sus_bookmark$more_args, ";")[[1]]

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
                inputId = inputId,
                value = as.logical(value)
              )
            } else if (widget_type == "s") {
              updateSliderInput(
                session = session,
                inputId = inputId,
                value = value
              )
            } else if (widget_type == "d") {
              delayupdatePickerInput(
                  session = session,
                  inputId = inputId,
                  selected = get_dropdown_list_nb(value)
              )
            }
          })
        }
        
        # Update var_right
        if (!is.null(sus_bookmark$var_right)) {
          delayupdatePickerInput(
            session = session,
            inputId = "compare-var",
            selected = get_dropdown_list_nb(sus_bookmark$var_right)
          )
        }
        
      }
    }, priority = -1, autoDestroy = TRUE, once = TRUE)
    
    
    
  })
}