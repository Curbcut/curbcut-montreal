#### BOOKMARK MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param map_viewstate A reactive which resolves to an output of a map view
#' change, coming from a mapdeck object.
#' @param var_left A reactive which resolves to a character string
#' representing the right variables to be mapped and analyzed. 
#' @param var_right A reactive which resolves to a character string
#' representing the right variables to be mapped and analyzed. 
#' @param select_id A reactive which resolves to a character string
#' representing the geometry selected on the map.
#' @param zoom_auto A reactive which resolves to a logical string
#' indicating if the user is an auto-zoom.
#' @param df A reactive which resolves to a character string representing the
#' underlying data set that is mapped. 
#' @param more_args Named vectors indicating other input that must be updated
#' following bookmarking.

bookmark_server <- function(id, r, map_viewstate = reactive(NULL), 
                            s_id = reactive(NULL),
                            df = reactive(NULL),
                            var_left = reactive(NULL),
                            var_right = reactive(NULL), 
                            more_args = reactive(NULL)) {
  
  stopifnot(is.reactive(map_viewstate))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(more_args))
  stopifnot(is.reactive(s_id))
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    # Update URL
    observe({
      
      # Map arguments
      if (!is.null(map_viewstate())) {
        zm <- floor(map_viewstate()$zoom * 100) / 100
        lon <- round(as.numeric(map_viewstate()$longitude), digits = 6)
        lat <- round(as.numeric(map_viewstate()$latitude), digits = 6)
      }
      
      # Right variable
      if (!is.null(var_left())) v_l <- 
          get_variables_rowid(unique(str_remove(var_left(), "_\\d{4}$")))
      if (!is.null(var_right())) v_r <- 
          get_variables_rowid(unique(str_remove(var_right(), "_\\d{4}$")))
      if (!is.null(input$zoom_auto)) zm_a <- str_extract(input$zoom_auto, "^.")

      # More arguments
      if (!is.null(more_args())) {
        widget_type <- names(more_args())
        widget_value <- more_args()
        more <- 
          paste(widget_type, widget_value, sep = ":", collapse = ";")
      }

      # If not supplied, shouldn't appear in the URL:
      default <- paste0("/?geo=", r$geo(), 
                        "&tb=", r$active_tab,
                        "&lng=", r$lang())
      
      add_arguments <- c("zm", "lat", "lon", "v_l", "v_r", "s_id", "zm_a", 
                         "df", "more")
      add_arguments <- 
        lapply(add_arguments, \(x) {
          value <- get0(x)
          if (is.reactive(value)) value <- value()
          
          if (!is.null(value)) return(paste0("&", x, "=", value))
        }) |> (\(x) x[lengths(x) != 0])()
      
      url <- Reduce(paste0, c(default, add_arguments))
      
      # Update the URL
      updateQueryString(url)
    })
    
  })
}
