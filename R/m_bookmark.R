#### BOOKMARK MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param map_view_change A reactive which resolves to an output of a map view
#' change, coming from a mapdeck object.
#' @param var_right A reactive which resolves to a character string
#' representing the right variables to be mapped and analyzed. 
#' @param select_id A reactive which resolves to a character string
#' representing the geometry selected on the map.
#' @param zoom_auto A reactive which resolves to a logical string
#' indicating if the user is an auto-zoom.
#' @param df A reactive which resolves to a character string representing the
#' underlying data set that is mapped. Currently available options are 
#' `c("borough", "building", "CT", "DA", "grid", "street")`.
#' @param map_id The id of the map that must be updated following bookmarking. 
#' @param more_args Named vectors indicating other input that must be updated
#' following bookmarking.

bookmark_server <- function(id, map_view_change = reactive(NULL), 
                            var_right = reactive(NULL), 
                            select_id = reactive(NULL), 
                            df = reactive(NULL), map_id = NULL, 
                            more_args = reactive(NULL)) {
  
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
        zm <- floor(map_view_change()$zoom * 100) / 100
        lon <- round(as.numeric(map_view_change()$longitude), digits = 6)
        lat <- round(as.numeric(map_view_change()$latitude), digits = 6)
      }
      
      # Right variable
      if (!is.null(var_right())) v_r <- get_variables_rowid(unique(str_remove(var_right(), "_\\d{4}$")))
      if (!is.null(select_id()) && !is.na(select_id())) s_id <- select_id()
      if (!is.null(input$zoom_auto)) zm_a <- str_extract(input$zoom_auto, "^.")
      if (!is.null(df())) df <- df()
      
      # More arguments
      if (!is.null(more_args())) {
        widget_type <- names(more_args())
        widget_value <- more_args()
        more <- 
          paste(widget_type, widget_value, sep = ":", collapse = ";")
      }
      
      # If not supplied, shouldn't appear in the URL:
      default <- paste0("/?tb=", sus_rv$active_tab(),"&lng=", sus_rv$lang())
      
      add_arguments <- c("zm", "lat", "lon", "v_r", "s_id", "zm_a", "df", "more")
      add_arguments <- 
        map(add_arguments, ~{
          if (exists(.x) && !is.null(.x)) {
            value <- get(.x)
            if (is.reactive(value)) return(NULL)
            return(paste0("&", .x, "=", value))
          }
        }) |> (\(x) x[lengths(x) != 0])()
      
      url <- reduce(c(default, add_arguments), paste0)
      
      # Update the URL
      updateQueryString(url)
    })
    
    
    # Update from bookmark
    observeEvent(sus_bookmark$active, {
      if (isTRUE(sus_bookmark$active)) {
        update_module(session = session,zoom = sus_bookmark$zoom, 
                      location = sus_bookmark$location, 
                      map_id = map_id, df = sus_bookmark$df, 
                      zoom_auto = sus_bookmark$zoom_auto, 
                      var_right = sus_bookmark$var_right, 
                      more_args = sus_bookmark$more_args)
      }
      
    }, priority = -1, autoDestroy = TRUE, once = TRUE)
    
  })
}
