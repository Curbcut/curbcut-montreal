#### MAP CHANGE FUNCTION #######################################################

#' @param id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @param x A reactive sf expression containing a data frame, with the 
#' geometries to draw on the map. 
#' @param df A reactive which resolves to a character string representing the
#' underlying data set to be loaded. Currently available options are 
#' `c("borough", "building", "CT", "DA", "grid", "street")`, and it is useful
#' to decide the `mapdeck::stroke_width` on polygons map.
#' @param selection A reactive which resolves to a character string giving the 
#' ID of a row in the input data frame (`x`) which has been selected.
#' @param legend A mapdeck manual legend to be placed at the bottom right of the 
#' map. The output of a `mapdeck::mapdeck_legend` and `mapdeck::legend_element` 
#' combination.
#' @param polygons_to_clear A strings vector indicating the specific layer_id 
#' of the map to be cleared every time the map is redrawn..
#' @param standard_width A reactive logical scalar. Should the polygon width be 
#' 0 instead?
#' @return An update version of the mapdeck map.

map_change <- function(id_map, x, df, zoom = df, click = reactive(NULL),
                       selection = reactive(NULL), 
                       legend = NULL,  polygons_to_clear = NULL, 
                       standard_width = reactive(TRUE),
                       legend_selection = reactive(NULL),
                       explore_clear = reactive(NULL)) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(click))
  stopifnot(is.reactive(selection))
  stopifnot(is.reactive(standard_width))
  stopifnot(is.reactive(legend_selection))
  stopifnot(is.reactive(explore_clear))
  
  # Get geometry type
  geom_type <- reactive({
    map_chr(as.character(unique(st_geometry_type(x()))), ~{
      switch(.x,  
             "POLYGON" = "polygon",
             "MULTIPOLYGON" = "polygon",
             "LINESTRING" = "line",
             "MULTILINESTRING" = "line",
             "MULTIPOINT" = "point",
             "POINT" = "point",
             "error")}) |> 
      unique()
  })
  
  # Create select_id -----------------------------------------------------------
  
  # Get on-click event
  observeEvent(click(), selection(click()))
  
  # Clear click status if prompted
  observeEvent(explore_clear(), selection(NA))
  
  # Clear selection on df change
  observeEvent(df(), selection(NA), ignoreInit = TRUE)
  
  # Process selection
  select_id <- reactive({
    
    select_id <- tryCatch(jsonlite::fromJSON(selection())$object$properties$id,
                          error = function(e) NULL)
    
    if (is.null(select_id)) select_id <- NA
    
    
    # Deal with buildings
    if (df() == "building") {
      
      select_id <- NA
    
    }
    
    return(select_id)
    
  })
  
  
  ## Update map on data change -------------------------------------------------
  
  observeEvent({#legend_selection()
    x()
    zoom()}, {
      
      # Used at all geometries:
      update_and_clean <- function() {
        mapdeck_update(map_id = id_map) #|>
          # clear_polygon() |>
          # clear_scatterplot() |> 
          # clear_heatmap() |> 
          # clear_path()
      }
      
      # Clear layer_ids fed with polygons_to_clear
      purrr::walk(polygons_to_clear, ~{
        mapdeck_update(map_id = id) |> clear_polygon(.x)
      })
      
      # Error handling
      if (geom_type() == "error") stop("`geom_type` invalid in `map_change`.")
      
      # Map updates for polygons  
      if (geom_type() == "polygon") {
        
        # Take the minimum width implied by the zoom or the df
        # TKTK Should probably replace this with separate zoom curves for
        # different dfs
        width <- 
          switch(zoom(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        width_2 <- 
          switch(df(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        width <- min(width, width_2)
        if (!standard_width()) width <- 0
        
        # Legend selection
        dat <- if (!is.null(legend_selection())) {
          x() |> 
            mutate(fill = case_when(str_detect(
              fill, paste0(legend_selection(), "..$", collapse = "|")) ~ fill,
              TRUE ~ str_replace(fill, "..$", "50")))
        } else x()
          
        # Set transparency based on zoom
        col_zoom <- colour_alpha[names(colour_alpha) == zoom()]
        # Override for building
        if (df() == "building") col_zoom <- "FF"
        dat <- mutate(dat, fill = paste0(fill, col_zoom))
        
        # Buildings should be extruded
        if (df() == "building") {
          update_and_clean() |> 
            add_polygon(
              data = dat, update_view = FALSE, id = "ID", elevation = 5, 
              fill_colour = "fill", auto_highlight = TRUE, 
              highlight_colour = "#FFFFFF80")
        } else {
          update_and_clean() |> 
            add_polygon(
              data = dat, stroke_width = width,
              stroke_colour = "#FFFFFF", fill_colour = "fill",
              update_view = FALSE, id = "ID", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF80")
        }
        
      # TKTK THIS HASN'T BE LOOKED AT YET
      } else if (geom_type() == "line") {
        
        update_and_clean() |>
          add_path(data = x(),
                   update_view = FALSE, id = "ID", stroke_width = 5,
                   stroke_colour = "fill", auto_highlight = TRUE,
                   highlight_colour = "#FFFFFF90")
        
      # TKTK THIS HASN'T BE LOOKED AT YET
      } else if (geom_type() == "point") {
        
        if (df() != "street") {
          update_and_clean() |>
            add_heatmap(data = x(), update_view = FALSE,
                        colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                         "#70999B", "#6E8EA8", "#6C83B5"),
                        intensity = 2)
          
        # TKTK THIS HASN'T BE LOOKED AT YET
        } else {
          
          update_and_clean() |>
            add_scatterplot(data = x(), update_view = FALSE,
                            id = "ID",
                            auto_highlight = TRUE,
                            highlight_colour = "#FFFFFF90",
                            fill_colour = "fill",
                            fill_opacity = 200,
                            legend = legend,
                            # tooltip = "label",
                            radius = 10,
                            radius_min_pixels = 8)
        }
      }
      
    })
  
  
  ## Update map on selection change --------------------------------------------
  
  observeEvent(select_id(), {
    
    if (geom_type() == "polygon") {
      if (!is.na(select_id())) {
        
        # Take the minimum width implied by the zoom or the df
        # TKTK Should probably replace this with separate zoom curves for
        # different dfs
        width <- 
          switch(zoom(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        width_2 <- 
          switch(df(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        width <- min(width, width_2)
        if (!standard_width()) width <- 0
          
        data_to_add <-
          x() |>
          filter(ID == select_id()) |>
          mutate(fill = substr(fill, 1, 7))
          
        mapdeck_update(map_id = id_map) |>
          add_polygon(
            data = data_to_add, elevation = 5, fill_colour = "fill",
            update_view = FALSE, layer_id = "poly_highlight",
            auto_highlight = TRUE, highlight_colour = "#FFFFFF80")
        
      } else {
        
        mapdeck_update(map_id = id_map) |>
          clear_polygon(layer_id = "poly_highlight")
        }
      }
    })
  
  return(select_id)
  
  }
