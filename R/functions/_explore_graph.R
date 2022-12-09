#### EXPLORE GRAPH #############################################################

explore_graph <- function(data, r = r, var_type, var_left, var_right, df, select_id, 
                          geo, build_str_as_DA = TRUE, plot_type = "auto") {
  
  ## Check arguments -----------------------------------------------------------
  
  stopifnot(!is.reactive(data))
  stopifnot(!is.reactive(var_type))
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  stopifnot(!is.reactive(select_id))
  stopifnot(!is.reactive(df))
  stopifnot(!is.reactive(build_str_as_DA))
  stopifnot(!is.reactive(plot_type))
  

  ## Deal with build_str_as_DA -------------------------------------------------
  
  if (build_str_as_DA && is_scale_in_df("building", df)) {
    
    if (!is.na(select_id)) {
      if (is_scale_in_df("building", df)) select_id <- 
          dbGetQuery(building_conn, 
                     paste0("SELECT DA_ID FROM ", paste(geo, "building", sep = "_"), 
                            " WHERE ID = '", select_id, "'"))$DA_ID
      if (is_scale_in_df("street", df)) select_id <- street$DA_ID[street$ID == select_id]
      if (sum(!is.na(select_id)) == 0) select_id <- NA
    }
    
    df <- paste(geo, "DA", sep = "_")
    
  }

  ## Decide on plot type -------------------------------------------------------
  
  if (plot_type == "auto") plot_type <- 
    get_plot_type(data, var_type, var_left, var_right, select_id, df, geo)
  graph_type <- sub("_.*$", "", plot_type)
  select_type <- sub("^.*_", "", plot_type)

  ## Deal with NA --------------------------------------------------------------
  
  if (plot_type == "all_na") return(NULL)
  
  ## Set up plotting variables -------------------------------------------------
  
  # Prepare x and y scales
  x_scale <- get_x_scale(graph_type, data, var_type, var_left, var_right, df, geo)
  y_scale <- get_y_scale(graph_type, data, var_type, var_left, var_right)
  
  # Prepare axis labels
  labs_xy <- get_axis_labels(r = r, graph_type, var_left, var_right)

  # Prepare default theme
  theme_default <- list(
    theme_minimal(),
    theme(text = element_text(family = "SourceSansPro", size = 12),
          legend.position = "none", 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.y = element_blank()))
      
  
  ## Render and return plot ----------------------------------------------------
  
  render_explore_graph(plot_type, data, var_left, var_right, df, geo, select_id, 
                       x_scale, y_scale, labs_xy, theme_default)
  
}
