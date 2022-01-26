#### GET EXPLORE GRAPH X SCALE #################################################

get_y_scale <- function(graph_type, var_left, var_right) {
  
  # Prepare y scale
  y_scale <- case_when(
    graph_type == "date" ~ 
      list(scale_y_continuous(labels = scales::comma)),
    # Multi_bi
    graph_type == "deltabi" ~ 
      list(scale_y_continuous(labels = scales::percent)),
    # Multi_uni, continuous scale, percent
    graph_type == "delta" & str_detect(var_left, "_pct") ~ 
      list(scale_y_continuous(labels = scales::percent)),
    # Continuous scale, dollar
    graph_type == "delta" & str_detect(var_left, "_dollar") ~ 
      list(scale_y_continuous(labels = scales::dollar)),
    # Continuous scale, comma
    graph_type == "delta" ~ 
      list(scale_y_continuous(labels = scales::comma)),
    # Continuous scale, comma, no decimal
    graph_type %in% c("hist", "bar") ~ 
      list(scale_y_continuous(labels = scales::label_comma(accuracy = 1))),
    # Scatterplot, continuous scale, percent
    graph_type == "scatter" & str_detect(var_left, "_pct") ~ 
      list(scale_y_continuous(labels = scales::percent)),
    # Scatterplot, continuous scale, dollar
    graph_type == "scatter" & str_detect(var_left, "_dollar") ~ 
      list(scale_y_continuous(labels = scales::dollar)),
    # Scatterplot, continuous scale, comma
    graph_type == "scatter" ~ list(scale_y_continuous(labels = scales::comma)),
    # Continuous scale, percent
    str_detect(var_right, "_pct") ~ 
      list(scale_y_continuous(labels = scales::percent)),
    # Continuous scale, dollar
    str_detect(var_right, "_dollar") ~ 
      list(scale_y_continuous(labels = scales::dollar)),
    # Continuous scale, comma
    TRUE ~ list(scale_y_continuous(labels = scales::comma))
  )
  
  unique(y_scale)
  
}