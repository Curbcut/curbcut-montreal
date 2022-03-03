#### PLACE EXPLORER FUNCTIONS ##################################################

#' @param df A character string representing the underlying data set to be 
#' loaded. The current options are borough, CT and DA.
#' @param select_id A character string giving the ID of a row in the input data 
#' frame (`df`) which has been selected.

update_scale_map <- function(id_map, location, init = TRUE) {
  
  (\(x) if (init) {
    mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)
  } else {
    mapdeck_update(map_id = id_map)
  })() |> 
    add_polygon(data = mutate(borough[borough$ID == st_intersection(borough, location)$ID, ],
                              df = "borough",
                              tooltip = "Borough/city"),
                layer_id = "borough",
                id = "df",
                tooltip = "tooltip",
                highlight_colour = "#FFFFFF80",
                fill_colour = "#BAE4B3BB",
                stroke_colour = "#FFFFFF",
                stroke_width = 10,
                auto_highlight = TRUE,
                update_view = FALSE) |>
    add_polygon(data = mutate(CT[CT$ID == st_intersection(CT, location)$ID, ],
                              df = "CT",
                              tooltip = "Census tract"),
                layer_id = "CT",
                id = "df",
                tooltip = "tooltip",
                highlight_colour = "#FFFFFF80",
                fill_colour = "#74C476BB",
                stroke_colour = "#FFFFFF",
                stroke_width = 10,
                auto_highlight = TRUE,
                update_view = TRUE) |>
    add_polygon(data = mutate(DA[DA$ID == st_intersection(DA, location)$ID, ],
                              df = "DA",
                              tooltip = "Dissemination area"),
                layer_id = "DA",
                id = "df",
                tooltip = "tooltip",
                highlight_colour = "#FFFFFF80",
                fill_colour = "#006D2CBB",
                stroke_colour = "#FFFFFF",
                stroke_width = 10,
                auto_highlight = TRUE,
                update_view = FALSE)
}

island_region_map <- function(location) {
  mapdeck(
    style = map_style, 
    token = map_token, 
    location = st_coordinates(location)) |> 
    add_polygon(data = mutate(st_as_sf(st_union(filter(borough, ID %in% island_CSDUID))),
                              click = "island",
                              tooltip = "Island only"), 
                layer_id = "island",
                id = "click",
                tooltip = "tooltip",
                fill_colour = "#006D2C", 
                stroke_colour = "#FFFFFF", 
                stroke_width = 500, 
                update_view = FALSE) |> 
    add_polygon(data = mutate(st_as_sf(st_union(filter(borough, !ID %in% island_CSDUID))),
                              click = "region",
                              tooltip = "Region-wide"), 
                layer_id = "region",
                id = "click",
                tooltip = "tooltip",
                fill_colour = "#74C476", 
                stroke_colour = "#FFFFFF", 
                stroke_width = 500, 
                update_view = TRUE)
}

place_explorer_block_text <- function(df, theme, select_id,
                                      island_or_region) {
  
  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  if (on_island && island_or_region == "region") on_island <- FALSE
  
  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$theme == theme & 
                           raw_data_order$ID == select_id, 
                   c("var_code")]
  variables_theme <- variables[variables$var_code %in% data_order$var_code, ]
  variables_theme <- 
    variables_theme[order(match(variables_theme$var_code, data_order$var_code)), 
                    c("var_title", "explanation")]
  data_order <- bind_cols(data_order, variables_theme)

  raw_data_var <- pe_var_hierarchy[[df]]
  names(raw_data_var) <- gsub("_\\d{4}", '', names(raw_data_var))
  cols_keep <- c(data_order$var_code, 
                    paste0(data_order$var_code, "_percentile"))
  data_var <- raw_data_var[raw_data_var$ID == select_id, cols_keep]
  data_var <- tidyr::pivot_longer(data_var, everything(), names_to = "var_code")
  out <- 
    bind_cols(
      data_order, 
      tibble(value = data_var[!grepl("_percentile", data_var$var_code), ]$value,
             percentile = data_var[grepl("_percentile", data_var$var_code), ]$value)
    )
  out <- out[!is.na(out$value), ]
  
  percentile <- map_chr(out$percentile, function(out_percentiles) {
    if (out_percentiles > 0.50) {
      per <- scales::percent(abs(out_percentiles - 1))
      if (per == "0%") per <- "1%"
      sus_translate("Top {per}")
    } else {
      per <- scales::percent(abs(out_percentiles))
      if (per == "0%") per <- "1%"
      sus_translate("Bottom {per}")
    }
  })
  out$percentile <- percentile
  out$value <- map2_chr(out$value, out$var_code, convert_unit)
  
  return(select(out, var_title, explanation, percentile, value))
  
}

place_explorer_block_plot <- function(df, theme, select_id,
                                      island_or_region) {
  
  on_island <- if (island_or_region == "region") FALSE else 
    data$CSDUID[data$ID == select_id] %in% island_CSDUID
  
  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$theme == theme & 
                                 raw_data_order$ID == select_id, ]$var_code
  
  raw_data_var <- pe_var_hierarchy[[df]]
  names(raw_data_var) <- gsub("_\\d{4}", '', names(raw_data_var))
  
  out_values <-
    raw_data_var[, c("ID", data_order)]
  
  # Plot
  plots <- 
    map(data_order, function(var_code) {
      
      hex_to_plot <- "#A9A9A9"

      data <- out_values[, var_code]
      names(data) <- "var"
      data <- data[!is.na(data$var), ]
      
      data_var <- out_values[out_values$ID == select_id, var_code][[var_code]]
      
      outlier <- if (data_var %in% remove_outliers(data$var)) FALSE else TRUE
      
      if (!is.na(data_var)) {
        data |> 
          (\(x) if (outlier) x else 
            x[x$var %in% c(remove_outliers(data$var)),])() |> 
          ggplot() +
          geom_density(aes(x = var), size = 1, color = hex_to_plot) +
          geom_vline(aes(xintercept = data_var),
                     color = "#000000", size = 1, alpha = 1) +
          theme_void()
      } else ggplot()
      
    })

}
