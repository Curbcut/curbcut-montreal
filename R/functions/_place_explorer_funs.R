#### PLACE EXPLORER FUNCTIONS ##################################################

#' @param df A character string representing the underlying data set to be 
#' loaded. The current options are borough, CT and DA.
#' @param select_id A character string giving the ID of a row in the input data 
#' frame (`df`) which has been selected.

update_scale_map <- function(id_map, location, init = TRUE) {
  
  borough <- st_set_agr(borough, "constant")
  CT <- st_set_agr(CT, "constant")
  DA <- st_set_agr(DA, "constant")
  location <- st_set_agr(location, "constant")
  
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
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme & 
                             data_order$ID == select_id, 
                   c("var_code")]
  
  if (df == "CT" && theme == "Transport") 
    data_order <- distinct(data_order, var_code)
  
  # Access for CT
  variables_var_codes <- 
    if (df == "CT" && theme == "Transport") {
      bind_rows(
        variables[!grepl("access", variables$var_code), ], {
          access_vars <- variables[grepl("access", variables$var_code), ]
          new_var_code <- 
            case_when(str_starts(access_vars$var_code, "access_jobs") ~
                        str_extract(access_vars$var_code, "access_jobs_[^_]*"),
                      TRUE ~ str_extract(access_vars$var_code, "access_[^_]*"))
          
          access_vars$var_code <- new_var_code
          access_vars <- distinct(access_vars, var_code, .keep_all = TRUE)
          
          exp_suffix <- c("at weekday peak service",
                          "at weekday off-peak service",
                          "at weekday night service",
                          "at weekend peak service",
                          "at weekend off-peak service",
                          "at weekend night service")
          
          access_vars$explanation <- 
            str_replace(access_vars$explanation, paste0(exp_suffix, collapse = "|"), 
                        "on average")
          
          access_vars
        }
      )
    } else variables
  
  variables_theme <- variables_var_codes[variables_var_codes$var_code %in% data_order$var_code, ]
  variables_theme <- 
    variables_theme[order(match(variables_theme$var_code, data_order$var_code)), 
                    c("var_title", "explanation")]
  data_order <- bind_cols(data_order, variables_theme)

  raw_data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in% 
                                           data_order$var_code]
  data_var <- map(raw_data_var, ~{.x[.x$ID == select_id, ]})
  
  col <- paste0(island_or_region, "_percentile")
  data_var <- map2(raw_data_var, names(raw_data_var), ~{
    df <- .x[.x$ID == select_id, c("var", col)]
    names(df)[1] <- .y
    tidyr::pivot_longer(df, 1, names_to = "var_code", values_to = "value")
    }) |> reduce(bind_rows)
  names(data_var)[1] <- "percentile"

  out <- left_join(data_order, data_var, by = "var_code")
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
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme & 
                             data_order$ID == select_id, 
                           c("var_code")]
  
  if (df == "CT" && theme == "Transport") 
    data_order <- distinct(data_order, var_code)
  
  raw_data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in% data_order$var_code]
  
  # Plot
  plots <- 
    map(data_order$var_code, function(var_code) {
      
      hex_to_plot <- "#A9A9A9"
      
      data <- raw_data_var[[var_code]]
      # data <- out_values[, var_code]
      data <- data[!is.na(data$var), ]
      
      data_var <- data[data$ID == select_id, ]$var
      
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
