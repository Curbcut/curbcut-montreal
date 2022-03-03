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
                                      island_only) {

  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  if (on_island && island_only == "region") on_island <- FALSE

  out <-
    pe_variable_order[[df]] |>
    filter(theme == !!theme, ID == select_id) |>
    arrange(variable_order) |>
    left_join(variables, by = "var_code") |>
    select(var_code, var_title, explanation) |> 
    filter(!str_starts(var_code, "access"))

  names(pe_var_hierarchy[[df]]) <-
    names(pe_var_hierarchy[[df]]) |> str_remove("_\\d{4}$")

  out <-
    pe_var_hierarchy[[df]] |>
    filter(ID == select_id) |>
    select(contains(all_of(out$var_code))) |>
    tidyr::pivot_longer(everything(), names_to = "var_code") |>
    (\(x) bind_cols(
      filter(x, !str_ends(var_code, "percentile")),
      filter(x, str_ends(var_code, "percentile")) |>
        transmute(percentile = value)
    ))() |>
    left_join(out, by = "var_code")

  out <-
    out |>
    mutate(percentile = round(percentile, digit = 2),
           raw_value = value,
           value = map2_chr(out$value, out$var_code, convert_unit))

  return(select(out, var_title, explanation, percentile, value))

}

place_explorer_block_plot <- function(df, theme, select_id,
                                      island_only) {
  
  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  if (on_island && island_only == "region") on_island <- FALSE
  
  order <-
    pe_variable_order[[df]] |>
    filter(theme == !!theme, ID == select_id) |>
    arrange(variable_order)  |> 
    filter(!str_starts(var_code, "access")) |> 
    pull(var_code)
  
  names(pe_var_hierarchy[[df]]) <-
    names(pe_var_hierarchy[[df]]) |> str_remove("_\\d{4}")
  
  out_values <-
    pe_var_hierarchy[[df]] |>
    select(ID, all_of(order))
  
  out_percentiles <-
    pe_var_hierarchy[[df]] |>
    filter(ID == select_id) |> 
    select(all_of(paste0(order, "_percentile")))
  
  # Plot
  plots <- 
    map(set_names(order), function(var_code) {
      
      hex_to_plot <- "#A9A9A9"

      data <- select(out_values, all_of(var_code)) |> rename(var = 1)
      data_var <- out_values |> filter(ID == select_id) |> select(all_of(var_code)) |> pull()
      
      outlier <- if (data_var %in% remove_outliers(data$var)) FALSE else TRUE
      
      if (length(hex_to_plot) > 0) {
        data |>
          filter(!is.na(var)) |>
          (\(x) if (outlier) x else 
            filter(x, var %in% c(remove_outliers(data$var))))() |> 
          ggplot() +
          geom_density(aes(x = var), size = 1, color = hex_to_plot) +
          geom_vline(aes(xintercept = data_var),
                     color = "#000000", size = 1, alpha = 1) +
          theme_void()
      } else ggplot()
      
    })

}
