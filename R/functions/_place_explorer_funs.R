#### PLACE EXPLORER FUNCTIONS ##################################################

#' @param df A character string representing the underlying data set to be 
#' loaded. The current options are borough, CT and DA.
#' @param select_id A character string giving the ID of a row in the input data 
#' frame (`df`) which has been selected.

# Testing purpose:
# df <- sample(c("borough", "CT", "DA"), 1)
# select_id <- sample(get(df)$ID, 1)
# 
# df <- "DA"
# select_id <- "24663329"
# 
# select_id <- sample(get(df)$ID, 1)
# title_card_indicators_fun(df, select_id, island_only_comparison = TRUE)

title_card_row_prep_fun <- function(df, select_id, ind, percent = TRUE,
                                    high_is_good = TRUE, val_digits = 0,
                                    island = TRUE, geo_area = geo_area,
                                    geo_areas = geo_areas) {
  
  hex_scale <- c("#CA0020", "#F4A582", "#A9A9A9", "#BAE4B3", "#31A354")
  
  scale <- if (island) "island" else "region"
  
  # Prepare list to store all data
  info <- list()
  
  data <- title_card_indicators[[ind]][[df]]
  data_var <- data[data$ID == select_id, ]$var
  info$pretty_data_var <- data_var |> 
    (\(x) if (percent) scales::percent(x) else round(x, digits = val_digits))()
  info$data_date <- title_card_index$date[title_card_index$name == 
                                            ind]
  
  if (df == "borough") {
    rank <- data[data$ID == select_id, ][[paste0(scale, "_rank")]]
    df_row <- if (island) data |> na.omit(island_rank) |> na.omit(var) |> nrow() else {
      df_row <- length(na.omit(data$var))
    }
    # If high is good, then last rank means 1st. Inverse!
    data_rank <- 
      if (high_is_good) df_row - rank + 1 else rank
    info$data_rank <- 
      if (data_rank > 1/3*df_row) {
        sus_translate("It ranks ", if (data_rank > 2/3*df_row) "relatively low at ", 
                      ordinal_form(data_rank), "/{df_row}", 
                      if (island) " on the island" else " in the region")
      } else {
        sus_translate("It ranks ", ordinal_form(data_rank),
                      "best", if (island) " on the island" else " in the region")
      }
  } else {
    data_rank <- data[data$ID == select_id, ][[paste0(scale, "_percentile")]]
    info$data_rank <- 
      if (is.na(data_rank)) {
        ""
      } else if (data_rank > 0.75) {
        sus_translate("The {geo_area} ranks in the ", 
                      if (high_is_good) "highest " else "lowest ",
                      if (abs(data_rank - 1) < 1) "1%" else scales::percent(abs(data_rank - 1)))
      } else if (data_rank < 0.25) {
        sus_translate("The {geo_area} ranks in the ",
                      if (!high_is_good) "highest " else "lowest ",
                      if (data_rank == 0) "1%" else scales::percent(data_rank))
      } else {
        sus_translate("Its value is higher than ", scales::percent(data_rank), 
                      " of ", geo_areas, if (island) " on the island" else " in the region")
      }
  }
  
  #### PLOT AND PERCENTILE (FOR COLOR)
  colors_which <- 
    if (high_is_good) c(0.1,0.3,0.5,0.7,0.9) else rev(c(0.1,0.3,0.5,0.7,0.9))
  hex_to_plot <- 
      hex_scale[which.min(abs(colors_which - 
                                data[data$ID == select_id, ][[paste0(scale, "_percentile")]]))]
      
  # In case it's higher than the threshold of 5
  if (ind == "air_quality_no2" && data_var >= 5) hex_to_plot <- "#CA0020"
  
  info$percentile <- {
    data_rank <- data[data$ID == select_id, ][[paste0(scale, "_percentile")]]
    
    if (data_rank > 0.50) {
      per <- scales::percent(abs(data_rank - 1))
      if (per == "0%") per <- "1%"
      sus_translate("<p style = 'font-size: small; ",
                    "margin:auto; text-align:center;",
                    "color:", hex_to_plot,"'>Top {per}</p>")
    } else {
      per <- scales::percent(abs(data_rank))
      if (per == "0%") per <- "1%"
      sus_translate("<p style = 'font-size: small; ",
                    "margin:auto; text-align:center;",
                    "color:", hex_to_plot,"'>Bottom {per}</p>")
    }
  }
  
  outlier <- if (data_var %in% remove_outliers(data$var)) FALSE else TRUE

  info$plot <- 
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
  
  return(info)
  
}


title_card_indicators_fun <- function(df, select_id, island_only_comparison) {
  
  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  
  if (on_island && island_only_comparison == "region") on_island <- FALSE
  
  indicators_table <- if (!filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID) {
    title_card_index[title_card_index$island_only == FALSE, ]
  } else title_card_index
  
  geo_area <- switch(df, "borough" = "borough/city",
                          "CT" = "census tract",
                          "DA" = "dessimination area")
  geo_areas <- switch(df, "borough" = "boroughs or cities",
                           "CT" = "census tracts",
                           "DA" = "dessimination areas")

  to_grid <- 
  map(set_names(indicators_table$name), ~{

    iteration_result <- 
    if (.x == "transit_walk_cycle_share") {
      
      z <- title_card_row_prep_fun(df, select_id, ind =  "transit_walk_cycle_share",
                                   island = on_island, geo_area = geo_area,
                                   geo_areas = geo_areas)
      
      text <- 
        if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
          sus_translate("{z$pretty_data_var} of residents ",
                        "use public transit, walk or ",
                        "bicycle to get to work. {z$data_rank}. ",
                        "(Data from {z$data_date})")
        }
      
      list(row_title = "Sus. transport",
           percentile = z$percentile,
           graph = z$plot,
           text = text)
      
    } else if (.x == "single_detached") {

      z <- title_card_row_prep_fun(df, select_id, ind = "single_detached",
                                   island = on_island, high_is_good = FALSE, 
                                   geo_area = geo_area, geo_areas = geo_areas)
      
      text <- 
        if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
          sus_translate("{z$pretty_data_var} of occupied dwellings are ",
                        "single-detached houses. {z$data_rank}. ",
                        "(Data from {z$data_date})")
        }

      
      list(row_title = "Housing",
           percentile = z$percentile,
           graph = z$plot,
           text = text)
      
    } else if (.x == "total_crash_per1k") {

      z <- title_card_row_prep_fun(df, select_id, ind = "total_crash_per1k",
                                   percent = FALSE,
                                   island = on_island, high_is_good = FALSE, 
                                   geo_area = geo_area, geo_areas = geo_areas)
      
      text <- 
        if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
          sus_translate("There were {z$pretty_data_var} total crashes ",
                        "per 1,000 residents in {z$data_date}. ", 
                        "{z$data_rank}. ")
        }
      
      list(row_title = "Road safety",
           percentile = z$percentile,
           graph = z$plot,
           text = text)
      
    } else if (.x == "air_quality_no2") {

      z <- title_card_row_prep_fun(df, select_id, ind = "air_quality_no2",
                                   percent = FALSE, val_digits = 1,
                                   island = on_island, high_is_good = FALSE, 
                                   geo_area = geo_area, geo_areas = geo_areas)
      
      higher_than_threshold <- 
      if (z$pretty_data_var > 5) {
        sus_translate("Its value is higher than WHO's guideline value of 5. ")
      } else ""
      
      list(row_title = "Air pollution",
           percentile = z$percentile,
           graph = z$plot,
           text = sus_translate("{z$data_rank} in terms of level of NO2 ",
                                "pollution. {higher_than_threshold}(NO2 = ",
                                "{z$pretty_data_var}, data from {z$data_date})"))
      
    } else if (.x == "green_space_ndvi") {

      z <- title_card_row_prep_fun(df, select_id, ind = "green_space_ndvi",
                                   percent = TRUE,
                                   island = on_island, high_is_good = TRUE, 
                                   geo_area = geo_area, geo_areas = geo_areas)
      
      text <- 
        if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
          sus_translate("{z$data_rank} in terms of greenery. (NDVI ", 
                        "= {z$pretty_data_var}, data from ", 
                        "{z$data_date})")
        }
      
      list(row_title = "Greenery",
           percentile = z$percentile,
           graph = z$plot,
           text = text)
      
      
    } else if (.x == "canale_index") {

      z <- title_card_row_prep_fun(df, select_id, ind = "canale_index",
                                   percent = FALSE,
                                   island = on_island, high_is_good = TRUE, 
                                   geo_area = geo_area, geo_areas = geo_areas)
      
      text <- 
        if (is.na(z$pretty_data_var)) sus_translate("No data.") else {
          sus_translate("{z$data_rank} in terms of active living. ",
                        "(Data from {z$data_date})")
        }
      
      list(row_title = "Active living",
           percentile = z$percentile,
           graph = z$plot,
           text = text)
    }
    
    iteration_result
    
  })
  
  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid
  
}

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
                                      island_only_comparison) {

  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  if (on_island && island_only_comparison == "region") on_island <- FALSE

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
                                      island_only_comparison) {
  
  on_island <- filter(get(df), ID == select_id)$CSDUID %in% island_CSDUID
  if (on_island && island_only_comparison == "region") on_island <- FALSE
  
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
