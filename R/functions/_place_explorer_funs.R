#### PLACE EXPLORER FUNCTIONS ##################################################

#' @param df A character string representing the underlying data set to be
#' loaded. The current options are borough, CT and DA.
#' @param select_id A character string giving the ID of a row in the input data
#' frame (`df`) which has been selected.

update_scale_map <- function(id_map, loc_DAUID, init = TRUE) {

  polygons <- list()
  polygons$borough <- borough[
    borough$CSDUID == DA[DA$DAUID == loc_DAUID, ]$CSDUID, ]
  polygons$CT <- CT[
    CT$CTUID == DA[DA$DAUID == loc_DAUID, ]$CTUID, ]
  polygons$DA <- DA[DA$DAUID == loc_DAUID, ]
  
  (\(x) if (init) {
    rdeck(
      map_style = map_base_style, 
      initial_view_state = 
        view_state(center = 
                     eval(parse(text = as.character(polygons$DA$centroid))),
                   zoom = 12))
  } else {
    rdeck_proxy(id = id_map,
                initial_view_state = 
                  view_state(center = 
                               eval(parse(text = 
                                            as.character(polygons$DA$centroid))),
                             zoom = 12))
  })() |>
    
    add_polygon_layer(data = polygons$borough,
                      # name = rlang::sym("tooltip"),
                      id = "borough",
                      pickable = TRUE,
                      highlight_color = "#FFFFFF80",
                      get_fill_color = "#BAE4B3BB",
                      get_line_color = "#FFFFFF",
                      get_line_width = 10,
                      auto_highlight = TRUE,
                      get_polygon = rlang::sym("geometry")) |>
    add_polygon_layer(data = polygons$CT,
                      # name = rlang::sym("tooltip"),
                      id = "CT",
                      pickable = TRUE,
                      highlight_color = "#FFFFFF80",
                      get_fill_color = "#74C476BB",
                      get_line_color = "#FFFFFF",
                      get_line_width = 10,
                      auto_highlight = TRUE,
                      get_polygon = rlang::sym("geometry")) |>
    add_polygon_layer(data = polygons$DA,
                      # name = rlang::sym("tooltip"),
                      id = "DA",
                      pickable = TRUE,
                      highlight_color = "#FFFFFF80",
                      get_fill_color = "#006D2CBB",
                      get_line_color = "#FFFFFF",
                      get_line_width = 10,
                      auto_highlight = TRUE,
                      get_polygon = rlang::sym("geometry"))
}

island_region_map <- function() {
  rdeck(map_style = map_base_style, 
        initial_view_state = view_state(
          center = map_location, 
          zoom = 6)) |>
    add_polygon_layer(data = place_explorer_island,
                      id = "island",
                      pickable = TRUE,
                      highlight_color = "#FFFFFF80",
                      get_fill_color = "#006D2C",
                      get_line_color = "#FFFFFF",
                      get_line_width = 10,
                      auto_highlight = TRUE,
                      get_polygon = rlang::sym("x")) |>
    add_polygon_layer(data = place_explorer_CMA,
                      id = "region",
                      pickable = TRUE,
                      highlight_color = "#FFFFFF80",
                      get_fill_color = "#74C476",
                      get_line_color = "#FFFFFF",
                      get_line_width = 10,
                      auto_highlight = TRUE,
                      get_polygon = rlang::sym("x"))
}

place_explorer_block_text <- function(df, theme, select_id,
                                      island_or_region) {

  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme &
                             data_order$ID == select_id, c("var_code")]
  
  if (nrow(data_order) == 0) return(data.frame())

  if (df == "CT" && theme == "Transport")
    data_order <- unique.data.frame(data_order)

  # Access for CT
  variables_var_codes <-
    if (df == "CT" && theme == "Transport") {
      rbind(
        variables[!grepl("access", variables$var_code), ], {
          access_vars <- variables[grepl("access", variables$var_code), ]

          new_var_code <- c(
          access_vars$var_code[str_starts(access_vars$var_code, "access_jobs")] |> 
            str_extract("access_jobs_[^_]*"),
          access_vars$var_code[!str_starts(access_vars$var_code, "access_jobs")] |> 
            str_extract("access_[^_]*"))

          access_vars$var_code <- new_var_code
          unique(access_vars, incomparables = FALSE, MARGIN = 2)
          access_vars <- access_vars[!duplicated(access_vars$var_code), ]

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
  data_order <- cbind(data_order, variables_theme)

  raw_data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in%
                                           data_order$var_code]
  data_var <- lapply(raw_data_var, \(x) {x[x$ID == select_id, ]})

  col <- paste0(island_or_region, "_percentile")
  data_var <- 
    data.frame(
      col = sapply(raw_data_var, \(x) x[x$ID == select_id,][[col]], 
                   USE.NAMES = FALSE),
      var_code = names(raw_data_var),
      value = sapply(raw_data_var, \(x) x$var[x$ID == select_id], 
                     USE.NAMES = FALSE),
      row.names = NULL
    ) |> 
    setNames(c(col, "var_code", "value"))
  
  names(data_var)[1] <- "percentile"

  out <- merge(data_order, data_var, by = "var_code")
  out <- out[!is.na(out$value), ]

  percentile <- sapply(out$percentile, \(out_percentiles) {
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
  out$value <- mapply(convert_unit, out$value, out$var_code, 
                      USE.NAMES = FALSE)

  return(out[, c("var_title", "explanation", "percentile", "value")])

}

place_explorer_block_plot <- function(df, theme, select_id,
                                      island_or_region) {

  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme &
                             data_order$ID == select_id,]["var_code"]

  if (df == "CT" && theme == "Transport")
    data_order <- unique.data.frame(data_order)

  raw_data_var <- pe_var_hierarchy[[df]][
    names(pe_var_hierarchy[[df]]) %in% data_order$var_code]

  # Plot
  plots <- lapply(data_order$var_code, \(var_code) {

    hex_to_plot <- "#A9A9A9"

    data <- raw_data_var[[var_code]]
    data <- data[!is.na(data$var), ]
    data_var <- data[data$ID == select_id, ]$var
    var_outlier <- remove_outliers(data$var)
    outlier <- if (data_var %in% var_outlier) FALSE else TRUE

    if (!is.na(data_var)) {
      data_out <- if (outlier) data[data$var %in% var_outlier,] else data
      ggplot(data_out) +
        geom_density(aes(x = var), size = 1, color = hex_to_plot) +
        geom_vline(aes(xintercept = data_var), color = "#000000", size = 1,
                   alpha = 1) +
        theme_void()
    } else ggplot()

  })

}
