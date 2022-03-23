#### GET TITLE CARD ############################################################

#' @param island_or_region A character string which is either "region" or "island".

get_title_card <- function(data, df, select_id, island_or_region) {

  ## Setup ---------------------------------------------------------------------

  # Test if comparison will be only on island
  on_island <- if (island_or_region == "island") TRUE else FALSE

  # Choose indicators based on data availability
  indicators_table <-
    if (on_island) title_card_index else {
      title_card_index[title_card_index$island_only == FALSE, ]
    }

  # Get scale names
  geo_area <- switch(df, "borough" = "borough/city", "CT" = "census tract",
                     "DA" = "dissemination area")
  geo_areas <- switch(df, "borough" = "boroughs or cities",
                      "CT" = "census tracts", "DA" = "dissemination areas")

  ## Generate output grid ------------------------------------------------------

  to_grid <- pmap(indicators_table, ~{
    z <- prep_title_card(df, select_id, ind =  ..1,
                    percent = ..5, high_is_good = ..6, val_digits = ..7,
                    link_module = ..9, link_var_left = ..10, link_outside = ..11,
                    island = on_island, geo_area = geo_area,
                    geo_areas = geo_areas)

    if (..1 == "air_quality_no2") higher_than_threshold <-
        if (z$pretty_data_var > 5) {
          sus_translate("Its value is higher than WHO's guideline value of 5. ")
        } else ""

    list(row_title = ..2,
         percentile = z$percentile,
         graph = z$plot,
         text = if (is.na(z$pretty_data_var))
           sus_translate("No data.") else sus_translate(..8),
         link = z$link,
         link_module = z$link_module,
         link_var_left = z$link_var_left)
  })

  names(to_grid) <- indicators_table$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid

}
