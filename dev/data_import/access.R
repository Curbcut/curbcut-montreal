## BUILD AND APPEND ACCESS DATA ################################################

build_and_append_access <- function(scales_variables_modules,
                                    DA_table,
                                    traveltimes,
                                    crs) {

  # Read and prepare data ---------------------------------------------------

  # ## DAYCARE
  # daycares <- tempfile(fileext = ".csv")
  # download.file("https://www.donneesquebec.ca/recherche/dataset/be36f85e-e419-4978-9c34-cb5795622595/resource/89af3537-4506-488c-8d0e-6d85b4033a0e/download/repertoire-installation.csv",
  #               daycares)
  # daycares <- tibble::as_tibble(read.csv(daycares))
  # Encoding(daycares$REGION) <- "latin1"
  # Encoding(daycares$ADRESSE) <- "latin1"
  # Encoding(daycares$NOM_MUN_COMPO) <- "latin1"
  # Encoding(daycares$NOM) <- "latin1"
  # daycares <-
  #   daycares |>
  #   dplyr::filter(REGION %in% c("6 - Montréal", "15 - Laurentides",
  #                               "14 - Lanaudière", "16 - Montérégie", "13 - Laval")) |>
  #   dplyr::mutate(ADRESSE =
  #                   stringr::str_remove_all(ADRESSE,
  #                                           ", (bureau| bureau|rez-de-chaussée|AG-10|local|suite|appartement|porte) .*$") |>
  #                   stringr::str_remove_all("      \\de étage|, \\de étage") |>
  #                   stringr::str_remove_all("(?<=\\d)-\\d*|[A-Z](?=,)")) |>
  #   dplyr::mutate(ADRESSE = paste0(ADRESSE, ", ",NOM_MUN_COMPO, ", QC"))
  # daycares <- daycares[c("ADRESSE", "PLACE_TOTAL")]
  # # Geolocate with addresses
  # daycares$geometry <- future.apply::future_sapply(
  #   daycares$ADRESSE, cc.data::geocode_localhost,
  #   simplify = FALSE, USE.NAMES = FALSE, future.seed = NULL)
  # daycares <- sf::st_as_sf(daycares, crs = 4326)
  # # One daycare spot = one point
  # progressr::with_progress({
  #   pb <- progressr::progressor(steps = nrow(daycares))
  #   daycares <-
  #     future.apply::future_lapply(seq_len(nrow(daycares)), \(r) {
  #       pb()
  #       row <- daycares[r, ]
  #       Reduce(rbind, lapply(seq_len(row$PLACE_TOTAL), \(x) row))["geometry"]
  #     })
  #   pb <- progressr::progressor(steps = length(daycares))
  #   daycares <-
  #     Reduce(\(a, b) {
  #       pb()
  #       rbind(a, b)
  #     }, daycares)
  # })
  # 
  # qs::qsave(daycares, "dev/data/built/daycares.qs")
  daycares <- qs::qread("dev/data/built/daycares.qs")

  # Add point data to DA ----------------------------------------------------

  point_DA <- accessibility_point_per_DA(point_data = list(daycarespots_2023 = daycares),
                                         DA_table = census_scales$DA,
                                         crs = crs)


  # Add access to point data by time intervals ------------------------------

  data <- accessibility_add_intervals(point_per_DA = point_DA,
                                      traveltimes = traveltimes)
  qs::qsave(data, "dev/data/built/access_data.qs")
  data <- qs::qread("dev/data/built/access_data.qs")

  # Get list of data variables ----------------------------------------------

  average_vars <- names(data)[!grepl("ID$", names(data))]
  additive_vars <- c()
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  names(data)[1] <- "DA_ID"
  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )


  # Calculate breaks --------------------------------------------------------
  
  unique_vars <- gsub("_\\d{4}$", "", average_vars)
  
  # Calculate breaks ONCE for 30 minutes. Use those breaks on all variables
  breaks_base <- sapply(unique_vars, paste, simplify = FALSE, USE.NAMES = TRUE)
  middle_val <- time_intervals[round(length(time_intervals)/2)]
  breaks_base <- lapply(breaks_base, \(x) gsub("_\\d{2}_", sprintf("_%s_", middle_val), x))
  
  types <- rep(list("avg"), length(unique_vars))
  names(types) <- unique_vars
  
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = average_vars,
      types = types
    )
  
  # Calculate region values -------------------------------------------------
  
  # Parent strings
  vars <- gsub("_2023$", "", average_vars)
  parent_strings <- rep(list("population"), length(vars))
  names(parent_strings) <- vars
  
  types <- rep(list("avg"), length(vars))
  names(types) <- vars
  
  # Region values
  region_values <- variables_get_region_vals(
    scales = with_breaks$scales,
    vars = vars,
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table,
    round_closest_5 = FALSE)
  
  
  # Variable measurements ----------------------------------------------------
  
  var_measurement <- data.frame(
    df = data_interpolated$avail_df,
    measurement = rep("scalar", length(data_interpolated$avail_df)))
  
  var_measurement$measurement[grepl("_DA$", var_measurement$df)] <-
    rep("ordinal", length(var_measurement$measurement[grepl("_DA$", var_measurement$df)]))

  # Variables table ---------------------------------------------------------
  
  new_variables <- lapply(vars, \(var) {
    
    theme <- "daycare spots"
    
    mode <- (\(x) {
      if (grepl("_car_", var)) return("car")
      if (grepl("_foot_", var)) return("walking")
      if (grepl("_bicycle_", var)) return("bicycle")
      if (grepl("_transit_opwe_", var)) return("public transit on off-peak weekend days")
      if (grepl("_transit_pwe_", var)) return("public transit on peak weekend days")
      if (grepl("_transit_nwd_", var)) return("public transit on weekdays at night")
      if (grepl("_transit_nwe_", var)) return("public transit on weekends at night")
      if (grepl("_transit_opwd_", var)) return("public transit on off-peak weekdays")
      if (grepl("_transit_pwd_", var)) return("public transit on peak weekdays")
    })()
    
    time <- gsub("_", "", stringr::str_extract(var, "_\\d*_"))
    
    var_title <- stringr::str_to_sentence(paste0(theme, " accessible by ", mode))
    var_short <- stringr::str_to_sentence(theme)
    
    
    explanation <- paste0(
      "the number of ", tolower(theme),
      " an average resident can reach within ", time, " minutes by ", mode
    )
    exp_q5 <- paste0(
      "the average resident has access to _X_ ", tolower(theme), " within ", time,
      " minutes by ", mode
    )
    
    # Cut timing out of the mode
    mode <- stringr::str_extract(mode, "(^car$)|(^walking$)|(^bicycle$)|(^public transit)")
    
    group_name <- paste("Access to", theme)
    group_diff <- list("Mode of transport" = stringr::str_to_sentence(mode),
                       "Transportation time" = time)
    
    if (grepl("_transit_", var)) {
      timing <- (\(x) {
        if (grepl("_transit_opwe_", var)) return("Weekend traffic off-peak")
        if (grepl("_transit_pwe_", var)) return("Weekend traffic peak")
        if (grepl("_transit_nwd_", var)) return("Weekday night")
        if (grepl("_transit_nwe_", var)) return("Weekend night")
        if (grepl("_transit_opwd_", var)) return("Weekday traffic off-peak")
        if (grepl("_transit_pwd_", var)) return("Weekday traffic peak")
      })()
      group_diff <- c(group_diff, list("Timing" = timing))
    }
    
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = "avg",
      var_title = var_title,
      var_short = var_short,
      explanation = explanation,
      exp_q5 = exp_q5,
      group_name = group_name,
      group_diff = group_diff,
      parent_vec = "population",
      theme = "Transport",
      private = FALSE,
      pe_include = var == "access_foot_20_daycarespots_2023",
      region_values = region_values[[var]],
      dates = with_breaks$avail_dates[[var]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      source = "Données Québec",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense"),
      var_measurement = var_measurement
    ) |>
      (\(x) x[nrow(x), ])()
  })
  
  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))
  
  
  # Modules table -----------------------------------------------------------

  # Module already added with the `ba_accessibility_points` function
  # We however need to make sure the new access points are available in it
  scales_variables_modules$modules$var_left[
    scales_variables_modules$modules$id == "access"
  ] <- list(variables[grepl("^access_", variables$var_code),
                      c("var_code", "group_name", "group_diff")])
  
  modules <- scales_variables_modules$modules
  
  
  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}
