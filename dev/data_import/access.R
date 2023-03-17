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
  # daycares <- qs::qread("dev/data/built/daycares.qs")
  
  # Add point data to DA ----------------------------------------------------
  
  # point_DA <- accessibility_point_per_DA(point_data = list(daycarespots_2023 = daycares),
  #                                        DA_table = census_scales$DA,
  #                                        crs = crs)
  # 
  # 
  # # Add access to point data by time intervals ------------------------------
  # 
  # data <- accessibility_add_intervals(point_per_DA = point_DA,
  #                                     traveltimes = traveltimes)
  # qs::qsave(data, "dev/data/built/access_data.qs")
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

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars
    )


  # Variables table ---------------------------------------------------------
  
  vars <- unique(gsub("_\\d{4}$", "", vars))
  
  new_variables <- lapply(vars, \(var) {
    
    theme <-
      if (grepl("daycarespots", var)) "daycare spots" else
        if (grepl("", var)) ""
    
    mode <-
      if (grepl("_car_", var)) "car" else
        if (grepl("_foot_", var)) "walking" else
          if (grepl("_bicycle_", var)) "bicycle" else
            if (grepl("_transit_", var)) "public transit"
    
    time <- gsub("_", "", stringr::str_extract(var, "_\\d*_"))
    
    var_title <- stringr::str_to_sentence(paste0(theme, " accessible by ", mode))
    var_short <- stringr::str_to_sentence(theme)
    explanation <- paste0("the average count of ", tolower(theme),
                          " accessible in ", time," minutes by ", mode)
    
    
    group_name <- paste("Access to", theme)
    group_diff <- list("Mode of transport" = stringr::str_to_sentence(mode),
                       "Transportation time" = time)
    
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = "avg",
      var_title = var_title,
      var_short = var_short,
      explanation = explanation,
      group_name = group_name,
      group_diff = group_diff,
      theme = "Transport",
      private = FALSE,
      pe_include = {var == "access_foot_20_daycarespots"},
      dates = with_breaks$avail_dates[[var]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      source = "Données Québec",
      interpolated = data_interpolated$interpolated_ref
    ) |> (\(x) x[nrow(x), ])()
  })
  
  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))
  
  
  # Modules table -----------------------------------------------------------

  # Module already added with the `ba_accessibility_points` function


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}
