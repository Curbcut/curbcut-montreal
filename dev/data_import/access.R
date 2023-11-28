## BUILD AND APPEND ACCESS DATA ################################################

build_and_append_access <- function(scales_variables_modules,
                                    DA_table,
                                    traveltimes,
                                    scales_sequences,
                                    crs) {

  # # Read and prepare data ---------------------------------------------------
  # 
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
# 
#   # Add point data to DA ----------------------------------------------------
# 
#   point_DA <- accessibility_point_per_DA(point_data = list(daycarespots = daycares),
#                                          DA_table = census_scales$DA,
#                                          crs = crs)
# 
# 
#   # Add access to point data by time intervals ------------------------------
# 
#   data <- accessibility_add_intervals(point_per_DA = point_DA,
#                                       traveltimes = traveltimes)
#   qs::qsave(data, "dev/data/built/access_data.qs")
  data <- qs::qread("dev/data/built/access_data.qs")
  
  # 2023 data
  names(data)[2:ncol(data)] <- paste0(names(data)[2:ncol(data)], "_2023")

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


  # Data tibble -------------------------------------------------------------
  
  time_regex <- "_\\d{4}$"
  unique_var <- gsub(time_regex, "", average_vars)
  unique_var <- gsub("_\\d{1,2}$", "", unique_var)
  unique_var <- unique(unique_var)
  
  # Construct the breaks_var list (take the breaks from a 20 minutes traject)
  breaks_var <- lapply(unique_var, paste0, "_20_2023")
  names(breaks_var) <- unique_var
  
  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = data_interpolated$scales,
                         unique_var = unique_var,
                         time_regex = time_regex,
                         schema = list(time = gsub("^_", "", time_regex),
                                       transportationtime = "(?<=_)\\d{1,2}(?=_)"),
                         breaks_var = breaks_var)
  
  
  # Types and parent vectors ------------------------------------------------
  
  parent_strings <- rep(list("population"), length(unique_var))
  names(parent_strings) <- unique_var
  
  types <- rep(list("avg"), length(unique_var))
  names(types) <- unique_var
  
  
  # Variable measurements ----------------------------------------------------
  
  var_measurement <- data.frame(
    scale = data_interpolated$avail_scale,
    measurement = rep("scalar", length(data_interpolated$avail_scale))
  )
  
  var_measurement$measurement[var_measurement$scale == "DA"] <- "ordinal"

  # Variables table ---------------------------------------------------------
  
  new_variables <- lapply(unique_var, \(var) {
    
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
    
    time <- "__transportationtime__"
    
    var_title <- stringr::str_to_sentence(paste0(theme, " accessible by ", mode))
    var_short <- stringr::str_to_sentence(theme)
    
    mode_text <- (\(x) {
      if (mode == "car") {
        return("drive")
      }
      if (mode == "walking") {
        return("walk")
      }
      if (mode == "bicycle") {
        return("bike ride")
      }
      if (grepl("public transit", mode)) {
        return(gsub("public transit", "transit journey", mode))
      }
    })()
    explanation <- paste0(
      "the number of ", tolower(theme),
      " a resident can reach within a ", time, "-minute ", mode_text
    )
    exp_q5 <- paste0(
      "a resident has access to, on average,  _X_ ", tolower(theme), " within a ",
      time, "-minute ", mode_text
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
      dates = "2023",
      avail_scale = data_interpolated$avail_scale,
      source = "Données Québec",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c(
        "exceptionally sparse", "unusually sparse",
        "just about average", "unusually dense",
        "exceptionally dense"
      ),
      var_measurement = var_measurement
    ) |>
      (\(x) x[nrow(x), ])()
  })
  
  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))
  
  
  # Possible sequences ------------------------------------------------------
  
  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)
  
  
  # Return ------------------------------------------------------------------

  return(list(
    scales = data_interpolated$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules,
    data = data
  ))

}
