## BUILD AND APPEND CLIMATE_RISK DATA ##########################################

build_and_append_climate_risk <- function(scales_variables_modules, crs,
                                          scales_sequences) {
  
  dates <- c(2015, 2022)

  # Read and prepare data ---------------------------------------------------
  
  climate_risk_2015 <- 
    sf::st_read("dev/data/climate_risk/vulnerabilite-changements-climatiques-mailles-2015.shp")
  climate_risk_2022 <- 
    sf::st_read("dev/data/climate_risk/vulnerabilite-changements-climatiques-mailles-2022.shp")
  
  climate_risk <- mapply(\(df, year) {
    df <- df |> 
      sf::st_make_valid() |> 
      sf::st_transform(32618)
    
    df <- df[c("PageName", "ChaleurCl", "SecheresCl", "PluiesCl", "CruesCl", 
               if (year == 2015) "TempetesCl" else "TempeteCl", "geometry")]
    names(df) <- c("ID", 
                   paste0("climate_heat_wave_", year), 
                   paste0("climate_drought_", year),
                   paste0("climate_heavy_rain_", year),
                   paste0("climate_flood_", year),
                   paste0("climate_destructive_storms_", year),
                   "geometry")
    df$ID <- paste0("grd_", df$ID)
    df
  }, list(climate_risk_2015, climate_risk_2022), c(2015, 2022), 
  SIMPLIFY = FALSE, USE.NAMES = TRUE)
  
  climate_risk <- merge(sf::st_drop_geometry(climate_risk[[1]]), climate_risk[[2]],
                        by = "ID")  |>
    tibble::as_tibble() |> 
    sf::st_as_sf() |> 
    sf::st_set_agr("constant")
  
  climate_risk$climate_flood_2022[climate_risk$climate_flood_2022 == "NoData"] <- 0
  climate_risk$climate_flood_2022 <- as.numeric(climate_risk$climate_flood_2022)

  
  # Get list of data variables ----------------------------------------------
  
  avg_vars <- names(climate_risk)[grepl("^climate_", names(climate_risk))]


  # Interpolate data to all possible scales ---------------------------------
  
  # Do not interpolate for grds
  only_scales <- names(scales_variables_modules$scales)[
    !grepl("^grd", names(scales_variables_modules$scales))
  ]
  
  data_interpolated <-
    interpolate_custom_geo(data = climate_risk,
                           all_scales = scales_variables_modules$scales,
                           average_vars = avg_vars,
                           name_interpolate_from = "grd25",
                           only_scales = only_scales,
                           crs = crs)
  
  # Add the grd25 climate data
  data_interpolated$scales$grd25 <- 
    merge(data_interpolated$scales$grd25,
          sf::st_drop_geometry(climate_risk), by = "ID")
  data_interpolated$avail_scale <- c(data_interpolated$avail_scale, "grd25")
  data_interpolated$interpolated_ref <- 
    rbind(data_interpolated$interpolated_ref,
          tibble::tibble(scale = "grd25", interpolated_from = FALSE))
  
  # # Create grd50 data
  # climate_risk <- sf::st_transform(climate_risk, crs = 4326)
  # centroids <- sf::st_centroid(climate_risk)
  # int <- sf::st_intersects(centroids, data_interpolated$scales$grd50)
  # climate_risk$grd50_ID <- sapply(int, \(i) data_interpolated$scales$grd50$ID[[i]])
  # 
  # progressr::with_progress({
  #   pb <- progressr::progressor(length(data_interpolated$scales$grd50$ID))
  #   climate_risk_50 <-
  #     lapply(data_interpolated$scales$grd50$ID, \(x) {
  #       z <- climate_risk[which(climate_risk$grd50_ID == x), ]
  #       modes <- sapply(avg_vars, \(v) {
  #         # Grab the mode. If 2 modes, take the highest value
  #         vals <- z[[v]]
  #         vals <- vals[!is.na(vals)]
  #         ordered <- sort(-table(vals))
  #         modes <- which(ordered == min(ordered))
  #         val <- if (length(modes) == 1) names(modes) else as.numeric(names(modes)) |> max()
  #         out <- tibble::tibble(v = as.numeric(val))
  #         names(out)[1] <- v
  #         out
  #       }, simplify = FALSE, USE.NAMES = TRUE)
  #       modes <- Reduce(cbind, modes)
  #       pb()
  #       tibble::tibble(ID = x, modes)
  #     })
  # })
  # climate_risk_50 <- Reduce(rbind, climate_risk_50)
  # qs::qsave(climate_risk_50, "dev/data/built/climate_risk/climate_risk_50.qs")
  climate_risk_50 <- qs::qread("dev/data/built/climate_risk/climate_risk_50.qs")
  
  data_interpolated$scales$grd50 <- 
    merge(data_interpolated$scales$grd50,
          climate_risk_50, by = "ID")
  data_interpolated$avail_scale <- c(data_interpolated$avail_scale, "grd50")
  data_interpolated$interpolated_ref <- 
    rbind(data_interpolated$interpolated_ref,
          tibble::tibble(scale = "grd50", interpolated_from = "grd25"))
  
  # # Create grd100 data
  # climate_risk <- sf::st_transform(climate_risk, crs = 4326)
  # centroids <- sf::st_centroid(climate_risk)
  # int <- sf::st_intersects(centroids, data_interpolated$scales$grd100)
  # climate_risk$grd100_ID <- sapply(int, \(i) data_interpolated$scales$grd100$ID[[i]])
  # 
  # climate_risk_100 <-
  #   future.apply::future_lapply(data_interpolated$scales$grd100$ID, \(x) {
  #     z <- climate_risk[which(climate_risk$grd100_ID == x), ] |>
  #       sf::st_drop_geometry()
  #     modes <- sapply(avg_vars, \(v) {
  #       # Grab the mode. If 2 modes, take the highest value
  #       vals <- z[[v]]
  #       vals <- vals[!is.na(vals)]
  #       ordered <- sort(-table(vals))
  #       modes <- which(ordered == min(ordered))
  #       val <- if (length(modes) == 1) names(modes) else as.numeric(names(modes)) |> max()
  #       out <- tibble::tibble(v = as.numeric(val))
  #       names(out)[1] <- v
  #       out
  #     }, simplify = FALSE, USE.NAMES = TRUE)
  #     modes <- Reduce(cbind, modes)
  #     tibble::tibble(ID = x, modes)
  #   })
  # climate_risk_100 <- Reduce(rbind, climate_risk_100)
  # qs::qsave(climate_risk_100, "dev/data/built/climate_risk/climate_risk_100.qs")
  climate_risk_100 <- qs::qread("dev/data/built/climate_risk/climate_risk_100.qs")
  
  data_interpolated$scales$grd100 <- 
    merge(data_interpolated$scales$grd100,
          climate_risk_100, by = "ID")
  data_interpolated$avail_scale <- c(data_interpolated$avail_scale, "grd100")
  data_interpolated$interpolated_ref <- 
    rbind(data_interpolated$interpolated_ref,
          tibble::tibble(scale = "grd100", interpolated_from = "grd25"))
  
  # # Create grd250 data
  # climate_risk <- sf::st_transform(climate_risk, crs = 4326)
  # centroids <- sf::st_centroid(climate_risk)
  # int <- sf::st_intersects(centroids, data_interpolated$scales$grd250)
  # climate_risk$grd250_ID <- sapply(int, \(i) data_interpolated$scales$grd250$ID[[i]])
  # 
  # climate_risk_250 <-
  #   lapply(data_interpolated$scales$grd250$ID, \(x) {
  #     z <- climate_risk[which(climate_risk$grd250_ID == x), ] |>
  #       sf::st_drop_geometry()
  #     modes <- sapply(avg_vars, \(v) {
  #       # Grab the mode. If 2 modes, take the highest value
  #       vals <- z[[v]]
  #       vals <- vals[!is.na(vals)]
  #       ordered <- sort(-table(vals))
  #       modes <- which(ordered == min(ordered))
  #       val <- if (length(modes) == 1) names(modes) else as.numeric(names(modes)) |> max()
  #       out <- tibble::tibble(v = as.numeric(val))
  #       names(out)[1] <- v
  #       out
  #     }, simplify = FALSE, USE.NAMES = TRUE)
  #     modes <- Reduce(cbind, modes)
  #     tibble::tibble(ID = x, modes)
  #   })
  # climate_risk_250 <- Reduce(rbind, climate_risk_250)
  # qs::qsave(climate_risk_250, "dev/data/built/climate_risk/climate_risk_250.qs")
  climate_risk_250 <- qs::qread("dev/data/built/climate_risk/climate_risk_250.qs")
  
  data_interpolated$scales$grd250 <- 
    merge(data_interpolated$scales$grd250,
          climate_risk_250, by = "ID")
  data_interpolated$avail_scale <- c(data_interpolated$avail_scale, "grd250")
  data_interpolated$interpolated_ref <- 
    rbind(data_interpolated$interpolated_ref,
          tibble::tibble(scale = "grd250", interpolated_from = "grd25"))
  
  
  # Data tibble -------------------------------------------------------------
  
  time_regex <- "_\\d{4}$"
  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = data_interpolated$scales,
                         unique_var = c("climate_drought", "climate_flood",
                                        "climate_heavy_rain", "climate_destructive_storms",
                                        "climate_heat_wave"),
                         time_regex = time_regex)
  

  # Prepare the variable measurements ---------------------------------------
  
  var_measurement <- data.frame(
    scale = data_interpolated$avail_scale,
    measurement = rep("scalar", length(data_interpolated$avail_scale)))
  var_measurement$measurement[var_measurement$scale == "grd25"] <- "ordinal"
  var_measurement$measurement[var_measurement$scale == "grd50"] <- "ordinal"
  var_measurement$measurement[var_measurement$scale == "grd100"] <- "ordinal"
  var_measurement$measurement[var_measurement$scale == "grd250"] <- "ordinal"
  

  # Variables table ---------------------------------------------------------

  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "climate_drought",
      type = "ind",
      var_title = "Drought vulnerability",
      var_short = "Drought",
      explanation = "the vulnerability to climate-change related drought",
      exp_q5 = "are living in areas with _X_ vulnerability to climate-change related drought⁠",
      theme = "Climate risk",
      private = FALSE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
      rank_name = c("Insignificant", "Minor", "Moderate", "Elevated", "Major"),
      rank_name_short = c("Insig.", "Minor", "Mod.", "Elev.", "Major"),
      var_measurement = var_measurement
    ) |> 
    add_variable(
      var_code = "climate_flood",
      type = "ind",
      var_title = "Flood vulnerability",
      var_short = "Flood",
      explanation = "the vulnerability to climate-change related flooding",
      exp_q5 = "are living in areas with _X_ vulnerability to climate-change related flooding⁠",
      theme = "Climate risk",
      private = FALSE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
      rank_name = c("Insignificant", "Minor", "Moderate", "Elevated", "Major"),
      rank_name_short = c("Insig.", "Minor", "Mod.", "Elev.", "Major"),
      var_measurement = var_measurement
    ) |> 
    add_variable(
      var_code = "climate_heavy_rain",
      type = "ind",
      var_title = "Heavy rain vulnerability",
      var_short = "Heavy rain",
      explanation = "the vulnerability to climate-change related heavy rain",
      exp_q5 = "are living in areas with _X_ vulnerability to climate-change related heavy rain⁠",
      theme = "Climate risk",
      private = FALSE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
      rank_name = c("Insignificant", "Minor", "Moderate", "Elevated", "Major"),
      rank_name_short = c("Insig.", "Minor", "Mod.", "Elev.", "Major"),
      var_measurement = var_measurement
    ) |> 
    add_variable(
      var_code = "climate_destructive_storms",
      type = "ind",
      var_title = "Destructive storm vulnerability",
      var_short = "Destr. storm",
      explanation = paste0("the vulnerability to climate-change related ",
                           "destructive storms"),
      exp_q5 = "are living in areas with _X_ vulnerability to climate-change related destructive storms⁠",
      theme = "Climate risk",
      private = FALSE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
      rank_name = c("Insignificant", "Minor", "Moderate", "Elevated", "Major"),
      rank_name_short = c("Insig.", "Minor", "Mod.", "Elev.", "Major"),
      var_measurement = var_measurement
    ) |> 
    add_variable(
      var_code = "climate_heat_wave",
      type = "ind",
      var_title = "Heat wave vulnerability",
      var_short = "Heat wave",
      explanation = "the vulnerability to climate-change related heat waves",
      exp_q5 = "are living in areas with _X_ vulnerability to climate-change related heat waves",
      theme = "Climate risk",
      private = FALSE,
      dates = dates,
      avail_scale = data_interpolated$avail_scale,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
      rank_name = c("Insignificant", "Minor", "Moderate", "Elevated", "Major"),
      rank_name_short = c("Insig.", "Minor", "Mod.", "Elev.", "Major"),
      var_measurement = var_measurement,
    )
  
  # Possible sequences ------------------------------------------------------
  
  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)
  nb <- length(avail_scale_combinations)
  # Make grid first
  avail_scale_combinations <- avail_scale_combinations[c(nb, 1:(nb-1))]

  # Modules table -----------------------------------------------------------

  modules <-
      scales_variables_modules$modules |>
        add_module(
          id = "climate_risk",
          theme = "Climate",
          nav_title = "Climate risk",
          title_text_title = "Climate change risk",
          title_text_main = paste0(
            "<p>Climate change will have increasingly negative impacts on communi",
            "ties in Montreal, but these will vary significantly by both geogr",
            "aphic and social factors. The distribution of five different clim",
            "ate risks – heat waves, flooding, heavy rain, drought, and destru",
            "ctive storms – is visualized here."),
          title_text_extra = paste0(
            "<p>The datasets visualized on this page are publicly available through",
            " the <a href = 'https://donnees.montreal.ca/dataset/vulnerabilite-chan",
            "gements-climatiques' target = '_blank'>Montreal Open Data Portal</a>. ",
            "These were developed as part of the City of Montreal's efforts to exam",
            "ine potential climate risks for the Montreal region in the <a href = '",
            "https://ville.montreal.qc.ca/pls/portal/docs/page/enviro_fr/media/docu",
            "ments/paccam_2015-2020_lesconstats.pdf' target = '_blank'>2015-2020 Ur",
            "ban Agglomeration Climate Change Adaptation Plan</a>."
          ),
          # Only allow for these specific regions (data is only available for the island)
          metadata = TRUE,
          dataset_info = paste0(
            "<p><a href = 'https://donnees.montreal.ca/ville-de-montreal/vuln",
            "erabilite-changements-climatiques'>",
            "The data presented on this page are cartographic representations of the ",
            "vulnerability analysis</a> developed as part of the Climate change ",
            "adaptation plan for the agglomeration of Montréal 2015-2020 for ",
            "the following climate hazards: heavy rainfall, heat waves, ",
            "destructive storms, droughts and floods.</p>"),
          var_left = c(
            "climate_drought", "climate_flood", "climate_heavy_rain",
            "climate_destructive_storms", "climate_heat_wave"
          ),
          dates = c(2015, 2022),
          main_dropdown_title = NA,
          var_right = scales_variables_modules$variables$var_code[
            scales_variables_modules$variables$source == "Canadian census" &
              !is.na(scales_variables_modules$variables$parent_vec)],
          default_var = "climate_drought",
          avail_scale_combinations = avail_scale_combinations
        )


  # Return ------------------------------------------------------------------

  return(list(
    scales = data_interpolated$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules,
    data = data
  ))

}

