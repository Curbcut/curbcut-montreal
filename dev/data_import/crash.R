## BUILD AND APPEND CRASH DATA #################################################

build_and_append_crash <- function(scales_variables_modules, crs, scales_sequences,
                                   island_IDs) {
  
  
  # Prepare data for graph. get week of crashes -----------------------------

  require(lubridate)

  data <- read.csv("dev/data/crash/collisions_routieres.csv") |>
    tibble::as_tibble()

  cyc <- data[data$NB_VICTIMES_VELO > 0, ]
  ped <- data[data$NB_VICTIMES_PIETON > 0, ]

  data <- list(cyc = cyc, ped = ped, all = data)

  data <- lapply(data, \(dat) {
    dat <- dat["DT_ACCDN"]
    dat$DT_ACCDN <-  as.Date(dat$DT_ACCDN)
    dat$year <- lubridate::year(dat$DT_ACCDN)
    dat$month <- lubridate::month(dat$DT_ACCDN, label = TRUE, abbr = TRUE)
    dat$week <- lubridate::week(dat$DT_ACCDN)
    dat <- dplyr::count(dat, year, month, week, name = "count")

    sapply(as.character(unique(dat$year)), \(y) dat[dat$year == y, ],
           USE.NAMES = TRUE, simplify = FALSE)
  })

  qs::qsave(data, file = "data/crash.qs")

  # Read and prepare data ---------------------------------------------------

  # Read the data placed in a folder in `dev/data/`
  data <- read.csv("dev/data/crash/collisions_routieres.csv") |> 
       tibble::as_tibble()
  
  cols <- c("AN", "GRAVITE",
            "NB_VICTIMES_PIETON", "NB_VICTIMES_VELO", "CD_GENRE_ACCDN",
            "NB_VICTIMES_TOTAL", "LOC_LONG", "LOC_LAT")
  data <- data[cols]
  
  # As sf
  data <- data[!is.na(data$LOC_LAT), ]
  data <- data[!is.na(data$LOC_LONG), ]
  data <- sf::st_as_sf(data, coords = c("LOC_LONG", "LOC_LAT"), crs = 4326)
  
  # Including ...
  cyc <- data[data$NB_VICTIMES_VELO > 0, ]
  ped <- data[data$NB_VICTIMES_PIETON > 0, ]

  # Add to all scales
  scales <- scales_variables_modules$scales
  scales <- scales[!grepl("(^grid)|(building)|(street)", names(scales))]
  years <- unique(data$AN)
  
  progressr::with_progress({
    pb <- progressr::progressor(length(years) * 3 * length(scales))
    dat <- mapply(\(scale_name, scale_df) {
      if (!scale_name %in% names(island_IDs)) return(scale_df)
      
      table <- scale_df[scale_df$ID %in% island_IDs[[scale_name]], c("ID", "area", "population")]
      
      dat <- mapply(\(df, mode) {
        # All possible years
        lapply(years, \(year) {
          
          col_name <- sprintf("crash_count_%s_%s", mode, year)
          z <- df[df$AN == year, ]
          table[[col_name]] <- sf::st_intersects(table, z) |> lengths()
          pb()
          sf::st_drop_geometry(table)
          
        })
      }, list(cyc, ped, data), c("cyc", "ped", "total"), SIMPLIFY = FALSE)
      
      dat <- lapply(dat, \(x) Reduce(\(x, y) merge(x, y, by = c("ID", "area", "population")), x))
      
      dat <- Reduce(\(x, y) merge(x, y, by = c("ID", "area", "population")), dat)
      
      return(dat)
      
    }, names(scales), scales, SIMPLIFY = FALSE)
  })
  
  # Add sqkm and per1k
  dat <- lapply(dat, \(df) {
    cols <- names(df)[!names(df) %in% c("ID", "area", "population")]
    
    # Calcualte per sqkm
    for (col in cols) {
      new_col <- gsub("crash_count", "crash_sqkm", col)
      df[[new_col]] <- df[[col]] / df$area * 1000000      
    }
    
    # Calcualte per 1k residents
    for (col in cols) {
      new_col <- gsub("crash_count", "crash_per1k", col)
      df[[new_col]] <- df[[col]] / df$population * 1000
    }
    
    return(df)
  })
  
  # Bind to the entire scales
  all_data <- mapply(\(scale_name, scale_df) {
    if (!scale_name %in% names(dat)) return(scale_df)
    
    merge(scale_df, dat[[scale_name]], by = c("ID", "area", "population"))
    
  }, names(scales_variables_modules$scales), scales_variables_modules$scales,
  SIMPLIFY = FALSE)
  
  
  # Other necessary pieces --------------------------------------------------
  
  vars <- names(dat$CSD[4:ncol(dat$CSD)])

  avail_scale <- names(scales)
  interpolated_ref <- 
    tibble::tibble(scale = names(scales), interpolated_from = rep(FALSE, length(scales)))
  
  
  # Data tibble -------------------------------------------------------------
  
  time_regex <- "_\\d{4}$"
  vars <- gsub(time_regex, "", vars) |> unique()
  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = all_data,
                         unique_var = vars,
                         time_regex = time_regex)


  # Variables table ---------------------------------------------------------

  new_variables <- lapply(vars, \(var) {
    
    title <- (\(x) {
      if (grepl("_cyc_", var)) return("Car crashes involving cyclists")
      if (grepl("_pec_", var)) return("Car crashes involving pedestrians")
      "Car crashes"
    })()
    if (grepl("sqkm_", var)) title <- paste0(title, " per square kilometre")
    if (grepl("per1k_", var)) title <- paste0(title, " per 1,000 residents")
    
    short <- (\(x) {
      if (grepl("_cyc_", var)) return("Crashes (cyc)")
      if (grepl("_pec_", var)) return("Crashes (ped)")
      "Crashes"
    })()
    
    explanation <- (\(x) {
      if (grepl("_cyc_", var)) return("the number of registered car crashes involving cyclists")
      if (grepl("_pec_", var)) return("the number of registered car crashes involving pedestrians")
      "the number of registered car crashes"
    })()
    
    exp_q5 <- (\(x) {
      if (grepl("_cyc_", var)) return("involving cyclists were registered")
      if (grepl("_pec_", var)) return("involving pedestrians were registered")
      "were registered"
    })()
    exp_q5 <- (\(x) {
      if (grepl("_per1k_", var)) return(paste0(explanation ," is _X_"))
      if (grepl("_sqkm_", var)) return(paste0(explanation ," is _X_"))
      exp_q5
    })()
    
    
    # Add per x
    if (grepl("sqkm_", var)) explanation <- paste0(explanation, " per square kilometre")
    if (grepl("per1k_", var)) explanation <- paste0(explanation, " per 1,000 residents")
    if (grepl("sqkm_", var)) exp_q5 <- paste0(exp_q5, " per square kilometre")
    if (grepl("per1k_", var)) exp_q5 <- paste0(exp_q5, " per 1,000 residents")
    
    type <- (\(x) {
      if (grepl("_per1k_", var)) return("per1k")
      if (grepl("_sqkm_", var)) return("sqkm")
      "count"
    })()
    
    parent_vec <- (\(x) {
      if (grepl("_per1k_", var)) return("population")
      if (grepl("_sqkm_", var)) return("area")
      "car crashes"
    })()
    
    
    # Auto variables
    group_name <- (\(x) {
      if (grepl("_ped_", var)) return("Car crashes involving pedestrians")
      if (grepl("_cyc_", var)) return("Car crashes involving cyclists")
      "Car crashes"
    })()
    group_diff <- list(
      "Data representation" = (\(x) {
        if (grepl("_per1k_", var)) return("Crash per 1,000 residents")
        if (grepl("_sqkm_", var)) return("Crash per square kilometer")
        "Crash count"
      })()
    )
    
    
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = type,
      var_title = title,
      var_short = short,
      explanation = explanation,
      exp_q5 = exp_q5,
      parent_vec = parent_vec,
      theme = "Transport",
      private = FALSE,
      dates = years,
      avail_scale = avail_scale,
      source = "City of Montreal's open data website",
      interpolated = interpolated_ref,
      group_name = group_name,
      group_diff = group_diff
    ) |> (\(x) x[nrow(x), ])()
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))

  # Possible sequences ------------------------------------------------------
  
  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = avail_scale)

  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "safety",
      theme = "Transport",
      nav_title = "Road safety",
      title_text_title = "Road safety: Car crashes",
      title_text_main = paste0(
        "<p>Road safety is an important consideration for wellbeing ",
        "and safety in cities. This page displays all road collisions ",
        "in the City of Montreal."),
      title_text_extra = paste0(
        "<p>Data is collected by the Service de Police de la ",
        "Ville de Montréal (SPVM) and compiled by the Société ",
        "d’Assurance Automobile du Québec (SAAQ), and contains ",
        "information related to every road collision, including the date, ",
        "location and type of parties involved (i.e. cars, bicycles ",
        "or pedestrians) and injury severity. ",
        "<p>For more information on road collisions and a temporal ",
        "analysis of the data, please consult the ", 
        "<i>Road safety analysis</i> below.</p>",
        "<p>References:</p><ul><li><a href = https://www.pietons.quebec/",
        "sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapproche",
        "visionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir ",
        "l’approche vision zéro en sécurité routière. Piétons Québec. Online:</a>",
        "<li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-",
        "routieres'>Ville de Montréal. (2021). Collisions routières. ",
        "Données Ouvertes Montréal.</a></ul>"),
      metadata = TRUE,
      dataset_info = paste0(""),
      var_left = variables[grepl("^crash_", variables$var_code), 
                           c("var_code", "group_name", "group_diff")], 
      main_dropdown_title = "Crash type", 
      dates = years,
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &
          !is.na(scales_variables_modules$variables$parent_vec)],
      default_var = "crash_count_total",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = all_data,
    variables = variables,
    modules = modules,
    data = data
  ))

}
