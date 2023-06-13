## BUILD AND APPEND CRASH DATA #################################################

build_and_append_crash <- function(scales_variables_modules, crs) {

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

  # Add to DAs
  DA_table <- scales_variables_modules$scales$island$DA["ID"]
  dat <- DA_table
  years <- unique(data$AN)
  
  progressr::with_progress({
    pb <- progressr::progressor(length(years) * 3)
    # Iterate over cyclist, pedestrian and total crashes
    dat <- mapply(\(df, mode) {
      # All possible years
      lapply(years, \(year) {
        
        col_name <- sprintf("crash_%s_%s", mode, year)
        z <- df[df$AN == year, ]
        dat[[col_name]] <- sf::st_intersects(DA_table, z) |> lengths()
        pb()
        sf::st_drop_geometry(dat)
        
      })
    }, list(cyc, ped, data), c("cyc", "ped", "total"), SIMPLIFY = FALSE)    
  })
  
  dat <- lapply(dat, \(x) Reduce(\(x, y) merge(x, y, by = "ID"), x))
  
  dat <- Reduce(\(x, y) merge(x, y, by = "ID"), dat)
  

  # Get list of data variables ----------------------------------------------

  average_vars <- c() 
  additive_vars <- names(dat)[!grepl("ID$", names(dat))]
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  names(dat)[1] <- "DA_ID"
  dat <- sf::st_drop_geometry(dat)
  
  data_interpolated <-
    interpolate_from_census_geo(
      data = dat,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "households",
      average_vars = average_vars,
      additive_vars = additive_vars, 
      only_regions = c("city", "island"),
      crs = crs
    )


  # Make a types named list -------------------------------------------------

  types <- list(`crash_cyc` = "count",
                `crash_ped` = "count",
                `crash_total` = "count")


  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars,
      types = types
    )


  # Get the variables values per regions ------------------------------------
  
  vars <- gsub("_\\d{4}$", "", vars) |> unique()

  # Make a parent string the same way as the types
  parent_strings <- list(`crash_cyc` = "car crashes",
                         `crash_ped` = "car crashes",
                         `crash_total` = "car crashes")

  region_vals <- variables_get_region_vals(
    scales = data_interpolated$scales,
    vars = vars,
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table)


  # Variables table ---------------------------------------------------------

  new_variables <- lapply(vars, \(var) {
    
    title <- (\(x) {
      if (var == "crash_cyc") return("Car crashes involving cyclists")
      if (var == "crash_ped") return("Car crashes involving pedestrians")
      "Car crashes"
    })()
    
    short <- (\(x) {
      if (var == "crash_cyc") return("Crashes (cyc)")
      if (var == "crash_ped") return("Crashes (ped)")
      "Crashes"
    })()
    
    explanation <- (\(x) {
      if (var == "crash_cyc") return("the number of registered car crashes involving cyclists")
      if (var == "crash_ped") return("the number of registered car crashes involving pedestrians")
      "the number of registered car crashes"
    })()
    
    exp_q5 <- (\(x) {
      if (var == "crash_cyc") return("involving cyclists were registered")
      if (var == "crash_ped") return("involving pedestrians were registered")
      "were registered"
    })()
    
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = types[[var]],
      var_title = title,
      var_short = short,
      explanation = explanation,
      exp_q5 = exp_q5,
      parent_vec = parent_strings[[var]],
      theme = "Transport",
      private = FALSE,
      dates = with_breaks$avail_dates[[var]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      region_values = region_vals[[var]],
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      var_measurement = tibble::tibble(
        df = data_interpolated$avail_df,
        measurement = rep("ordinal", length(data_interpolated$avail_df)))
    ) |> (\(x) x[nrow(x), ])()
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "crash",
      theme = "Transport",
      nav_title = "Road safety",
      title_text_title = "Road safety: Car crashes",
      title_text_main = paste0(
        "Road safety is an important consideration for wellbeing ",
        "and safety in cities. This page ",
        "provides an overview and analysis of road collisions ",
        "in the City of Montreal, ranging from 2012 to today."),
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
      regions = data_interpolated$regions,
      metadata = TRUE,
      dataset_info = paste0(""),
      var_left = vars, 
      main_dropdown_title = "Car crash", 
      dates = with_breaks$avail_dates[[vars[[1]]]],
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &
          !is.na(scales_variables_modules$variables$parent_vec)]
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = modules
  ))

}
