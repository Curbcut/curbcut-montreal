## BUILD AND APPEND SHORT_DISTANCE_CITY DATA ###################################

build_and_append_short_distance_city <- function(scales_variables_modules, crs) {

  # Read and prepare data ---------------------------------------------------

  bike <- 
    sf::read_sf("dev/data/short_distance_city/Indicateurs_accessibilite_velo.shp",
            crs = 4326) |> 
    sf::st_drop_geometry() |> 
    dplyr::relocate(ID = GeoUID, .before = Bibliotequ) |> 
    dplyr::select(-Population) |> 
    purrr::set_names("ID", "library", "cultural", "Communita", "daycare", "preschool", 
              "primary", "secondary", "postsecondary", "Sport_inte", "Equi_Sante", "commercial_zone", 
              "alimentation", "pharmacy", "laundromat", "P_quartier", "big_park", "playground", 
              "cemeteries", "nature", "aquatic", "winter", "summer") |> 
    dplyr::rename_with(~paste0("city_amenities_", str_to_lower(.x), "_bike"), 
                where(is.numeric))
  
  walk <- 
    sf::read_sf("dev/data/short_distance_city/Indicateurs_accessibilite_marche.shp",
            crs = 4326) |> 
    sf::st_drop_geometry() |> 
    dplyr::relocate(ID = GeoUID, .before = Bibliotequ) |> 
    dplyr::select(-Population) |> 
    purrr::set_names("ID", "library", "cultural", "Communita", "daycare", "preschool", 
              "primary", "secondary", "postsecondary", "Sport_inte", "Equi_Sante", "commercial_zone", 
              "alimentation", "pharmacy", "laundromat", "P_quartier", "big_park", "playground", 
              "cemeteries", "nature", "aquatic", "winter", "summer") |> 
    dplyr::rename_with(~paste0("city_amenities_", str_to_lower(.x), "_walk"), 
                where(is.numeric))
  
  city_amenities <- merge(bike, walk, by = "ID")
  city_amenities <- city_amenities |> 
    dplyr::select(!dplyr::contains(c("library", "communita")))


  # Get list of data variables ----------------------------------------------

  # Build a character vector of all data variables that will be added to all
  # scales. Average and additive vars are for interpolation. A count variable
  # like number of households is additive. The percentage of tenants is average.
  average_vars <-  names(city_amenities)[!grepl("ID$", names(city_amenities))]
  additive_vars <- c()
  vars <- c(average_vars, additive_vars)

  
  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  names(city_amenities)[1] <- "DB_ID"
  data_interpolated <-
    interpolate_from_census_geo(
      data = city_amenities,
      base_scale = "DB",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      only_regions = c("city"),
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )


  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars,
      time_regex = ""
    )


  # Variables table ---------------------------------------------------------
  
  new_rows <-
    lapply(vars, function(var) {

      # TITLE
      category <- case_when(str_detect(var, "_library_") ~
                              "libraries",
                            str_detect(var, "_cultural_") ~
                              "cultural facilities",
                            str_detect(var, "_communita_") ~
                              "community facilities",
                            str_detect(var, "_daycare_") ~
                              "daycares",
                            str_detect(var, "_preschool_") ~ 
                              "preschools",
                            str_detect(var, "_primary_") ~ 
                              "primary scools",
                            str_detect(var, "_secondary_") ~ 
                              "secondary schools",
                            str_detect(var, "_postsecondary_") ~ 
                              "postsecondary schools",
                            str_detect(var, "_sport_inte_") ~ 
                              "indoor sports activities",
                            str_detect(var, "_equi_sante_") ~ 
                              "health and social services facilities",
                            str_detect(var, "_commercial_zone_") ~ 
                              "commercial zones",
                            str_detect(var, "_alimentation_") ~ 
                              "food stores",
                            str_detect(var, "_pharmacy_") ~ 
                              "pharmacies",
                            str_detect(var, "_laundromat_") ~ 
                              "laundromats",
                            str_detect(var, "_p_quartier_") ~ 
                              "neighbourhood parks",
                            str_detect(var, "_big_park_") ~ 
                              "big parks",
                            str_detect(var, "_playground_") ~ 
                              "playgrounds",
                            str_detect(var, "_cemeteries_") ~ 
                              "cemeteries",
                            str_detect(var, "_nature_") ~ 
                              "nature facilities",
                            str_detect(var, "_aquatic_") ~ 
                              "outdoor water activities",
                            str_detect(var, "_winter_") ~ 
                              "outdoor winter activities",
                            str_detect(var, "_summer_") ~ 
                              "outdoor summer activities")
      
      mode_title <- case_when(str_detect(var, "_walk$") ~
                                "(Walk)",
                              str_detect(var, "_bike$") ~
                                "(Cycling)")
      
      mode_exp <- case_when(str_detect(var, "_walk$") ~
                              "within a 15-minute walk",
                            str_detect(var, "_bike$") ~
                              "within a 20-minute cycling time")
      
      title <- paste0("Accessibility to ", category, " ", mode_title)
      short <- str_to_sentence(category)
      exp <- paste0("the average number of ", category, 
                    " a resident of the area can reach ", mode_exp)
      
      mode_group <- 
        case_when(
          str_detect(var, "_walk$") ~ "15-minute walking time",
          str_detect(var, "_bike$") ~ "20-minute cycling time")
      
      group_diff <- list("Mode of transport" = mode_group)
      
      # ADDED ROW
      out <-
        add_variable(variables = scales_variables_modules$variables,
                      var_code = var,
                      type = "avg",
                      var_title = title,
                      var_short = short,
                      explanation = exp,
                      theme = "City amenities",
                      private = TRUE,
                      dates = with_breaks$avail_dates[[var]],
                     avail_df = data_interpolated$avail_df,
                      breaks_q3 = with_breaks$q3_breaks_table[[var]],
                      breaks_q5 = with_breaks$q5_breaks_table[[var]],
                      source = "Open data from the City of Montreal, DMTI data, and OpenStreetMap data",
                      interpolated = data_interpolated$interpolated_ref,
                      group_name = paste0("Accessibility to ", category),
                      group_diff = group_diff)
      
      out[out$var_code == var, ]
      
    })
  
  new_rows <- data.table::rbindlist(new_rows)
  variables <- rbind(scales_variables_modules$variables, new_rows)


  # Modules table -----------------------------------------------------------

  # Facultative. If a page is to be added accompanying this data, add modules
  # description. Read the documentation of `add_module`. If no module is to be
  # created, assign `scales_variables_modules$modules` to modules.
  modules <- scales_variables_modules$modules

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "short_distance_city",
      theme = "Transport",
      nav_title = "Short-distance city",
      title_text_title = "Short-distance city",
      title_text_main = paste0(
        "This module presents accessibility indicators for walking and cycling",
        ", calculated on the City of Montreal's territory. This project is led",
        " by Polytechnique Montréal, McGill University and the Institut nation",
        "al de la recherche scientifique (INRS) and funded by the City of Mont",
        "real and Mitacs. This module is a preliminary version and will be the",
        " subject of future developments."),
      title_text_extra = paste0(
        "In an urban planning context, accessibility refers to the ease with w",
        "hich individuals can reach opportunities dispersed throughout the ter",
        "ritory. High walking and cycling accessibility is associated with hig",
        "h quality of life and social equity. A good access to destinations by",
        " walk and by bike also promotes active travel and thus, leads to gree",
        "nhouse gases (GHG) emissions reductions, savings in transportation co",
        "sts and improved population health. Therefore, accessibility indicato",
        "rs contribute to the integration of transportation and land-use in pl",
        "anning for sustainable mobility.",
        "<br><br><p>Further resources: <ul><li><a href='https://github.com/Vil",
        "ledeMontreal/MontrealEnCommun'>Montréal En Commun</a>.</ul>",
        "<p><i>Module lead authors: Geneviève Boisjoly, Kevin Manaugh, Owen Wa",
        "ygood, Philippe Apparicio, José Arturo Jasso Chávez, Julien Verdier, ",
        "Karl El-Murr</i>"),
      regions = data_interpolated$regions,
      metadata = TRUE,
      dataset_info = paste0(
        "The indicators of this module represent the number of ",
        "destinations accessible by walking and cycling from a ",
        "specified origin within a given time (cumulative-opportunities ",
        "method).",
        "<p>Each indicator is calculed at the dissemination blocks level, the ",
        "smallest geographic area defined by Statistics Canada.",
        "<p>The defined cutoff times (15-minute walk ; 20-minute bike ride) do",
        " not vary throughout the study area, i.e., the City of Montreal. ",
        "<p>The characteristics and quality of the destinations are not taken ",
        "into account.",
        "<p>Travel times are calculated using the r5r ('Rapid Realistic Routin",
        "g with R5') package in R.",
        "<p>Calculations take into consideration walking and cycling infrastru",
        "ctures as well as streets' slopes.",
        "<p>The costs of turning and crossing an intersection are not consider",
        "ed.",
        "<p>The data used to calculate the indicators includes open data from ",
        "the City of Montreal, DMTI data, and OpenStreetMap data."
      )
    )


  # Add the disclaimer ------------------------------------------------------
  
  city_amenities_disclaimer <- 
    readr::read_csv("dev/data/short_distance_city/disclaimer.csv") |> 
    dplyr::mutate(var_code_walk = paste0("city_amenities_", tolower(var_code),
                                  "_walk"),
           var_code_bike = paste0("city_amenities_", tolower(var_code),
                                  "_bike")) |> 
    dplyr::rowwise() |> 
    dplyr::transmute(var_code = list(list(var_code_walk, var_code_bike)),
              disclaimer) |> 
    dplyr::filter(!is.na(disclaimer)) |> 
    dplyr::ungroup()
  
  qs::qsave(city_amenities_disclaimer, "data/city_amenities_disclaimer.qs")  
  
  # Return ------------------------------------------------------------------
  
  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}
