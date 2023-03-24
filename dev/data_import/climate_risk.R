## BUILD AND APPEND CLIMATE_RISK DATA ##########################################

build_and_append_climate_risk <- function(scales_variables_modules, crs) {

  # Read and prepare data ---------------------------------------------------

  climate_risk <- 
    map2(list.files("dev/data/climate_risk", "*.shp$", full.names = TRUE), 
         c("climate_flood_2017", "climate_heavy_rain_2017", "climate_drought_2017", 
           "climate_destructive_storms_2017", "climate_heat_wave_2017"), ~{
             .x |> 
               read_sf() |>
               st_zm() |> 
               st_make_valid() |> 
               st_transform(4326) |> 
               transmute(
                 ID = seq_along(geometry),
                 vulnerability = case_when(
                   VULN_CAT == "Non-significative" ~ 1, VULN_CAT == "Mineure" ~ 2,
                   VULN_CAT == "Modérée" ~ 3, VULN_CAT == "Élevée" ~ 4,
                   VULN_CAT == "Majeure" ~ 5)) |>
               set_names(c("ID", .y, "geometry"))
           })
  
  grid <- scales_variables_modules$scales$grid$grid
  
  climate_risk <- 
    map(climate_risk, \(x) {
      df <- 
        x |> 
        st_transform(32618) |> 
        st_set_agr("constant") |> 
        rename(grid_ID = ID) |> 
        filter(units::drop_units(st_area(geometry)) > 10) |> 
        distinct(geometry, .keep_all = TRUE) |> 
        st_set_agr("constant")
      
      grid |>  
        select(ID) |> 
        st_transform(32618) |> 
        st_set_agr("constant") |> 
        st_intersection(df) |>
        mutate(area_int = units::drop_units(st_area(geometry))) |> 
        st_drop_geometry() |> 
        group_by(grid_ID) |> 
        filter(area_int == max(area_int)) |> 
        ungroup() |>
        select(-grid_ID, -area_int) |> 
        full_join(st_drop_geometry(select(grid, ID)), by = "ID") |> 
        mutate(across(starts_with("climate"), ~replace(., is.na(.), 0)))
    })
  
  climate_risk <-
    climate_risk |>  
    reduce(left_join, by = "ID", .init = select(grid, ID)) |> 
    relocate(geometry, .after = last_col()) |>
    st_set_agr("constant")
  
  # Get list of data variables ----------------------------------------------
  
  avg_vars <- names(climate_risk)[2:6]


  # Interpolate data to all possible scales ---------------------------------
  
  data_interpolated <-
    interpolate_custom_geo(data = climate_risk,
                           all_scales = scales_variables_modules$scales,
                           average_vars = avg_vars,
                           name_interpolate_from = "grid",
                           crs = crs)
  

  # Get all the types in a named list ---------------------------------------
  
  types <- list(climate_drought = "ind",
                climate_flood = "ind",
                climate_heavy_rain = "ind",
                climate_destructive_storms = "ind",
                climate_heat_wave = "ind")


  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = avg_vars,
      types = types,
      rank_name = c("Insignificant", "Minor", "Moderate", "Elevated", "Major"),
      rank_name_short = c("Insig.", "Minor", "Mod.", "Elev.", "Major")
    )

  ### ADJUST THE BASE DATA
  
  # Update grid q5 to be the same value as the indicator
  with_breaks$scales$grid$grid$climate_drought_q5_2017 <- 
    with_breaks$scales$grid$grid$climate_drought_2017
  with_breaks$scales$grid$grid$climate_flood_q5_2017 <- 
    with_breaks$scales$grid$grid$climate_flood_2017
  with_breaks$scales$grid$grid$climate_heavy_rain_q5_2017 <- 
    with_breaks$scales$grid$grid$climate_heavy_rain_2017
  with_breaks$scales$grid$grid$climate_destructive_storms_q5_2017 <- 
    with_breaks$scales$grid$grid$climate_destructive_storms_2017
  with_breaks$scales$grid$grid$climate_heat_wave_q5_2017 <- 
    with_breaks$scales$grid$grid$climate_heat_wave_2017
  
  with_breaks$q5_breaks_table$climate_drought$var[
    with_breaks$q5_breaks_table$climate_drought$df == "grid_grid"
  ] <- 0:5
  with_breaks$q5_breaks_table$climate_flood$var[
    with_breaks$q5_breaks_table$climate_flood$df == "grid_grid"
  ] <- 0:5
  with_breaks$q5_breaks_table$climate_heavy_rain$var[
    with_breaks$q5_breaks_table$climate_heavy_rain$df == "grid_grid"
  ] <- 0:5
  with_breaks$q5_breaks_table$climate_destructive_storms$var[
    with_breaks$q5_breaks_table$climate_destructive_storms$df == "grid_grid"
  ] <- 0:5
  with_breaks$q5_breaks_table$climate_heat_wave$var[
    with_breaks$q5_breaks_table$climate_heat_wave$df == "grid_grid"
  ] <- 0:5
  

  # Get the region values ---------------------------------------------------
  
  parent_strings <- list(climate_drought = "households",
                climate_flood = "households",
                climate_heavy_rain = "households",
                climate_destructive_storms = "households",
                climate_heat_wave = "households")
  
  region_vals <- 
    variables_get_region_vals(scales = data_interpolated$scales,
                              vars = gsub("_\\d{4}$", "", avg_vars),
                              types = types,
                              parent_strings = parent_strings,
                              breaks = with_breaks$q5_breaks_table,
                              time_regex = "_\\d{4}$")
  

  # Prepare the variable measurements ---------------------------------------
  
  var_measurement <- data.frame(
    df = data_interpolated$avail_df,
    measurement = rep("scalar", length(data_interpolated$avail_df)))
  var_measurement$measurement[var_measurement$df == "grid_grid"] <- "ordinal"
  

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
      dates = with_breaks$avail_dates[["climate_drought"]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[["climate_drought"]],
      breaks_q5 = with_breaks$q5_breaks_table[["climate_drought"]],
      region_values = region_vals$climate_drought,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
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
      dates = with_breaks$avail_dates[["climate_flood"]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[["climate_flood"]],
      breaks_q5 = with_breaks$q5_breaks_table[["climate_flood"]],
      region_values = region_vals$climate_flood,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
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
      dates = with_breaks$avail_dates[["climate_heavy_rain"]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[["climate_heavy_rain"]],
      breaks_q5 = with_breaks$q5_breaks_table[["climate_heavy_rain"]],
      region_values = region_vals$climate_heavy_rain,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
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
      dates = with_breaks$avail_dates[["climate_destructive_storms"]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[["climate_destructive_storms"]],
      breaks_q5 = with_breaks$q5_breaks_table[["climate_destructive_storms"]],
      region_values = region_vals$climate_destructive_storms,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
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
      dates = with_breaks$avail_dates[["climate_heat_wave"]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[["climate_heat_wave"]],
      breaks_q5 = with_breaks$q5_breaks_table[["climate_heat_wave"]],
      region_values = region_vals$climate_heat_wave,
      parent_vec = "households",
      pe_include = TRUE,
      source = "City of Montreal's open data website",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally unsusceptable", "unusually unsusceptable", 
                       "just about average vulnerability", "unusually vulnerable", 
                       "exceptionally vulnerable"),
      var_measurement = var_measurement
    )

  # Modules table -----------------------------------------------------------

  modules <-
      scales_variables_modules$modules |>
        add_module(
          id = "climate_risk",
          theme = "Climate",
          nav_title = "Climate risk",
          title_text_title = "Climate change risk",
          title_text_main = paste0(
            "Climate change will have increasingly negative ",
            "impacts on communities in Montreal, but these ",
            "impacts will vary significantly by both geography ",
            "and social factors. The distribution of five ",
            "different climate risks—heat waves, flooding, heavy ",
            "rain, drought, and destructive storms—is visualized here."),
          title_text_extra = paste0(
            "<p>The Climate Change Risk datasets come from the City of Montreal's ",
            "efforts to examine potential climate risks for the Montreal region in ",
            "the 2015-2020 Urban Agglomeration Climate Change Adaptation Plan. The ",
            "plan identifies the five variables of heat waves, flooding, heavy rain, ",
            "drought, and destructive storms as the primary climate risk factors ",
            "(alongside rising temperatures) for the Montreal agglomeration. The ",
            "Adaptation Plan includes projections for potential climate-change ",
            "impacts on buildings, municipal operations, the local environment, ",
            "and Montreal communities.<p>The datasets visualized here are publicly ",
            "available through the Montreal Open Data Portal.<ul><li>Heat waves ",
            "include a range of extreme heat events based on temperature and ",
            "duration. Montreal has generally seen an upward trend in extreme heat ",
            "events, most noticeably during the 2000s. Heat waves are especially ",
            "of concern in Montreal due to more than one quarter (28%) of the ",
            "island containing heat islands.<li>Flooding, specifically river ",
            "flooding, refers to flow rate or river level exceeding the critical ",
            "threshold. The Montreal agglomeration's flood risk is concentrated ",
            "along the Des Prairies River.<li>Heavy rain can cause rivers to ",
            "overflow, put strain on infrastructures, cause public health problems, ",
            "and negatively affect natural environments. Episodes of heavy rain ",
            "are on the upward trend in Quebec.<li>Drought includes meteorological ",
            "drought (amount of precipitation), agricultural drought (soil dryness), ",
            "hydrological drought (surface and groundwaters), and socioeconomic ",
            "drought (actions of humans on water resources). Montreal has seen a very ",
            "slight upward trend in meteorological droughts. <li>Destructive storms ",
            "include wind storms, hail storms, heavy snowstorms, and freezing rain. ",
            "Events of freezing rain increased 26% from 1979 to 2008, and heavy ",
            "snowstorms have also increased over the past 70 years.</ul>"),
          regions = data_interpolated$regions,
          metadata = TRUE,
          dataset_info = paste0(
            "<p><a href = 'https://donnees.montreal.ca/ville-de-montreal/vuln",
            "erabilite-changements-climatiques'>",
            "The data in this module are cartographic representations of the ",
            "vulnerability analysis</a> developed as part of the Climate change ",
            "adaptation plan for the agglomeration of Montréal 2015-2020 for ",
            "the following climate hazards: heavy rainfall, heat waves, ",
            "destructive storms, droughts and floods.</p>")
        )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}

