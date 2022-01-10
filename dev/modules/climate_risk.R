#### Climate risk data setup ###################################################

# Add climate risk data to grid -------------------------------------------

climate_risk <- 
  map2(list.files("dev/data/climate_shp", "*.shp$", full.names = TRUE), 
       c("climate_flood_ind", "climate_heavy_rain_ind", "climate_drought_ind", 
         "climate_destructive_storms_ind", "climate_heat_wave_ind"), ~{
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

climate_risk <- 
  map(climate_risk, ~{
    df <- 
      .x |> 
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
      full_join(st_drop_geometry(select(grid, ID)), by = "ID") |> 
      mutate(area_int = units::drop_units(st_area(geometry))) |> 
      st_drop_geometry() |> 
      group_by(grid_ID) |> 
      filter(area_int == max(area_int)) |> 
      ungroup() |>
      select(-grid_ID, -area_int) |> 
      mutate(across(starts_with("climate"), ~replace(., is.na(.), 0)))
    })

grid <-
  climate_risk |>  
  reduce(left_join, by = "ID", .init = grid) |> 
  relocate(geometry, .after = last_col()) |>
  mutate(across(climate_flood_ind:climate_heat_wave_ind, 
                ~pmin(.x, 3), .names = "{.col}_q3"),
         across(climate_flood_ind:climate_heat_wave_ind,
                ~.x, .names = "{.col}_q5")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")


# Add climate risk data to census tables ----------------------------------

climate_census_fun <- function(x) {
  
  x_int <- 
    x |> 
    st_transform(32618) |> 
    mutate(area_x = units::drop_units(st_area(geometry))) |> 
    st_set_agr("constant")
  
  grid |> 
    select(grid_ID = ID) |> 
    st_transform(32618) |> 
    st_set_agr("constant") |> 
    st_intersection(x_int) |> 
    mutate(area_int = units::drop_units(st_area(geometry))) |> 
    st_drop_geometry() |> 
    group_by(grid_ID) |> 
    filter(area_int == max(area_int)) |> 
    ungroup() |> 
    select(grid_ID, ID, area_x) |>
    inner_join(select(grid, grid_ID = ID,
                      climate_flood_ind:climate_heat_wave_ind),
               by = "grid_ID") |>
    mutate(area_int = units::drop_units(st_area(geometry))) |>
    group_by(ID) |>
    summarize(
      across(climate_flood_ind:climate_heat_wave_ind, ~{
      if (sum(area_int[!is.na(.x)]) >= 0.5 * max(area_x)) {
        weighted.mean(.x, area_int, na.rm = TRUE)
        } else NA_real_}), .groups = "drop") |>
    right_join(x, by = "ID") |>
    relocate(climate_flood_ind:climate_heat_wave_ind, .before = geometry) |>
    mutate(across(c(climate_flood_ind:climate_heat_wave_ind), ntile, 3,
                  .names = "{.col}_q3")) |>
    relocate(any_of(c("buffer", "centroid", "building", "geometry")), 
             .after = last_col()) |>
    st_as_sf(sf_column_name = "geometry") |>
    st_set_agr("constant")
}

borough <- climate_census_fun(borough)
CT <- climate_census_fun(CT)
DA <- climate_census_fun(DA)

rm(climate_risk, climate_census_fun)


# Add climate data risk to building and street ----------------------------

street <- 
  street |> 
  left_join(select(st_drop_geometry(grid), grid_ID = ID, 
                   climate_flood_ind:climate_heat_wave_ind_q3), 
            by = "grid_ID") |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")


# Add breaks --------------------------------------------------------------

climate_risk_q3 <- 
  map(list(DA, CT, borough, grid), ~{
    .x |> 
      st_drop_geometry() |>
      get_breaks_q3(c("climate_flood_ind", "climate_heavy_rain_ind", 
                      "climate_drought_ind", "climate_destructive_storms_ind",
                      "climate_heat_wave_ind"))
    })

climate_risk_q5 <- 
  map(list(DA, CT, borough), ~{
    .x |> 
      st_drop_geometry() |>
      get_breaks_q5(c("climate_flood_ind", "climate_heavy_rain_ind", 
                      "climate_drought_ind", "climate_destructive_storms_ind",
                      "climate_heat_wave_ind"))
  })

grid_q5 <- 
  grid |> 
  st_drop_geometry() |> 
  select(climate_flood_ind:climate_heat_wave_ind) |> 
  slice(1:6) |> 
  mutate(across(everything(), ~c("No risk" = 0, "Insignificant" = 1, 
                                 "Minor" = 2, "Moderate" = 3, "High" = 4,
                                 "Major" = 5))) |> 
  list()

climate_risk_q5 <- c(climate_risk_q5, grid_q5)

DA <-
  DA |> 
  st_drop_geometry() |> 
  add_q5(climate_risk_q5[[1]]) |> 
  bind_cols(DA) |> 
  relocate(climate_flood_ind_q5:climate_heat_wave_ind_q5, .before = buffer) |> 
  st_as_sf(sf_column_name = "geometry") |> 
  st_set_agr("constant")

CT <-
  CT |> 
  st_drop_geometry() |> 
  add_q5(climate_risk_q5[[1]]) |> 
  bind_cols(CT) |> 
  relocate(climate_flood_ind_q5:climate_heat_wave_ind_q5, .before = geometry) |> 
  st_as_sf(sf_column_name = "geometry") |> 
  st_set_agr("constant")

borough <-
  borough |> 
  st_drop_geometry() |> 
  add_q5(climate_risk_q5[[1]]) |> 
  bind_cols(borough) |> 
  relocate(climate_flood_ind_q5:climate_heat_wave_ind_q5, .before = geometry) |> 
  st_as_sf(sf_column_name = "geometry") |> 
  st_set_agr("constant")


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add variable explanations -----------------------------------------------

# Get breaks_q3
breaks_q3_active <-
  map2_dfr(climate_risk_q3, c("DA", "CT", "borough", "grid"), \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <- 
  map2_dfr(climate_risk_q5, c("DA", "CT", "borough", "grid"), \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, 
                                 rank = seq_len(nrow(x)) - 1, .before = 1)})

variables <- 
  variables |>
  add_variables(
    var_code = "climate_drought_ind",
    var_title = "Drought vulnerability",
    var_short = "Drought",
    explanation = "the vulnerability to climate-change related drought",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = select(breaks_q3_active, scale:rank, var = climate_drought_ind),
    breaks_q5 = select(breaks_q5_active, scale:rank, var = climate_drought_ind),
    source = "VdM") |> 
  add_variables(
    var_code = "climate_flood_ind",
    var_title = "Flood vulnerability",
    var_short = "Flood",
    explanation = "the vulnerability to climate-change related flooding",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = select(breaks_q3_active, scale:rank, var = climate_flood_ind),
    breaks_q5 = select(breaks_q5_active, scale:rank, var = climate_flood_ind),
    source = "VdM") |> 
  add_variables(
    var_code = "climate_heavy_rain_ind",
    var_title = "Heavy rain vulnerability",
    var_short = "Heavy rain",
    explanation = "the vulnerability to climate-change related heavy rain",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = select(breaks_q3_active, scale:rank, 
                       var = climate_heavy_rain_ind),
    breaks_q5 = select(breaks_q5_active, scale:rank, 
                       var = climate_heavy_rain_ind),
    source = "VdM") |> 
  add_variables(
    var_code = "climate_destructive_storms_ind",
    var_title = "Destructive storm vulnerability",
    var_short = "Destr. storm",
    explanation = paste0("the vulnerability to climate-change related ",
                         "destructive storms"),
    category = NA,
    private = FALSE,
    dates = NA,
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = select(breaks_q3_active, scale:rank, 
                       var = climate_destructive_storms_ind),
    breaks_q5 = select(breaks_q5_active, scale:rank, 
                       var = climate_destructive_storms_ind),
    source = "VdM") |> 
  add_variables(
    var_code = "climate_heat_wave_ind",
    var_title = "Heat wave vulnerability",
    var_short = "Heat wave",
    explanation = "the vulnerability to climate-change related heat waves",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = select(breaks_q3_active, scale:rank, 
                       var = climate_heat_wave_ind),
    breaks_q5 = select(breaks_q5_active, scale:rank, 
                       var = climate_heat_wave_ind),
    source = "VdM")


# Clean up ----------------------------------------------------------------

rm(breaks_q3_active, breaks_q5_active, climate_risk_q3, climate_risk_q5,
   grid_q5)
