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


# Add climate risk data to census tables ----------------------------------

all_climate_risk <- 
  interpolate_scales(data = climate_risk, 
                     base_scale = "grid", 
                     all_tables = all_tables, 
                     add_to_grid = TRUE)


# Add breaks --------------------------------------------------------------

all_climate_risk <- 
  calculate_breaks(tables_list = all_climate_risk)

# Adjust breaks for grid
all_climate_risk$tables_list$grid <- 
  climate_risk |>
  st_drop_geometry() |> 
  mutate(across(climate_flood_ind:climate_heat_wave_ind,
                ~pmax(pmin(.x, 3), 1), .names = "{.col}_q3"),
         across(climate_flood_ind:climate_heat_wave_ind,
                ~pmax(.x, 1), .names = "{.col}_q5"))


# Calculate qualitative q5 ------------------------------------------------

climate_risk_q5 <-
  imap(all_climate_risk$tables_q5, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, rank = seq_len(nrow(x)) - 1, 
                                 .before = 1)})

# Make table with qualitative values to join to grid
climate_join <- 
  tibble(var = 0:5, rank = 0:5,
         var_name = c("None", "Insignificant", "Minor", "Moderate", "Elevated", 
                      "Major"),
         var_name_short = c("None", "Insig.", "Minor", "Mod.", "Elev.", 
                            "Major"))

# Join each variable in grid to climate_join
grid_q5 <- 
  climate_risk |> 
  st_drop_geometry() |> 
  select(climate_flood_ind:climate_heat_wave_ind) |> 
  map(unique) |> 
  map(sort) |> 
  map(tibble) |> 
  map(set_names, "var") |> 
  map(mutate, scale = "grid", .before = var) |> 
  map(left_join, climate_join, by = "var") |> 
  map(relocate, rank, .after = scale)

# Consolidate q5 breaks table
climate_risk_q5 <- 
  imap(grid_q5, \(x, y) {
    imap_dfr(climate_risk_q5[names(climate_risk_q5) != "grid"], ~mutate(.x, scale = .y)) |> 
      select(scale, rank, all_of(y)) |> 
      set_names(c("scale", "rank", "var")) |> 
      bind_rows(x)})



# Assign all --------------------------------------------------------------

assign_tables(module_tables = all_climate_risk)


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add variable explanations -----------------------------------------------

# Breaks q3
breaks_q3_active <-
  imap_dfr(all_climate_risk$tables_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                 .before = 1)})

interpolation_keys <- 
  map(set_names(names(all_climate_risk$tables_list)), ~{
    if (.x == "grid") FALSE else "250-m grid cells"
  })

variables <- 
  variables |>
  add_variables(
    var_code = "climate_drought_ind",
    var_title = "Drought vulnerability",
    var_short = "Drought",
    explanation = "the vulnerability to climate-change related drought",
    category = NA,
    theme = "Climate risk",
    private = FALSE,
    dates = NA,
    scales = names(all_climate_risk$tables_list),
    breaks_q3 = select(breaks_q3_active, scale:rank, var = climate_drought_ind),
    breaks_q5 = climate_risk_q5$climate_drought_ind,
    source = "City of Montreal's open data website",
    interpolated = interpolation_keys) |> 
  add_variables(
    var_code = "climate_flood_ind",
    var_title = "Flood vulnerability",
    var_short = "Flood",
    explanation = "the vulnerability to climate-change related flooding",
    category = NA,
    theme = "Climate risk",
    private = FALSE,
    dates = NA,
    scales = names(all_climate_risk$tables_list),
    breaks_q3 = select(breaks_q3_active, scale:rank, var = climate_flood_ind),
    breaks_q5 = climate_risk_q5$climate_flood_ind,
    source = "City of Montreal's open data website",
    interpolated = interpolation_keys) |> 
  add_variables(
    var_code = "climate_heavy_rain_ind",
    var_title = "Heavy rain vulnerability",
    var_short = "Heavy rain",
    explanation = "the vulnerability to climate-change related heavy rain",
    category = NA,
    theme = "Climate risk",
    private = FALSE,
    dates = NA,
    scales = names(all_climate_risk$tables_list),
    breaks_q3 = select(breaks_q3_active, scale:rank, 
                       var = climate_heavy_rain_ind),
    breaks_q5 = climate_risk_q5$climate_heavy_rain_ind,
    source = "City of Montreal's open data website",
    interpolated = interpolation_keys) |> 
  add_variables(
    var_code = "climate_destructive_storms_ind",
    var_title = "Destructive storm vulnerability",
    var_short = "Destr. storm",
    explanation = paste0("the vulnerability to climate-change related ",
                         "destructive storms"),
    category = NA,
    theme = "Climate risk",
    private = FALSE,
    dates = NA,
    scales = names(all_climate_risk$tables_list),
    breaks_q3 = select(breaks_q3_active, scale:rank, 
                       var = climate_destructive_storms_ind),
    breaks_q5 = climate_risk_q5$climate_destructive_storms_ind,
    source = "City of Montreal's open data website",
    interpolated = interpolation_keys) |> 
  add_variables(
    var_code = "climate_heat_wave_ind",
    var_title = "Heat wave vulnerability",
    var_short = "Heat wave",
    explanation = "the vulnerability to climate-change related heat waves",
    category = NA,
    theme = "Climate risk",
    private = FALSE,
    dates = NA,
    scales = names(all_climate_risk$tables_list),
    breaks_q3 = select(breaks_q3_active, scale:rank, 
                       var = climate_heat_wave_ind),
    breaks_q5 = climate_risk_q5$climate_heat_wave_ind,
    source = "City of Montreal's open data website",
    interpolated = interpolation_keys)


# Clean up ----------------------------------------------------------------

rm(climate_risk, all_climate_risk, interpolation_keys,
   breaks_q3_active, climate_join, climate_risk_q5, grid_q5)
