#### Climate risk data setup ###################################################

# This script relies on objects created in dev/build_geometries.R


# Add climate risk data to grid -------------------------------------------

climate_risk <- 
  map2(
    list.files("dev/data/climate_shp", "*.shp$", full.names = TRUE), 
    c("flood_ind", "heavy_rain_ind", "drought_ind", "destructive_storms_ind", 
      "heat_wave_ind"),
    ~{
      .x %>% 
        read_sf() %>% 
        st_zm() %>% 
        st_make_valid() %>% 
        st_transform(4326) %>% 
        transmute(
          ID = seq_along(geometry),
          vulnerability = case_when(
            VULN_CAT == "Non-significative" ~ 0, VULN_CAT == "Mineure" ~ 1,
            VULN_CAT == "Modérée" ~ 2, VULN_CAT == "Élevée" ~ 3,
            VULN_CAT == "Majeure" ~ 4)) %>%
        set_names(c("ID", .y, "geometry"))
    })

climate_risk <- 
  map(climate_risk, ~{
    df <- 
      .x %>% 
      st_transform(32618) %>% 
      st_set_agr("constant") %>% 
      rename(grid_ID = ID) %>% 
      filter(units::drop_units(st_area(geometry)) > 10) %>% 
      distinct(geometry, .keep_all = TRUE) %>% 
      st_set_agr("constant")
    
    grid %>% 
      select(ID) %>% 
      st_transform(32618) %>% 
      st_set_agr("constant") %>% 
      st_intersection(df) %>% 
      mutate(area_int = units::drop_units(st_area(geometry))) %>% 
      st_drop_geometry() %>% 
      group_by(grid_ID) %>% 
      filter(area_int == max(area_int)) %>% 
      ungroup() %>% 
      select(-grid_ID, -area_int)
    })

grid <- 
  climate_risk %>% 
  reduce(left_join, by = "ID", .init = grid) %>% 
  relocate(geometry, .after = last_col()) %>% 
  mutate(across(flood_ind:heat_wave_ind, ~if_else(.x > 2, 3, .x + 1), 
                .names = "{.col}_q3")) %>% 
  relocate(geometry, .after = last_col()) %>% 
  st_set_agr("constant")


# Add climate risk data to census tables ----------------------------------

climate_census_fun <- function(x) {
  grid %>% 
    select(grid_ID = ID) %>% 
    st_transform(32618) %>% 
    st_intersection(st_transform(x, 32618)) %>% 
    mutate(area_int = units::drop_units(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(grid_ID) %>% 
    filter(area_int == max(area_int)) %>% 
    ungroup() %>% 
    select(grid_ID, ID) %>% 
    inner_join(select(grid, grid_ID = ID, flood_ind:heat_wave_ind), ., 
               by = "grid_ID") %>% 
    mutate(area_int = units::drop_units(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>% 
    summarize(across(flood_ind:heat_wave_ind, ~{
      if (sum(!is.na(.x)) > 0) {
        weighted.mean(.x, area_int, na.rm = TRUE) 
        } else NA_real_}), 
      .groups = "drop") %>% 
    left_join(x, ., by = "ID") %>% 
    mutate(across(c(flood_ind:heat_wave_ind), ntile, 3, 
                  .names = "{.col}_q3")) %>% 
    relocate(geometry, .after = last_col()) %>% 
    st_set_agr("constant")
}

borough <- climate_census_fun(borough)
CT <- climate_census_fun(CT)
DA <- climate_census_fun(DA)

rm(climate_risk, climate_census_fun)


# Add climate data risk to building and street ----------------------------

building <- 
  building |> 
  left_join(select(st_drop_geometry(grid), grid_ID = ID, 
                   flood_ind:heat_wave_ind_q3), by = "grid_ID") |> 
  relocate(geometry, .after = last_col())

street <- 
  street |> 
  left_join(select(st_drop_geometry(grid), grid_ID = ID, 
                   flood_ind:heat_wave_ind_q3), by = "grid_ID") |> 
  relocate(geometry, .after = last_col())


# Add variable explanations -----------------------------------------------

variables <- 
variables |>
  add_variables(
    var_code = "drought_ind",
    var_title = "Drought vulnerability",
    var_short = "Drought",
    explanation = "the vulnerability to climate-change related drought events",
    category = NA,
    private = FALSE,
    dates = "2016",
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK") |> 
  add_variables(
    var_code = "flood_ind",
    var_title = "Flood vulnerability",
    var_short = "Flood",
    explanation = "the vulnerability to climate-change related flooding events",
    category = NA,
    private = FALSE,
    dates = "2016",
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK") |> 
  add_variables(
    var_code = "heavy_rain_ind",
    var_title = "Heavy rain vulnerability",
    var_short = "Heavy rain",
    explanation = "the vulnerability to climate-change related heavy rain events",
    category = NA,
    private = FALSE,
    dates = "2016",
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK") |> 
  add_variables(
    var_code = "destructive_storms_ind",
    var_title = "Destructive storm vulnerability",
    var_short = "Destr. storm",
    explanation = "the vulnerability to climate-change related destructive storm events",
    category = NA,
    private = FALSE,
    dates = "2016",
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK") |> 
  add_variables(
    var_code = "heat_wave_ind",
    var_title = "Heat wave vulnerability",
    var_short = "Heat wave",
    explanation = "the vulnerability to climate-change related heat wave events",
    category = NA,
    private = FALSE,
    dates = "2016",
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK")

# To save output, run dev/build_data.R, which calls this script
