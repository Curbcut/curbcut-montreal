#### CanALE data setup #########################################################

# This script relies on objects created in dev/census.R

source("dev/modules/census/03_process_and_breaks.R")


# Get data ----------------------------------------------------------------

canale <- 
  read_sf("dev/data/Mtl_DA_CANALE/Mtl_DA_CANALE.shp") |> 
  st_transform(4326) |> 
  st_cast("MULTIPOLYGON") |> 
  st_set_agr("constant") |> 
  st_drop_geometry() |> 
  select(DAUID, CTUID, canale_ind_2016 = ale_index)


# Data testing ------------------------------------------------------------

data_testing(data = list("canale" = canale))


# Interpolate -------------------------------------------------------------

DA_canale <- 
  DA |> 
  left_join(canale, by = c("ID" = "DAUID", "CTUID")) |>
  st_transform(32618) |> 
  mutate(area = st_area(geometry)) |>
  st_set_agr("constant") |> 
  select(ID, CTUID, CSDUID, area, households, canale_ind_2016)

CT_canale <- 
  DA_canale |> 
  st_drop_geometry() |> 
  group_by(CTUID) |> 
  summarize(canale_ind_2016 = weighted.mean(canale_ind_2016, 
                                            households, na.rm = TRUE)) |> 
  right_join(CT, by = c("CTUID" = "ID")) |> 
  select(ID = CTUID, canale_ind_2016)

borough_canale <- 
  DA_canale |> 
  st_drop_geometry() |> 
  group_by(CSDUID) |>
  summarize(canale_ind_2016 = weighted.mean(canale_ind_2016, 
                                            households, na.rm = TRUE)) |> 
  right_join(borough, by = c("CSDUID" = "ID")) |> 
  select(ID = CSDUID, canale_ind_2016)

grid_canale <-
  grid |> 
  select(ID, households) |> 
  st_transform(32618) |> 
  st_set_agr("constant") |> 
  st_intersection(DA_canale) |> 
  mutate(area_prop = st_area(geometry) / area) |> 
  mutate(canale_ind_2016 = canale_ind_2016 * units::drop_units(area_prop)) |> 
  st_drop_geometry() |> 
  group_by(ID) |> 
  summarize(canale_ind_2016 = weighted.mean(canale_ind_2016, 
                                            households, na.rm = TRUE))

DA_canale <- 
  DA_canale |> 
  st_drop_geometry() |> 
  select(ID, canale_ind_2016)


# Calculate breaks --------------------------------------------------------

DA_canale <- add_q3(DA_canale)
CT_canale <- add_q3(CT_canale)
borough_canale <- add_q3(borough_canale)
grid_canale <- add_q3(grid_canale)

canale_q3 <- map(list(DA_canale, CT_canale, borough_canale, grid_canale), 
                 get_breaks_q3, "canale_ind_2016")
canale_q5 <- map(list(DA_canale, CT_canale, borough_canale, grid_canale), 
                 get_breaks_q5, "canale_ind_2016")

DA_canale <-
  DA_canale |> 
  add_q5(canale_q5[[1]]) |> 
  bind_cols(DA_canale) |> 
  relocate(canale_ind_2016_q5, .after = last_col())

CT_canale <-
  CT_canale |> 
  add_q5(canale_q5[[1]]) |> 
  bind_cols(CT_canale) |> 
  relocate(canale_ind_2016_q5, .after = last_col())

borough_canale <-
  borough_canale |> 
  add_q5(canale_q5[[1]]) |> 
  bind_cols(borough_canale) |> 
  relocate(canale_ind_2016_q5, .after = last_col())

grid_canale <-
  grid_canale |> 
  add_q5(canale_q5[[1]]) |> 
  bind_cols(grid_canale) |> 
  relocate(canale_ind_2016_q5, .after = last_col())


# Add to existing geographies ---------------------------------------------

DA <- 
  DA |> 
  left_join(DA_canale, by = "ID") |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

CT <- 
  CT |> 
  left_join(CT_canale, by = "ID") |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

borough <- 
  borough |> 
  left_join(borough_canale, by = "ID") |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

grid <- 
  grid |> 
  left_join(grid_canale, by = "ID") |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

building <- 
  building |> 
  left_join(DA_canale, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

street <- 
  street |> 
  left_join(DA_canale, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")


# Meta testing ------------------------------------------------------------

meta_testing()


# Add variable explanations -----------------------------------------------

# Get breaks_q3
breaks_q3_active <-
  map2_dfr(canale_q3, c("DA", "CT", "borough", "grid"), function(x, scale) {
   if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                .before = canale_ind_2016)})

# Get breaks_q5
breaks_q5_active <- 
  map2_dfr(canale_q5, c("DA", "CT", "borough", "grid"), function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:5,
                                 .before = canale_ind_2016)})

variables <- 
  variables |>
  add_variables(
    var_code = "canale_ind",
    var_title = "CanALE index",
    var_short = "CanALE",
    explanation = "the potential for active living",
    category = NA,
    private = FALSE,
    dates = c("2016"),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active,
    breaks_q5 = breaks_q5_active,
    source = "mcgill_geosdh_research_group")

# To save output, run dev/build_data.R, which calls this script
