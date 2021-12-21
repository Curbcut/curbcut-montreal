#### CanALE data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

canale <- 
  read_sf("dev/data/Mtl_DA_CANALE/Mtl_DA_CANALE.shp") %>% 
  st_transform(4326) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_set_agr("constant") %>% 
  st_drop_geometry() %>% 
  select(DAUID, CTUID, canale_ind_2016 = ale_index)


# Add to existing geographies ---------------------------------------------

DA <- 
  DA %>% 
  left_join(canale, by = c("ID" = "DAUID", "CTUID")) %>% 
  relocate(canale_ind_2016, .before = geometry) %>% 
  mutate(canale_ind_q3_2016 = ntile(canale_ind_2016, 3), 
         .after = canale_ind_2016) %>% 
  st_set_agr("constant")

CT <- 
  DA %>% 
  st_drop_geometry() %>% 
  select(CTUID, households, canale_ind_2016) %>% 
  group_by(CTUID) %>% 
  summarize(canale_ind_2016 = weighted.mean(canale_ind_2016, 
                                       households, na.rm = TRUE)) %>% 
  left_join(CT, ., by = c("ID" = "CTUID")) %>% 
  relocate(canale_ind_2016, .before = geometry) %>% 
  mutate(canale_ind_q3_2016 = ntile(canale_ind_2016, 3), 
         .after = canale_ind_2016) %>% 
  st_set_agr("constant")

borough <- 
  DA %>% 
  st_drop_geometry() %>% 
  select(CSDUID, households, canale_ind_2016) %>% 
  group_by(CSDUID) %>% 
  summarize(canale_ind_2016 = weighted.mean(canale_ind_2016, 
                                       households, na.rm = TRUE)) %>% 
  left_join(borough, ., by = c("ID" = "CSDUID")) %>% 
  relocate(canale_ind_2016, .before = geometry) %>% 
  mutate(canale_ind_q3_2016 = ntile(canale_ind_2016, 3), 
         .after = canale_ind_2016) %>% 
  st_set_agr("constant")

DA_data <-
  DA %>% 
  select(ID, canale_ind_2016) %>% 
  st_transform(32618) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

grid_data <-
  grid %>% 
  select(ID, households) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_data) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(canale_ind_2016 = canale_ind_2016 * units::drop_units(area_prop)) %>% 
  select(-ID.1, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarize(canale_ind_2016 = weighted.mean(canale_ind_2016, 
                                       households, na.rm = TRUE))

grid <- 
  grid %>% 
  left_join(grid_data, by = "ID") %>% 
  relocate(geometry, .after = last_col())

building <- 
  building |> 
  left_join(canale, by = c("DAUID", "CTUID")) %>% 
  relocate(canale_ind_2016, .before = geometry) %>% 
  mutate(canale_ind_q3_2016 = ntile(canale_ind_2016, 3), 
         .after = canale_ind_2016) %>% 
  st_set_agr("constant")

street <- 
  street |> 
  left_join(canale, by = c("DAUID", "CTUID")) %>% 
  relocate(canale_ind_2016, .before = geometry) %>% 
  mutate(canale_ind_q3_2016 = ntile(canale_ind_2016, 3), 
         .after = canale_ind_2016) %>% 
  st_set_agr("constant")

rm(canale, DA_data, grid_data)


# Add variable explanations -----------------------------------------------

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
    breaks_q3 = "TKTK",
    breaks_q5 = "TKTK",
    source = "mcgill_geosdh_research_group")
warning(paste0("q3 and q5 not fully integrated for canale_ind in the ",
               "variables table. Warning coming from `dev/modules/canale.R`"))
  
# To save output, run dev/build_data.R, which calls this script
