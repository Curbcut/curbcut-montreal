#### Natural infrastructure data setup #########################################

# This script relies on objects created in dev/census.R

# Natural infrastructure abbreviated to NI

# Load libraries ----------------------------------------------------------

# future::plan(future::multisession, workers = 12)

suppressPackageStartupMessages({
  library(raster)
  # library(anglr)
  library(stars)
  library(furrr)
  library(tidyverse)
})


# Convert raster data to grid polygon -------------------------------------

datas <- c("habitat_quality" = "Fig11a.asc", 
           "habitat_connectivity" = "Fig11b.asc", 
           "favorable_climatic_conditions" = "Fig11c.asc",
           "ni_contribution_flood_prevention" = "Fig13.tif", 
           "ni_contribution_biodiversity_conservation" = "Fig14.asc",
           "ni_contribution_heat_island_reduction" = "Fig15.tif", 
           "conservation_prioritization" = "Fig16.tif")

# # Data used to identify priority areas for conservation
# habitat_quality <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig11a.asc")
# 
# habitat_connectivity <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig11b.asc")
# # The following is: Climatic conditions favorable to the 14 vertebrate species
# # selected for analysis in "Maure, F., et al. (2018). Le rôle des 
# # infrastructures naturelles dans la prévention des inondations dans la CMM."
# favorable_climatic_conditions <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig11c.asc")
# 
# # The following is: pixels are ranked according to their importance in the 
# # natural infrastructure network in terms of runoff reduction
# ni_contribution_flood_prevention <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig13.tif")
# 
# # The following is: pixels are ranked according to their importance in the 
# # natural infrastructure network in terms of biodiversity conservation
# ni_contribution_biodiversity_conservation <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig14.asc")
# 
# # The following is: pixels are ranked according to their importance in the 
# # natural infrastructure network in terms of urban head island reduction
# ni_contribution_heat_island_reduction <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig15.tif")
# 
# # The following is: pixels are ranked according to their importance 
# # (conservation prioritization) in the natural infrastructure network in 
# # terms of the previous variables:
# # - Runoff reduction
# # - Biodiversity conservation
# # - Urban heat island reduction.
# conservation_prioritization <- 
#   read_stars("dev/data/2018_FDS_InfraNat_Conn/Fig16.tif")

grid <- 
  borough  |> 
  st_transform(32618) |> 
  st_make_grid(c(250, 250)) |> 
  st_intersection(st_transform(borough, 32618)) |> 
  as_tibble() |> 
  st_as_sf(crs = st_crs(32618))

grid <- 
  grid |> 
  mutate(ID = row_number(),
         area = units::drop_units(st_area(geometry)),
         .before = "geometry")

natural_infrastructure <- 
  future_map2(names(datas), datas, function(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid()
    
    data <- 
      st_transform(data, 4326) |> 
      st_transform(32618)
    
    intersected <- 
      grid |> st_intersection(data)
    
    interpolated <- 
      intersected |> 
      mutate(percent_area = units::drop_units(st_area(geometry))/area) |> 
      st_drop_geometry() |> 
      rename(var = 3) |> 
      group_by(ID) |> 
      filter(!is.na(var),
             sum(percent_area) > 0.5) |> 
      summarize(conservation_prioritization = mean(var))
    
    names(interpolated) <- c("ID", name)
    
    interpolated
  }) |> reduce(left_join, by = "ID")

natural_infrastructure <- 
  left_join(grid, natural_infrastructure) |> 
  relocate(geometry, .after = last_col()) |> 
  select(-area)


# Get breaks --------------------------------------------------------------

ni_results <- add_q3(st_drop_geometry(natural_infrastructure))
ni_q3 <- get_breaks_q3(ni_results)
ni_q5 <- get_breaks_q5(ni_results)
ni_results <- bind_cols(ni_results, add_q5(ni_results, ni_q5))

natural_infrastructure <- 
  left_join(ni_results, select(natural_infrastructure, ID),
            by = "ID") |> 
  st_as_sf()

# Adding usual grid columns -----------------------------------------------

# qs::qload("data/census.qsm")

DA_data <- 
  DA %>% 
  st_transform(32618) %>% 
  select(ID, population, households) %>% 
  mutate(area = st_area(geometry), .before = geometry) %>% 
  st_set_agr("constant")

natural_infrastructure_census <-
  natural_infrastructure |> 
  select(ID) |> 
  st_transform(32618) |> 
  st_set_agr("constant") |> 
  st_intersection(DA_data) |> 
  mutate(area_prop = st_area(geometry) / area) |> 
  mutate(across(population:households, 
                ~{.x * units::drop_units(area_prop)})) |> 
  select(ID, population, households, geometry) |> 
  st_drop_geometry() |> 
  arrange(ID) |> 
  group_by(ID) |> 
  summarize(across(population:households, sum, na.rm = TRUE))

natural_infrastructure <- 
  natural_infrastructure |> 
  left_join(natural_infrastructure_census, by = "ID") |> 
  mutate(name = ID, .after = ID) |> 
  relocate(geometry, .after = last_col()) |> 
  relocate(population, .after = name) |> 
  relocate(households, .after = name) |> 
  st_set_agr("constant")

borough_index <- 
  natural_infrastructure |> 
  st_transform(32618) |> 
  st_centroid() |> 
  st_nearest_feature(st_transform(borough, 32618))

natural_infrastructure <- 
  natural_infrastructure |> 
  mutate(name = ID, .after = ID) |> 
  mutate(CSDUID = map_chr(borough_index, ~borough$ID[.x]), .after = name) |> 
  st_set_agr("constant")

natural_infrastructure <- 
  natural_infrastructure |> 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name), 
            by = "CSDUID") |> 
  relocate(name_2, .after = name) |> 
  st_set_agr("constant")


# Final SF operations -----------------------------------------------------

natural_infrastructure <- 
  natural_infrastructure |> 
  st_transform(4326) |>
  filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |> 
  st_cast("MULTIPOLYGON")

# Cleanup -----------------------------------------------------------------

# future::plan(future::multisession, workers = 1)

rm(datas, grid, borough_index, DA_data, natural_infrastructure_census)
