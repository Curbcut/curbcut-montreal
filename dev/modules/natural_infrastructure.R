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
  library(qs)
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

natural_infrastructure_tiles <- 
  future_map2(set_names(names(datas)), datas, function(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid()
    
    data <-
      data |>
      rename(var = 1) |>
      mutate(rank = ntile(var, 100)) |> 
      mutate(var = round(var, digits = 2)) |> 
      group_by(var, rank) |> 
      summarize()
    
    data <- 
      st_transform(data, 4326) |>
      filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
      st_cast("MULTIPOLYGON")
    
    names(data)[1] <- name
    names(data)[2] <- paste0(name, "_q100")
    
    data
    
  })

qsave(natural_infrastructure_tiles, "dev/data/natural_infrastructure_tiles.qs")


# Join all pixels into one df ---------------------------------------------

natural_infrastructure <- 
  future_map2(set_names(names(datas)), datas, function(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, #merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid()
    
    data <- 
      st_transform(data, 4326)
    
    data <-
      data |>
      rename(var = 1) |>
      mutate(rank = ntile(var, 100))
    
    names(data)[1] <- name
    names(data)[3] <- paste0(name, "_q100")
    
    data
    
  })

# Move to centroids for the first dataframe
natural_infrastructure[[1]] <- 
  natural_infrastructure[[1]] |> 
  mutate(geometry = st_centroid(geometry))

natural_infrastructure <- reduce(natural_infrastructure, st_join) |> 
  st_drop_geometry()

# Get breaks --------------------------------------------------------------

# ni_results <- map(map(natural_infrastructure, st_drop_geometry), add_q3)
# ni_q3 <- map(ni_results, get_breaks_q3)
# ni_q5 <- map(ni_results, get_breaks_q5)
# ni_results <- map2(ni_results, ni_q5, ~{bind_cols(.x, add_q5(.x, .y))})

# ni_results <- add_q3(st_drop_geometry(natural_infrastructure))
# ni_q3 <- get_breaks_q3(ni_results)
# ni_q5 <- get_breaks_q5(ni_results)
# ni_results <- bind_cols(ni_results, add_q5(ni_results, ni_q5))

# natural_infrastructure <- 
#   map2(natural_infrastructure, ni_results, left_join)

# natural_infrastructure <-
#   left_join(ni_results, select(natural_infrastructure, ID),
#             by = "ID") |>
#   st_as_sf()


# Final SF operations -----------------------------------------------------

# natural_infrastructure <-
#   natural_infrastructure |>
#   st_transform(4326) |>
#   filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
#   st_cast("MULTIPOLYGON")

# Cleanup -----------------------------------------------------------------

rm(datas)
