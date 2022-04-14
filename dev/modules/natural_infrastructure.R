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
           "conservation_prioritization" = "Fig16.tif",
           "flood_risks" = "Flood_CMM/Flood_CMM.tif",
           "heat_islands" = "heatcoolislands/heatislands3.asc",
           "cool_islands" = "heatcoolislands/coolislands5.asc")

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

# plan(multisession, workers = length(datas))

natural_infrastructure_tiles <- 
  future_map2(set_names(names(datas)), datas, function(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid()
    
    if (name %in% c("heat_islands", "cool_islands")) {
      data <- 
        data |> 
        mutate(area = st_area(geometry)) |> 
        filter(area != max(area)) |> 
        dplyr::select(-area)
    }
    
    if (name == "flood_risks") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 200) |> 
        group_by(var, rank) |> 
        summarize()
    } else if (name == "heat_islands") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 200) |> 
        group_by(var, rank) |> 
        summarize()
    } else if (name == "cool_islands") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 101) |> 
        group_by(var, rank) |> 
        summarize()
    } else {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = ntile(var, 100) + 100) |> 
        mutate(var = round(var, digits = 2)) |> 
        group_by(var, rank) |> 
        summarize()
    }

    
    data <- 
      st_transform(data, 4326) |>
      filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
      st_cast("MULTIPOLYGON")
    
    names(data)[1] <- name
    names(data)[2] <- paste0(name, "_q100")
    
    data
    
  })

qsave(natural_infrastructure_tiles, "dev/data/natural_infrastructure_tiles.qs")



# To enable unique sets of priorities -------------------------------------

natural_infrastructure_tiles_unioned_nis <- 
  future_map2(set_names(names(datas))[4:6], datas[4:6], function(name, path) {
    
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
      mutate(rank = ntile(var, 20))
    
    names(data)[3] <- paste0(name, "_q20")
    
    data[, paste0(name, "_q20")]
    
  })

# Change list order
natural_infrastructure_tiles_unioned_nis <- 
natural_infrastructure_tiles_unioned_nis[c("ni_contribution_biodiversity_conservation",
                                           "ni_contribution_heat_island_reduction",
                                           "ni_contribution_flood_prevention")]

natural_infrastructure_tiles_unioned_nis[[1]] <- 
natural_infrastructure_tiles_unioned_nis[[1]] |> 
  mutate(ID = row_number())

natural_infrastructure_tiles_unioned_nis_2 <- 
  natural_infrastructure_tiles_unioned_nis

natural_infrastructure_tiles_unioned_nis_2[[1]] <- 
  natural_infrastructure_tiles_unioned_nis_2[[1]] |> 
  mutate(geometry = st_centroid(geometry))

natural_infrastructure_tiles_unioned_nis_2 <- 
  reduce(natural_infrastructure_tiles_unioned_nis_2, st_join)

natural_infrastructure_tiles_unioned_nis_2 <- 
  natural_infrastructure_tiles_unioned_nis_2 |> 
  st_drop_geometry() |> 
  left_join(select(natural_infrastructure_tiles_unioned_nis[[1]],
                   -everything(), ID), by = "ID") |> 
  select(-ID) |> 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) |> 
  st_as_sf()

natural_infrastructure_tiles_unioned_nis <- 
  natural_infrastructure_tiles_unioned_nis_2 |> 
  group_by(ni_contribution_biodiversity_conservation_q20,
           ni_contribution_heat_island_reduction_q20,
           ni_contribution_flood_prevention_q20) |> 
  summarize(.groups = "drop")

natural_infrastructure_tiles_unioned_nis <- 
  natural_infrastructure_tiles_unioned_nis |>
  filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
  st_cast("MULTIPOLYGON")

natural_infrastructure_tiles_unioned_nis <- 
natural_infrastructure_tiles_unioned_nis |> 
  mutate(ID = dplyr::row_number(), .before = 1)

qsave(natural_infrastructure_tiles_unioned_nis, 
      "dev/data/natural_infrastructure_tiles_unioned_nis.qs")

rm(natural_infrastructure_tiles_unioned_nis_2)


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
    
    if (name %in% c("heat_islands", "cool_islands")) {
      data <- 
        data |> 
        mutate(area = st_area(geometry)) |> 
        filter(area != max(area)) |> 
        dplyr::select(-area)
    }
    
    data <- 
      st_transform(data, 4326)
    
    if (name == "flood_risks") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 100)
    } else if (name == "heat_islands") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 100)
    } else if (name == "cool_islands") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 1)
    } else {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = ntile(var, 100))
    }
    
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

qsave(natural_infrastructure, "dev/data/natural_infrastructure.qs")


# Pre-computations --------------------------------------------------------
natural_infrastructure_ <- qread("dev/data/natural_infrastructure.qs")

natural_infrastructure <- list()
# Original priorities
natural_infrastructure$original_priorities <- 
  map_dfr(1:100, ~{
    
    dat <-
      natural_infrastructure_[
        natural_infrastructure_$conservation_prioritization_q100 >=
          abs(.x - 100), str_starts(names(natural_infrastructure_), "ni_")]
    
    flood_prevention <- {dat$ni_contribution_flood_prevention |>
        sum(na.rm = TRUE)}/{
          natural_infrastructure_$ni_contribution_flood_prevention |>
            sum(na.rm = TRUE)}
    
    biodiversity_conservation <- {dat$ni_contribution_biodiversity_conservation |>
        sum(na.rm = TRUE)}/{
          natural_infrastructure_$ni_contribution_biodiversity_conservation |>
            sum(na.rm = TRUE)}
    
    heat_island_reduction <- {dat$ni_contribution_heat_island_reduction |>
        sum(na.rm = TRUE)}/{
          natural_infrastructure_$ni_contribution_heat_island_reduction |>
            sum(na.rm = TRUE)}
    
    tibble(percent_conservation = .x,
           flood_prevention = flood_prevention,
           biodiversity_conservation = biodiversity_conservation,
           heat_island_reduction = heat_island_reduction)
  })

# Custom priorities
natural_infrastructure_tiles_unioned_nis <-
  qread("dev/data/natural_infrastructure_tiles_unioned_nis.qs")

natural_infrastructure_custom_priority_unioned <-
  natural_infrastructure_tiles_unioned_nis |>
  st_drop_geometry()

# plan(multisession, workers = 10)

natural_infrastructure$custom_priorities <- 
  future_map_dfr(0:20, function(biodiversity) {
    map_dfr(0:20, function(heat_island) {
      map_dfr(0:20, function(flood_prevention) {
        
        ids <- 
          natural_infrastructure_custom_priority_unioned |> 
          filter(ni_contribution_biodiversity_conservation_q20 <=
                   biodiversity,
                 ni_contribution_heat_island_reduction_q20 <=
                   heat_island,
                 ni_contribution_flood_prevention_q20 <=
                   flood_prevention) |> 
          pull(ID)
        
        tibble(ID = list(ids),
               "biodiversity" = 
                 biodiversity,
               "heat_island" = 
                 heat_island,
               "flood_prevention" = 
                 flood_prevention)
      })
    })
  })

# Cleanup -----------------------------------------------------------------

rm(datas)
