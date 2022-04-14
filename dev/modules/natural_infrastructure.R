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
  mutate(area = units::drop_units(st_area(geometry))) |> 
  st_drop_geometry()

# plan(multisession, workers = 10)

slider_values <- c(0, 0.5, 1, 1.5, 2)

top_slider_ <- (0:50)/2

qload("data/colours.qsm")

natural_infrastructure$custom_priorities <- 
  map_dfr(top_slider_, function(top_slider) {
    sum(natural_infrastructure_custom_priority_unioned$area)
    
    kept_pct <- top_slider/25
    kept_area <- kept_pct*sum(natural_infrastructure_custom_priority_unioned$area)
    
    map_dfr(slider_values, function(biodiversity) {
      map_dfr(slider_values, function(heat_island) {
        map_dfr(slider_values, function(flood_prevention) {
          natural_infrastructure_custom_priority_unioned |> 
            mutate(ni_contribution_biodiversity_conservation_q20 = 
                     ni_contribution_biodiversity_conservation_q20*biodiversity,
                   ni_contribution_heat_island_reduction_q20 = 
                     ni_contribution_heat_island_reduction_q20*heat_island,
                   ni_contribution_flood_prevention_q20 = 
                     ni_contribution_flood_prevention_q20*flood_prevention) |> 
            group_by(ID, area) |> 
            summarize(score = sum(ni_contribution_biodiversity_conservation_q20,
                                  ni_contribution_heat_island_reduction_q20,
                                  ni_contribution_flood_prevention_q20),
                      .groups = "drop") |> 
            arrange(-score) |> 
            mutate(ite_area = slider::slide_dbl(area, sum, .before = n())) |> 
            filter(!ite_area > kept_area) |> 
            mutate(percent_conservation =
                     top_slider,
                   "biodiversity" = 
                     biodiversity,
                   "heat_island" = 
                     heat_island,
                   "flood" = 
                     flood_prevention) |> 
            # Pre-compute ID&score column to directly fit in
            # `scale_fill_natural_infrastructure`
            rename(group = ID) |> 
            mutate(score = as.character(round(score/max(score)*100) + 100),
                   group = as.character(group)) |> 
            left_join(select(colour_table, group, value), by = c("score" = "group")) |> 
            select(-area, -ite_area, -score) |> 
            mutate(value = if_else(is.na(value), 
                                   colour_table$value[colour_table$group == 101], 
                                   value))
        })
      })
    })
  })

# Pre-compute values for the explore panel
total_areas <- 
natural_infrastructure_custom_priority_unioned |> 
  mutate(total_biodiversity = ni_contribution_biodiversity_conservation_q20 * area,
         total_heat_island = ni_contribution_heat_island_reduction_q20 * area,
         total_flood = ni_contribution_flood_prevention_q20 * area) |> 
  summarize(total_biodiversity = sum(total_biodiversity),
            total_heat_island = sum(total_heat_island),
            total_flood = sum(total_flood))


natural_infrastructure$custom_priorities_explore_values <- 
  map_dfr(top_slider_, function(top_slider) {
    map_dfr(slider_values, function(biodiversity) {
      map_dfr(slider_values, function(heat_island) {
        map_dfr(slider_values, function(flood) {
          
          ids <- 
            natural_infrastructure$custom_priorities |> 
            filter(percent_conservation == top_slider,
                   biodiversity == !!biodiversity,
                   heat_island == !!heat_island,
                   flood_prevention == !!flood_prevention) |> 
            pull(group)
          
          perc_protection <- 
          natural_infrastructure_custom_priority_unioned |> 
            filter(ID %in% ids) |> 
            mutate(biodiversity = ni_contribution_biodiversity_conservation_q20 * area,
                   heat_island = ni_contribution_heat_island_reduction_q20 * area,
                   flood = ni_contribution_flood_prevention_q20 * area) |> 
            summarize(biodiversity = sum(biodiversity)/total_areas$total_biodiversity,
                      heat_island = sum(heat_island)/total_areas$total_heat_island,
                      flood_prevention = sum(flood)/total_areas$total_flood)
          
          tibble(percent_conservation = top_slider,
                 biodiversity = biodiversity,
                 heat_island = heat_island,
                 flood = flood,
                 flood_prevention = perc_protection$flood_prevention,
                 biodiversity_conservation = perc_protection$biodiversity,
                 heat_island_reduction = perc_protection$heat_island)
          
        })
      })
    })
  })
  

# Cleanup -----------------------------------------------------------------

rm(datas)
