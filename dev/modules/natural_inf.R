#### Natural infrastructure data setup #########################################

# This script relies on objects created in dev/census.R

# Natural infrastructure abbreviated to NI

# Load libraries ----------------------------------------------------------

suppressPackageStartupMessages({
  library(raster)
  # library(anglr)
  library(stars)
  library(furrr)
  library(tidyverse)
  library(qs)
})


# Convert raster data to grid polygon -------------------------------------

datasets <- c("habitat_qual" = "Fig11a.asc", 
              "habitat_con" = "Fig11b.asc", 
              "favorable_cc" = "Fig11c.asc",
              "c_flood" = "Fig13.tif", 
              "c_bio" = "Fig14.asc",
              "c_heat" = "Fig15.tif", 
              "c_priority" = "Fig16.tif",
              "flood" = "Flood_CMM/Flood_CMM.tif",
              "heat" = "heatcoolislands/heatislands3.asc",
              "cool" = "heatcoolislands/coolislands5.asc")

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

# plan(multisession, workers = length(datasets))

natural_inf_tiles_raw <- 
  future_map2(set_names(names(datasets)), datasets, function(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid()
    
    if (name %in% c("heat", "cool")) {
      data <- 
        data |> 
        mutate(area = st_area(geometry)) |> 
        filter(area != max(area)) |> 
        dplyr::select(-area)
    }
    
    
    if (name == "flood") {
      data_q20 <-
        data |>
        rename(var = 1) |>
        mutate(rank = 45) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    } else if (name == "heat") {
      data_q20 <-
        data |>
        rename(var = 1) |>
        mutate(rank = 45) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    } else if (name == "cool") {
      data_q20 <-
        data |>
        rename(var = 1) |>
        mutate(rank = 26) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    } else {
      data_q20 <-
        data |>
        rename(var = 1) |>
        mutate(rank = ntile(var, 20) + 25) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    }
    
    if (name == "flood") {
      data_q10 <-
        data |>
        rename(var = 1) |>
        mutate(rank = 45) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    } else if (name == "heat") {
      data_q10 <-
        data |>
        rename(var = 1) |>
        mutate(rank = 45) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    } else if (name == "cool") {
      data_q10 <-
        data |>
        rename(var = 1) |>
        mutate(rank = 26) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    } else {
      data_q10 <-
        data |>
        rename(var = 1) |>
        mutate(rank = ntile(var, 10) * 2 + 25) |>
        group_by(rank) |>
        summarize(.groups = "drop")
    }
    
    data_q20 <-
      st_transform(data_q20, 4326) |>
      filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
      st_cast("MULTIPOLYGON") |> 
      mutate(ID = seq_len(n()), .before = 1)
    
    data_q10 <-
      st_transform(data_q10, 4326) |>
      filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
      st_cast("MULTIPOLYGON") |> 
      mutate(ID = seq_len(n()), .before = 1)
    
    names(data_q20)[2] <- paste0(name, "_q20")
    names(data_q10)[2] <- paste0(name, "_q20")
    
    out <- list(data_q20, data_q10)
    names(out) <- c("high", "low")

    out
    
  })

qsave(natural_inf_tiles_raw, "dev/data/natural_inf_tiles_raw.qs")


# To enable unique sets of priorities -------------------------------------

natural_inf_tiles <- 
  future_map2(set_names(names(datasets))[4:6], datasets[4:6], \(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, #merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid() |> 
      st_transform(4326)
    
    data <-
      data |>
      rename(var = 1) |>
      mutate(rank = ntile(var, 20))
    
    names(data)[3] <- paste0(name, "_q20")
    
    data[, paste0(name, "_q20")]
    
  })

# Change list order
natural_inf_tiles <- natural_inf_tiles[c("c_bio", "c_heat", "c_flood")]

natural_inf_tiles[[1]] <- 
  natural_inf_tiles[[1]] |> 
  mutate(ID = row_number())

natural_inf_tiles_2 <- natural_inf_tiles

natural_inf_tiles_2[[1]] <- 
  natural_inf_tiles_2[[1]] |> 
  mutate(geometry = st_centroid(geometry))

natural_inf_tiles_2 <- 
  reduce(natural_inf_tiles_2, st_join)

natural_inf_tiles_2 <- 
  natural_inf_tiles_2 |> 
  st_drop_geometry() |> 
  left_join(select(natural_inf_tiles[[1]],
                   -everything(), ID), by = "ID") |> 
  select(-ID) |> 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) |> 
  st_as_sf()

natural_inf_tiles <- 
  natural_inf_tiles_2 |> 
  group_by(c_bio_q20,
           c_heat_q20,
           c_flood_q20) |> 
  summarize(.groups = "drop")

natural_inf_tiles <- 
  natural_inf_tiles |>
  filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
  st_cast("MULTIPOLYGON")

natural_inf_tiles <- 
  natural_inf_tiles |> 
  mutate(ID = dplyr::row_number(), .before = 1)

natural_inf_tiles <- 
  natural_inf_tiles |> 
  set_names(c("ID", "biodiversity_q20", "heat_island_q20", "flood_q20", 
              "geometry"))

qsave(natural_inf_tiles, "dev/data/natural_inf_tiles.qs")

rm(natural_inf_tiles_2)


# Join all pixels into one df ---------------------------------------------

natural_inf <- 
  future_map2(set_names(names(datasets)), datasets, function(name, path) {
    
    data <- read_stars(paste0("dev/data/2018_FDS_InfraNat_Conn/", path))
    
    data <- 
      st_as_sf(data, as_points = FALSE, #merge = TRUE,
               crs = st_crs(2950)) |> 
      as_tibble() |> 
      st_as_sf(crs = st_crs(2950)) |> 
      st_make_valid()
    
    if (name %in% c("heat", "cool")) {
      data <- 
        data |> 
        mutate(area = st_area(geometry)) |> 
        filter(area != max(area)) |> 
        dplyr::select(-area)
    }
    
    data <- 
      st_transform(data, 4326)
    
    if (name == "flood") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 100)
    } else if (name == "heat") {
      data <-
        data |>
        rename(var = 1) |>
        mutate(rank = 100)
    } else if (name == "cool") {
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
natural_inf[[1]] <- 
  natural_inf[[1]] |> 
  mutate(geometry = st_centroid(geometry))

natural_inf <- reduce(natural_inf, st_join) |> 
  st_drop_geometry()

qsave(natural_inf, "dev/data/natural_inf.qs")


# Pre-computations --------------------------------------------------------

natural_inf_ <- qread("dev/data/natural_inf.qs")

natural_inf <- list()

# Original priorities
natural_inf$original_priorities <- map_dfr(1:100, ~{
  
  dat <-
    natural_inf_[
      natural_inf_$conservation_prioritization_q100 >=
        abs(.x - 100), str_starts(names(natural_inf_), "ni_")]
  
  biodiversity <- {dat$ni_contribution_biodiversity_conservation |>
      sum(na.rm = TRUE)} / {
        natural_inf_$ni_contribution_biodiversity_conservation |>
          sum(na.rm = TRUE)}
  
  heat_island <- {dat$ni_contribution_heat_island_reduction |>
      sum(na.rm = TRUE)} / {
        natural_inf_$ni_contribution_heat_island_reduction |>
          sum(na.rm = TRUE)}
  
  flood <- {dat$ni_contribution_flood_prevention |>
      sum(na.rm = TRUE)} / {
        natural_inf_$ni_contribution_flood_prevention |>
          sum(na.rm = TRUE)}
  
  tibble(conservation_pct = .x,
         biodiversity = biodiversity,
         heat_island = heat_island,
         flood = flood)
  
})

# Custom priorities
natural_inf_tiles <- qread("dev/data/natural_inf_tiles.qs")

natural_inf_custom <-
  natural_inf_tiles |>
  mutate(area = units::drop_units(st_area(geometry))) |> 
  st_drop_geometry()

# plan(multisession, workers = 10)

slider_values <- c(0, 0.5, 1, 1.5, 2)
top_slider <- 0:50 / 2
qload("data/colours.qsm")

natural_inf$custom <- map_dfr(top_slider, \(x) {
  
  kept_pct <- x / 25
  kept_area <- kept_pct * sum(natural_inf_custom$area)
    
  all_sliders <- 
    expand.grid(slider_values, slider_values, slider_values) |> 
    as_tibble() |> 
    set_names(c("biodiversity", "heat_island", "flood"))
  
  map_dfr(seq_len(nrow(all_sliders)), \(y) {
    
    df <- all_sliders[y,]
    
    natural_inf_custom |> 
      mutate(biodiversity_q20 = biodiversity_q20 * df$biodiversity,
             heat_island_q20 = heat_island_q20 * df$heat_island,
             flood_q20 = flood_q20 * df$flood) |> 
      mutate(score = biodiversity_q20 + heat_island_q20 + flood_q20) |> 
      select(-biodiversity_q20, -heat_island_q20, -flood_q20) |> 
      arrange(-score) |> 
      mutate(ite_area = slider::slide_dbl(area, sum, .before = n())) |> 
      filter(!ite_area > kept_area) |> 
      mutate(conservation_pct = x,
             biodiversity = df$biodiversity,
             heat_island = df$heat_island,
             flood = df$flood) |> 
      # Pre-compute ID/score columns to directly fit in `scale_fill_natural_inf`
      rename(group = ID) |> 
      mutate(score = as.character(round(score / max(score) * 100) + 100),
             group = as.character(group)) |> 
      left_join(select(colour_table, group, value), 
                by = c("score" = "group")) |> 
      select(-area, -ite_area, -score) |> 
      mutate(value = if_else(is.na(value), colour_table$value[
        colour_table$group == 101], value))
    })
  })

# Pre-compute values for the explore panel
total_areas <- 
  natural_inf_custom |> 
  mutate(total_biodiversity = biodiversity_q20 * area,
         total_heat_island = heat_island_q20 * area,
         total_flood = flood_q20 * area) |> 
  summarize(total_biodiversity = sum(total_biodiversity),
            total_heat_island = sum(total_heat_island),
            total_flood = sum(total_flood))

natural_inf$custom_explore <- map_dfr(top_slider, function(top_slider) {
  
  all_sliders <- 
    expand.grid(slider_values, slider_values, slider_values) |> 
    as_tibble() |> 
    set_names(c("biodiversity", "heat_island", "flood"))
  
  map_dfr(seq_len(nrow(all_sliders)), \(y) {
    
    df <- all_sliders[y,]
    
    ids <- 
      natural_inf$custom |> 
      filter(conservation_pct == top_slider,
             biodiversity == df$biodiversity,
             heat_island == df$heat_island,
             flood == df$flood) |> 
      pull(group)
    
    perc_protection <- 
      natural_inf_custom |> 
      filter(ID %in% ids) |> 
      mutate(biodiversity = biodiversity_q20 * area,
             heat_island = heat_island_q20 * area,
             flood = flood_q20 * area) |> 
      summarize(
        biodiversity = sum(biodiversity) / total_areas$total_biodiversity,
        heat_island = sum(heat_island) / total_areas$total_heat_island,
        flood = sum(flood) / total_areas$total_flood)
    
    tibble(conservation_pct = top_slider,
           biodiversity = df$biodiversity,
           heat_island = df$heat_island,
           flood = df$flood,
           biodiversity_conservation = perc_protection$biodiversity,
           heat_island_reduction = perc_protection$heat_island,
           flood_prevention = perc_protection$flood)
  })
  
})
  

# Add variable explanations -----------------------------------------------

variables <- 
  variables |>
  add_variables(
    var_code = "habitat_qual",
    var_title = "Habitat quality",
    var_short = "Quality",
    explanation = paste0("the ability of the ecosystem to provide conditions ",
                         "appropriate for individual and population persistence"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "habitat_con",
    var_title = "Habitat connectivity",
    var_short = "Connectivity",
    explanation = paste0("the degree to which the landscape facilitates or ",
                         "impedes animal movement and other ecological processes"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "favorable_cc",
    var_title = "Favorable climatic conditions",
    var_short = "Favorable CC",
    explanation = paste0("the degree to which climatic conditions (past and f",
                         "uture) are favourable to the life of species"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "c_flood",
    var_title = "Contribution to flood prevention",
    var_short = "Flood prev.",
    explanation = paste0("the effect of natural infrastructure on flood ",
                         "prevention"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "c_bio",
    var_title = "Contribution to biodiversity conservation",
    var_short = "Biodiversity cons.",
    explanation = paste0("the effect of natural infrastructure on ",
                         "biodiversity conservation"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "c_heat",
    var_title = "Contribution to heat island reduction",
    var_short = "Heat island reduct.",
    explanation = paste0("the effect of natural infrastructure on ",
                         "heat island reduction"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "c_priority",
    var_title = "Conservation prioritization",
    var_short = "Conservation",
    explanation = paste0(""),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "flood",
    var_title = "Flood risks",
    var_short = "Flood risks",
    explanation = paste0("the land areas at risk of flooding"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "heat",
    var_title = "Heat islands",
    var_short = "Heat islands",
    explanation = paste0("the intra-urban areas with a higher air or surface ",
                         "temperature than other areas in the same urban ",
                         "environment"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  ) |>
  add_variables(
    var_code = "cool",
    var_title = "Cool islands",
    var_short = "Cool islands",
    explanation = paste0("the intra-urban areas with a lower air or surface ",
                         "temperature than other areas in the same urban ",
                         "environment"),
    category = NA,
    theme = "Ecology",
    private = TRUE,
    dates = NA,
    scales = NA,
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "David Suzuki Foundation"
  )


# Cleanup -----------------------------------------------------------------

rm(datasets)
