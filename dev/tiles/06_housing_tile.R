#### HOUSING TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
building_full <- qread("data/building_full.qs", nthreads = 4)
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")
source("R/functions/_utils.R")
qload("data/colours.qsm")


# Get variables to add ----------------------------------------------------

left_vars <-
  variables |> 
  filter(theme == "Housing", source == "census") |> 
  pull(var_code)

all_years <- 
  variables |> 
  filter(theme == "Housing", source == "census") |> 
  pull(dates) |> 
  unlist() |> 
  unique() |> 
  sort()

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment", "Housing"), !is.na(theme)) |> 
  pull(var_code)


# Processing function -----------------------------------------------------

make_housing_tiles <- function(df) {
  
  dat <- get(df)
  
  # Get all multi-date right vars
  rv_delta_bivar <- map_dfc(right_vars, \(var) {
    
    var_year <-
      variables |> 
      filter(var_code == var) |> 
      pull(dates) |> 
      pluck(1)
    
    if (length(var_year) != length(all_years)) return(NULL)
    
    var_year_comb <-
      expand.grid(var_year, var_year) |> 
      as_tibble() |> 
      mutate(across(everything(), as.character)) |> 
      mutate(across(everything(), as.numeric)) |> 
      filter(Var1 < Var2) |> 
      as.vector() |> 
      split(1:10) |> 
      map(unlist) |> 
      map(unname)
    
    # Get delta values for all 2-year comparisons
    multi_year <-
      map_dfc(var_year_comb, ~{
        
        val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                  dat[[paste(var, .x[1], sep = "_")]]) / 
          dat[[paste(var, .x[1], sep = "_")]]
        
        val <- replace(val, is.nan(val), NA)
        val <- replace(val, is.infinite(val), NA)
        q3 <- ntile(val, 3)
        tibble(q3) |> 
          set_names(paste(var, "q3", paste(.x, collapse = "_"), sep = "_"))
      })
    
  })
  
  # Get all combinations for all left vars
  dat |> 
    select(ID, name, any_of(c("building", "geometry"))) |>
    bind_cols(map_dfc(left_vars, \(var) {
      
      var_year <-
        variables |> 
        filter(var_code == var) |> 
        pull(dates) |> 
        pluck(1)
      
      var_year_comb <-
        expand.grid(var_year, var_year) |> 
        as_tibble() |> 
        mutate(across(everything(), as.character)) |> 
        mutate(across(everything(), as.numeric)) |> 
        filter(Var1 < Var2) |> 
        as.vector() |> 
        # Need to calculate triangular number to see how many pairs there are
        split(1:(length(var_year) * (length(var_year) - 1) / 2)) |> 
        map(unlist) |> 
        map(unname)
      
      # Get q5 values for individual years
      single_year_q5 <- 
        map_dfc(var_year, ~dat[[paste(var, "q5", .x, sep = "_")]]) |> 
        set_names(paste(var, var_year, sep = "_")) |> 
        mutate(across(everything(), as.character))
      
      # Get q3 values for individual years
      single_year_q3 <- 
        map_dfc(var_year, ~dat[[paste(var, "q3", .x, sep = "_")]]) |> 
        set_names(paste(var, "q3", var_year, sep = "_")) |> 
        mutate(across(everything(), as.character))
      
      # Get delta values for all 2-year comparisons
      multi_year <- if (length(var_year) != length(all_years)) NULL else {
        
        map_dfc(var_year_comb, ~{
          
          val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                    dat[[paste(var, .x[1], sep = "_")]]) / 
            abs(dat[[paste(var, .x[1], sep = "_")]])
          
          val <- replace(val, is.nan(val), NA)
          val <- replace(val, is.infinite(val), NA)
          q3 <- ntile(val, 3)
          data_med <- median(abs(val[abs(val) > 0.02]), na.rm = TRUE)
          val_delta <- rep("19", length(val))
          val_delta[val < data_med] <- "18"
          val_delta[val < 0.02] <- "17"
          val_delta[val < -0.02] <- "16"
          val_delta[val < -1 * data_med] <- "15"
          val_delta[is.na(val)] <- "0"
          tibble(val_delta, q3) |> 
            set_names(c(paste(var, paste(.x, collapse = "_"), sep = "_"),
                        paste(var, "q3", paste(.x, collapse = "_"), sep = "_")))
        })
      }
      
      multi_year_q3 <- if (is.null(multi_year)) NULL else {
        multi_year |> 
          select(contains("q3"))
      }
      
      multi_year <- if (is.null(multi_year)) NULL else {
        multi_year |> 
          select(!contains("q3"))
      }
      
      # Get bivar values for all single years
      bivar <- map_dfc(right_vars, \(rv) {
        
        map_dfc(names(single_year_q3), ~{
          year <- str_extract(.x, "\\d{4}$")
          rv_2 <- paste(rv, "q3", year, sep = "_") |> 
            return_closest_year(df)
          paste(single_year_q3[[.x]], dat[[rv_2]], sep = " - ") |> 
            trans_var()}) |> 
          set_names(paste(var, rv, str_extract(names(single_year_q3), 
                                               "\\d{4}$"), sep = "_"))
        
      })
      
      # Get delta bivar values for all year combinations
      delta_bivar <- if (is.null(multi_year_q3)) NULL else {
        
        multi_year_q3 |> 
          names() |> 
          map_dfc(\(x) {
            
            vec <- multi_year_q3[[x]]
            years <- str_extract(x, "\\d{4}_\\d{4}$")
            
            rv_names <- 
              rv_delta_bivar |> 
              select(contains(years)) |> 
              names()
            
            rv_delta_bivar[rv_names] |> 
              mutate(across(everything(), 
                            ~trans_var(paste(vec, .x, sep = " - ")))) |> 
              set_names(paste(str_remove(x, "_q3.*$"), 
                              str_remove(rv_names, "_q3"),
                              sep = "_"))
            
          })
        
      }
      
      bind_cols(single_year_q5, multi_year, bivar, delta_bivar)
      
    })) |> 
    relocate(any_of(c("building", "geometry")), .after = last_col())
  
}


# Process geometries ------------------------------------------------------

borough_housing <- make_housing_tiles("borough")
CT_housing <- make_housing_tiles("CT")
DA_housing <- make_housing_tiles("DA")
DA_building_housing <- DA_housing |> 
  st_set_geometry("building") |> 
  select(-geometry) |> 
  rename(geometry = building)
DA_housing <- select(DA_housing, -building)
building_housing <- 
  building_full |> 
  select(ID, name, DAUID) |> 
  left_join(select(st_drop_geometry(DA_housing), -name), 
            by = c("DAUID" = "ID")) |> 
  select(-DAUID)


# Construct tile lookup table ---------------------------------------------

tile_lookup <- qread("data/tile_lookup.qs")

var_groups <- 
  map(left_vars, ~{
    names(borough_housing) |> 
      str_subset(.x)
  })

var_groups <- 
  map(var_groups, ~{
  if (length(.x) < 100) return(list(.x))
  one_year <- str_subset(.x, "\\d{4}_\\d{4}$", negate = TRUE)
  two_year <- str_subset(.x, "\\d{4}_\\d{4}$")
  two_year_1 <- str_subset(two_year, "1996|2001_2006")
  two_year_2 <- str_subset(two_year, "1996|2001_2006", negate = TRUE)
  list(one_year, two_year_1, two_year_2)}) |> 
  unlist(recursive = FALSE)

var_groups_long <- var_groups[lengths(var_groups) > 100]
var_groups_short <- var_groups[lengths(var_groups) <= 100]
var_groups_short <- list(c(var_groups_short[[1]], var_groups_short[[2]]),
                         c(var_groups_short[[3]], var_groups_short[[4]]))
var_groups <- c(var_groups_long, var_groups_short)
rm(var_groups_short, var_groups_long)

tile_lookup <- 
  tile_lookup |> 
  filter(module != "housing") |> 
  bind_rows(
    map2_dfr(var_groups, seq_along(var_groups), ~tibble(
      module = "housing", tile2 = .x, suffix = paste0("-", .y)))
  )

qsave(tile_lookup, "data/tile_lookup.qs")


# Process and upload borough ----------------------------------------------

list_tile_sources() |> filter(str_detect(id, "housing-borough"))

for (i in seq_along(var_groups)) {
  
  borough_housing |> 
    select(ID, name, all_of(var_groups[[i]]), geometry) |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("housing-borough-", i))
  
}


# Create borough recipe and publish ---------------------------------------

# Create recipes
borough_recipes <- 
  map(seq_along(var_groups), ~{
    create_recipe(
      layer_names = "borough",
      source = paste0(
        "mapbox://tileset-source/sus-mcgill/housing-borough-", .x),
      minzoom = 3,
      maxzoom = 11, 
      layer_size = 2500,
      simplification_zoom = 11,
      recipe_name = paste0("housing-borough-", .x))
  })

# Create tilesets
for (i in seq_along(var_groups)) {
  out <- create_tileset(paste0("housing-borough-", i), borough_recipes[[i]])
  if (out$status_code != 200) stop(var)
  Sys.sleep(1)
}

# Publish tilesets
for (i in seq_along(var_groups)) {
  out <- publish_tileset(paste0("housing-borough-", i))
  if (out$status_code != 200) stop(var)
  Sys.sleep(30)
}


#### TEST NOW! ####


# Process and upload CT ---------------------------------------------------

list_tile_sources() |> filter(str_detect(id, "housing-CT"))

for (i in seq_along(var_groups)) {
  
  CT_housing |> 
    select(ID, name, all_of(var_groups[[i]]), geometry) |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("housing-CT-", i))
  
}


# Process and upload DA ---------------------------------------------------

list_tile_sources() |> filter(str_detect(id, "housing-DA"))

for (i in seq_along(var_groups)) {
  
  DA_housing |> 
    select(ID, name, all_of(var_groups[[i]]), geometry) |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("housing-DA-", i))
  
}


# Process and upload DA_building ------------------------------------------

list_tile_sources() |> filter(str_detect(id, "housing-DA_building"))

for (i in seq_along(var_groups)) {
  
  DA_building_housing |> 
    select(ID, name, all_of(var_groups[[i]]), geometry) |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("housing-DA_building-", i))
  
}


# Process and upload building ---------------------------------------------

library(future)  
plan(multisession, workers = 8)
options('future.globals.maxSize' = Inf)

list_tile_sources() |> filter(str_detect(id, "housing-building"))

for (i in seq_along(var_groups)) {
  
  cat(paste0("\nProcessing group ", i, "\n"))
  
  building_to_process <- 
    building_housing |> 
    select(ID, name, all_of(var_groups[[i]]), geometry)
  
  iter_size <- ceiling(nrow(building_to_process) / 100)
  
  building_to_process_list <- 
    furrr::future_map(1:100, ~{
      building_to_process |> 
        slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
        geojsonsf::sf_geojson() |> 
        paste0(collapse = " ") |> 
        geojson::featurecollection()  
    })
  
  # Iteratively post files to tile source
  tmp <- tempfile(fileext = ".json")
  tmp_list <- map(1:10, ~tempfile(fileext = ".json"))
  
  cat(paste0("\nUploading group ", i, "\n"))
  
  for (j in 1:10) {
    
    try <- 0L
    success <- FALSE
    
    while (try < 4 && !success) {
      
      try <- try + 1L
      
      cat(paste0("\n", j, "\n"))
      to_process <- building_to_process_list[((j - 1) * 10 + 1):(j * 10)]
      walk2(to_process, tmp_list, geojson::ndgeo_write)
      
      # Concatenate geoJSONs
      out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
      system(out)
      
      # Upload to MTS
      out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                    'sus-mcgill/housing-building-', i, '?access_token=', 
                    .sus_token, '" -F file=@', tmp, 
                    ' --header "Content-Type: multipart/form-data"')
      system(out)
      
      file_n <- 
        list_tile_sources() |> 
        filter(str_detect(id, paste0("housing-building-", i))) |> 
        pull(files)
      
      if (file_n == j) success <- TRUE
      
    }
  
  }
  
}


# Create auto_zoom recipe and publish -------------------------------------

# Create recipes
auto_zoom_recipes <- 
  map(seq_along(var_groups), ~{
    
    create_recipe(
      layer_names = c("borough", "CT", "DA", "building"),
      source = c(
        borough = paste0(
          "mapbox://tileset-source/sus-mcgill/housing-borough-", .x),
        CT = paste0("mapbox://tileset-source/sus-mcgill/housing-CT-", .x),
        DA = paste0("mapbox://tileset-source/sus-mcgill/housing-DA-", .x),
        building = paste0(
          "mapbox://tileset-source/sus-mcgill/housing-building-", .x)),
      minzoom = c(borough = 2, CT = 11, DA = 13, building = 16),
      maxzoom = c(borough = 10, CT = 12, DA = 15, building = 16), 
      layer_size = c(borough = NA, CT = NA, DA = NA, building = 2500),
      recipe_name = paste0("housing-auto_zoom-", .x))
  })

# Create tilesets
for (i in seq_along(var_groups)) {
  out <- create_tileset(paste0("housing-auto_zoom-", i), auto_zoom_recipes[[i]])
  if (out$status_code != 200) stop(var)
  Sys.sleep(1)
}

# Publish tilesets
for (i in seq_along(var_groups)) {
  out <- publish_tileset(paste0("housing-auto_zoom-", i))
  if (out$status_code != 200) stop(var)
  Sys.sleep(30)
}


# Create CT recipe and publish --------------------------------------------

# Create recipes
CT_recipes <- 
  map(seq_along(var_groups), ~{
    create_recipe(
      layer_names = "CT",
      source = paste0(
        "mapbox://tileset-source/sus-mcgill/housing-CT-", .x),
      minzoom = 3,
      maxzoom = 12, 
      layer_size = 2500,
      simplification_zoom = 12,
      recipe_name = paste0("housing-CT-", .x))
  })

# Create tilesets
for (i in seq_along(var_groups)) {
  out <- create_tileset(paste0("housing-CT-", i), CT_recipes[[i]])
  if (out$status_code != 200) stop(var)
  Sys.sleep(1)
}

# Publish tilesets
for (i in seq_along(var_groups)) {
  out <- publish_tileset(paste0("housing-CT-", i))
  if (out$status_code != 200) stop(var)
  Sys.sleep(30)
}


# Create DA recipe and publish --------------------------------------------

# Create recipes
DA_recipes <- 
  map(seq_along(var_groups), ~{
    create_recipe(
      layer_names = c("DA_empty", "DA"),
      source = c(
        DA_empty = "mapbox://tileset-source/sus-mcgill/DA_empty",
        DA = paste0("mapbox://tileset-source/sus-mcgill/housing-DA-", .x)),
      minzoom = c(DA_empty = 3, DA = 9),
      maxzoom = c(DA_empty = 8, DA = 13), 
      layer_size = c(DA_empty = NA, DA = 2500),
      simplification_zoom = c(DA_empty = NA, DA = 13),
      recipe_name = paste0("housing-DA-", .x))
  })

# Create tilesets
for (i in seq_along(var_groups)) {
  out <- create_tileset(paste0("housing-DA-", i), DA_recipes[[i]])
  if (out$status_code != 200) stop(var)
  Sys.sleep(1)
}

# Publish tilesets
for (i in seq_along(var_groups)) {
  out <- publish_tileset(paste0("housing-DA-", i))
  if (out$status_code != 200) stop(var)
  Sys.sleep(30)
}


# Create building recipe and publish --------------------------------------

# Create recipes
building_recipes <- 
  map(seq_along(var_groups), ~{
    
    create_recipe(
      layer_names = c("DA_building_empty", "DA_building", "building"),
      source = c(
        DA_building_empty = 
          "mapbox://tileset-source/sus-mcgill/DA_building_empty",
        DA_building = paste0(
          "mapbox://tileset-source/sus-mcgill/housing-DA_building-", .x),
        building = paste0(
          "mapbox://tileset-source/sus-mcgill/housing-building-", .x)),
      minzoom = c(DA_building_empty = 3, DA_building = 9, building = 13),
      maxzoom = c(DA_building_empty = 8, DA_building = 12, building = 16), 
      layer_size = c(DA_building_empty = NA, DA_building = NA, building = 2500),
      recipe_name = paste0("housing-building-", .x))
  })

# Create tilesets
for (i in seq_along(var_groups)) {
  out <- create_tileset(paste0("housing-building-", i), building_recipes[[i]])
  if (out$status_code != 200) stop(var)
  Sys.sleep(1)
}

# Publish tilesets
for (i in seq_along(var_groups)) {
  out <- publish_tileset(paste0("housing-building-", i))
  if (out$status_code != 200) stop(var)
  Sys.sleep(30)
}
