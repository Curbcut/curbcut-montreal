## UPLOAD ALL EMPTY TILESETS ###################################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")
qload("data2/census_full.qsm")
centraide <- qread("data2/centraide_full.qs")
grid <- qread("data2/grid_full.qs")
building <- qread("data2/building_full.qs")

CT <- CT_full
borough <- borough_full
DA <- DA_full


# Combinations ------------------------------------------------------------

combinations <- 
  list(
    "CMA" = list(
      "borough" = "borough",
      "CT" = "CT",
      "DA" = "DA",
      "building" = "building",
      "auto_zoom" = c("borough", "CT", "DA", "building"),
      "auto_zoom_max_CT" = c("borough", "CT")),
    
    "island" = list(
      "borough" = "borough",
      "CT" = "CT",
      "DA" = "DA",
      "building" = "building",
      "auto_zoom" = c("borough", "CT", "DA", "building")),
    
    "centraide" = list(
      "centraide" = "centraide",
      "CT" = "CT",
      "DA" = "DA",
      "building" = "building",
      "auto_zoom" = c("centraide", "CT", "DA", "building"),
      "auto_zoom_max_CT" = c("centraide", "CT")))


# Island CSDUIDs ----------------------------------------------------------

island_CSDUID <- 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072", "2466023")


# Reset -------------------------------------------------------------------

# imap(combinations, function(scales, geo) {
#   map(scales, function(scale) {
#     if (length(scale) != 1) return()
#       delete_tileset_source(id = paste(geo, scale, sep = "-"))
#   })
# })
# 
# imap(combinations, function(scales, geo) {
#   map(set_names(paste(geo, names(scales), sep = "-")), function(tileset_name) {
#     delete_tileset(tileset_name)
#   })
# })



# Building source ---------------------------------------------------------

create_building_tileset <- function(name, building_to_process) {
  iter_size <- ceiling(nrow(building_to_process) / 100)
  
  building_to_process_list <- 
    map(1:100, ~{
      building_to_process |> 
        slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
        geojsonsf::sf_geojson() |> 
        paste0(collapse = " ") |> 
        geojson::featurecollection()  
    })
  
  # Iteratively post files to tile source
  tmp <- tempfile(fileext = ".json")
  tmp_list <- map(1:10, ~tempfile(fileext = ".json"))
  
  map(1:10, function(x) {
    
    to_process <- building_to_process_list[((x - 1) * 10 + 1):(x * 10)]
    walk2(to_process, tmp_list, geojson::ndgeo_write)
    
    # Concatenate geoJSONs
    if (Sys.info()[["sysname"]] == "Windows") {
      out <- paste0("type ", paste(tmp_list, collapse = " "), " > ", tmp)
      shell(out)    
    } else {
      out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
      system(out)   
    }
    
    # Upload to MTS
    out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                  'sus-mcgill/', name, '?access_token=', .sus_token, 
                  '" -F file=@', tmp, 
                  ' --header "Content-Type: multipart/form-data"')
    system(out)
    
  })
  
}


# Tileset sources ---------------------------------------------------------

imap(combinations, function(scales, geo) {
  map(scales, function(scale) {
    if (length(scale) != 1) return()
    
    if (scale == "building") {
      building_to_process <- 
        building |> 
        (\(x) if (geo == "island") {
          filter(x, CSDUID %in% island_CSDUID)
        } else {
          filter(x, .data[[geo]])
        })() |> 
        select(ID, ID_color = DAUID)
      
      create_building_tileset(name = paste(geo, scale, sep = "-"),
                              building_to_process = building_to_process)
    } else {
      get(scale) |> 
        (\(x) if (geo == "island") {
          filter(x, CSDUID %in% island_CSDUID)
        } else if (scale %in% c("CT", "DA")) {
          filter(x, .data[[geo]])
        } else x)() |> 
        select(ID, ID_color = ID) |> 
        st_set_agr("constant") |>
        upload_tile_source(id = paste(geo, scale, sep = "-"),
                           access_token = .sus_token)
    }
  })
})


# Tileset recipes ---------------------------------------------------------

first_level_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    layer_size = 2500,
    recipe_name = name)
}

CT_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = name)
}

DA_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 13, 
    simp_zoom = 13,
    layer_size = 2500,
    recipe_name = name)
}

building_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 16, 
    layer_size = 2500,
    recipe_name = name)
}

all_recipes <- 
  imap(combinations, function(scales, geo) {
    map2(scales, seq_along(scales), function(scale, level) {
      
      # Auto-zoom cases
      if (length(scale) != 1) {
        warning(
          paste0("Don't forget autozoom for ", paste(geo, sep = "-")))
        return()
      }
      
      scale_fun <- if (level == 1) "first_level" else scale
      do.call(paste0(scale_fun, "_recipe_fun"), 
              list(paste(geo, scale, sep = "-")))
    })
  })

# Tileset recipes for auto-zooms
all_recipes$CMA$auto_zoom <- 
  create_recipe(
    layer_names = c("borough", "CT", "DA", "building"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/CMA-borough",
      CT = "mapbox://tileset-source/sus-mcgill/CMA-CT",
      DA = "mapbox://tileset-source/sus-mcgill/CMA-DA",
      building = "mapbox://tileset-source/sus-mcgill/CMA-building"),
    minzoom = c(borough = 2, CT = 11, DA = 13, building = 16),
    maxzoom = c(borough = 10, CT = 12, DA = 15, building = 16), 
    layer_size = c(borough = NA, CT = NA, DA = NA, building = NA),
    recipe_name = "CMA-auto_zoom")
  
all_recipes$CMA$`auto_zoom_max_CT` <- 
  create_recipe(
    layer_names = c("borough", "CT"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/CMA-borough",
      CT = "mapbox://tileset-source/sus-mcgill/CMA-CT"),
    minzoom = c(borough = 2, CT = 11),
    maxzoom = c(borough = 10, CT = 12), 
    layer_size = c(borough = NA, CT = NA),
    recipe_name = "CMA-auto_zoom_max_CT")

all_recipes$island$auto_zoom <- 
  create_recipe(
    layer_names = c("borough", "CT", "DA", "building"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/island-borough",
      CT = "mapbox://tileset-source/sus-mcgill/island-CT",
      DA = "mapbox://tileset-source/sus-mcgill/island-DA",
      building = "mapbox://tileset-source/sus-mcgill/island-building"),
    minzoom = c(borough = 2, CT = 11, DA = 13, building = 16),
    maxzoom = c(borough = 10, CT = 12, DA = 15, building = 16), 
    layer_size = c(borough = NA, CT = NA, DA = NA, building = NA),
    recipe_name = "island-auto_zoom")

all_recipes$centraide$auto_zoom <- 
  create_recipe(
    layer_names = c("centraide", "CT", "DA", "building"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/centraide-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/centraide-CT",
      DA = "mapbox://tileset-source/sus-mcgill/centraide-DA",
      building = "mapbox://tileset-source/sus-mcgill/centraide-building"),
    minzoom = c(centraide = 2, CT = 11, DA = 13, building = 16),
    maxzoom = c(centraide = 10, CT = 12, DA = 15, building = 16), 
    layer_size = c(centraide = NA, CT = NA, DA = NA, building = NA),
    recipe_name = "centraide-auto_zoom")
  
all_recipes$centraide$`auto_zoom_max_CT` <- 
  create_recipe(
    layer_names = c("centraide", "CT"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/centraide-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/centraide-CT"),
    minzoom = c(centraide = 2, CT = 11),
    maxzoom = c(centraide = 10, CT = 12), 
    layer_size = c(centraide = NA, CT = NA),
    recipe_name = "centraide-auto_zoom_max_CT")


if (map(all_recipes, map_lgl, is.null) |> unlist() |> sum() > 0) {
  stop("You need to add a manual recipe for an auto-zoom. You missed some!")
}


# Create tilesets ---------------------------------------------------------

imap(all_recipes, function(recipes, geo) {
  map2(recipes, seq_along(recipes), function(recipe, level) {
    create_tileset(paste(geo, names(recipes)[level], sep = "-"), recipe)
  })
})


# Publish tilesets --------------------------------------------------------

imap(all_recipes, function(recipes, geo) {
  map2(recipes, seq_along(recipes), function(recipe, level) {
    publish_tileset(paste(geo, names(recipes)[level], sep = "-"))
  })
})


### Grid -----------------------------------------------------------------------

grid |>
  transmute(ID = as.character(ID), 
            ID_color = as.character(ID)) |> 
  upload_tile_source(id = "island-grid",
                     access_token = .sus_token)

grid_recipe <- 
  create_recipe(
    layer_names = "grid",
    source = paste0("mapbox://tileset-source/sus-mcgill/", "island-grid"),
    minzoom = 3,
    maxzoom = 13, 
    simp_zoom = 13,
    layer_size = 2500,
    recipe_name = "island-grid")

create_tileset("island-grid", grid_recipe)
publish_tileset("island-grid")
