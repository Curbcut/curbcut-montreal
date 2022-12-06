## UPLOAD ALL EMPTY TILESETS ###################################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")


# All combinations --------------------------------------------------------

all_tables <- 
  list("CMA" = c("CSD", "CT", "DA", "grid", "building"),
       "island" = c("CSD", "CT", "DA", "grid", "building"),
       "city" = c("CSD", "CT", "DA", "grid", "building"),
       "centraide" = c("centraide", "CT", "DA", "grid", "building"))

# TKTK REVIEW FOR CMHC ZONES, THEY GET AN AUTOZOOM ATTACHED!
combinations <- 
  imap(all_tables, function(scales, geo) {
    top_of_geo <- geo
    if (geo %in% c("CMA", "island", "city")) top_of_geo <- "CSD"
    
    out <- 
      c(setNames(scales, scales),
        list("auto_zoom" = c(top_of_geo, "CT", "DA", "building"),
             "auto_zoom_max_CT" = c(top_of_geo, "CT")))
    
    if (geo == "city")
      out <- c(out, list("auto_zoom_max_DB" = c(top_of_geo, "CT", "DA", "DB")))
    
    out
  })


# Load the data -----------------------------------------------------------

invisible(lapply(paste0("data2/", names(all_tables), "_full.qsm"), 
                 qload, env = .GlobalEnv))


iwalk(combinations, function(scales, geo) {
  walk(scales, function(scale) {
    
    if (length(scale) != 1) return()
    geo_scale <- paste(geo, scale, sep = "_")
    geo_scale_full <- paste0(geo_scale, "_full")

    assign(geo_scale, get(geo_scale_full), envir = .GlobalEnv)
    rm(list = geo_scale_full, envir = .GlobalEnv)
    
  })
})


# Reset -------------------------------------------------------------------

imap(combinations, function(scales, geo) {
  map(scales, function(scale) {
    if (length(scale) != 1) return()
      delete_tileset_source(id = paste(geo, scale, sep = "_"))
  })
})

imap(combinations, function(scales, geo) {
  map(set_names(paste(geo, names(scales), sep = "_")), function(tileset_name) {
    delete_tileset(tileset_name)
  })
})


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

    geo_scale <- paste(geo, scale, sep = "_")

    if (scale == "building") {
      building_to_process <-
        get(geo_scale) |>
        transmute(ID, ID_color = DAUID) |>
        st_transform(4326)

      create_building_tileset(name = geo_scale,
                              building_to_process = building_to_process)
    } else {
      get(geo_scale) |>
        transmute(ID, ID_color = ID) |>
        st_transform(4326) |> 
        st_set_agr("constant") |>
        upload_tile_source(id = geo_scale, access_token = .sus_token)
    }
  })
})


# Tileset recipes ---------------------------------------------------------

first_level_recipe_fun <- function(name, scales) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    layer_size = 2500,
    recipe_name = name)
}

CT_recipe_fun <- function(name, scales) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = name)
}

DA_recipe_fun <- function(name, scales) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 13, 
    simp_zoom = 13,
    layer_size = 2500,
    recipe_name = name)
}

DB_recipe_fun <- function(name, scales) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 14, 
    simp_zoom = 14,
    layer_size = 2500,
    recipe_name = name)
}

grid_recipe_fun <- function(name, scales) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 13, 
    simp_zoom = 13,
    layer_size = 2500,
    recipe_name = name)
}

building_recipe_fun <- function(name, scales) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 16, 
    layer_size = 2500,
    recipe_name = name)
}

auto_zoom_recipe_fun <- function(name, scales) {
  sources <- set_names(paste0("mapbox://tileset-source/sus-mcgill/", scales), 
                       scales)
  minzooms <- 
    map_dbl(set_names(scales), ~case_when(str_detect(.x, "_CT$") ~ 11,
                                          str_detect(.x, "_DA$") ~ 13,
                                          str_detect(.x, "_building$") ~ 16,
                                          # CSD, centraide... First level
                                          TRUE ~ 2))
  
  maxzooms <- 
    map_dbl(set_names(scales), ~case_when(str_detect(.x, "_CT$") ~ 12,
                                          str_detect(.x, "_DA$") ~ 15,
                                          str_detect(.x, "_building$") ~ 16,
                                          # CSD, centraide... First level
                                          TRUE ~ 10))
  
  if (sum(str_detect(scales, "DB")) == 1) {
    minzooms <- 
      map_dbl(set_names(scales), ~case_when(str_detect(.x, "_CT$") ~ 11,
                                            str_detect(.x, "_DA$") ~ 13,
                                            str_detect(.x, "_DB$") ~ 15,
                                            # CSD, centraide... First level
                                            TRUE ~ 2))
    
    maxzooms <- 
      map_dbl(set_names(scales), ~case_when(str_detect(.x, "_CT$") ~ 12,
                                            str_detect(.x, "_DA$") ~ 14,
                                            str_detect(.x, "_DB$") ~ 16,
                                            # CSD, centraide... First level
                                            TRUE ~ 10))
  }
  
  layer_sizes <- 
    set_names(rep(NA, length(scales)), scales)
  
  create_recipe(
    layer_names = scales,
    source = sources,
    minzoom = minzooms,
    maxzoom = maxzooms, 
    layer_size = layer_sizes,
    recipe_name = name)
}

all_recipes <- 
  imap(combinations, function(scales, geo) {
    map2(scales, seq_along(scales), function(scale, level) {
      
      scale_fun <- if (level == 1) "first_level" else scale
      function_name <- paste0(scale_fun, "_recipe_fun")
      if (length(scale) != 1) function_name <- "auto_zoom_recipe_fun"
      
      scale_name <- names(combinations[[geo]][level])
      
      do.call(function_name, list(name = paste(geo, scale_name, sep = "_"),
                                  scales = paste(geo, scale, sep = "_")))
    })
  })
  

# Create tilesets ---------------------------------------------------------

created <- 
  imap(all_recipes, function(recipes, geo) {
    map2(recipes, seq_along(recipes), function(recipe, level) {
      create_tileset(paste(geo, names(recipes)[level], sep = "_"), recipe)
    })
  })

walk(created, ~map(.x, ~map(.x, ~{
  if (!str_detect(.x, "^Success"))
    stop(paste0("One or more tileset hasn't succesfully been created. Look at ",
                "the `created` object."))
})))

rm(created)


# Publish tilesets --------------------------------------------------------

published <- 
  imap(all_recipes, function(recipes, geo) {
    map2(recipes, seq_along(recipes), function(recipe, level) {
      publish_tileset(paste(geo, names(recipes)[level], sep = "_"))
    })
  })

walk(published, ~map(.x, ~{
  if (!str_detect(.x$message, "^Processing"))
    stop(paste0("One or more tileset hasn't succesfully published. Look at ",
                "the `published` object."))
}))

rm(published)

