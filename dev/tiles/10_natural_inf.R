#### NATURAL INFRASTRUCTURE PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
natural_inf_tiles_raw <- qread("dev/data/natural_inf_tiles_raw.qs")
natural_inf_tiles <- qread("dev/data/natural_inf_tiles.qs")
source("dev/tiles/_tile_functions.R")



# Upload tile sources -----------------------------------------------------

for (i in seq_len(length(natural_inf_tiles_raw))) {
  
  delete_tileset_source(paste0(
    "natural_inf-", names(natural_inf_tiles_raw)[[i]]))
  
  natural_inf_tiles_raw[[i]] |>
    mutate(across(where(is.numeric), as.character)) |>
    st_set_agr("constant") |>
    upload_tile_source(paste0(
      "natural_inf-", names(natural_inf_tiles_raw)[[i]]))
}



# Create recipes ----------------------------------------------------------

natural_inf_recipe <- imap(natural_inf_tiles_raw, \(cat_data, cat) {
  
  create_recipe(
    layer_names = c(paste0(cat, "_a"),
                    paste0(cat, "_b"),
                    paste0(cat, "_c")),
    source = c(
      setNames(nm = paste0(cat, "_a"), paste0(
        "mapbox://tileset-source/sus-mcgill/natural_inf-", cat)),
      setNames(nm = paste0(cat, "_b"), paste0(
        "mapbox://tileset-source/sus-mcgill/natural_inf-", cat)),
      setNames(nm = paste0(cat, "_c"), paste0(
        "mapbox://tileset-source/sus-mcgill/natural_inf-", cat))),
    minzoom = c(setNames(nm = paste0(cat, "_a"), 3),
                setNames(nm = paste0(cat, "_b"), 9),
                setNames(nm = paste0(cat, "_c"), 11)),
    maxzoom = c(setNames(nm = paste0(cat, "_a"), 8),
                setNames(nm = paste0(cat, "_b"), 10),
                setNames(nm = paste0(cat, "_c"), 15)),
    layer_size = c(setNames(nm = paste0(cat, "_a"), 2500),
                   setNames(nm = paste0(cat, "_b"), 2500),
                   setNames(nm = paste0(cat, "_c"), 2500)),
    simp_zoom = c(setNames(nm = paste0(cat, "_a"), NA),
                  setNames(nm = paste0(cat, "_b"), 10),
                  setNames(nm = paste0(cat, "_c"), 15)),
    simp_value = c(setNames(nm = paste0(cat, "_a"), NA),
                   setNames(nm = paste0(cat, "_b"), 1),
                   setNames(nm = paste0(cat, "_c"), 1)),
    fallback_simp_zoom = c(setNames(nm = paste0(cat, "_a"), NA),
                           setNames(nm = paste0(cat, "_b"), 1),
                           setNames(nm = paste0(cat, "_c"), 4)),
    recipe_name = paste0("natural_inf-", cat))
  
})
  

# Create tilesets ---------------------------------------------------------


for (i in seq_len(length(natural_inf_tiles_raw))) {
  create_tileset(paste0("natural_inf-", names(natural_inf_tiles_raw)[[i]]), 
                 natural_inf_recipe[[i]])
  Sys.sleep(5)
}
  

# Publish tilesets --------------------------------------------------------

for (i in seq_len(length(natural_inf_tiles_raw))) {
  publish_tileset(paste0("natural_inf-", names(natural_inf_tiles_raw)[[i]]))
  Sys.sleep(15)
}



# Tileset for custom priorities -------------------------------------------

natural_inf_tiles <- 
  natural_inf_tiles |> 
  mutate(ID = as.character(ID))

iter_size <- ceiling(nrow(natural_inf_tiles) / 100)

union_to_process_list <- 
  map(1:100, ~{
    natural_inf_tiles |> 
      slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
      geojsonsf::sf_geojson() |> 
      paste0(collapse = " ") |> 
      geojson::featurecollection()  
  })

# Iteratively post files to tile source
tmp <- tempfile(fileext = ".json")
tmp_list <- map(1:10, ~tempfile(fileext = ".json"))

map(1:10, ~{

  to_process <- union_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
  walk2(to_process, tmp_list, geojson::ndgeo_write)
  
  # Concatenate geoJSONs
  out_file <- file(tmp, "w")
  for (i in tmp_list){
    x <- readLines(i)
    writeLines(x, out_file) 
  }
  close(out_file)

  # Upload to MTS
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/',
                'sus-mcgill/natural_inf-custom?access_token=', .sus_token,
                '" -F file=@', tmp,
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})

natural_inf_recipe <-
  create_recipe(
    layer_names = "natural_inf-custom",
    source = paste0("mapbox://tileset-source/sus-mcgill/natural_inf-custom"),
    minzoom = 3,
    maxzoom = 14,
    layer_size = 2500,
    recipe_name = "natural_inf-custom")

create_tileset("natural_inf-custom", natural_inf_recipe)
publish_tileset("natural_inf-custom")
