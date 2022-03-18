#### STREET TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
library(future)
library(furrr)
source("dev/tiles/tile_functions.R")
# Load raw street network download
street <- qread("dev/data/street_network.qs")

# Filter the street network -----------------------------------------------

street <-
  street |>
  filter(highway %in% c("motorway", "trunk", "primary",
                        "secondary", "tertiary",
                        "motorway_link", "trunk_link",
                        "primary_link", "residential", "service"))


# Filter columns ----------------------------------------------------------

streets <- qread("dev/tiles/texture/streets.qs")

# Upload street tile source -----------------------------------------------

# Street testing bbox
filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  bb <- sf::st_bbox(.data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
}
streets <-
  filter_sf(streets, -73.63, -73.56, 45.45, 45.51)

streets |>
  mutate(group = case_when(highway %in% c("motorway", "trunk", "primary") ~ "low",
                           highway %in% c("secondary", "tertiary",
                                          "motorway_link", "trunk_link",
                                          "primary_link", "residential") ~ "mid",
                           TRUE ~ "high")) |>
  select(group, street_type = highway) |>
  upload_tile_source("streets2", "maxbdb2", .sus_token)


# # LOW ---------------------------------------------------------------------
# 
# street |> 
#   filter(highway %in% c("motorway", "trunk", "primary")) |> 
#   select(osm_id, street_type = highway) |> 
#   upload_tile_source("street-low", "maxbdb2", .sus_token)
# 
# 
# # MID ---------------------------------------------------------------------
# 
# streets_to_process <- 
#   street |> 
#   filter(highway %in% c("motorway_link", "trunk_link",
#                         "primary_link", "residential",
#                         "secondary")) |> 
#   select(osm_id, street_type = highway) 
# 
# iter_size <- ceiling(nrow(streets_to_process) / 10)
# 
# street_to_process_list <- 
#   map(1:10, ~{
#     streets_to_process |> 
#       slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
#       geojsonsf::sf_geojson() |> 
#       paste0(collapse = " ") |> 
#       geojson::featurecollection()  
#   })
# 
# # Iteratively post files to tile source
# tmp <- tempfile(fileext = ".json")
# tmp_list <- map(1:10, ~tempfile(fileext = ".json"))
# 
# walk(1, ~{
#   
#   to_process <- street_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
#   walk2(to_process, tmp_list, geojson::ndgeo_write)
#   
#   # Concatenate geoJSONs
#   out <- paste0("copy ", paste(tmp_list, collapse = "+"), " ", tmp, "/Y")
#   system(out)
#   
#   # Upload to MTS
#   out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
#                 'maxbdb2/street-mid3?access_token=', .sus_token, 
#                 '" -F file=@', tmp, 
#                 ' --header "Content-Type: multipart/form-data"')
#   system(out)
#   
# })
# 
# 
# # HIGH --------------------------------------------------------------------
# 
# streets_to_process <- 
# street |> 
#   filter(!highway %in% c("motorway", "trunk", "primary",
#                          "motorway_link", "trunk_link",
#                          "primary_link", "residential",
#                          "secondary")) |> 
#   select(osm_id, street_type = highway)
# 
# iter_size <- ceiling(nrow(streets_to_process) / 10)
# 
# street_to_process_list <- 
#   map(1:10, ~{
#     streets_to_process |> 
#       slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
#       geojsonsf::sf_geojson() |> 
#       paste0(collapse = " ") |> 
#       geojson::featurecollection()  
#   })
# 
# # Iteratively post files to tile source
# tmp <- tempfile(fileext = ".json")
# tmp_list <- map(1:10, ~tempfile(fileext = ".json"))
# 
# walk(1, ~{
#   
#   to_process <- street_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
#   walk2(to_process, tmp_list, geojson::ndgeo_write)
#   
#   # Concatenate geoJSONs
#   out <- paste0("copy ", paste(tmp_list, collapse = "+"), " ", tmp, "/Y")
#   system(out)
#   
#   # Upload to MTS
#   out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
#                 'maxbdb2/street-high3?access_token=', .sus_token, 
#                 '" -F file=@', tmp, 
#                 ' --header "Content-Type: multipart/form-data"')
#   system(out)
#   
# })



# Clean-up ----------------------------------------------------------------

rm(streets)

