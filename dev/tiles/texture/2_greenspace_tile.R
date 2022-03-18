#### GREEN SPACE TILE PROCESSING ###############################################

suppressPackageStartupMessages({library(osmdata)})


# Retrieve green spaces ---------------------------------------------------

# # Bounding box of CMA Montreal
# CMA_mtl_bb <- c(-74.32797, 45.21754, -73.12856, 45.96849)
# 
# # Retrieve ALL OSM leisure features for Montreal
# parks <-
#   CMA_mtl_bb %>%
#   opq(timeout = 200) %>%
#   add_osm_feature(key = "leisure") %>%
#   osmdata_sf() |>
#   pluck("osm_polygons")
# 
# parks <-
#   parks |>
#   st_cast("POLYGON") |>
#   as_tibble() |>
#   st_as_sf() |>
#   st_set_agr("constant")
# 
# parks <-
#   parks |> filter(leisure != "nature_reserve",
#                   !is.na(leisure)) |>
#   select(name)
# 
# qsave(parks, file = "dev/tiles/texture/parks.qs")

parks <- qread("dev/tiles/texture/parks.qs")

# Upload green space tile source ------------------------------------------

parks |> 
  upload_tile_source("parks", "maxbdb2", .sus_token)


# Clean-up ----------------------------------------------------------------

rm(parks)
