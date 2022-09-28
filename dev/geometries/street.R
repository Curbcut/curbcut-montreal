#### Import street network #####################################################

# This script loads street data from OSM and separates streets in edges.
# We should load data from OSM once in a while for updates, but this script
# is very computing intensive. 

# suppressPackageStartupMessages({
#   library(osmdata)
#   library(tidyverse)
#   library(sf)
#   library(qs)
# })
# 
# 
# # Bounding Box, Highway Key Values -----------------------------------------
# 
# # Bounding box of CMA Montreal
# CMA_mtl_bb <- c(-74.32797, 45.21754, -73.12856, 45.96849)
# 
# # Highway key values for cars
# kv_highway_cars <-
#   c("motorway", "trunk", "primary", "secondary", "tertiary", "residential",
#     "unclassified", "service", "motorway_link", "trunk_link", "primary_link",
#     "secondary_link", "tertiary_link")
# 
# # # Highway key values for people
# kv_highway_people <-
#   c("cycleway", "living_street", "pedestrian", "track", "road", "footway",
#     "path", "steps", "crossing")
# 
# 
# # Load and Save OSM Road ---------------------------------------------------
# 
# # Retrieve ALL OSM road features for Montreal
# street_network <-
#   CMA_mtl_bb %>%
#   opq(timeout = 200) %>%
#   # Retrieve features with the key "highway" which means street
#   add_osm_feature(key = "highway") %>%
#   # Get the data as sf
#   osmdata_sf()
# 
# # Cast "non-area" POLYGONS into LINESTRING and bind with existing LINESTRINGS
# street_network <-
#   street_network$osm_polygons %>%
#   filter(is.na(area) | area == 'no') %>%
#   st_set_agr("constant") %>%
#   st_cast("LINESTRING") %>%
#   bind_rows(street_network$osm_lines) %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   st_set_agr("constant") %>%
#   # latin1 encoding - weird accents. This line fixes it
#   mutate(name = textutils::HTMLdecode(name))
# 
# # Save file
# qsave(street_network, "dev/data/street_network.qs")
# 
# 
# # Filter results ----------------------------------------------------------
# 
# street_network <- qread("dev/data/street_network.qs")
# 
# street_network <-
#   street_network |>
#   st_filter(CSD)
# 
# street_network <-
#   street_network |>
#   filter(highway %in% kv_highway_cars)
# 
# rm(CMA_mtl_bb, kv_highway_cars, kv_highway_people)
# 
# 
# # Slicing streets ---------------------------------------------------------
# 
# library(future)
# plan(multisession)
# options(future.globals.maxSize = Inf)
# street_network <- st_transform(street_network, 32618)
# 
# # Make grid to parallelize calculations
# street_grid <- st_make_grid(street_network, n = c(16, 8))
# 
# # Do calculations
# street_list <-
#   furrr::future_map(street_grid, ~{
# 
#     street <-
#       street_network |>
#       select(osm_id, geometry) |>
#       st_filter(.x)
# 
#     if (nrow(street) > 0) {
#       nodes <-
#         street |>
#         st_intersection() |>
#         filter(st_is(geometry, "POINT"))
# 
#       if (nrow(nodes) > 0) {
#         street <-
#           street |>
#           lwgeom::st_split(nodes) |>
#           st_collection_extract("LINESTRING")
#       }
#     } else nodes <- NULL
# 
#     return(list(street, nodes))
# 
#   }, .progress = FALSE)
# 
# # Get street and nodes
# street <- map_dfr(street_list, `[[`, 1) |> distinct()
# nodes <- map_dfr(street_list, `[[`, 2)
# 
# # Check edges which crossed a grid boundary
# street_grid_edges <- st_cast(street_grid, "LINESTRING")
# edges_to_check <-
#   street_network |>
#   st_filter(street_grid_edges) |>
#   pull(osm_id)
# 
# # Re-split these edges
# new_street <-
#   street_network |>
#   select(osm_id, geometry) |>
#   filter(osm_id %in% edges_to_check) |>
#   lwgeom::st_split(nodes) |>
#   st_collection_extract("LINESTRING")
# 
# # Merge with other results
# street <-
#   street |>
#   filter(!osm_id %in% edges_to_check) |>
#   bind_rows(new_street)
# 
# street <-
#   street |>
#   rowid_to_column("ID") |>
#   select(ID, osm_id, geometry) |>
#   st_transform(4326)
# 
# # Add name_2 and street_type from original OSM download
# street <-
#   street_network |>
#   st_drop_geometry() |>
#   select(osm_id, name_2 = name, street_type = highway) |>
#   right_join(street, by = "osm_id") |>
#   relocate(name_2, street_type, .after = ID) |>
#   rename(osm_ID = osm_id) |>
#   st_as_sf() |>
#   st_set_agr("constant")
# 
# 
# # Add census metadata from DA ---------------------------------------------
# 
# street_DA <-
#   street |>
#   st_set_agr("constant") |>
#   st_transform(32618) |>
#   st_centroid() |>
#   st_nearest_feature(st_transform(DA, 32618))
# 
# DA_to_add <-
#   DA |>
#   st_drop_geometry() |>
#   select(DAUID = ID, CTUID, CSDUID, population, households) |>
#   slice(street_DA)
# 
# street <-
#   street |>
#   bind_cols(DA_to_add) |>
#   relocate(geometry, .after = last_col()) |>
#   st_set_agr("constant") |>
#   arrange(ID) |>
#   relocate(osm_ID, .after = CSDUID)
# 
# 
# # Add grid_ID -------------------------------------------------------------
# 
# street_grid <-
#   street |>
#   st_transform(32618) |>
#   st_set_agr("constant") |>
#   st_centroid() |>
#   st_join(st_transform(select(grid, grid_ID = ID, geometry), 32618)) |>
#   st_drop_geometry() |>
#   select(ID, grid_ID)
# 
# street <-
#   street |>
#   left_join(street_grid, by = "ID") |>
#   relocate(grid_ID, .after = osm_ID) |>
#   st_set_agr("constant")
# 
# qsave(street, file = "dev/data/street.qs")
# 
# rm(DA_to_add, new_street, nodes, street_grid, street_grid_edges, street_list,
#    street_network, edges_to_check, street_DA)


# Load data after processing ----------------------------------------------

street <- qread("dev/data/street.qs")


# Temporarily trim to island ----------------------------------------------

island_CSDUID <- 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072", "2466023")

street <- 
  street |> 
  filter(CSDUID %in% island_CSDUID)

rm(island_CSDUID)

