#### Import street network #####################################################

# This script loads street data from OSM and separates streets in edges.
# We should load data from OSM once in a while for updates, but this script
# is very computing intensive. 


# Bounding Box, Highway Key Values -----------------------------------------

# library(osmdata)
# 
# # Bounding box of CMA Montreal
# CMA_mtl_bb <- c(-74.32797, 45.21754, -73.12856, 45.96849)
# 
# # Highway key values for Cars
# kv_highway_cars <- c("motorway", "trunk", "primary", "secondary", "tertiary",
#                      "residential", "unclassified", "service", "motorway_link",
#                      "trunk_link", "primary_link", "secondary_link", "tertiary_link")
# 
# # # Highway key values for People
# kv_highway_people <- c("cycleway", "living_street", "pedestrian", "track", "road",
#                        "footway", "path", "steps", "crossing")
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
#   filter(is.na(area) | area=='no') %>%
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
# # Slicing streets ---------------------------------------------------------
# 
# # load the previously saved data
# street_network <- qread("dev/data/street_network.qs")
# 
# library(dodgr)
# 
# # Street slicing
# net <-
#   weight_streetnet(street_network, keep_cols = c("osm_id", "name")) %>%
#   data.frame()
# 
# # Need to convert to SF in order to put in the st_nn
# street_edges <-
#   net %>% 
#   as_tibble() %>% 
#   rowwise() %>% 
#   mutate(from_point = st_sfc(list(st_point(c(from_lon, from_lat)))),
#          to_point = st_sfc(list(st_point(c(to_lon, to_lat)))),
#          multi_point = st_union(from_point, to_point)) %>% 
#   ungroup() %>% 
#   mutate(geometry = st_cast(multi_point, "LINESTRING")) %>% 
#   transmute(ID = edge_id, name_2 = name, street_type = highway, geometry) %>% 
#   st_as_sf(crs = 4326)
# 
# qsave(street_edges, "dev/data/streets.qs")

streets <- qread("dev/data/streets.qs")