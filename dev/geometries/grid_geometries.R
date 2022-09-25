#### Import grid geometries ####################################################
# Independent script 

grid <-
  read_sf("dev/data/climate_shp/Vulnerabilité_secheresses_2016.shp") %>% 
  st_zm() %>% 
  st_make_valid() %>% 
  st_transform(32618) %>% 
  transmute(ID = as.character(seq_along(geometry))) %>% 
  filter(units::drop_units(st_area(geometry)) > 10) %>% 
  distinct(geometry, .keep_all = TRUE) %>% 
  st_transform(4326) %>% 
  st_set_agr("constant")

# # # Make sure the grid only covers land, to not cause further issues with future
# # # interpolation. 
# suppressPackageStartupMessages({library(osmdata)})
# water_grid_osm <-
#   st_bbox(grid) |>
#   opq(timeout = 200) |>
#   add_osm_feature(key = "water") |>
#   osmdata_sf()
# 
# water_grid_osm <-
# rbind(water_grid_osm$osm_polygons |>
#         select(osm_id),
#       water_grid_osm$osm_multipolygons |>
#         select(osm_id)) |> 
#   filter(osm_id %in% c("5363267", "20915538", "129896286", "1808142", 
#                        "1775822", "20916725", "1769291"))
# 
# grid <- 
#   grid |> 
#   st_difference(st_make_valid(st_union(water_grid_osm)))
# 
# grid |> 
#   group_by(ID) |> 
#   st_combine() |> 
#   ungroup()
# 
# ?st_combine
# 
# grid_raw |> 
#   filter(!ID %in% grid$ID) |> mapview::mapview()
