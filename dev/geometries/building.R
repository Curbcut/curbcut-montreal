#### Construct building table ##################################################
# Needs to be run after `borough_geometries.R`

# OSM buildings -----------------------------------------------------------

suppressPackageStartupMessages({library(osmdata)})

# Bounding box of CMA Montreal
master_polygon_bbox <- st_bbox(master_polygon)

# Retrieve ALL OSM building features for Montreal
building_osm <-
  master_polygon_bbox |> 
  opq(timeout = 200) |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()

rm(master_polygon_bbox)
qsave(building_osm, file = "dev/data/building_osm.qs")

building_osm <- qread("dev/data/building_osm.qs",
                      nthreads = future::availableCores())

building <-
  map_dfr(list(building_osm$osm_polygons, building_osm$osm_multipolygons,
               building_osm$osm_multilines), ~suppressWarnings({
                 .x |>
                   as_tibble() |>
                   st_as_sf() |>
                   select(osm_ID = osm_id, geometry) |>
                   st_cast("MULTIPOLYGON") |>
                   st_make_valid()})) |>
  st_transform(32618) |>
  distinct(osm_ID, .keep_all = TRUE) |>
  st_cast("MULTIPOLYGON") |>
  mutate(ID = as.character(seq_along(osm_ID)), .before = osm_ID)

# Get centroids for self-intersection
building_centroid <-
  building |>
  st_set_agr("constant") |>
  st_centroid()

# Find self-intersections
self_intersects <- st_intersects(building_centroid, building)
to_merge <- self_intersects[lengths(self_intersects) > 1]

# Reducer function
reduce <- function(x) {

  Reduce(function(a, b) {
    merge_index <- lapply(a, intersect, b)

    if (sum(lengths(merge_index)) > 0) {
      merge_index <- which(lengths(merge_index) > 0)
      merged <- a[merge_index]
      merged <- unlist(merged)
      merged <- union(merged, b)
      merged <- list(sort(merged))
      not_merged <- a[-merge_index]
      out <- c(merged, not_merged)
    } else out <- c(a, list(b))
  }, x, init = list())

}

# Get reduced lists of intersections
merged <- reduce(to_merge)

# Get buildings that will not be merged
building_remaining <-
  building |>
  filter(!ID %in% unlist(merged))

# Merge buildings that have self-intersection
building_merged <-
  map_dfr(merged, ~{
    building |>
      filter(ID %in% .x) |>
      summarize(ID = ID[1], osm_ID = osm_ID[1])
  })

# Bind everything together
building <-
  building_merged |>
  st_cast("MULTIPOLYGON") |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  bind_rows(building_remaining) |>
  st_set_agr("constant")

# Get centroids again
building_centroid <-
  building |>
  st_centroid()

# Get IDs of buildings whose centroids intersect a DA
building_cma_id <-
  building_centroid |>
  st_filter(st_transform(DA, 32618)) |>
  pull(ID)

# Get DA information for those centroids
building_DA <-
  building_centroid |>
  filter(ID %in% building_cma_id) |>
  st_join(DA |> select(DAUID, name_2:CSDUID) |> st_transform(32618)) |>
  st_drop_geometry()

# Filter building to only ones whose centroids intersect a DA
building <-
  building |>
  filter(ID %in% building_cma_id) |>
  inner_join(building_DA, by = c("ID", "osm_ID")) |>
  select(ID, DAUID:CSDUID, osm_ID, geometry)

# Consolidate and clean up output
building <-
  building |>
  select(ID, name_2, DAUID, CTUID, CSDUID, osm_ID, geometry) |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  st_set_agr("constant")

rm(building_centroid, building_DA, building_merged, building_remaining,
   building_osm, merged, self_intersects, to_merge, building_cma_id, reduce)


# Get MS buildings --------------------------------------------------------

ms_building <- geojsonsf::geojson_sf("dev/data/Quebec.geojson")

# Only keep polygons which intersect with DAs and don't intersect with buildings
ms_building <-
  ms_building |>
  st_transform(32618) |>
  st_filter(st_transform(DA, 32618)) |>
  filter(lengths(st_intersects(geometry, building)) == 0)

# Drop very small polygons and add temporary ID
ms_building <-
  ms_building |>
  filter(units::drop_units(st_area(geometry)) > 10) |>
  st_cast("MULTIPOLYGON") |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  mutate(ID = as.character(seq_along(geometry)), .before = geometry) |>
  as_tibble() |>
  st_as_sf()

# Get centroids
ms_centroid <-
  ms_building |>
  st_set_agr("constant") |>
  st_centroid()

# Get IDs of centroids which touch DAs
ms_cma_id <-
  ms_centroid |>
  st_filter(st_transform(DA, 32618)) |>
  pull(ID)

# Spatial join centroids to DAs
ms_DA <-
  ms_centroid |>
  filter(ID %in% ms_cma_id) |>
  st_join(DA |> select(DAUID, name_2:CSDUID) |> st_transform(32618)) |>
  st_drop_geometry()

# Add DAUID to buildings
ms_building <-
  ms_building |>
  filter(ID %in% ms_cma_id) |>
  inner_join(ms_DA, by = "ID")

# Finish ms_building
ms_building <-
  ms_building |>
  select(ID, name_2, DAUID, CTUID, CSDUID, geometry) |>
  st_make_valid() |>
  filter(!st_is_empty(geometry))

rm(ms_centroid, ms_DA, ms_cma_id)


# Bring building and ms_building together ---------------------------------

# Bind rows and remove small polygons
building <-
  building |>
  bind_rows(ms_building) |>
  filter(units::drop_units(st_area(geometry)) > 10) |>
  mutate(ID = as.character(seq_along(ID))) |>
  st_transform(4326) |>
  st_set_agr("constant") |> 
  mutate(geometry = st_make_valid(geometry))


# Add grid_ID -------------------------------------------------------------

building_grid <-
  building |>
  st_transform(32618) |>
  st_centroid() |>
  st_join(st_transform(select(grid, grid_ID = ID, geometry), 32618)) |>
  st_drop_geometry() |>
  select(ID, grid_ID)

building <-
  building |>
  left_join(building_grid, by = "ID") |>
  relocate(grid_ID, .after = osm_ID)


# Clean up ----------------------------------------------------------------

# Save pre-geocoding output
qsave(building, file = "dev/data/building.qs",
      nthreads = future::availableCores())

rm(building_grid, ms_building)

building <- qread("dev/data/building.qs", nthreads = future::availableCores())
