#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
library(sf)
x <- lapply(list.files("dev/data_import", full.names = TRUE), source, verbose = FALSE)

# Base of the study region and dictionaries -------------------------------

# All regions
all_tables <-
  list("CMA" = c("CSD", "CT", "DA", "building"),
       "island" = c("CSD", "CT", "DA", "building"),
       "city" = c("CSD", "CT", "DA", "building"),
       "centraide" = c("centraide", "CT", "DA", "building"),
       "cmhc" = c("cmhczone"),
       "grid" = c("grid250", "grid100", "grid50", "grid25"))


# List all the regions geometries to create the master polygon
cancensus_cma_code <- 24462
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 2466023),
                    island = "dev/data/geometry/island.shp",
                    centraide = "dev/data/geometry/centraide_2023.shp",
                    cmhc = get_cmhc_zones(list(CMA = cancensus_cma_code)),
                    grid = "dev/data/climate_risk/vulnerabilite-changements-climatiques-mailles-2022.shp")

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_tables = all_tables,
    region = c("CMA", "island", "city", "centraide", "cmhc", "grid"),
    name = c(CMA = "Metropolitan Area",
             island = "Island of Montreal",
             city = "City of Montreal",
             centraide = "Centraide of Greater Montreal",
             cmhc = "Canada Mortgage and Housing Corporation zones",
             grid = "250-m"),
    to_compare = c(CMA = "in the Montreal region",
                   island = "on the island of Montreal",
                   city = "in the City of Montreal",
                   centraide = "in the Centraide of Greater Montreal territory",
                   cmhc = "in the Montreal region",
                   grid = "on the island of Montreal"),
    to_compare_determ = c(CMA = "the Montreal region",
                   island = "the island of Montreal",
                   city = "the City of Montreal",
                   centraide = "the Centraide of Greater Montreal territory",
                   cmhc = "the Montreal region",
                   grid = "the island of Montreal"),
    to_compare_short = c(CMA = "in the region",
                   island = "on the island",
                   city = "in the City",
                   centraide = "in the territory",
                   cmhc = "in the region",
                   grid = "on the island"),
    pickable = c(CMA = TRUE,
                 island = TRUE,
                 city = TRUE,
                 centraide = TRUE,
                 cmhc = FALSE,
                 grid = FALSE))


# Build scales ------------------------------------------------------------

### Build census scales
census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      levels = c("CSD", "CT", "DA"),
                      crs = crs)
# Switch the City of Montreal for the boroughs
boroughs <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")
boroughs$type <- "Borough"
census_scales$CSD <- split_scale(destination = census_scales$CSD,
                                 cutting_layer = boroughs,
                                 DA_table = census_scales$DA,
                                 crs = crs)
# Switch the City of Laval for the Sector
laval <- sf::st_read(paste0("dev/data/centraide/shapefiles/",
                            "Secteurs_damenagement_Ville_de_Laval.shp")) |>
  sf::st_transform(32618)
laval$name <- gsub("Secteur \\d - ", "", laval$Secteur)
laval$type <- "Sector"
laval <- laval[c("name", "type")]

census_scales$CSD <- cc.buildr::split_scale(destination = census_scales$CSD,
                                            cutting_layer = laval,
                                            DA_table = census_scales$DA,
                                            destination_pct_threshold = 0.9,
                                            crs = crs)
# Create the census scales dictionary
scales_dictionary <- census_scales_dictionary(census_scales)
# Switch the CSD scale for borough/city
scales_dictionary[1, ] <- list(scale = "CSD",
                               sing = "borough/city",
                               plur = "boroughs or cities",
                               slider_title = "Borough/City",
                               place_heading = "{name_2} of {name}",
                               place_name = "{name}")

### Build building scale
# # From MySQL
# building <- cc.data::db_read_long_table(table = "buildings",
#                                          DA_ID = census_scales$DA$ID)
# qs::qsave(building, file = "dev/data/built/building.qs")
# # From Local
# building <- qs::qread("dev/data/canada_buildings.qs")
# building <- building[building$DA_ID %in% census_scales$DA$ID, ]
# building <- qs::qsave(building, "dev/data/built/building.qs")
building <- qs::qread("dev/data/built/building.qs")

# Add building scale to the dictionary
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "building",
                             sing = "building",
                             plur = "buildings",
                             slider_title = "Building",
                             place_heading = "{name}",
                             place_name = "{name}")

### Build CMHC scale
cmhczone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhczone <- additional_scale(additional_table = cmhczone,
                             DA_table = census_scales$DA,
                             ID_prefix = "cmhc",
                             name_2 = "CMHC zone",
                             crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhczone",
                             sing = "CMHC zone",
                             plur = "CMHC zones",
                             slider_title = "CMHC zone",
                             place_heading = "{name}",
                             place_name = "{name}")

### Build Centraide scale
centraide <- sf::st_read("dev/data/geometry/centraide_2023.shp")
centraide <- additional_scale(additional_table = centraide,
                              DA_table = census_scales$DA,
                              ID_prefix = "centraide",
                              name_2 = "Centraide zone",
                              crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "centraide",
                             sing = "centraide zone",
                             plur = "centraide zones",
                             slider_title = "Centraide zone",
                             place_heading = "Centraide zone of {name}",
                             place_name = "{name}")

### Build 25-m grid scale
# grid <-
#   sf::st_read("dev/data/climate_risk/vulnerabilite-changements-climatiques-mailles-2022.shp")
# grid <- grid[, c("PageName", "geometry")]
# names(grid)[1] <- "ID"
# grid$ID <- paste0("grid25_", grid$ID)
# grid <- rev_geocode_sf(master_polygon = grid,
#                        sf_df = grid,
#                        province_code = "QC",
#                        crs = crs)
# grid_geocode <- sf::st_transform(grid, 4326)
# grid_geocode <- sf::st_centroid(grid_geocode)
# progressr::with_progress({
#   pb <- progressr::progressor(nrow(grid_geocode))
#   grid$name <- future.apply::future_sapply(grid_geocode$geometry, \(x) {
#     pb()
#     cc.data::rev_geocode_localhost(point_sf = x)
#   }, future.seed = NULL)
# })
# grid <- grid[, c("ID", "name", "geometry")]
# qs::qsave(grid, file = "dev/data/built/grid.qs")
# # Append DA IDs
# grid <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                 df = grid, crs = crs)
# grid25 <- qs::qread("dev/data/built/grid.qs")
#
# grid50_geometry <- sf::st_make_grid(sf::st_transform(grid25, 2950),
#                                       cellsize = c(50, 50),
#                                       crs = 2950)
# grid50 <- tibble::tibble(ID = seq_along(grid50_geometry))
# grid50 <- tibble::as_tibble(cbind(grid50, grid50_geometry))
# grid50 <- sf::st_as_sf(grid50)
# centroid <- sf::st_point_on_surface(sf::st_transform(grid25, 2950))
# index <- sf::st_intersects(centroid, grid50)
# centroid$grid50 <- unlist(index)
# grouped <- cbind(grid25, sf::st_drop_geometry(centroid["grid50"]))
#
# grid50 <- aggregate(grouped["grid50"], by = list(grouped$grid50),
#                       FUN = function(x) length(unique(x)))
# grid50 <- sf::st_transform(grid50, crs = crs)
# grid50 <- grid50["geometry"]
# grid50$ID <- paste0("grid50_", seq_along(grid50$geometry))
# grid_geocode <- sf::st_transform(grid50, 4326)
# grid_geocode <- sf::st_centroid(grid_geocode)
# progressr::with_progress({
#   pb <- progressr::progressor(nrow(grid_geocode))
#   grid50$name <- future.apply::future_sapply(grid_geocode$geometry, \(x) {
#     pb()
#     cc.data::rev_geocode_localhost(point_sf = x)
#   }, future.seed = NULL)
# })
# grid50 <- grid50[, c("ID", "name", "geometry")]
# # Append DA IDs
# grid50 <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                     df = grid50, crs = crs)
# qs::qsave(grid50, file = "dev/data/built/grid50.qs")
#
# grid100_geometry <- sf::st_make_grid(sf::st_transform(grid25, 2950),
#                                       cellsize = c(100, 100),
#                                       crs = 2950)
# grid100 <- tibble::tibble(ID = seq_along(grid100_geometry))
# grid100 <- tibble::as_tibble(cbind(grid100, grid100_geometry))
# grid100 <- sf::st_as_sf(grid100)
# centroid <- sf::st_point_on_surface(sf::st_transform(grid25, 2950))
# index <- sf::st_intersects(centroid, grid100)
# centroid$grid100 <- unlist(index)
# grouped <- cbind(grid25, sf::st_drop_geometry(centroid["grid100"]))
#
# grid100 <- aggregate(grouped["grid100"], by = list(grouped$grid100),
#                       FUN = function(x) length(unique(x)))
# grid100 <- sf::st_transform(grid100, crs = crs)
# grid100 <- grid100["geometry"]
# grid100$ID <- paste0("grid100_", seq_along(grid100$geometry))
# grid_geocode <- sf::st_transform(grid100, 4326)
# grid_geocode <- sf::st_centroid(grid_geocode)
# progressr::with_progress({
#   pb <- progressr::progressor(nrow(grid_geocode))
#   grid100$name <- future.apply::future_sapply(grid_geocode$geometry, \(x) {
#     pb()
#     cc.data::rev_geocode_localhost(point_sf = x)
#   }, future.seed = NULL)
# })
# grid100 <- grid100[, c("ID", "name", "geometry")]
# # Append DA IDs
# grid100 <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                 df = grid100, crs = crs)
# qs::qsave(grid100, file = "dev/data/built/grid100.qs")
#
# grid250_geometry <- sf::st_make_grid(sf::st_transform(grid25, 2950),
#                                       cellsize = c(250, 250),
#                                       crs = 2950)
# grid250 <- tibble::tibble(ID = seq_along(grid250_geometry))
# grid250 <- tibble::as_tibble(cbind(grid250, grid250_geometry))
# grid250 <- sf::st_as_sf(grid250)
#
# centroid <- sf::st_point_on_surface(sf::st_transform(grid25, 2950))
# index <- sf::st_intersects(centroid, grid250)
#
# centroid$grid250 <- unlist(index)
# grouped <- cbind(grid25, sf::st_drop_geometry(centroid["grid250"]))
#
# grid250 <- aggregate(grouped["grid250"], by = list(grouped$grid250),
#                       FUN = function(x) length(unique(x)))
# grid250 <- sf::st_transform(grid250, crs = crs)
# grid250 <- grid250["geometry"]
# grid250$ID <- paste0("grid250_", seq_along(grid250$geometry))
# grid_geocode <- sf::st_transform(grid250, 4326)
# grid_geocode <- sf::st_centroid(grid_geocode)
# progressr::with_progress({
#   pb <- progressr::progressor(nrow(grid_geocode))
#   grid250$name <- future.apply::future_sapply(grid_geocode$geometry, \(x) {
#     pb()
#     cc.data::rev_geocode_localhost(point_sf = x)
#   }, future.seed = NULL)
# })
# grid250 <- grid250[, c("ID", "name", "geometry")]
#
# # Add census info (population / households) to the larger grid
# grid250 <- additional_scale(additional_table = grid250,
#                             DA_table = census_scales$DA,
#                             ID_prefix = "",
#                             name_2 = "250-m",
#                             crs = crs)
# grid250$ID <- gsub("^_", "", grid250$ID)
# # Append DA IDs
# grid250 <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                     df = grid250, crs = crs)
# qs::qsave(grid250, file = "dev/data/built/grid250.qs")
#
# qs::qsavem(grid25, grid50, grid100, grid250, file = "dev/data/built/grids.qsm")
qs::qload("dev/data/built/grids.qsm")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid25",
                             sing = "area at the 25m scale",
                             plur = "areas at the 25m scale",
                             slider_title = "25m",
                             place_heading = "{name}",
                             place_name = "the 25m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid50",
                             sing = "area at the 50m scale",
                             plur = "areas at the 50m scale",
                             slider_title = "50m",
                             place_heading = "{name}",
                             place_name = "the 50m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid100",
                             sing = "area at the 100m scale",
                             plur = "areas at the 100m scale",
                             slider_title = "100m",
                             place_heading = "{name}",
                             place_name = "the 100m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid250",
                             sing = "area at the 250m scale",
                             plur = "areas at the 250m scale",
                             slider_title = "250m",
                             place_heading = "{name}",
                             place_name = "the 250m grid area around {name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(building = building),
                list(centraide = centraide),
                list(cmhczone = cmhczone),
                list(grid250 = grid250),
                list(grid100 = grid100),
                list(grid50 = grid50),
                list(grid25 = grid25))


scales_consolidated <- consolidate_scales(all_tables = all_tables,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs,
                                          match_with_centroids_regions = "grid")


# Verify conformity -------------------------------------------------------

verify_dictionaries(all_tables = all_tables,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)

qs::qsavem(census_scales, scales_variables_modules, crs,
          scales_dictionary, regions_dictionary, all_tables, base_polygons,
          cancensus_cma_code, scales_consolidated, all_scales,
          file = "dev/data/built/empty_scales_variables_modules.qsm")
qs::qload("dev/data/built/empty_scales_variables_modules.qsm")

# Build the datasets ------------------------------------------------------

future::plan(future::multisession, workers = 4)
                  # future::tweak(future::multisession,
                                # workers = length(cc.data::census_years))))

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE,
                 skip_scale_interpolation = c("grid25", "grid50", "grid100"))
census_variables <- get_census_vectors_details()

save.image("dev/data/built/pre_census.RData")
load("dev/data/built/pre_census.RData")

future::plan(future::multisession(), workers = 6)
scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              crs = crs, geo_uid = cancensus_cma_code)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         crs = crs)

scales_variables_modules <- 
  ba_ndvi(scales_variables_modules = scales_variables_modules, 
          master_polygon = base_polygons$master_polygon, 
          all_scales = all_scales, data_output_path = "dev/data/ndvi/", 
          skip_scales = c("grid25", "grid50", "grid100", "grid250"), 
          crs = crs)

# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

future::plan(future::multisession(), workers = 2)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes,
                          crs = crs)

save.image("dev/data/built/svm_after_access.qs")
load("dev/data/built/svm_after_access.qs")

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))

# Additional access variables
scales_variables_modules <-
  build_and_append_access(scales_variables_modules = scales_variables_modules,
                          DA_table = census_scales$DA,
                          traveltimes = traveltimes,
                          crs = crs)

# Montreal specific modules
save.image("dev/data/built/before_mtl.RData")
load("dev/data/built/before_mtl.RData")

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))

future::plan(future::multisession(), workers = 3)

scales_variables_modules <-
  build_and_append_climate_risk(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

scales_variables_modules <-
  build_and_append_natural_inf(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

scales_variables_modules <-
  build_and_append_alley(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

scales_variables_modules <- 
  build_and_append_crash(
    scales_variables_modules = scales_variables_modules,
    crs = crs
  )

save.image("dev/data/built/before_centraide.RData")
load("dev/data/built/before_centraide.RData")
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))


scales_variables_modules <-
  build_and_append_tenure(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

future::plan(future::multisession(), workers = 3)
scales_variables_modules <-
  build_and_append_afford_pop(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

scales_variables_modules <-
  build_and_append_afford_hou(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

# Post process
scales_variables_modules$scales <- 
  cc.buildr::post_processing(scales = scales_variables_modules$scales)

qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           scales_dictionary, regions_dictionary, all_tables, base_polygons,
           all_scales,
           file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

# postal_codes <- build_postal_codes(census_scales$DA$ID)
# postal_codes <- sf::st_drop_geometry(postal_codes)
# qs::qsave(postal_codes, "data/postal_codes.qs")


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  all_tables = all_tables,
  zoom_levels = list(first = 0, CT = 10, DA = 12, building = 16, 
                     grid100 = 11, grid50 = 13, grid25 = 14))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "CMA",
    suffix = "max_CT",
    content = c("CSD" = 0, "CT" = 10))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "city",
    suffix = "max_CT",
    content = c("CSD" = 0, "CT" = 10))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "island",
    suffix = "max_CT",
    content = c("CSD" = 0, "CT" = 10))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "centraide",
    suffix = "max_CT",
    content = c("centraide" = 0, "CT" = 10))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# Tilesets ----------------------------------------------------------------

# # Prepare by getting the census scales with geometries which spans over water
# # Build census scales
# full_census_scales <-
#   build_census_scales(master_polygon = base_polygons$master_polygon,
#                       regions = base_polygons$province_cancensus_code,
#                       levels = c("CSD", "CT", "DA"),
#                       crs = crs, 
#                       switch_full_geos = TRUE)
# # Switch the City of Montreal for the boroughs
# boroughs <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")
# boroughs$type <- "Borough"
# full_census_scales$CSD <- split_scale(destination = full_census_scales$CSD,
#                                       cutting_layer = boroughs,
#                                       DA_table = census_scales$DA,
#                                       crs = crs)
# # Switch the City of Laval for the Sector
# laval <- sf::st_read(paste0("dev/data/centraide/shapefiles/",
#                             "Secteurs_damenagement_Ville_de_Laval.shp")) |>
#   sf::st_transform(32618)
# laval$name <- gsub("Secteur \\d - ", "", laval$Secteur)
# laval$type <- "Sector"
# laval <- laval[c("name", "type")]
# 
# full_census_scales$CSD <- cc.buildr::split_scale(destination = full_census_scales$CSD,
#                                                  cutting_layer = laval,
#                                                  DA_table = census_scales$DA,
#                                                  destination_pct_threshold = 0.9,
#                                                  crs = crs)
# 
# qs::qsave(full_census_scales,
#            file = "dev/data/built/full_census_scales.qs")
# full_census_scales <- qs::qload("dev/data/built/full_census_scales.qs")
# 
# 
# # Do not upload grids, as there is a function just for it.
# all_scales_t <- scales_variables_modules$scales[names(scales_variables_modules$scales) != "grid"]
# map_zoom_levels_t <- map_zoom_levels[names(map_zoom_levels) != "grid"]
# 
# # Before loading the tilesets, switch the geometries.
# all_scales_t <- cc.buildr::map_over_scales(
#   all_scales_t, 
#   fun = \(geo = geo, scales = scales,
#           scale_name = scale_name, scale_df = scale_df) {
#     
#     # If it'S not census, return raw
#     if (!scale_name %in% names(full_census_scales)) return(scale_df)
#     
#     df <- sf::st_drop_geometry(scale_df)
#     merge(df, full_census_scales[[scale_name]]["ID"], by = "ID", all.x = TRUE,
#           all.y = FALSE)
#     
#   })


# tileset_upload_all(all_scales = all_scales_t,
#                    map_zoom_levels = map_zoom_levels_t,
#                    prefix = "mtl",
#                    username = "curbcut",
#                    access_token = .cc_mb_token)
# 
# source("dev/tiles/grid_tiles.R")
# tileset_upload_grid(region = "grid",
#                     all_scales = scales_variables_modules$scales,
#                     map_zoom_levels = map_zoom_levels,
#                     max_zoom = list(grid250 = 13, grid100 = 14, grid50 = 15, grid25 = 16),
#                     vars = c("climate_drought", "climate_flood", "climate_destructive_storms",
#                              "climate_heat_wave", "climate_heavy_rain"),
#                     prefix = "mtl",
#                     username = "curbcut",
#                     access_token = .cc_mb_token)


# Place explorer page ----------------------------------------------------

# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = "Explorer",
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "<p>Select a location by entering a postal code or clicking on the ",
               "map and see how it compares to the rest of the Montreal region ",
               "or island across a variety of sustainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             regions = regions_dictionary$region[regions_dictionary$pickable])

# Did you know ------------------------------------------------------------

# variables <- scales_variables_modules$variables
# source("dev/other/dyk.R")
# qs::qsave(dyk, "data/dyk.qs")


# Produce colours ---------------------------------------------------------

colours_dfs <- cc.buildr::build_colours()

# Add natural inf data colours
colours_dfs$viridis_25 <- 
  tibble::tibble(group = as.character(26:50),
                 fill = scales::viridis_pal()(25))
qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# # TKTK MAKE SURE YOU HAVE THIS VERSION OF LEAFLET, IF NOT THE MAPS IN THE HTML
# # DOCUMENTS WON'T BE INTERACTIVES:
# # devtools::install_github("dmurdoch/leaflet@crosstalk4")
stories <- build_stories()
qs::qsave(stories, file = "data/stories.qs")
# stories_create_tileset(stories = stories,
#                        prefix = "mtl",
#                        username = "curbcut",
#                        access_token = .cc_mb_token)
# cc.buildr::resize_image(folder = "www/stories/photos/", max_size_in_MB = 1)


# Add Montréal stories
scales_variables_modules$modules <-
  scales_variables_modules$modules |>
  add_module(
    id = "stories",
    theme = "Urban life",
    nav_title = "Montreal stories",
    title_text_title = "Montreal stories",
    title_text_main = paste0(
      "<p>Explore stories about urban sustainability and planning in Montreal. Learn ",
      "about stories rooted in specific geographic locations or those that ",
      "have an impact on the whole city."),
    title_text_extra = paste0(
      "<p>These narrative case studies are written by the Curbcut team and its contributors."),
    metadata = FALSE,
    dataset_info = ""
  )


# Translation -------------------------------------------------------------

variables <- scales_variables_modules$variables
modules <- scales_variables_modules$modules
source("dev/translation/build_translation.R", encoding = "utf-8")


# Home page ---------------------------------------------------------------

home_page()


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")

# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                       all_scales = scales_variables_modules$scales,
                       variables = scales_variables_modules$variables)


# Save .qsm ---------------------------------------------------------------

save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales,
                     skip_scales = c("building", "grid25", "grid50", "grid100"))
save_geometry_export(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)


# Save large dfs as sqlite ------------------------------------------------

save_bslike_sqlite("building", all_scales = scales_variables_modules$scales)

lapply(c("grid25", "grid50", "grid100"), save_bslike_sqlite,
       all_scales = scales_variables_modules$scales)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")

# For compare, only keep the large brackets of age
scales_variables_modules$modules$var_right <- lapply(
  scales_variables_modules$modules$var_right, \(x) {
    if (is.null(x)) return(NULL)
    not_age <- x[!grepl("^age_", x)]
    age <- x[grepl("^age_", x)]
    
    age_keep <- age[age %in% c("age_0_14", "age_15_64", "age_65_plus")]
    
    c(not_age, age_keep)
  })

qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")
tictoc::toc()


# Create DYKs -------------------------------------------------------------

vars_dyk <- dyk_prep(scales_variables_modules, all_tables)
dyk <- dyk_uni(vars_dyk, scales_variables_modules)
dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")


# Place explorer content creation -----------------------------------------

# Should be done once the data is saved
future::plan(future::multisession(), workers = 4)

# pe_main_card_data <- placeex_main_card_data(scales = scales_variables_modules$scales,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary)
# 
# # ONLY KEEP FIRST SCALE
# pe_main_card_data$main_card_data <- lapply(pe_main_card_data$main_card_data, lapply, `[`, 1)
# pe_main_card_data$avail_df <- dplyr::distinct(pe_main_card_data$avail_df, region, .keep_all = TRUE)
# 
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

svm_first_scale <- scales_variables_modules
svm_first_scale$scales <- lapply(svm_first_scale$scales, `[`, 1)

placeex_main_card_rmd(scales_variables_modules = svm_first_scale,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "en",
                      tileset_prefix = "mtl",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = TRUE)

translation_df <- qs::qread("data/translation_df.qs")
placeex_main_card_rmd(scales_variables_modules = svm_first_scale,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "fr",
                      tileset_prefix = "mtl",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = TRUE)

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")


# Write the data to the bucket --------------------------------------------

cc.data::bucket_write_folder("data", "curbcut.montreal.data")
cc.data::bucket_write_folder("dev/data", "curbcut.montreal.dev.data")


# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-montreal-centraide") # Centraide
# heroku_deploy("cc-montreal-dev") # Development
# heroku_deploy("cc-montreal") # Production

