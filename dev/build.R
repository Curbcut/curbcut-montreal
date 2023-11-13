#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
library(sf)
x <- lapply(list.files("dev/data_import", full.names = TRUE), source, verbose = FALSE)

# Base of the study region and dictionaries -------------------------------

# Possible sequences for autozooms. Every module must have one or multiple of these
# possible scale sequences.
scales_sequences <- list(c("boroughCSD", "CT", "DA", "building"),
                         c("boroughCSD", "CT"),
                         c("CSD", "CT", "DA", "building"),
                         c("CSD", "CT"),
                         c("borough", "CT", "DA", "building"),
                         c("borough", "CT"),
                         # c("lavalsector", "CT", "DA", "building"),
                         # c("tablequartier", "CT", "DA", "building"),
                         # c("tablequartier", "CT"),
                         c("centraide", "CT", "DA", "building"),
                         c("centraide", "CT"),
                         c("cmhczone", "CT", "DA", "building"),
                         c("cmhczone", "CT"),
                         c("cmhczone"),
                         c("CIUSSS", "CT", "DA", "building"),
                         c("CIUSSS", "CT"),
                         c("CLSC", "CT", "DA", "building"),
                         c("CLSC", "CT"),
                         c("grid250", "grid100", "grid50", "grid25"))


# List all the regions geometries to create the master polygon
cancensus_cma_code <- 24462
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 2466023),
                    island = "dev/data/geometry/island.shp",
                    centraide = "dev/data/geometry/centraide_2023.shp")

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_regions = all_regions,
    region = c("CMA", "island", "city", "centraide"),
    name = c(CMA = "Metropolitan Area",
             island = "Island of Montreal",
             city = "City of Montreal",
             centraide = "Centraide of Greater Montreal"),
    to_compare = c(CMA = "in the Montreal region",
                   island = "on the island of Montreal",
                   city = "in the City of Montreal",
                   centraide = "in the Centraide of Greater Montreal territory"),
    to_compare_determ = c(CMA = "the Montreal region",
                   island = "the island of Montreal",
                   city = "the City of Montreal",
                   centraide = "the Centraide of Greater Montreal territory"),
    to_compare_short = c(CMA = "in the region",
                   island = "on the island",
                   city = "in the City",
                   centraide = "in the territory"),
    pickable = c(CMA = TRUE,
                 island = TRUE,
                 city = TRUE,
                 centraide = TRUE))


# Build census scales -----------------------------------------------------

census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      crs = crs,
                      DA_carto = base_polygons$DA_carto)

# Create the census scale dictionary
scales_dictionary <- census_scales_dictionary(census_scales)


# Add a borough scale -----------------------------------------------------

borough <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")[c("name", "geometry")]

borough <- additional_scale(additional_table = borough,
                            DA_table = census_scales$DA,
                            ID_prefix = "borough",
                            name_2 = "Borough",
                            crs = crs,
                            DA_carto = base_polygons$DA_carto)

# Update CSD naming for Montreal
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "borough",
  sing = "borough",
  sing_with_article = "the borough",
  plur = "boroughs",
  slider_title = "Borough",
  place_heading = "Borough of {name}",
  place_name = "{name}")


# Merge boroughs with CSDs for the boroughCSD scale
boroughs <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")
boroughs$type <- "Borough"

boroughCSD <- split_scale(destination = census_scales$CSD,
                          cutting_layer = boroughs,
                          DA_table = census_scales$DA,
                          crs = crs,
                          DA_carto = base_polygons$DA_carto)


boroughCSD$ID <- paste0("bc_", boroughCSD$ID)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "boroughCSD",
  sing = "borough/city",
  sing_with_article = "the borough/city",
  plur = "boroughs or cities",
  slider_title = "Borough/City",
  place_heading = "{name_2} of {name}",
  place_name = "{name}")

# Reorder to place boroughCSD first
scales_dictionary <- scales_dictionary[c(5, 1:(nrow(scales_dictionary)-1)), ]



# Add a CIUSSS scale ------------------------------------------------------

# source("dev/other/msss_data_ret.R")
# CIUSSS <- msss_ret(paste0("https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Car",
#                           "to_IPS/IPS_total/MapServer/1/query"))
# qs::qsave(CIUSSS, "dev/data/shp/CIUSSS.qs")
CIUSSS <- qs::qread("dev/data/shp/CIUSSS.qs")

CIUSSS <- CIUSSS[6:10, ]
CIUSSS$name <- CIUSSS$nomEtab
CIUSSS <- CIUSSS["name"]

CIUSSS <- additional_scale(additional_table = CIUSSS,
                           DA_table = census_scales$DA,
                           ID_prefix = "CIUSSS",
                           name_2 = "CIUSSS",
                           crs = crs,
                           DA_carto = base_polygons$DA_carto)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "CIUSSS",
  sing = "Centres intégrés universitaires de santé et de services sociaux",
  sing_with_article = "the CIUSSS",
  plur = "CIUSSS",
  slider_title = "CIUSSS",
  place_heading = "{name}",
  place_name = "{name}")

# Add a CLSC scale ------------------------------------------------------

# source("dev/other/msss_data_ret.R")
# CLSC <- msss_ret(paste0("https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/s",
#                           "ervices/LimitesTerritoriales/TerritoiresSociosanita",
#                           "ires/MapServer/0/query"))
# qs::qsave(CLSC, "dev/data/shp/CLSC.qs")
CLSC <- qs::qread("dev/data/shp/CLSC.qs")

CLSC <- sf::st_filter(CLSC, base_polygons$master_polygon)

CLSC$name <- CLSC$CLSC_nom
CLSC <- CLSC["name"]

CLSC <- additional_scale(additional_table = CLSC,
                           DA_table = census_scales$DA,
                           ID_prefix = "CLSC",
                           name_2 = "CLSC",
                           crs = crs,
                         DA_carto = base_polygons$DA_carto)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "CLSC",
  sing = "Centre local de service communautaire",
  sing_with_article = "the CLSC",
  plur = "CLSC",
  slider_title = "CLSC",
  place_heading = "CLSC {name}",
  place_name = "{name}")

# Add a Laval Sector scale ------------------------------------------------

# # Switch the City of Laval for the Sector
# lavalsector <- sf::st_read(paste0("dev/data/centraide/shapefiles/",
#                             "Secteurs_damenagement_Ville_de_Laval.shp")) |>
#   sf::st_transform(4326)
# lavalsector$name <- gsub("Secteur \\d - ", "", lavalsector$Secteur)
# lavalsector$type <- "Sector"
# lavalsector <- lavalsector[c("name")]
#
# lavalsector <- additional_scale(additional_table = lavalsector,
#                                 DA_table = census_scales$DA,
#                                 ID_prefix = "laval",
#                                 name_2 = "Sector",
#                                 crs = crs)
#
# scales_dictionary <- append_scale_to_dictionary(
#   scales_dictionary,
#   scale = "lavalsector",
#   sing = "Laval Sector",
#   sing_with_article = "the Laval Sector",
#   plur = "Laval Sectors",
#   slider_title = "Sector",
#   place_heading = "Laval Sector of {name}",
#   place_name = "{name}")
#

# Building scale ----------------------------------------------------------

# # Build building scale
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
                             sing_with_article = "the building",
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
                             crs = crs,
                             DA_carto = base_polygons$DA_carto)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhczone",
                             sing = "CMHC zone",
                             sing_with_article = "the CMHC zone",
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
                              crs = crs,
                              DA_carto = base_polygons$DA_carto)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "centraide",
                             sing = "Centraide zone",
                             sing_with_article = "the Centraide zone",
                             plur = "Centraide zones",
                             slider_title = "Centraide zone",
                             place_heading = "Centraide zone of {name}",
                             place_name = "{name}")

# ### Build table de quartier
# tablequartier <- sf::st_read("dev/data/geometry/quartiers_sociologiques_2014.shp")
# tablequartier <- tablequartier["Table"]
# names(tablequartier)[1] <- "name"
# tablequartier <- additional_scale(additional_table = tablequartier,
#                               DA_table = census_scales$DA,
#                               ID_prefix = "tablequartier",
#                               name_2 = "Table de quartier",
#                               crs = crs)
#
# scales_dictionary <-
#   append_scale_to_dictionary(scales_dictionary,
#                              scale = "tablequartier",
#                              sing = "Table de quartier",
#                              sing_with_article = "the Table de quartier",
#                              plur = "Table de quartier",
#                              slider_title = "Table de quartier",
#                              place_heading = "Table de quartier of {name}",
#                              place_name = "{name}")

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
                             sing_with_article = "the area at the 25m scale",
                             plur = "areas at the 25m scale",
                             slider_title = "25m",
                             place_heading = "{name}",
                             place_name = "the 25m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid50",
                             sing = "area at the 50m scale",
                             sing_with_article = "the area at the 50m scale",
                             plur = "areas at the 50m scale",
                             slider_title = "50m",
                             place_heading = "{name}",
                             place_name = "the 50m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid100",
                             sing = "area at the 100m scale",
                             sing_with_article = "the area at the 100m scale",
                             plur = "areas at the 100m scale",
                             slider_title = "100m",
                             place_heading = "{name}",
                             place_name = "the 100m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid250",
                             sing = "area at the 250m scale",
                             sing_with_article = "the area at the 250m scale",
                             plur = "areas at the 250m scale",
                             slider_title = "250m",
                             place_heading = "{name}",
                             place_name = "the 250m grid area around {name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(borough = borough),
                list(boroughCSD = boroughCSD),
                # list(lavalsector = lavalsector),
                list(building = building),
                list(centraide = centraide),
                list(cmhczone = cmhczone),
                list(CIUSSS = CIUSSS),
                list(CLSC = CLSC),
                list(grid250 = grid250),
                list(grid100 = grid100),
                list(grid50 = grid50),
                list(grid25 = grid25))

save.image("dev/data/built/pre_consolidate.RData")
load("dev/data/built/pre_consolidate.RData")

scales_consolidated <- consolidate_scales(scales_sequences = scales_sequences,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs)

regions_dictionary <- regions_dictionary_add_scales(
  regions_dictionary = regions_dictionary,
  region_dict_scales = scales_consolidated$for_region_dict_scales)

scales_dictionary <- add_regions_to_scales_dictionary(
  scales_dictionary = scales_dictionary, regions = base_polygons$regions,
  scales_consolidated = scales_consolidated,
  known_regions = list(grid250 = c("island", "city"),
                       grid100 = c("island", "city"),
                       grid50 = c("island", "city"),
                       grid25 = c("island", "city")),
  DA_carto = base_polygons$DA_carto)


# Verify conformity -------------------------------------------------------

verify_dictionaries(scales = scales_consolidated$scales,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated$scales)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)
scales_variables_modules$data <- lapply(scales_consolidated$scales, \(x) list())

qs::qsavem(census_scales, scales_variables_modules, crs,
          scales_dictionary, regions_dictionary, base_polygons,
          cancensus_cma_code, scales_consolidated, all_scales, scales_sequences,
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
                 scales_sequences = scales_sequences,
                 skip_scale_interpolation = c("grid25", "grid50", "grid100"))
census_variables <- get_census_vectors_details()

save.image("dev/data/built/pre_census.RData")
load("dev/data/built/pre_census.RData")

future::plan(future::multisession(), workers = 6)
scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              scales_sequences = scales_sequences,
              crs = crs, geo_uid = cancensus_cma_code)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         scales_sequences = scales_sequences,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             scales_sequences = scales_sequences,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         scales_sequences = scales_sequences,
         crs = crs)

save.image("dev/data/built/pre_ndvi.RData")
load("dev/data/built/pre_ndvi.RData")

scales_variables_modules <-
  ba_ndvi(scales_variables_modules = scales_variables_modules,
          master_polygon = base_polygons$master_polygon,
          all_scales = all_scales,
          skip_scales = c("grid25", "grid50", "grid100", "grid250"),
          scales_sequences = scales_sequences,
          crs = crs)

# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

future::plan(future::multisession(), workers = 3)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs)

save.image("dev/data/built/svm_after_access.qs")
load("dev/data/built/svm_after_access.qs")

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
future::plan(future::multisession(), workers = 4)

# Additional access variables
scales_variables_modules <-
  build_and_append_access(scales_variables_modules = scales_variables_modules,
                          DA_table = census_scales$DA,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs)

# Montreal specific modules
save.image("dev/data/built/before_mtl.RData")
load("dev/data/built/before_mtl.RData")

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
future::plan(future::multisession(), workers = 4)

scales_variables_modules <-
  build_and_append_climate_risk(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs)

scales_variables_modules <-
  build_and_append_natural_inf(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

scales_variables_modules <-
  build_and_append_alley(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    city_scales_ID = regions_dictionary$scales[regions_dictionary$region == "city"][[1]])

scales_variables_modules <-
  build_and_append_crash(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    island_IDs = regions_dictionary$scales[regions_dictionary$region == "island"][[1]]
  )

save.image("dev/data/built/before_centraide.RData")
load("dev/data/built/before_centraide.RData")
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
future::plan(future::multisession(), workers = 4)

scales_variables_modules <-
  build_and_append_tenure(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs)

scales_variables_modules <-
  build_and_append_afford_pop(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs)

scales_variables_modules <-
  build_and_append_afford_hou(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs)

# Post process
scales_variables_modules$scales <- 
  cc.buildr::post_processing(scales = scales_variables_modules$scales)

qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           scales_dictionary, regions_dictionary, base_polygons,
           all_scales, scales_sequences,
           file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

# postal_codes <- build_postal_codes(census_scales$DA$ID)
# postal_codes <- sf::st_drop_geometry(postal_codes)
# qs::qsave(postal_codes, "data/postal_codes.qs")


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  scales_sequences = scales_sequences,
  zoom_levels = list(first = 0, CT = 10, DA = 12, building = 16,
                     grid100 = 11, grid50 = 13, grid25 = 14))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# Tilesets ----------------------------------------------------------------

# Do not upload grids, as there is a function just for it.
all_scales_t <- scales_variables_modules$scales[!grepl("^grid", names(scales_variables_modules$scales))]
map_zoom_levels_t <- map_zoom_levels[!grepl("_grid", names(map_zoom_levels))]

tileset_upload_all(all_scales = all_scales_t,
                   map_zoom_levels = map_zoom_levels_t,
                   prefix = "mtl",
                   username = "curbcut",
                   access_token = .cc_mb_token,
                   no_reset = "building")

source("dev/tiles/grid_tiles.R")
tileset_upload_grid(region = "grid",
                    all_scales = scales_variables_modules$scales,
                    map_zoom_levels = map_zoom_levels,
                    max_zoom = list(grid250 = 13, grid100 = 14, grid50 = 15, grid25 = 16),
                    vars = c("climate_drought", "climate_flood", "climate_destructive_storms",
                             "climate_heat_wave", "climate_heavy_rain"),
                    prefix = "mtl",
                    username = "curbcut",
                    access_token = .cc_mb_token)



# Add possible regions to modules -----------------------------------------

modules <- scales_variables_modules$modules
data <- scales_variables_modules$data
modules$regions <- lapply(modules$id, \(id) {
  vl <- modules$var_left[modules$id == id][[1]]
  vl <- if (!is.data.frame(vl)) vl else vl$var_code
  if (length(vl) > 100) vl <- vl[sample(1:length(vl), 50)]
  
  # Go over all possible variables on one module
  every_val_reg <- lapply(vl, \(v) {
    
    # Which are the regions to keep
    avl_reg <- sapply(regions_dictionary$region, \(reg) {
      
      region_scales <- regions_dictionary$scales[regions_dictionary$region == reg][[1]]
      
      # Go over all scales in the region
      scales_presence <- mapply(\(scale_name, scale_IDs) {
        # If there are no features for a scale inside this region, remove it
        # from the count
        if (length(scale_IDs) == 0) return(NULL)
        
        dat <- data[[scale_name]][[v]]
        # If the data is not present as a certain scale, do not count it in 
        # the count.
        if (is.null(dat)) return(NULL)
        
        # Length of IDs from this region present in the scales' data
        present_in_data <- dat[dat$ID %in% scale_IDs, ] 
        
        # Do not count if there are missing values (NAs) for more than half of 
        # the observations!
        present_in_data <- subset(present_in_data, select = -ID)
        only_nas <- apply(present_in_data, 1, \(x) all(is.na(x)))
        present_in_data <- present_in_data[!only_nas, ]
        
        # As a percentage of all the IDs needed to fill in the region
        nrow(present_in_data) / length(scale_IDs)
      }, names(region_scales), region_scales)
      
      # Remove the scales for which no data was calculated
      scales_presence <- scales_presence[!sapply(scales_presence, is.null)]
      # Get the average: what is the average percentage of features that are
      # from a region that are filled with data for this particular variable.
      scales_presence_avg <- mean(unlist(scales_presence))
      
      # For the region to be valid, it needs to have at least 80% of the
      # features filled with data.
      scales_presence_avg > .80
      
    }, simplify = TRUE, USE.NAMES = TRUE)
    
    names(avl_reg)[avl_reg]
  })
  
  # Only keep regions if they are present for 3/4 the variable on the page
  kept_regs <- table(unlist(every_val_reg)) / length(every_val_reg) > 0.75
  kept_regs <- names(kept_regs)[kept_regs]
  
  # Keep the order from the regions dictionary
  kept_regs <- kept_regs[order(match(kept_regs, regions_dictionary$region))]
  
  return(kept_regs)
  
})
scales_variables_modules$modules <- modules

# Place explorer page ----------------------------------------------------

# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = "Explorer",
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "Select a location by entering a postal code or clicking on the map to ",
               "see how it compares to the rest of the region across a variety of sust",
               "ainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a>."
             ),
             metadata = FALSE,
             dataset_info = "")


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
# devtools::install_github("dmurdoch/leaflet@crosstalk4")
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
      "Explore narrative case studies about specific urban sustainability and ",
      "planning issues in the Montreal region."),
    title_text_extra = paste0(
      "<p>These narrative case studies are written by the Curbcut team and its contributors."),
    metadata = FALSE,
    dataset_info = ""
  )


# Translation -------------------------------------------------------------

variables <- scales_variables_modules$variables
modules <- scales_variables_modules$modules
source("dev/translation/build_translation.R", encoding = "utf-8")

# Create DYKs -------------------------------------------------------------

library(tidyverse)
translation_df <- qs::qread("data/translation_df.qs")
vars_dyk <- dyk_prep(svm = scales_variables_modules, scales_dictionary = scales_dictionary)
dyk <- dyk_uni(vars_dyk,
               svm = scales_variables_modules,
               translation_df = translation_df,
               langs = c("en", "fr"),
               scales_dictionary = scales_dictionary)
# dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
# dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")


# Home page ---------------------------------------------------------------

home_page(modules = modules, stories = stories, translation_df = translation_df)


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                   svm_data = scales_variables_modules$data)


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
# pe_main_card_data$avail_scale <- dplyr::distinct(pe_main_card_data$avail_scale, region, .keep_all = TRUE)
# 
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

svm_first_scale <- scales_variables_modules
svm_first_scale$scales <- lapply(svm_first_scale$scales, `[`, 1)

library(curbcut)
placeex_main_card_rmd(scales_variables_modules = svm_first_scale,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "en",
                      tileset_prefix = "mtl",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE)

translation_df <- qs::qread("data/translation_df.qs")
placeex_main_card_rmd(scales_variables_modules = svm_first_scale,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "fr",
                      tileset_prefix = "mtl",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE)

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")


# Write the data to the bucket --------------------------------------------

# cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.montreal.data")
cc.data::bucket_write_folder(folder = "dev/data", bucket = "curbcut.montreal.dev.data")
cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.montreal.beta.data")


# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-montreal-centraide") # Centraide
# heroku_deploy("cc-montreal-dev") # Development
# heroku_deploy("cc-montreal") # Production

