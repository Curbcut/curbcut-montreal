#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))


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
                    centraide = "dev/data/geometry/centraide.shp",
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
laval <- sf::st_read(paste0("dev/data/centraide/StatCan_Recensement2016/_Geograph",
                            "ie/Secteurs_damenagement_Ville_de_Laval.shp")) |>
  sf::st_transform(4326)
laval$name <- gsub("Secteur \\d - ", "", laval$Secteur)
laval$type <- "Sector"
laval <- laval[c("name", "type")]

census_scales$CSD <- cc.buildr::split_scale(destination = census_scales$CSD,
                                            cutting_layer = laval,
                                            DA_table = census_scales$DA,
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
centraide <- sf::st_read("dev/data/geometry/centraide.shp")
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
                             place_name = "25m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid50",
                             sing = "area at the 50m scale",
                             plur = "areas at the 50m scale",
                             slider_title = "50m",
                             place_heading = "{name}",
                             place_name = "50m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid100",
                             sing = "area at the 100m scale",
                             plur = "areas at the 100m scale",
                             slider_title = "100m",
                             place_heading = "{name}",
                             place_name = "100m grid area around {name}")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid250",
                             sing = "area at the 250m scale",
                             plur = "areas at the 250m scale",
                             slider_title = "250m",
                             place_heading = "{name}",
                             place_name = "250m grid area around {name}")


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
                                          crs = crs)


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
          cancensus_cma_code, scales_consolidated,
          file = "dev/data/built/empty_scales_variables_modules.qsm")
qs::qload("dev/data/built/empty_scales_variables_modules.qsm")

# Build the datasets ------------------------------------------------------

future::plan(list(future::tweak(future::multisession,
                                workers = 3),
                  future::tweak(future::multisession,
                                workers = length(cc.data::census_years))))

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE,
                 skip_scale_interpolation = c("grid25", "grid50", "grid100"))
census_variables <- get_census_vectors_details()

save.image()
load(".RData")

future::plan(future::multisession())

scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              crs = crs, geo_uid = cancensus_cma_code)
scales_variables_modules <-
  ru_canale(scales_variables_modules = scales_variables_modules,
            crs = crs,
            region_DA_IDs = census_scales$DA$ID)
# scales_variables_modules <-
#   ru_canbics(scales_variables_modules = scales_variables_modules,
#              crs = crs,
#              region_DA_IDs = census_scales$DA$ID)

# Montreal specific modules
save.image("before_mtl.RData")
load("before_mtl.RData")

# scales_variables_modules <-
#   build_and_append_centraide_pop(
#     scales_variables_modules = scales_variables_modules,
#     crs = crs,
#     CT_table = census_scales$CT,
#     region_CT_IDs = census_scales$CT$ID)
# scales_variables_modules <-
#   build_and_append_centraide_hou(
#     scales_variables_modules = scales_variables_modules,
#     crs = crs,
#     CT_table = census_scales$CT,
#     region_CT_IDs = census_scales$CT$ID)
scales_variables_modules <-
  build_and_append_climate_risk(
    scales_variables_modules = scales_variables_modules,
    crs = crs)
# scales_variables_modules <-
#   build_and_append_short_distance_city(
#     scales_variables_modules = scales_variables_modules,
#     crs = crs)

# scales_variables_modules <-
#   build_and_append_natural_inf(
#     scales_variables_modules = scales_variables_modules,
#     crs = crs)


# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

future::plan(future::multisession(), workers = 4)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes,
                          crs = crs)
# # Additional access variables
# scales_variables_modules <- 
#   build_and_append_access(scales_variables_modules = scales_variables_modules,
#                           DA_table = census_scales$DA,
#                           traveltimes = traveltimes,
#                           crs = crs)

# Post process
scales_variables_modules$scales <- 
  cc.buildr::post_processing(scales = scales_variables_modules$scales)

qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           scales_dictionary, regions_dictionary, all_tables, base_polygons,
           file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

postal_codes <- build_postal_codes(census_scales$DA$ID)
postal_codes <- sf::st_drop_geometry(postal_codes)
qs::qsave(postal_codes, "data/postal_codes.qs")


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  all_tables = all_tables,
  zoom_levels = list(first = 0, CT = 10.5, DA = 12.5, building = 15.5, 
                     grid100 = 10.5, grid50 = 12.5, grid25 = 13.5))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "CMA",
    suffix = "max_CT",
    content = c("CSD" = 0, "CT" = 10.5))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "city",
    suffix = "max_CT",
    content = c("CSD" = 0, "CT" = 10.5))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "island",
    suffix = "max_CT",
    content = c("CSD" = 0, "CT" = 10.5))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "centraide",
    suffix = "max_CT",
    content = c("centraide" = 0, "CT" = 10.5))

map_zoom_levels <-
  map_zoom_levels_create_custom(
    map_zoom_levels = map_zoom_levels,
    all_tables = all_tables,
    region = "city",
    suffix = "max_DB",
    content = c("CSD" = 0, "CT" = 10.5, "DA" = 12.5, "DB" = 14.5))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# # Tilesets ----------------------------------------------------------------

# tileset_upload_all(all_scales = scales_variables_modules$scales,
#                    map_zoom_levels = map_zoom_levels,
#                    tweak_max_zoom = list(grid250 = 11, grid100 = 12, grid50 = 13),
                   # prefix = "mtl",
                   # username = "sus-mcgill",
                   # access_token = .cc_mb_token)

source("dev/tiles/grid_tiles.R")
tileset_upload_grid(region = "grid",
                    all_scales = scales_variables_modules$scales,
                    map_zoom_levels = map_zoom_levels,
                    max_zoom = list(grid250 = 11, grid100 = 12, grid50 = 13, grid25 = 14),
                    vars = c("climate_drought", "climate_flood", "climate_destructive_storms",
                             "climate_heat_wave", "climate_heavy_rain"),
                    prefix = "mtl",
                    username = "sus-mcgill",
                    access_token = .cc_mb_token)


# 
# tileset_labels(scales = scales_variables_modules$scales,
#                crs = crs,
#                prefix = "mtl",
#                username = "sus-mcgill",
#                access_token = .cc_mb_token)
# 
# #street <- cc.data::db_read_data(table = "streets",
# #                                column_to_select = "DA_ID",
# #                                IDs = census_scales$DA$ID)
# qs::qsave(street, "dev/data/built/street.qs")
# street <- qs::qread("dev/data/built/street.qs")
# 
# tileset_streets(master_polygon = base_polygons$master_polygon,
#                 street = street,
#                 crs = crs,
#                 prefix = "mtl",
#                 username = "sus-mcgill",
#                 access_token = .cc_mb_token)


# Place explorer ----------------------------------------------------------

# future::plan(future::multisession(), workers = 18)
# 
# scales_variables_modules$scales <- scales_variables_modules$scales[1]
# 
# pe_main_card_data <- placeex_main_card_data(scales = scales_variables_modules$scales,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary)
# 
# placeex_main_card_rmd(scales_variables_modules = scales_variables_modules,
#                       pe_main_card_data = pe_main_card_data,
#                       regions_dictionary = regions_dictionary,
#                       scales_dictionary = scales_dictionary,
#                       lang = "en",
#                       tileset_prefix = "mtl",
#                       mapbox_username = "sus-mcgill",
#                       rev_geocode_from_localhost = TRUE,
#                       overwrite = TRUE)
# 
# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = NA,
             regions = list(NULL),
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "Select a location by entering a postal code or clicking on the ",
               "map and see how it compares to the rest of the Montreal region ",
               "or island across a variety of sustainability indicators."
             ),
             title_text_extra = paste0(
               "The data in the Place Explorer is taken from other Curbcut pages with ",
               "two exceptions: <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a> and <a href = 'https://www.canueda",
               "ta.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf'>green space</a> data are ta",
               "ken from <a href = 'https://www.canuedata.ca'>CANUE</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             regions = regions_dictionary$region[regions_dictionary$pickable])

# Did you know ------------------------------------------------------------

# variables <- scales_variables_modules$variables
# source("dev/other/dyk.R")
# qs::qsave(dyk, "data/dyk.qs")


# Translation -------------------------------------------------------------

source("dev/translation/build_translation.R", encoding = "utf-8")


# Produce colours ---------------------------------------------------------

colours_dfs <- cc.buildr::build_colours()
qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# TKTK MAKE SURE YOU HAVE THIS VERSION OF LEAFLET, IF NOT THE MAPS IN THE HTML
# DOCUMENTS WON'T BE INTERACTIVES:
# devtools::install_github("dmurdoch/leaflet@crosstalk4")
stories <- build_stories()
stories_mapping <- stories$stories_mapping
stories <- stories$stories
qs::qsavem(stories, stories_mapping, file = "data/stories.qsm")
# stories_create_tileset(stories = stories,
#                        prefix = "mtl",
#                        username = "sus-mcgill",
#                        access_token = .cc_mb_token)

# Add Montréal stories
scales_variables_modules$modules <-
  scales_variables_modules$modules |>
  add_module(
    id = "stories",
    theme = NA,
    nav_title = "Montréal stories",
    title_text_title = "Montréal stories",
    title_text_main = paste0(
      "Explore stories about urban sustainability and planning in Montreal. Learn ",
      "about stories rooted in specific geographic locations or those that ",
      "have an impact on the whole city."),
    title_text_extra = paste0(
      "These narrative case studies are written by Curbcut contributors.."),
    metadata = FALSE,
    dataset_info = ""
  )


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# # Build data scripts ------------------------------------------------------
# new_pages <- list.files("dev/pages", full.names = TRUE)
# new_pages <- new_pages[!grepl("/access.R|/climaterisk.R", new_pages)]
# 
# lapply(new_pages, create_page_script, overwrite = TRUE) |> 
#   invisible()


# Save SQLite data --------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                       all_scales = scales_variables_modules$scales,
                       variables = scales_variables_modules$variables)


# Save .qsm ---------------------------------------------------------------

save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)
save_geometry_export(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")
qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")
# qs::qsave(scales_variables_modules$scales[[1]][[1]] |> 
#             sf::st_transform(crs) |> 
#             sf::st_union() |> 
#             sf::st_centroid() |> 
#             sf::st_transform(4326) |> 
#             sf::st_coordinates() |> 
#             as.numeric(), file = "data/map_loc.qs")
tictoc::toc()

# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-montreal-centraide") # Centraide
# heroku_deploy("cc-montreal-dev") # Development
# heroku_deploy("cc-montreal") # Production

