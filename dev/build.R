#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
library(sf)
x <- lapply(list.files("dev/data_import", full.names = TRUE), source, verbose = FALSE)

# Base of the study region and dictionaries -------------------------------

inst_prefix <- "mtl"

# Possible sequences for autozooms. Every module must have one or multiple of these
# possible scale sequences.
scales_sequences <- list(c("boroughCSD", "CT", "DA"),
                         c("boroughCSD", "CT", "DA", "DB"),
                         c("boroughCSD", "CT"),
                         c("CSD", "CT", "DA"),
                         c("CSD", "CT", "DA", "DB"),
                         c("CSD", "CT"),
                         c("borough", "CT", "DA"),
                         c("borough", "CT", "DA", "DB"),
                         c("borough", "CT"),
                         c("tablequartier", "CT", "DA"),
                         c("tablequartier", "CT", "DA", "DB"),
                         c("tablequartier", "CT"),
                         c("centraide", "CT", "DA"),
                         c("centraide", "CT", "DA", "DB"),
                         c("centraide", "CT"),
                         c("cmhczone", "CT", "DA"),
                         c("cmhczone", "CT", "DA", "DB"),
                         c("cmhczone", "CT"),
                         c("cmhczone"),
                         c("RTS", "CT", "DA"),
                         c("RTS", "CT", "DA", "DB"),
                         c("RTS", "CT"),
                         c("CLSC", "CT", "DA"),
                         c("CLSC", "CT", "DA", "DB"),
                         c("CLSC", "CT"),
                         c("grd250", "grd100", "grd50", "grd25"),
                         c("grd600", "grd300", "grd120", "grd60", "grd30"))


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
    name = c(CMA = "Montreal region",
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

# # Track DBs
# DB_table <- cc.data::DB_get(crs = crs, DA_carto = base_polygons$DA_carto)
# qs::qsave(DB_table, "dev/data/built/DB.qs")
DB_table <- qs::qread("dev/data/built/DB.qs")


# Add a borough scale -----------------------------------------------------

borough <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")[c("name", "geometry")]

borough <- additional_scale(additional_table = borough,
                            DB_table = census_scales$DB,
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
  place_heading = "Borough {name}",
  place_name = "{name}",
  subtext = "Municipal administrations (19) in the City of Montreal")


# Merge boroughs with CSDs for the boroughCSD scale
boroughs <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")
boroughs$type <- "Borough"

boroughCSD <- split_scale(destination = census_scales$CSD,
                          cutting_layer = boroughs,
                          crs = crs,
                          DA_table = census_scales$DA,
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
  place_name = "{name}",
  subtext = "Municipal administrations in the City of Montreal with census subdivisions in the region")

# Reorder to place boroughCSD first
scales_dictionary <- scales_dictionary[
  c(which(scales_dictionary$scale == "boroughCSD"), 1:(nrow(scales_dictionary)-1)), ]



# Add a RTS scale ------------------------------------------------------

# source("dev/other/msss_data_ret.R")
# RTS <- cc.data::arcgis_rest_services_ret(paste0("https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Car",
#                           "to_IPS/IPS_total/MapServer/1/query"))
# qs::qsave(RTS, "dev/data/shp/RTS.qs")
RTS <- qs::qread("dev/data/shp/RTS.qs")

RTS <- RTS[6:10, ]
RTS$name <- RTS$nomEtab
RTS <- RTS["name"]

RTS <- additional_scale(additional_table = RTS,
                           DB_table = census_scales$DB,
                           ID_prefix = "RTS",
                           name_2 = "RTS",
                           crs = crs,
                           DA_carto = base_polygons$DA_carto)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "RTS",
  sing = "territorial service network",
  sing_with_article = "the territorial service network",
  plur = "territorial service networks",
  slider_title = "RTS",
  place_heading = "{name}",
  place_name = "{name}",
  subtext = paste0("Territories for which the local integrated center (CISSS/CIUSSS) ",
                   "is responsible for ensuring the development and smooth ",
                   "operation of the health and social services network"))

# Add a CLSC scale ------------------------------------------------------

# source("dev/other/msss_data_ret.R")
# CLSC <- cc.data::arcgis_rest_services_ret(paste0("https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/s",
#                           "ervices/LimitesTerritoriales/TerritoiresSociosanita",
#                           "ires/MapServer/0/query"))
# qs::qsave(CLSC, "dev/data/shp/CLSC.qs")
CLSC <- qs::qread("dev/data/shp/CLSC.qs")

CLSC <- sf::st_filter(CLSC, base_polygons$master_polygon)

CLSC$name <- CLSC$CLSC_nom
CLSC <- CLSC["name"]

CLSC <- additional_scale(additional_table = CLSC,
                           DB_table = census_scales$DB,
                           ID_prefix = "CLSC",
                           name_2 = "CLSC",
                           crs = crs,
                         DA_carto = base_polygons$DA_carto)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "CLSC",
  sing = "local community service centre",
  sing_with_article = "the local community service centre",
  plur = "local community services centres",
  slider_title = "CLSC",
  place_heading = "CLSC {name}",
  place_name = "{name}",
  subtext = paste0("Territories for which the local community service centre ",
                   "(CLSC) has the mission to provide routine, front-line health ",
                   "and social services to the population"))

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
#                                 DB_table = census_scales$DB,
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
# building <- qs::qread("dev/data/built/building.qs")
# 
# # Add building scale to the dictionary
# scales_dictionary <-
#   append_scale_to_dictionary(
#     scales_dictionary,
#     scale = "building",
#     sing = "building",
#     sing_with_article = "the building",
#     plur = "buildings",
#     slider_title = "Building",
#     place_heading = "{name}",
#     place_name = "{name}",
#     subtext = NA)

### Build CMHC scale
cmhczone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhczone <- additional_scale(additional_table = cmhczone,
                             DB_table = census_scales$DB,
                             ID_prefix = "cmhc",
                             name_2 = "CMHC zone",
                             crs = crs,
                             DA_carto = base_polygons$DA_carto)
scales_dictionary <-
  append_scale_to_dictionary(
    scales_dictionary,
    scale = "cmhczone",
    sing = "CMHC zone",
    sing_with_article = "the CMHC zone",
    plur = "CMHC zones",
    slider_title = "CMHC zone",
    place_heading = "{name}",
    place_name = "{name}",
    subtext = paste0("Designated areas in Canada for housing market analysis ",
                     "by Canada Mortgage and Housing Corporation (CMHC)"))

### Build Centraide scale
centraide <- sf::st_read("dev/data/geometry/centraide_2023.shp")
centraide <- additional_scale(additional_table = centraide,
                              DB_table = census_scales$DB,
                              ID_prefix = "centraide",
                              name_2 = "Centraide zone",
                              crs = crs,
                              DA_carto = base_polygons$DA_carto)
scales_dictionary <-
  append_scale_to_dictionary(
    scales_dictionary,
    scale = "centraide",
    sing = "Centraide zone",
    sing_with_article = "the Centraide zone",
    plur = "Centraide zones",
    slider_title = "Centraide Zone",
    place_heading = "Centraide zone of {name}",
    place_name = "{name}",
    subtext = "Areas for Centraide's community support and development activities")

### Build table de quartier
tablequartier <- sf::st_read("dev/data/geometry/Tables_de_Quartier_32_MTL_2023.shp")
tablequartier <- tablequartier["FIRST_Nom2"]
names(tablequartier)[1] <- "name"
tablequartier <- additional_scale(additional_table = tablequartier,
                                  DB_table = census_scales$DB,
                                  ID_prefix = "tq",
                                  name_2 = "Table de quartier",
                                  crs = crs, 
                                  switch_to_carto = FALSE)

scales_dictionary <-
  append_scale_to_dictionary(
    scales_dictionary,
    scale = "tablequartier",
    sing = "Table de quartier",
    sing_with_article = "the Table de quartier",
    plur = "Tables de quartier",
    slider_title = "Tables de quartier",
    place_heading = "{name}",
    place_name = "{name}",
    subtext = paste0("Territories where local stakeholders collaborate to ",
                     "enhance neighborhood living quality and conditions."))


# NDVI's grid -------------------------------------------------------------

ndvigrids <- ndvi_grids(census_scales = census_scales,
                        base_polygons = base_polygons,
                        overwrite_ndvi_tiles = FALSE,
                        overwrite_final_grids = FALSE,
                        crs = crs)

scales_dictionary <- scales_dictionary_ndvi(scales_dictionary)


# Build vulnerability to climate risk scale -------------------------------

# grd <-
#   sf::st_read("dev/data/climate_risk/vulnerabilite-changements-climatiques-mailles-2022.shp")
# grd <- grd[, c("PageName", "geometry")]
# names(grd)[1] <- "ID"
# grd$ID <- paste0("grd25_", grd$ID)
# grd <- rev_geocode_sf(master_polygon = grd,
#                        sf_df = grd,
#                        province_code = "QC",
#                        crs = crs)
# grd_geocode <- sf::st_transform(grd, 4326)
# grd_geocode <- sf::st_centroid(grd_geocode)
# progressr::with_progress({
#   pb <- progressr::progressor(nrow(grd_geocode))
#   grd$name <- future.apply::future_sapply(grd_geocode$geometry, \(x) {
#     pb()
#     cc.data::rev_geocode_localhost(point_sf = x)
#   }, future.seed = NULL)
# })
# grd <- grd[, c("ID", "name", "geometry")]
# qs::qsave(grd, file = "dev/data/built/grd.qs")
# # Append DA IDs
# grd <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                 df = grd, crs = crs)
# grd25 <- qs::qread("dev/data/built/grid.qs")

# grd50 <- new_grid(original_grid = grd25, cellsize = 50, crs = crs,
#                   DA_table = census_scales$DA)
# qs::qsave(grd50, file = "dev/data/built/grd50.qs")

# grd100 <- new_grid(original_grid = grd25, cellsize = 100, crs = crs,
#                   DA_table = census_scales$DA)
# qs::qsave(grd100, file = "dev/data/built/grd100.qs")

# grd250 <- new_grid(original_grid = grd25, cellsize = 250, crs = crs,
#          DA_table = census_scales$DA)
# # Add census info (population / households) to the larger grd
# grd250 <- additional_scale(additional_table = grd250[c("ID", "name")],
#                            DB_table = census_scales$DB,
#                            ID_prefix = "",
#                            name_2 = "250-m",
#                            DA_carto = base_polygons$DA_carto,
#                            crs = crs,
# switch_to_carto = FALSE,
# )
# grd250$ID <- gsub("^_", "", grd250$ID)
# # Append DA IDs
# grd250 <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                     df = grd250, crs = crs)
# qs::qsave(grd250, file = "dev/data/built/grd250.qs")
#
# qs::qsavem(grd25, grd50, grd100, grd250, file = "dev/data/built/grids.qsm")
qs::qload("dev/data/built/grids.qsm")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd25",
                             sing = "area at the 25m scale",
                             sing_with_article = "the area at the 25m scale",
                             plur = "areas at the 25m scale",
                             slider_title = "25m",
                             place_heading = "{name}",
                             place_name = "the 25m grid area around {name}",
                             subtext = "Small square areas, each measuring 25 meters by 25 meters")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd50",
                             sing = "area at the 50m scale",
                             sing_with_article = "the area at the 50m scale",
                             plur = "areas at the 50m scale",
                             slider_title = "50m",
                             place_heading = "{name}",
                             place_name = "the 50m grid area around {name}",
                             subtext = "Small square areas, each measuring 50 meters by 50 meters")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd100",
                             sing = "area at the 100m scale",
                             sing_with_article = "the area at the 100m scale",
                             plur = "areas at the 100m scale",
                             slider_title = "100m",
                             place_heading = "{name}",
                             place_name = "the 100m grid area around {name}",
                             subtext = "Small square areas, each measuring 100 meters by 100 meters")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd250",
                             sing = "area at the 250m scale",
                             sing_with_article = "the area at the 250m scale",
                             plur = "areas at the 250m scale",
                             slider_title = "250m",
                             place_heading = "{name}",
                             place_name = "the 250m grid area around {name}",
                             subtext = "Small square areas, each measuring 250 meters by 250 meters")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(borough = borough),
                list(boroughCSD = boroughCSD),
                # list(building = building),
                list(centraide = centraide),
                list(cmhczone = cmhczone),
                list(RTS = RTS),
                list(CLSC = CLSC),
                list(tablequartier = tablequartier),
                list(grd250 = grd250),
                list(grd100 = grd100),
                list(grd50 = grd50),
                list(grd25 = grd25),
                ndvigrids)

# Character vector of the tables that will be saved in the database instead of
# in the container. These tables WON'T be available for dynamic filtering using
# region in the `curbcut::data_get()` function.
large_tables_db <- c(#"building", 
                     "grd25", "grd50", "grd100", "grd30", "grd60", 
                     "grd120", "grd300")

save.image("dev/data/built/pre_consolidate.RData")
load("dev/data/built/pre_consolidate.RData")

future::plan(future::sequential())

scales_consolidated <- consolidate_scales(scales_sequences = scales_sequences,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs,
                                          large_tables_db = large_tables_db)

regions_dictionary <- regions_dictionary_add_scales(
  regions_dictionary = regions_dictionary,
  region_dict_scales = scales_consolidated$for_region_dict_scales)

scales_dictionary <- add_regions_to_scales_dictionary(
  scales_dictionary = scales_dictionary, regions = base_polygons$regions,
  scales_consolidated = scales_consolidated,
  known_regions = list(grd250 = c("island"),
                       grd100 = c("island"),
                       grd50 = c("island"),
                       grd25 = c("island"),
                       tablequartier = c("island", "city"),
                       CLSC = c("island", "CMA", "centraide"),
                       RTS = c("island")),
  DA_carto = base_polygons$DA_carto,
  regions_dictionary = regions_dictionary, 
  crs = crs)

save.image("dev/data/built/post_consolidate.RData")
load("dev/data/built/post_consolidate.RData")


# Verify conformity -------------------------------------------------------

verify_dictionaries(scales = scales_consolidated$scales,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Save pieces that will be untouched from this point ----------------------

save_geometry_export(data_folder = "data/", 
                     all_scales = scales_consolidated$scales,
                     skip_scales = names(all_scales))
save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_consolidated$scales,
                     skip_scales = names(all_scales))
save_bslike_postgresql(all_scales = scales_consolidated$scales,
                       tables_to_save_db = names(all_scales),
                       inst_prefix = inst_prefix,
                       overwrite = FALSE)

qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated$scales)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)
scales_variables_modules$data <- lapply(scales_consolidated$scales, \(x) list())

qs::qsave(scales_consolidated, "dev/data/built/scales_consolidated.qs")
qs::qsavem(census_scales, scales_variables_modules, crs, base_polygons, 
           cancensus_cma_code, scales_consolidated, scales_sequences, DB_table,
           scales_dictionary, regions_dictionary, inst_prefix, large_tables_db,
           file = "dev/data/built/empty_scales_variables_modules.qsm")
rm(list = ls())
library(cc.buildr)
library(sf)
qs::qload("dev/data/built/empty_scales_variables_modules.qsm")

# Build the datasets ------------------------------------------------------

future::plan(future::multisession, workers = 4)

# # No data is added to the buildings yet, onload it
# scales_variables_modules$scales <- 
#   unload_scales(scales_variables_modules$scales, unload = c("building"))

# Interpolte census, not for smaller scales
scales_to_interpolate_census <- 
  names(scales_variables_modules$scales)[
    !names(scales_variables_modules$scales) %in% 
      c("grd25", "grd50", "grd100", "grd30", "grd60", "grd120", "grd300",
        "DB", "building")
  ]

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 DB_table = DB_table,
                 crs = crs,
                 scales_sequences = scales_sequences,
                 scales_to_interpolate = scales_to_interpolate_census,
                 overwrite = FALSE,
                 inst_prefix = inst_prefix)
census_variables <- get_census_vectors_details()

# Build NDVI first to unload heavy grids
scales_variables_modules <-
  ba_ndvi(scales_variables_modules = scales_variables_modules,
          skip_scales = c("grd25", "grd50", "grd100"),
          scales_sequences = scales_sequences,
          crs = crs,
          overwrite = FALSE,
          inst_prefix = inst_prefix)

scales_variables_modules$scales <-
  unload_scales(scales_variables_modules$scales,
                unload = c("grd30", "grd60", "grd120", "grd300"))

future::plan(future::multisession(), workers = 6)
scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              scales_sequences = scales_sequences,
              crs = crs, geo_uid = cancensus_cma_code,
              inst_prefix = inst_prefix)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         scales_sequences = scales_sequences,
         crs = crs, 
         region_DB_IDs = census_scales$DB$ID,
         overwrite = FALSE,
         inst_prefix = inst_prefix)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             scales_sequences = scales_sequences,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID,
             overwrite = FALSE,
             inst_prefix = inst_prefix)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         scales_sequences = scales_sequences,
         crs = crs,
         overwrite = FALSE,
         inst_prefix = inst_prefix)


# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

# Additional access variables
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
scales_variables_modules <-
  build_and_append_access(scales_variables_modules = scales_variables_modules,
                          DA_table = census_scales$DA,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs,
                          overwrite = FALSE,
                          inst_prefix = inst_prefix)

future::plan(future::multisession(), workers = 3)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_or_DB_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs,
                          overwrite = FALSE,
                          inst_prefix = inst_prefix)


future::plan(future::multisession(), workers = 4)


scales_variables_modules <-
  build_and_append_climate_risk(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)
scales_variables_modules$scales <- 
  unload_scales(scales_variables_modules$scales, 
                unload = c("grd25", "grd50", "grd100", "grd250"))

scales_variables_modules <-
  build_and_append_natural_inf(
    scales_variables_modules = scales_variables_modules,
    crs = crs)

scales_variables_modules <-
  build_and_append_alley(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    city_scales_ID = regions_dictionary$scales[regions_dictionary$region == "city"][[1]],
    inst_prefix = inst_prefix)

scales_variables_modules <-
  build_and_append_crash(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    island_IDs = regions_dictionary$scales[regions_dictionary$region == "island"][[1]],
    overwrite = FALSE,
    inst_prefix = inst_prefix
  )

future::plan(future::multisession(), workers = 4)

scales_variables_modules <-
  build_and_append_tenure(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)

scales_variables_modules <-
  build_and_append_afford_pop(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)

scales_variables_modules <-
  build_and_append_afford_hou(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)

# Post process
scales_variables_modules$scales <- 
  post_processing(scales = scales_variables_modules$scales)
# Adding right-hand variables
scales_variables_modules <- add_var_right(scales_variables_modules)
# Add high correlation combinations
scales_variables_modules <- add_high_corr_combination(scales_variables_modules)

qs::qsavem(census_scales, scales_variables_modules, crs, census_variables, 
           base_polygons, scales_sequences, regions_dictionary, inst_prefix,
           large_tables_db,
           scales_dictionary, file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

save_postal_codes(census_scales$DA$ID, overwrite = FALSE)


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  scales_sequences = scales_sequences,
  zoom_levels = list(first = 0, CT = 10, DA = 12, DB = 14, building = 16,
                     grd100 = 11, grd50 = 13, grd25 = 14,
                     grd300 = 10, grd120 = 11, grd60 = 12, grd30 = 13))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# Tilesets ----------------------------------------------------------------

# tileset_upload_all(map_zoom_levels = map_zoom_levels,
#                    inst_prefix = inst_prefix,
#                    username = "curbcut",
#                    access_token = .cc_mb_token,
#                    overwrite = FALSE)
# 
# tileset_upload_ndvi(map_zoom_levels = map_zoom_levels,
#                     regions = base_polygons$regions,
#                     inst_prefix = inst_prefix,
#                     username = "curbcut",
#                     access_token = .cc_mb_token,
#                     crs = crs)
# 
# source("dev/tiles/grid_tiles.R")
# grid_scales <- scales_variables_modules$scales[grepl("^grd", names(scales_variables_modules$scales))]
# grid_mzl <- map_zoom_levels[grepl("_grd", names(map_zoom_levels))]
# tileset_upload_grid(all_scales = grid_scales,
#                     map_zoom_levels = grid_mzl,
#                     max_zoom = list(grd250 = 13, grd100 = 14, grd50 = 15, grd25 = 16),
#                     vars = c("climate_drought", "climate_flood", "climate_destructive_storms",
#                              "climate_heat_wave", "climate_heavy_rain"),
#                     inst_prefix = inst_prefix,
#                     username = "curbcut",
#                     access_token = .cc_mb_token)





# Add possible regions to modules -----------------------------------------

scales_variables_modules <- pages_regions(svm = scales_variables_modules,
                                          regions_dictionary = regions_dictionary,
                                          inst_prefix = inst_prefix)

scales_variables_modules$modules[
  scales_variables_modules$modules$id == "climate_risk",
]$regions <- 
  list(c("island", "city"))


# Place explorer page ----------------------------------------------------

avail_scale_combinations <- scales_sequences[!grepl("grd", scales_sequences)]
avail_scale_combinations <- sapply(avail_scale_combinations, paste0, collapse = "_")


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
             dataset_info = "",
             avail_scale_combinations = avail_scale_combinations,
             regions = regions_dictionary$region)


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
stories_create_tileset(stories = stories,
                       inst_prefix = inst_prefix,
                       username = "curbcut",
                       access_token = .cc_mb_token)
cc.buildr::resize_image(folder = "www/stories/photos/", max_size_in_MB = 1)


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


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(scales_dictionary = scales_dictionary)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")
qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
tictoc::toc()

# Create DYKs -------------------------------------------------------------

library(tidyverse)
translation_df <- qs::qread("data/translation_df.qs")
vars_dyk <- dyk_prep(svm = scales_variables_modules, scales_dictionary = scales_dictionary)
dyk <- dyk_uni(vars_dyk,
               svm = scales_variables_modules,
               translation_df = translation_df,
               langs = c("en", "fr"),
               scales_dictionary = scales_dictionary,
               regions_dictionary = regions_dictionary)
# dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
# dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")


# Home page ---------------------------------------------------------------

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
cc.buildr::convert_svg_to_ico("#E08565")
home_page(modules = modules, stories = stories, translation_df = translation_df,
          c_city_svg = "www/landing/c-montreal.svg")


# Place explorer content creation -----------------------------------------

# Should be done once the data is saved
future::plan(future::multisession(), workers = 12)

# pe_main_card_data <- placeex_main_card_data(scales_dictionary = scales_dictionary,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary,
#                                             inst_prefix = inst_prefix,
#                                             first_scales = unique(sapply(scales_sequences, `[[`, 1)),
#                                             ignore_scales = c("building", "grd250", "grd100", "grd50", "grd25",
#                                                               "grd600", "grd300", "grd120", "grd60", "grd30"))
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")


library(curbcut)
translation_df <- qs::qread("data/translation_df.qs")
placeex_main_card_rmd(pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = c("en", "fr"),
                      tileset_prefix = inst_prefix,
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE,
                      scales_sequences = scales_sequences)

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")


# Write the data to the bucket --------------------------------------------

# cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.montreal.data")
# cc.data::bucket_write_folder(folder = "dev/data", bucket = "curbcut.montreal.dev.data")
# cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.montreal.beta.data")


# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-montreal-centraide") # Centraide
# heroku_deploy("cc-montreal-dev") # Development
# heroku_deploy("cc-montreal") # Production

