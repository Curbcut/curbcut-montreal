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
                         c("RTS", "CT", "DA", "building"),
                         c("RTS", "CT"),
                         c("CLSC", "CT", "DA", "building"),
                         c("CLSC", "CT"),
                         c("grd250", "grd100", "grd50", "grd25"),
                         c("grd300", "grd120", "grd60", "grd30"))


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
  place_heading = "Borough {name}",
  place_name = "{name}",
  subtext = "Municipal administrations (19) in the City of Montreal")


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
  place_name = "{name}",
  subtext = "Municipal administrations in the City of Montreal with census subdivisions in the region")

# Reorder to place boroughCSD first
scales_dictionary <- scales_dictionary[c(5, 1:(nrow(scales_dictionary)-1)), ]



# Add a RTS scale ------------------------------------------------------

# source("dev/other/msss_data_ret.R")
# RTS <- msss_ret(paste0("https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Car",
#                           "to_IPS/IPS_total/MapServer/1/query"))
# qs::qsave(RTS, "dev/data/shp/RTS.qs")
RTS <- qs::qread("dev/data/shp/RTS.qs")

RTS <- RTS[6:10, ]
RTS$name <- RTS$nomEtab
RTS <- RTS["name"]

RTS <- additional_scale(additional_table = RTS,
                           DA_table = census_scales$DA,
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
  sing = "local community service centre",
  sing_with_article = "the local community service centre",
  plur = "local community services centres",
  slider_title = "CLSC",
  place_heading = "CLSC {name}",
  place_name = "{name}",
  subtext = paste0("The territories for which the local community service centre ",
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
  append_scale_to_dictionary(
    scales_dictionary,
    scale = "building",
    sing = "building",
    sing_with_article = "the building",
    plur = "buildings",
    slider_title = "Building",
    place_heading = "{name}",
    place_name = "{name}",
    subtext = NA)

### Build CMHC scale
cmhczone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhczone <- additional_scale(additional_table = cmhczone,
                             DA_table = census_scales$DA,
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
                              DA_table = census_scales$DA,
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


# NDVI's grid -------------------------------------------------------------

# output_path <- "dev/data/ndvi/"
# output_path_tmp <- sprintf("%stmp/", output_path)
# latest_year <- cc.data::ndvi_years()[length(cc.data::ndvi_years())]
#   
# cc.data::ndvi_import_from_masterpolygon(
#   master_polygon = base_polygons$master_polygon,
#   years = latest_year,
#   output_path = output_path,
#   temp_folder = output_path_tmp,
#   overwrite = FALSE)
# 
# alltiles <- list.files(sprintf("%s%s/", output_path, latest_year), full.names = TRUE)
# grd30 <- lapply(alltiles, qs::qread)
# grd30 <- Reduce(rbind, grd30)
# qs::qsave(grd30, file = "dev/data/built/grd30.qs")
grd30 <- qs::qread("dev/data/built/grd30.qs")

IDs_progress <- grd30$ID[seq_along(grd30$ID) %% 100000 == 0]
IDs_progress <- c(IDs_progress, grd30$ID[nrow(grd30)])

grd30_crs <- sf::st_crs(grd30)

progressr::with_progress({
  pb <- progressr::progressor(length(IDs_progress))
  grd30$name <- future.apply::future_sapply(grd30$ID, \(x) {
    
    # Now that the geometry is a polygon, let's use the ID to grab centroid
    spl_ID <- strsplit(x, split = "_")[[1]]
    pt <- sf::st_point(as.numeric(spl_ID))
    pt_sf <- sf::st_sfc(pt, crs = grd30_crs)
    pt_transformed <- sf::st_transform(pt_sf, crs = 4326)

    # Use progress only on a few value to not slow down the process
    if (x %in% IDs_progress) pb()
    
    cc.data::rev_geocode_localhost(point_sf = pt_transformed)
  }, future.seed = NULL)
})
grd30$ID <- paste0("grd30_", grd30$ID)
grd30 <- grd30[, c("ID", "name", "geometry")]
qs::qsave(grd30, file = "dev/data/built/grd30.qs")


grd60 <- new_grid(original_grid = grd30, cellsize = 60, crs = crs,
                  DA_table = census_scales$DA)
qs::qsave(grd100, file = "dev/data/built/grd60.qs")
grd120 <- new_grid(original_grid = grd30, cellsize = 120, crs = crs,
                   DA_table = census_scales$DA)
qs::qsave(grd100, file = "dev/data/built/grd120.qs")
grd300 <- new_grid(original_grid = grd30, cellsize = 300, crs = crs,
                   DA_table = census_scales$DA)
qs::qsave(grd100, file = "dev/data/built/grd300.qs")

qs::qsavem(grd30, grd60, grd120, grd300, file = "dev/data/built/ndvigrids.qsm")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd30",
                             sing = "area at the 30m scale",
                             sing_with_article = "the area at the 30m scale",
                             plur = "areas at the 30m scale",
                             slider_title = "30m",
                             place_heading = "{name}",
                             place_name = "the 30m grid area around {name}",
                             subtext = "Small square areas, each measuring 30 meters by 30 meters")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd60",
                             sing = "area at the 60m scale",
                             sing_with_article = "the area at the 60m scale",
                             plur = "areas at the 60m scale",
                             slider_title = "60m",
                             place_heading = "{name}",
                             place_name = "the 60m grid area around {name}",
                             subtext = "Small square areas, each measuring 60 meters by 60 meters")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd120",
                             sing = "area at the 120m scale",
                             sing_with_article = "the area at the 120m scale",
                             plur = "areas at the 120m scale",
                             slider_title = "120m",
                             place_heading = "{name}",
                             place_name = "the 120m grid area around {name}",
                             subtext = "Small square areas, each measuring 120 meters by 120 meters")

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grd300",
                             sing = "area at the 300m scale",
                             sing_with_article = "the area at the 300m scale",
                             plur = "areas at the 300m scale",
                             slider_title = "300m",
                             place_heading = "{name}",
                             place_name = "the 300m grid area around {name}",
                             subtext = "Small square areas, each measuring 300 meters by 300 meters")


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
#                            DA_table = census_scales$DA,
#                            ID_prefix = "",
#                            name_2 = "250-m",
#                            DA_carto = base_polygons$DA_carto,
#                            crs = crs)
# grd250$ID <- gsub("^_", "", grd250$ID)
# # Append DA IDs
# grd250 <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
#                                     df = grd250, crs = crs)
qs::qsave(grd250, file = "dev/data/built/grd250.qs")
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
                # list(lavalsector = lavalsector),
                list(building = building),
                list(centraide = centraide),
                list(cmhczone = cmhczone),
                list(RTS = RTS),
                list(CLSC = CLSC),
                list(grd250 = grd250),
                list(grd100 = grd100),
                list(grd50 = grd50),
                list(grd25 = grd25))

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
  known_regions = list(grd250 = c("island"),
                       grd100 = c("island"),
                       grd50 = c("island"),
                       grd25 = c("island"),
                       CLSC = c("island", "CMA", "centraide"),
                       RTS = c("island")),
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

scales_to_interpolate_census <- 
  names(scales_variables_modules$scales)[
    !names(scales_variables_modules$scales) %in% c("grd25", "grd50", "grd100", "building")
  ]

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE,
                 scales_sequences = scales_sequences,
                 scales_to_interpolate = scales_to_interpolate_census)
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
          skip_scales = c("grd25", "grd50", "grd100", "grd250"),
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
                          region_DA_or_DB_IDs = census_scales$DA$ID,
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
                     grd100 = 11, grd50 = 13, grd25 = 14))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# # Tilesets ----------------------------------------------------------------
# 
# # Do not upload grids, as there is a function just for it.
# all_scales_t <- scales_variables_modules$scales[!grepl("^grd", names(scales_variables_modules$scales))]
# map_zoom_levels_t <- map_zoom_levels[!grepl("_grd", names(map_zoom_levels))]
# 
# tileset_upload_all(all_scales = all_scales_t,
#                    map_zoom_levels = map_zoom_levels_t,
#                    prefix = "mtl",
#                    username = "curbcut",
#                    access_token = .cc_mb_token,
#                    no_reset = "building")
# 
# source("dev/tiles/grid_tiles.R")
# grid_scales <- scales_variables_modules$scales[grepl("^grd", names(scales_variables_modules$scales))]
# grid_mzl <- map_zoom_levels[grepl("_grd", names(map_zoom_levels))]
# tileset_upload_grid(all_scales = grid_scales,
#                     map_zoom_levels = grid_mzl,
#                     max_zoom = list(grd250 = 13, grd100 = 14, grd50 = 15, grd25 = 16),
#                     vars = c("climate_drought", "climate_flood", "climate_destructive_storms",
#                              "climate_heat_wave", "climate_heavy_rain"),
#                     prefix = "mtl",
#                     username = "curbcut",
#                     access_token = .cc_mb_token)



# Add possible regions to modules -----------------------------------------

scales_variables_modules <- pages_regions(svm = scales_variables_modules,
                                          regions_dictionary = regions_dictionary)

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
# stories <- build_stories()
# qs::qsave(stories, file = "data/stories.qs")
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


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                   svm = scales_variables_modules)


# Save .qsm ---------------------------------------------------------------

save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales,
                     skip_scales = c("building", "grd25", "grd50", "grd100"))
save_geometry_export(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)


# Save large dfs as sqlite ------------------------------------------------

save_bslike_sqlite("building", all_scales = scales_variables_modules$scales)

lapply(c("grd25", "grd50", "grd100"), save_bslike_sqlite,
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
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

placeex_main_card_rmd(scales_variables_modules = scales_variables_modules,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "en",
                      tileset_prefix = "mtl",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE,
                      scales_sequences = scales_sequences,
                      skip_scales = c("building", "grd250", "grd100", "grd50", "grd25"),
                      data_path = .curbcut_montreal_data)

translation_df <- qs::qread("data/translation_df.qs")
placeex_main_card_rmd(scales_variables_modules = scales_variables_modules,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "fr",
                      tileset_prefix = "mtl",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE, 
                      scales_sequences = scales_sequences,
                      skip_scales = c("building", "grd250", "grd100", "grd50", "grd25"),
                      data_path = .curbcut_montreal_data)

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

