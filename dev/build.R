#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------

library(cc.buildr)
future::plan(future::multisession())


# Base of the study region and dictionaries -------------------------------

# All regions
all_tables <-
  list("CMA" = c("CSD", "CT", "DA", "building"),
       "island" = c("CSD", "CT", "DA", "building"),
       "city" = c("CSD", "CT", "DA", "building"),
       "centraide" = c("centraide", "CT", "DA", "building"),
       "cmhc" = c("cmhc_zone"),
       "grid" = c("grid"))

# List all the regions geometries to create the master polygon
cancensus_cma_code <- 24462
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 2466023),
                    island = "dev/data/geometry/island.shp",
                    centraide = "dev/data/geometry/centraide.shp",
                    cmhc = get_cmhc_zones(list(CMA = cancensus_cma_code)),
                    grid = "dev/data/climate_risk/Vulnerabilité_secheresses_2016.shp")

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_tables = all_tables,
    geo = c("CMA", "island", "city", "centraide", "cmhc", "grid"),
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
                      crs = crs)
# Switch the City of Montreal for the boroughs
boroughs <- sf::st_read("dev/data/geometry/arrondissements_mtl.shp")
census_scales$CSD <- split_scale(destination = census_scales$CSD,
                                 cutting_layer = boroughs,
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
# # # From MySQL
# # building <- cc.data::db_read_long_table(table = "buildings",
# #                                          DA_ID = census_scales$DA$ID)
# # qs::qsave(building, file = "dev/data/built/building.qs")
# # # From Local
building <- qs::qread("dev/data/canada_buildings.qs")
building_ids <- cc.data::db_read_data(table = "buildings_DA_dict",
                                      column_to_select = "DA_ID",
                                      IDs = census_scales$DA$ID)
building_ids <-
  unlist(sapply(building_ids$IDs, jsonlite::fromJSON, USE.NAMES = FALSE))
building <- building[building$ID %in% building_ids, ]
building <- qs::qsave(building, "dev/data/built/building.qs")
building <- qs::qread("dev/data/built/building.qs")

# Add building scale to the dictionary
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "building",
                             sing = "dissemination area",
                             plur = "dissemination areas",
                             slider_title = "Building",
                             place_heading = "{name}",
                             place_name = "The dissemination area around {name}")

### Build CMHC scale
cmhc_zone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhc_zone <- additional_scale(additional_table = cmhc_zone,
                              DA_table = census_scales$DA,
                              ID_prefix = "cmhc",
                              name_2 = "CMHC zone",
                              crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhc_zone",
                             sing = "CMHC zone",
                             plur = "CMHC zones",
                             slider_title = "CMHC zone",
                             place_heading = "CMHC zone of {name}",
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

### Build 250-m grid scale
# grid <- sf::st_read("dev/data/climate_risk/Vulnerabilité_secheresses_2016.shp")
# grid <- grid[, "geometry"]
# grid$ID <- seq_len(nrow(grid))
# grid <- rev_geocode_sf(master_polygon = grid,
#                        sf_df = grid,
#                        province_code = "QC",
#                        crs = crs)
# grid <- grid[, c("name", "geometry")]
# qs::qsave(grid, file = "dev/data/built/grid.qs")
grid <- qs::qread("dev/data/built/grid.qs")

grid <- additional_scale(additional_table = grid,
                         DA_table = census_scales$DA,
                         ID_prefix = "grid",
                         name_2 = "250-m",
                         crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "grid",
                             sing = "250-m grid cell",
                             plur = "250-m grid cells",
                             slider_title = "250-m",
                             place_heading = "{name}",
                             place_name = "250-m grid cell around {name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(building = building),
                list(centraide = centraide),
                list(cmhc_zone = cmhc_zone),
                list(grid = grid))

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


# Build the datasets ------------------------------------------------------

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE)

scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              crs = crs, geo_uid = cancensus_cma_code)
scales_variables_modules <-
  ru_canale(scales_variables_modules = scales_variables_modules,
            crs = crs)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             crs = crs)


# Did you know ------------------------------------------------------------

source("dev/other/dyk.R")


# Translation -------------------------------------------------------------

source("dev/translation/build_translation.R", encoding = "utf-8")

