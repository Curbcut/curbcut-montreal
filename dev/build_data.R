#### Build geometries ##########################################################
# Caller Script 


# Create raw borough/CT/DA/grid tables ------------------------------------

# Import DA, CT and borough geometries (independent script)
source("dev/geometries/census_geometries.R")

# Add centroids and buffers to DA (dependent script: needs 'DA')
source("dev/geometries/DA_centroids.R")

# Import grid geometries (independent script)
source("dev/geometries/grid_geometries.R")

# Geocode grid centroids (dependent script: needs 'borough')
source("dev/geometries/grid_geocode.R")

# Add metadata to grid (dependent: needs 'borough', 'CT', 'DA' and 'grid')
source("dev/geometries/grid_process.R")

# Import building (dependent: needs 'DA')
source("dev/geometries/building.R")

# Geocode building centroids (dependent: needs 'building')
source("dev/geometries/building_geocode.R")

# Import street edges (dependent: needs 'borough' and 'DA')
source("dev/geometries/street.R")

# Geocode street edges centroids (dependent script: needs 'street')
source("dev/geometries/street_geocode.R")


# Error checking ----------------------------------------------------------

# Eventually there should be some proper error checking here, but for now....
stopifnot(
  names(borough) == c("ID", "name", "name_2", "population", "households", 
                      "geometry"),
  names(building) == c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                       "osm_ID", "grid_ID", "population", "households", 
                       "geometry"),
  names(CT) == c("ID", "name", "name_2", "CSDUID", "population", "households", 
                 "geometry"),
  names(DA) == c("ID", "name", "name_2", "CTUID", "CSDUID", "population", 
                 "households", "centroid", "buffer", "geometry"),
  names(grid) == c("ID", "name", "name_2", "CSDUID", "population", "households", 
                   "geometry"),
  names(street) == c("ID", "name", "name_2", "street_type", "DAUID", "CTUID", 
                     "CSDUID", "osm_ID", "grid_ID", "population", "households",
                     "geometry"),
  nrow(borough) == 111,
  nrow(building) == 56614,
  nrow(CT) == 970,
  nrow(DA) == 6469,
  nrow(grid) == 9923,
  nrow(street) == 68938
)


# Add topic variables (modules) -------------------------------------------

var_exp <- tibble(var_code = character(), var_name = character(),
        explanation = character())

source("dev/modules/census/build_census.R")
source("dev/modules/canale.R")
source("dev/modules/climate_risk.R")
# source("dev/modules/dmti.R")
# source("dev/modules/alley.R")
source("dev/modules/crash.R")
# source("dev/modules/ped.R")


# Save data files ---------------------------------------------------------

qsave(var_exp, file = "data/var_exp.qs")
qsavem(borough, CT, DA, file = "data/census.qsm")
qsave(grid, file = "data/grid.qs")
qsave(building, file = "data/building.qs")
qsave(street, file = "data/street.qs")
# qsave(street, file = "data/street.qs")
# qsave(green_space, file = "data/green_space.qs")
# qsavem(alleys, alley_text, file = "data/alleys.qsm")
qsave(crash, file = "data/crash.qs")


# Produce left and right maps ---------------------------------------------

library(patchwork)
source("dev/geometries/colours.R")

# Dependent script: needs 'borough' object
source("dev/geometries/produce_maps.R")
