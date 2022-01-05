#### BUILD ALL SUS DATA ########################################################

# Create raw borough/CT/DA/grid tables ------------------------------------

# Import DA, CT and borough geometries
source("dev/geometries/census_geometries.R")

# Add centroids and buffers to DA
source("dev/geometries/DA_centroids.R")

# Import grid geometries
source("dev/geometries/grid_geometries.R")

# Geocode grid centroids
source("dev/geometries/grid_geocode.R")

# Add metadata to grid
source("dev/geometries/grid_process.R")

# Import building
source("dev/geometries/building.R")

# Geocode building centroids
source("dev/geometries/building_geocode.R")

# Import street edges
source("dev/geometries/street.R")

# Geocode street edges centroids
source("dev/geometries/street_geocode.R")


# Error checking ----------------------------------------------------------

stopifnot(
  
  # Check field names
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
  
  # Check row numbers
  nrow(borough) == 111,
  nrow(building) %in% c(56614, 66884),
  nrow(CT) == 970,
  nrow(DA) == 6469,
  nrow(grid) == 9923,
  nrow(street) == 68938,
  
  # Check geometry relations 
  sum(st_agr(borough) != "constant") == 0,
  sum(st_agr(building) != "constant") == 0,
  sum(st_agr(CT) != "constant") == 0,
  sum(st_agr(DA) != "constant") == 0,
  sum(st_agr(grid) != "constant") == 0,
  sum(st_agr(street) != "constant") == 0
)

source("dev/other/data_testing.R")
source("dev/other/meta_testing.R", encoding = "utf-8")
source("dev/other/breaks.R")


# Build variable table ----------------------------------------------------

variables <-
  tibble(
    var_code = character(),
    var_title = character(),
    var_short = character(),
    explanation = character(),
    category = character(),
    private = logical(),
    dates = list(),
    scales = list(),
    breaks_q3 = list(),
    breaks_q5 = list(),
    source = character()
  )

source("dev/other/add_variables.R")


# Add topic variables (modules) -------------------------------------------

source("dev/modules/census.R")
source("dev/modules/canale.R")
source("dev/modules/climate_risk.R")
source("dev/modules/covid.R", encoding = "utf-8")
source("dev/modules/crash.R")
source("dev/modules/access.R")
source("dev/modules/alley.R")
source("dev/modules/gentrification.R")
source("dev/modules/green_space.R")
# source("dev/modules/dmti.R")
source("dev/modules/stories.R")


# Run tests and post-processing -------------------------------------------

source("dev/other/post_processing.R")


# Save data files ---------------------------------------------------------

qsave(variables, file = "data/variables.qs")
qsavem(borough, CT, DA, file = "data/census.qsm")
qsave(grid, file = "data/grid.qs")
qsave(building, file = "data/building.qs")
qsave(street, file = "data/street.qs")
qsave(crash, file = "data/crash.qs")
qsave(tt_matrix, file = "data/tt_matrix.qs")
qsavem(alleys, alley_text, file = "data/alleys.qsm")
qsave(green_space, file = "data/green_space.qs")
qsave(stories, file = "data/stories.qs")
qsave(metro_lines, file = "data/metro_lines.qs")
qsavem(covid, covid_pics, file = "data/covid.qsm")


# Produce left and right maps ---------------------------------------------

library(patchwork)
source("dev/other/colours.R")

# Dependent script: needs 'borough' object
source("dev/other/produce_maps.R")


# Copy large data files to Dropbox ----------------------------------------

unlink(list.files("~/Dropbox/sus_sync/dev_data", full.names = TRUE),
       recursive = TRUE)
unlink(list.files("~/Dropbox/sus_sync/data", full.names = TRUE),
       recursive = TRUE)
unlink(list.files("~/Dropbox/sus_sync/www_maps", full.names = TRUE),
       recursive = TRUE)

invisible(file.copy(list.files("dev/data", full.names = TRUE),
                    "~/Dropbox/sus_sync/dev_data", recursive = TRUE))
invisible(file.copy(list.files("data", full.names = TRUE),
                    "~/Dropbox/sus_sync/data"))
invisible(file.copy(list.files("www/maps", full.names = TRUE),
                    "~/Dropbox/sus_sync/www_maps"))


# Cleanup -----------------------------------------------------------------

rm(add_q3, add_q5, add_variables, data_testing, find_breaks_q5, get_breaks_q3,
   get_breaks_q5, meta_testing)

