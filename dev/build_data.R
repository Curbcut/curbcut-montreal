#### BUILD ALL SUS DATA ########################################################


# Load funtions -----------------------------------------------------------

library(tidyverse)
library(sf)
library(qs)
source("dev/other/data_testing.R")
source("dev/other/meta_testing.R", encoding = "utf-8")
source("dev/other/breaks.R")
source("dev/other/char_fix.R", encoding = "utf-8")
source("dev/other/interpolate_assign.R")
source("dev/other/is_in_geometry.R")


# Vector of tables --------------------------------------------------------

all_tables <- 
  list("CMA" = c("borough", "CT", "DA", "grid", "building"),
       "island" = c("borough", "CT", "DA", "grid", "building"),
       "city" = c("borough", "CT", "DA", "grid", "building"),
       "centraide" = c("centraide", "CT", "DA", "grid", "building"))


# Import all geometries, and create master polygon ------------------------

shp_present <- 
  list.files("dev/data/geometries/") |> 
  str_subset("\\.shp$") |> 
  str_remove("\\.shp$")

# Error check
if (!all(names(all_tables) %in% shp_present)) {
  missing_shp <- names(all_tables)[which(!names(all_tables) %in% shp_present)]
  stop(paste0("The shapefile for `", missing_shp, 
              "` is missing in 'dev/data/geometry'."))
}

# A polygon covering all our geographies
walk(names(all_tables), function(shp_file) {
  out <- paste0("dev/data/geometries/", shp_file, ".shp") |> 
    read_sf() |> 
    st_transform(32618) |> 
    st_union() |> 
    st_make_valid()
  
  assign(paste0(shp_file, "_shp"), out, envir = .GlobalEnv)
})

# Master polygon creation
master_polygon <- 
  map(paste0(names(all_tables), "_shp"), get) |> 
  reduce(st_union) |> 
  st_transform(4326)

rm(shp_present)


# Create raw tables --------------------------------------------------------

# Import DA, CT and borough geometries
source("dev/geometries/census_geometries.R")

# Import centraide geometries
source("dev/geometries/centraide_geometries.R")

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


# Separate DA, CT and borough in their macro scale ------------------------

is_in_geometry(all_tables, crs = 32618, 
               update_name_2_for = c("centraide"))


# Error checking ----------------------------------------------------------

# stopifnot(
#   
#   # Check field names
#   names(borough) == c("ID", "name", "name_2", "CSDUID", "population", 
#                       "households", "geometry"),
#   names(building) == c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
#                        "osm_ID", "grid_ID", "geometry"),
#   names(CT) == c("ID", "name", "name_2", "CTUID", "CSDUID", "population", 
#                  "households", "geometry"),
#   names(DA) == c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
#                  "population", "households", "centroid", "buffer", "building",
#                  "geometry"),
#   names(grid) == c("ID", "name", "name_2", "CSDUID", "population", "households", 
#                    "geometry"),
#   names(street) == c("ID", "name", "name_2", "street_type", "DAUID", "CTUID", 
#                      "CSDUID", "osm_ID", "grid_ID", "population", "households",
#                      "geometry"),
#   
#   # Check row numbers
#   nrow(borough) == 111,
#   nrow(building) == 860499,
#   nrow(CT) == 970,
#   nrow(DA) == 6469,
#   nrow(grid) == 9923,
#   nrow(street) == 88538,
#   
#   # Check geometry columns,
#   inherits(borough$geometry, "sfc"),
#   inherits(building$geometry, "sfc"),
#   inherits(CT$geometry, "sfc"),
#   inherits(DA$buffer, "sfc"),
#   inherits(DA$centroid, "sfc"),
#   inherits(DA$building, "sfc"),
#   inherits(DA$geometry, "sfc"),
#   inherits(grid$geometry, "sfc"),
#   inherits(street$geometry, "sfc"),
#   mean(st_is(borough$geometry, "MULTIPOLYGON")) == 1,
#   mean(st_is(building$geometry, "MULTIPOLYGON")) == 1,
#   mean(st_is(CT$geometry, "MULTIPOLYGON")) == 1,
#   mean(st_is(DA$buffer, "POLYGON")) == 1,
#   mean(st_is(DA$centroid, "POINT")) == 1,
#   mean(st_is(DA$building, "MULTIPOLYGON")) == 1,
#   mean(st_is(DA$geometry, "MULTIPOLYGON")) == 1,
#   mean(st_is(grid$geometry, "MULTIPOLYGON")) == 1,
#   mean(st_is(street$geometry, "LINESTRING")) == 1,
#   
#   # Check geometry relations 
#   sum(st_agr(borough) != "constant") == 0,
#   sum(st_agr(building) != "constant") == 0,
#   sum(st_agr(CT) != "constant") == 0,
#   sum(st_agr(DA) != "constant") == 0,
#   sum(st_agr(grid) != "constant") == 0,
#   sum(st_agr(street) != "constant") == 0
# )


# Build variable table ----------------------------------------------------

variables <-
  tibble(
    var_code = character(),
    var_title = character(),
    var_short = character(),
    explanation = character(),
    theme = character(),
    category = character(),
    private = logical(),
    dates = list(),
    scales = list(),
    breaks_q3 = list(),
    breaks_q5 = list(),
    source = character(),
    interpolated = list(),
    grouping = character(),
    group_diff = list()
  )

source("dev/other/add_variables.R")

# Produce colours ---------------------------------------------------------

source("dev/other/colours.R")


# Build module table ------------------------------------------------------

modules <-
  tibble(
    id = character(),
    metadata = logical(),
    dataset_info = character(),
  )

source("dev/other/add_modules.R")


# Add topic variables (modules) -------------------------------------------

source("dev/modules/census.R")
source("dev/modules/canale.R")
source("dev/modules/climate_risk.R")
source("dev/modules/access.R")
source("dev/modules/alley.R")
source("dev/modules/cent_d.R")
source("dev/modules/cent_p.R")
source("dev/modules/amenities.R")
source("dev/modules/natural_inf.R")

source("dev/modules/stories.R", encoding = "utf-8")
source("dev/modules/place_explorer.R")


# Post-processing ---------------------------------------------------------

source("dev/other/post_processing.R")


# Produce title_text and dyk ----------------------------------------------

source("dev/other/dyk.R")
source("dev/other/title_text.R")


# Build translation qs ----------------------------------------------------

source("dev/translation/build_translation.R", encoding = "utf-8")


# Remove geometries -------------------------------------------------------
progressr::with_progress({
  p <- progressr::progressor(steps = sum(map_int(all_tables, length)))
  
  iwalk(all_tables, function(scales, geo) {
    walk(scales, function(scale) {
      geo_scale <- paste(geo, scale, sep = "_")
      
      # Assign the df _full with the geometry
      assign(paste0(geo_scale, "_full"), get(geo_scale), envir = .GlobalEnv)
      
      # Assign the non geometry df
      if (scale == "building") {
        assign(geo_scale, st_drop_geometry(get(geo_scale)), envir = .GlobalEnv)
      } else {
        assign(geo_scale,
               get(paste0(geo_scale, "_full")) |> 
                 select(-any_of(c("building", "buffer", "centroid"))) |> 
                 rowwise() |> 
                 mutate(centroid = 
                          list(as.numeric(st_coordinates(st_centroid(geometry))))) |> 
                 ungroup() |> 
                 st_drop_geometry(), 
               envir = .GlobalEnv)
      }

      p()

    })
  })
})

# Save data files ---------------------------------------------------------

# Save the tables in data and data2, except buildings
iwalk(all_tables, function(scales, geo) {
  
  scales_no_full <- scales[scales != "building"]
  geo_scales <- paste(geo, scales_no_full, sep = "_")
  geo_scales_full <- paste(geo, scales, "full", sep = "_")
  
  do.call(qsavem, c(map(geo_scales, rlang::sym),
                    file = paste0("data/", geo, ".qsm"),
                    nthreads = length(scales)))
  
  do.call(qsavem, c(map(geo_scales_full, rlang::sym),
                    file = paste0("data2/", geo, "_full.qsm"),
                    nthreads = length(scales)))
  
})

## global data
qsave(variables, file = "data/variables.qs")
qsave(modules, file = "data/modules.qs")
qsave(postal_codes, file = "data/postal_codes.qs")
qsave(metro_lines, file = "data2/metro_lines.qs")
qsave(dyk, "data/dyk.qs")
qsave(title_text, "data/title_text.qs")

## census related
qsave(census_variables, file = "data/census_variables.qs")

## other
qsavem(alley, alley_text, file = "data/alley.qsm")
qsavem(title_card_indicators, pe_var_hierarchy, pe_theme_order, CSDUID_groups,
       title_card_index, pe_variable_order, file = "data/place_explorer.qsm")
qsavem(stories, stories_mapping, file = "data/stories.qsm")

# data/geometry_export
iwalk(all_tables, function(scales, geo) {
  walk(scales, function(scale) {
    geo_scale <- paste(geo, scale, sep = "_")
    out <- select(get(geo_scale), ID)
    file_link <- paste0("data/geometry_export/", geo_scale, ".qs")
    
    qsave(out, file = file_link)
  })
})


# Save files we'll save in the SQL to data2 -------------------------------

qsavem(natural_inf, natural_inf_custom, file = "data2/natural_inf.qsm")
qsave(tt_matrix, file = "data2/tt_matrix.qs")
# qsave(building, file = "data2/building.qs")
# qsave(grid, file = "data2/grid.qs")


# Save data to the sql db -------------------------------------------------

library(RSQLite)
sqlite_path <- "data/sql_db.sqlite"

# Overwrite!
unlink(sqlite_path)

# Create the db
db <- dbConnect(SQLite(), sqlite_path)

# natural_inf
dbWriteTable(db, "natural_inf_custom", natural_inf_custom)

purrr::walk2(names(natural_inf), natural_inf, function(name, df) {
  if (!is.data.frame(df)) {
    purrr::walk2(df, seq_along(df), function(x, y) {
      dbWriteTable(db, paste("natural_inf", name, y, sep = "_"), x)
      dbExecute(db, paste0("CREATE INDEX index_biodiversity_", y, 
                            " ON natural_inf_custom_", y, " (biodiversity)"))
    })
  }
  else {
    dbWriteTable(db, paste("natural_inf", name, sep = "_"), df)
  }
  
  if (name == "custom_explore") {
    dbExecute(db, paste0("CREATE INDEX index_natural_inf_custom_explore_slider",
                         " ON natural_inf_custom_explore (slider)"))
  }
})

# tt_matrix
dbWriteTable(db, "tt_matrix", tt_matrix)
dbExecute(db, paste0("CREATE INDEX index_tt_matrix_destination",
                     " ON tt_matrix (destination)"))

# building with primary key
iwalk(all_tables, function(scales, geo) {
  if ("building" %in% scales) {
    geo_scale <- paste0(geo, "_building")
    # dbExecute(db, paste0("DROP TABLE ", geo_scale))
    df <- select(get(geo_scale), ID, name, name_2, DAUID)
    dbWriteTable(db, "pre_pk_building", df)
    dbExecute(db, paste0("CREATE TABLE ", geo_scale,
                         " (ID VARCHAR, ",
                         "name VARCHAR, ",
                         "name_2 VARCHAR, ",
                         "DAUID VARCHAR,
                     CONSTRAINT building_pk PRIMARY KEY (ID))"))
    dbExecute(db, paste0("INSERT INTO ", geo_scale, " SELECT * FROM pre_pk_building"))
    dbExecute(db, "DROP TABLE pre_pk_building")
    
  }
})

# # grid with primary key
# dbWriteTable(db, "pre_pk_grid", grid)
# # Construct column names and type to
# col_names_types <- 
#   paste0(names(grid), " ", purrr::map_chr(names(grid), ~{class(grid[[.x]])}) |> 
#          str_replace_all("character", "VARCHAR") |> 
#          str_replace_all("integer", "INTEGER") |> 
#          str_replace_all("numeric", "DOUBLE")) |> 
#   paste0(collapse = ", ")
# dbExecute(db, paste0("CREATE TABLE grid ",
#                      "(", col_names_types, ", ",
#                      "CONSTRAINT grid_pk PRIMARY KEY (ID))"))
# dbExecute(db, "INSERT INTO grid SELECT * FROM pre_pk_grid")
# dbExecute(db, "DROP TABLE pre_pk_grid")

# List active dataframes in the db
dbListTables(db)
# Close the connection
dbDisconnect(db)


# Copy large data files to Dropbox ----------------------------------------

source("dev/dropb_automation/01_zip_and_export.R")


# Add hash ----------------------------------------------------------------

hash <- sapply(list.files("data", full.names = TRUE, recursive = TRUE), 
               digest::digest, file = TRUE)
qsave(hash, file = "hash.qs")


# Cleanup -----------------------------------------------------------------

rm(add_q3, add_q5, add_variables, data_testing, find_breaks_q5, get_breaks_q3,
   get_breaks_q5, meta_testing, natural_infrastructure)


# Deploy app --------------------------------------------------------------

hash <- qread("hash.qs")
stopifnot(all(sapply(list.files("data", full.names = TRUE, recursive = TRUE), 
                     digest::digest, 
                     file = TRUE) == hash))
source("dev/other/deploy_sus.R")

deploy_sus("sus-mcgill-centraide") # Centraide
deploy_sus("sus-mcgill-test") # Development
deploy_sus("sus-mcgill") # Production
