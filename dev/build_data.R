#### BUILD ALL SUS DATA ########################################################


# Load functions ----------------------------------------------------------

library(tidyverse)
library(sf)
library(qs)
library(furrr)
warning("Don't forget to set a `future::plan()` for faster data building.")
source("dev/other/data_testing.R")
source("dev/other/meta_testing.R", encoding = "utf-8")
source("dev/other/breaks.R")
source("dev/other/char_fix.R", encoding = "utf-8")
source("dev/other/get_census.R")
source("dev/other/interpolate_assign.R")
source("dev/other/is_in_geometry.R")


# Vector of tables --------------------------------------------------------

all_tables <- 
  list("CMA" = c("CSD", "CT", "DA", "grid", "building"),
       "island" = c("CSD", "CT", "DA", "grid", "building"),
       "city" = c("CSD", "CT", "DA", "DB", "grid", "building"),
       "centraide" = c("centraide", "CT", "DA", "grid", "building"),
       "cmhc" = "cmhczone")


# Import all geometries, and create master polygon ------------------------

shp_present <- 
  list.files("dev/data/geometry/") |> 
  str_subset("\\.shp$") |> 
  str_remove("\\.shp$")

# Error check
if (!all(names(all_tables) %in% shp_present)) {
  missing_shp <- names(all_tables)[which(!names(all_tables) %in% shp_present)]
  stop(paste0("The shapefile for `", missing_shp, 
              "` is missing in 'dev/data/geometry/'."))
}

# A polygon covering all our geographies
walk(names(all_tables), function(shp_file) {
  out <- paste0("dev/data/geometry/", shp_file, ".shp") |> 
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

# Import building
source("dev/geometries/building.R")

# Geocode building centroids
source("dev/geometries/building_geocode.R")

# Import centraide geometries
source("dev/geometries/centraide_geometries.R")

# Import CMHC zones
cmhczone <- read_sf("dev/data/geometry/cmhc.shp") |> 
  mutate(name_2 = "CMHC Zone")

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
               update_name_2_for = c("centraide", "cmhc"))


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
source("dev/modules/canbics.R")
source("dev/modules/climate_risk.R")
source("dev/modules/access.R")
source("dev/modules/alley.R")
source("dev/modules/cent_d.R")
source("dev/modules/cent_p.R")
source("dev/modules/vac_rate.R")
source("dev/modules/amenities.R")
source("dev/modules/city_amenities.R")
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


# # Remove geometries -------------------------------------------------------
# progressr::with_progress({
#   p <- progressr::progressor(steps = sum(map_int(all_tables, length)))
#   
#   iwalk(all_tables, function(scales, geo) {
#     walk(scales, function(scale) {
#       geo_scale <- paste(geo, scale, sep = "_")
#       
#       # Assign the df _full with the geometry
#       assign(paste0(geo_scale, "_full"), get(geo_scale), envir = .GlobalEnv)
#       
#       # Assign the non geometry df
#       if (scale == "building") {
#         assign(geo_scale, st_drop_geometry(get(geo_scale)), envir = .GlobalEnv)
#       } else {
#         assign(geo_scale,
#                get(paste0(geo_scale, "_full")) |> 
#                  select(-any_of(c("building", "buffer", "centroid"))) |> 
#                  rowwise() |> 
#                  mutate(centroid = 
#                           list(as.numeric(st_coordinates(st_centroid(geometry))))) |> 
#                  ungroup() |> 
#                  st_drop_geometry(), 
#                envir = .GlobalEnv)
#       }
# 
#       p()
# 
#     })
#   })
# })

# Save data to the sql db -------------------------------------------------

library(RSQLite)

# Save natural infrastructure in its own database
natural_inf_path <- "data/natural_inf.sqlite"
if (natural_inf_path %in% list.files("data", full.names = TRUE)) 
  unlink(natural_inf_path)
natural_inf_sql <- dbConnect(SQLite(), natural_inf_path)

dbWriteTable(natural_inf_sql, "natural_inf_custom", natural_inf_custom)
purrr::walk2(names(natural_inf), natural_inf, function(name, df) {
  if (!is.data.frame(df)) {
    purrr::walk2(df, seq_along(df), function(x, y) {
      dbWriteTable(natural_inf_sql, paste("natural_inf", name, y, sep = "_"), x)
      dbExecute(natural_inf_sql, paste0("CREATE INDEX index_biodiversity_", y, 
                           " ON natural_inf_custom_", y, " (biodiversity)"))
    })
  }
  else {
    dbWriteTable(natural_inf_sql, paste("natural_inf", name, sep = "_"), df)
  }
  
  if (name == "custom_explore") {
    dbExecute(natural_inf_sql, paste0("CREATE INDEX index_natural_inf_custom_explore_slider",
                         " ON natural_inf_custom_explore (slider)"))
  }
})

dbListTables(natural_inf_sql)
dbDisconnect(natural_inf_sql)


# Save tt_matrix in its own database
tt_matrix_path <- "data/tt_matrix.sqlite"
if (tt_matrix_path %in% list.files("data", full.names = TRUE)) 
  unlink(tt_matrix_path)
tt_matrix_sql <- dbConnect(SQLite(), tt_matrix_path)

dbWriteTable(tt_matrix_sql, "tt_matrix", tt_matrix)
dbExecute(tt_matrix_sql, paste0("CREATE INDEX index_tt_matrix_destination",
                     " ON tt_matrix (destination)"))

dbListTables(tt_matrix_sql)
dbDisconnect(tt_matrix_sql)

# Save all buildings in the same database
building_path <- "data/building.sqlite"
if (building_path %in% list.files("data", full.names = TRUE)) 
  unlink(building_path)
building_sql <- dbConnect(SQLite(), building_path)

iwalk(all_tables, function(scales, geo) {
  if ("building" %in% scales) {
    geo_scale <- paste0(geo, "_building")
    # dbExecute(building_sql, paste0("DROP TABLE ", geo_scale))
    df <- select(st_drop_geometry(get(geo_scale)), ID, name, name_2, DAUID)
    dbWriteTable(building_sql, "pre_pk_building", df)
    dbExecute(building_sql, paste0("CREATE TABLE ", geo_scale,
                         " (ID VARCHAR, ",
                         "name VARCHAR, ",
                         "name_2 VARCHAR, ",
                         "DAUID VARCHAR,
                     CONSTRAINT building_pk PRIMARY KEY (ID))"))
    dbExecute(building_sql, paste0("INSERT INTO ", geo_scale, " SELECT * FROM pre_pk_building"))
    dbExecute(building_sql, "DROP TABLE pre_pk_building")
    
  }
})

dbListTables(building_sql)
dbDisconnect(building_sql)


# Save all_tables in each their database. Each region/scale combination is a db.

# Make use of parallelization to create the lists of tables
plan(list(tweak(multisession, workers = 5),
          tweak(multisession, workers = 4)))

all_d <- imap(all_tables, function(scales, geo) {
  scales_no_full <- scales[scales != "building"]
  map(set_names(scales_no_full), function(scale) {
    st_drop_geometry(get(paste(geo, scale, sep = "_")))
  })
})

progressr::with_progress({
  p <- progressr::progressor(steps = sum(map_int(map(all_tables, ~.x[.x != "building"]), 
                                                 length)))
  sql_table_list <- 
    future_imap(all_tables, function(scales, geo) {
      scales_no_full <- scales[scales != "building"]
      future_map(set_names(scales_no_full), function(scale) {
        
        geo_scale <- paste(geo, scale, sep = "_")
        geo_scale_data <- all_d[[geo]][[scale]]
        
        var_combinations <- 
          lapply(variables$var_code, \(y) {
            vars <- names(geo_scale_data)[grepl(y, names(geo_scale_data))]
            vars <- str_subset(vars, "_q5|_q3", negate = TRUE)
            
            sapply(vars, \(x) {
              time_format <- "\\d{4}$"
              q3 <- paste0(str_remove(x, time_format), 
                           if (str_detect(x, time_format)) "q3_" else "_q3", 
                           na.omit(str_extract(x, time_format)))
              q5 <- paste0(str_remove(x, time_format), 
                           if (str_detect(x, time_format)) "q5_" else "_q5", 
                           na.omit(str_extract(x, time_format)))
              
              c(x, q3, q5)
            }, simplify = FALSE, USE.NAMES = TRUE)
          }) |> reduce(c)
        
        p()
        
        lapply(var_combinations, \(x) geo_scale_data[, c("ID", x)])
        
      })
    })
})

progressr::with_progress({
  p <- progressr::progressor(steps = sum(map_int(map(all_tables, ~.x[.x != "building"]), 
                                                 length)))
  
  iwalk(all_tables, function(scales, geo) {
    scales_no_full <- scales[scales != "building"]
    walk(scales_no_full, function(scale) {
      
      geo_scale <- paste(geo, scale, sep = "_")
      
      geo_scale_table_list <- sql_table_list[[geo]][[scale]]
      
      sqlite_path <- paste0("data/", geo_scale, ".sqlite")
      
      db <- dbConnect(SQLite(), sqlite_path)
      iwalk(geo_scale_table_list, \(df, y) dbWriteTable(db, y, df, overwrite = TRUE))
      dbDisconnect(db)
      
      p()
    })
  })
})

# Add centroid
progressr::with_progress({
  p <- progressr::progressor(steps = sum(map_int(map(all_tables, ~.x[.x != "building"]), 
                                                 length)))
  
  iwalk(all_tables, function(scales, geo) {
    scales_no_full <- scales[scales != "building"]
    walk(scales_no_full, function(scale) {
      
      geo_scale <- paste(geo, scale, sep = "_")
      
      df <- get(geo_scale)[, "ID"]
      centroids <- lapply(df$geometry, st_centroid)
      lat <- sapply(centroids, `[[`, 1)
      lon <- sapply(centroids, `[[`, 2)
      
      df <- st_drop_geometry(df)
      
      df$lat <- lat
      df$lon <- lon
      
      sqlite_path <- paste0("data/", geo_scale, ".sqlite")

      db <- dbConnect(SQLite(), sqlite_path)
      dbWriteTable(db, "centroid", df, overwrite = TRUE)
      dbDisconnect(db)
      
      p()
    })
  })
})


# Save data files to data2 ------------------------------------------------

# Keep all_tables in data2/
iwalk(all_tables, function(scales, geo) {
  geo_scales <- paste(geo, scales, sep = "_")
  do.call(qsavem, c(map(geo_scales, rlang::sym),
                    file = paste0("data2/", geo, ".qsm"),
                    nthreads = length(scales)))
})

# Other data which will be in the SQL databases. Keep versions in data2/
qsavem(natural_inf, natural_inf_custom, file = "data2/natural_inf.qsm")
qsave(tt_matrix, file = "data2/tt_matrix.qs")

# Save data files ---------------------------------------------------------

# data/geometry_export before dropping geometries
iwalk(all_tables, function(scales, geo) {
  walk(scales, function(scale) {
    geo_scale <- paste(geo, scale, sep = "_")
    out <- select(get(geo_scale), ID)
    file_link <- paste0("data/geometry_export/", geo_scale, ".qs")
    
    qsave(out, file = file_link)
  })
})


# Save the shorter tables in data except buildings. 
# Start by dropping all variables.
future_imap(all_tables, function(scales, geo) {
  scales_no_full <- scales[scales != "building"]
  map(scales_no_full, function(scale) {
    geo_scale <- paste(geo, scale, sep = "_")
    assign(geo_scale,
           get(geo_scale, envir = .GlobalEnv) |>
             st_drop_geometry() |>
             select(ID:households),
           envir = .GlobalEnv)
  })
})

# Save all the tables
iwalk(all_tables, function(scales, geo) {
  scales_no_full <- scales[scales != "building"]
  geo_scales <- paste(geo, scales_no_full, sep = "_")
  
  do.call(qsavem, c(map(geo_scales, rlang::sym),
                    file = paste0("data/", geo, ".qsm"),
                    nthreads = length(scales)))
})

# Keep strings of all available tables in each db
geos_scales <- 
  imap(all_tables, function(scales, geo) {
    scales_no_full <- scales[scales != "building"]
    map(scales_no_full, function(scale) {
      paste(geo, scale, sep = "_")
    })
  }) |> unlist() |> unname()
tables_in_sql <- 
map(set_names(geos_scales), \(x) {
  db <- dbConnect(SQLite(), paste0("data/", x, ".sqlite"))
  out <- dbListTables(db)
  dbDisconnect(db)
  
  out
})

# Save list of tables in db
qsave(tables_in_sql, file = "data/tables_in_sql.qs")

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
qsavem(title_card_indicators, pe_var_hierarchy, pe_theme_order,
       title_card_index, pe_variable_order, file = "data/place_explorer.qsm")
qsavem(stories, stories_mapping, file = "data/stories.qsm")


# Copy data files to a AWS bucket -----------------------------------------

cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.montreal.data")


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

renv::activate()

