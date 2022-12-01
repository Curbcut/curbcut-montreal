#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------

library(cc.buildr)
library(DBI)
library(RSQLite)
future::plan(future::multisession())


# Base of the study region and dictionaries -------------------------------

# All regions
all_tables <-
  list("CMA" = c("CSD", "CT", "DA", "building"),
       "island" = c("CSD", "CT", "DA", "building"),
       "city" = c("CSD", "CT", "DA", "building"),
       "centraide" = c("centraide", "CT", "DA", "building"),
       "cmhc" = c("cmhczone"),
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
# Switch the City of Laval for the Sector
laval <- sf::st_read(paste0("dev/data/centraide/StatCan_Recensement2016/_Geograph",
                            "ie/Secteurs_damenagement_Ville_de_Laval.shp")) |> 
  sf::st_transform(4326) |> 
  transmute(name = gsub("Secteur \\d - ", "", Secteur), type = "Sector")

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
# # # From MySQL
# # building <- cc.data::db_read_long_table(table = "buildings",
# #                                          DA_ID = census_scales$DA$ID)
# # qs::qsave(building, file = "dev/data/built/building.qs")
# # # From Local
# building <- qs::qread("dev/data/canada_buildings.qs")
# building_ids <- cc.data::db_read_data(table = "buildings_DA_dict",
#                                       column_to_select = "DA_ID",
#                                       IDs = census_scales$DA$ID)
# building_ids <-
#   unlist(sapply(building_ids$IDs, jsonlite::fromJSON, USE.NAMES = FALSE))
# building <- building[building$ID %in% building_ids, ]
# building <- qs::qsave(building, "dev/data/built/building.qs")
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
grid <- qs::qread("dev/data/built/grid.qs") |> 
  sf::st_zm()

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
                list(cmhczone = cmhczone),
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

future::plan(list(future::tweak(future::multisession,
                                workers = 4),
                  future::tweak(future::multisession,
                                workers = length(cc.data::census_years))))

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

scales_variables_modules$scales <- 
  cc.buildr::reorder_columns(scales_variables_modules$scales)

qs::qsave(scales_variables_modules, 
          file = "dev/data/built/scales_variables_modules.qs")


# Did you know ------------------------------------------------------------

variables <- scales_variables_modules$variables
source("dev/other/dyk.R")
qs::qsave(dyk, "data/dyk.qs")


# Translation -------------------------------------------------------------

source("dev/translation/build_translation.R", encoding = "utf-8")


# Produce colours ---------------------------------------------------------

source("dev/other/colours.R")


# Postal codes ------------------------------------------------------------

#### POSTAL CODES


# Write stories -----------------------------------------------------------

source("dev/modules/stories.R", encoding = "utf-8")
qs::qsavem(stories, stories_mapping, file = "data/stories.qsm")


# Save data ---------------------------------------------------------------

# Building
# Save all buildings in the same database
building_path <- "data/building.sqlite"
if (building_path %in% list.files("data", full.names = TRUE)) unlink(building_path)
building_sql <- dbConnect(SQLite(), building_path)

map_over_scales(
  all_scales = scales_variables_modules$scales,
  fun = \(geo = geo, scales = scales, scale_name = scale_name,
          scale_df = scale_df) {
    if (scale_name != "building") return()
    geo_scale <- paste0(geo, "_building")
    df <- sf::st_drop_geometry(scale_df)[, c("ID", "name", "name_2", "DA_ID")]
    
    if (geo_scale %in% dbListTables(building_sql)) 
      dbRemoveTable(building_sql, geo_scale)
    
    dbWriteTable(building_sql, "pre_pk_building", df)
    dbExecute(building_sql, paste0("CREATE TABLE ", geo_scale,
                                   " (ID VARCHAR, ",
                                   "name VARCHAR, ",
                                   "name_2 VARCHAR, ",
                                   "DA_ID VARCHAR,
                     CONSTRAINT building_pk PRIMARY KEY (ID))"))
    dbExecute(building_sql, 
              paste0("INSERT INTO ", geo_scale, " SELECT * FROM pre_pk_building"))
    dbExecute(building_sql, "DROP TABLE pre_pk_building")
  })

# Drop geometry of other scales
all_scales <- 
  map_over_scales(
    all_scales = scales_variables_modules$scales,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
            scale_df = scale_df) {
      if (scale_name == "building") return()
      sf::st_drop_geometry(scale_df)
    })
all_scales <- lapply(all_scales, \(x) x[!sapply(x, is.null)])
variables <- scales_variables_modules$variables

# List all the future tables insite each database
progressr::with_progress({
  pb <- progressr::progressor(steps = sum(sapply(all_scales, length)))
  
  sql_table_list <- 
    map_over_scales(
      all_scales = all_scales,
      fun = \(geo = geo, scales = scales, scale_name = scale_name,
              scale_df = scale_df) {
        
        var_combinations <- 
          lapply(variables$var_code, \(y) {
            vars <- names(scale_df)[grepl(y, names(scale_df))]
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
        
        pb()
        lapply(var_combinations, \(x) scale_df[, c("ID", x)])
        
      })
})

progressr::with_progress({
  pb <- progressr::progressor(steps = sum(sapply(all_scales, length)))
  
  map_over_scales(
    all_scales = all_scales,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
            scale_df = scale_df) {
      
      geo_scale <- paste(geo, scale_name, sep = "_")
      
      geo_scale_table_list <- sql_table_list[[geo]][[scale_name]]
      
      sqlite_path <- paste0("data/", geo_scale, ".sqlite")
      
      db <- dbConnect(SQLite(), sqlite_path)
      mapply(\(df, y) 
             dbWriteTable(db, y, df, overwrite = TRUE),
             geo_scale_table_list, names(geo_scale_table_list))
      dbDisconnect(db)
      
      pb()
    }) |> invisible()
})

# Add centroid
progressr::with_progress({
  pb <- progressr::progressor(steps = sum(sapply(all_scales, length)))
  
  map_over_scales(
    all_scales = all_scales,
    fun = \(geo = geo, scales = scales, scale_name = scale_name,
            scale_df = scale_df) {
      
      geo_scale <- paste(geo, scale_name, sep = "_")
      with_geo <- scales_variables_modules$scales[[geo]][[scale_name]][, "ID"]
      
      centroids <- lapply(with_geo$geometry, sf::st_centroid)
      lat <- sapply(centroids, `[[`, 1)
      lon <- sapply(centroids, `[[`, 2)
      
      df <- sf::st_drop_geometry(with_geo)
      
      df$lat <- lat
      df$lon <- lon
      
      sqlite_path <- paste0("data/", geo_scale, ".sqlite")
      
      db <- dbConnect(SQLite(), sqlite_path)
      dbWriteTable(db, "centroid", df, overwrite = TRUE)
      dbDisconnect(db)
      
      pb()
    }) |> invisible()
})


# Save data files to data2 ------------------------------------------------

# Keep all_tables in data2/
qs::qsave(scales_variables_modules, file = "data2/scales_variables_modules.qs")


# Save data files ---------------------------------------------------------

# data/geometry_export before dropping geometries
map_over_scales(
  all_scales = all_scales,
  fun = \(geo = geo, scales = scales, scale_name = scale_name,
          scale_df = scale_df) {
    geo_scale <- paste(geo, scale_name, sep = "_")
    out <- scale_df[, "ID"]
    file_link <- paste0("data/geometry_export/", geo_scale, ".qs")
    qs::qsave(out, file = file_link)
  }) |> invisible()

# Save the shorter tables in data/
iwalk(all_scales, function(scls, geo) {
  
  scls <- lapply(scls, \(x) {
    x |> 
      sf::st_drop_geometry() |>
      dplyr::select(ID:households)
  })
  names(scls) <- paste(geo, names(scls), sep = "_")
  
  purrr::imap(scls, \(s, n) {
    assign(n, s, envir = .GlobalEnv)
  })
  
  do.call(qsavem, c(map(names(scls), rlang::sym),
                    file = paste0("data/", geo, ".qsm")))
})

# Keep strings of all available tables in each db
tables_in_sql <- map_over_scales(
  all_scales = sql_table_list,
  fun = \(geo = geo, scales = scales, scale_name = scale_name,
          scale_df = scale_df) {
    names(scale_df)
  })
tables_in_sql <- unlist(tables_in_sql, recursive = FALSE)
names(tables_in_sql) <- gsub("\\.", "_", names(tables_in_sql))
qsave(tables_in_sql, file = "data/tables_in_sql.qs")

## global data
qsave(scales_variables_modules$variables, file = "data/variables.qs")
qsave(scales_variables_modules$modules, file = "data/modules.qs")
qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qsave(regions_dictionary, file = "data/regions_dictionary.qs")
