#### CC GLOBALS ################################################################


# Platform dependant ------------------------------------------------------

site_name <- "Curbcut Montréal"
site_url <- "https://montreal.curbcut.ca"
stories_page <- "Montréal stories"
tileset_prefix <- "mtl"
mapbox_username <- "curbcut"

# For a location lock placeholder in advanced options
default_random_address <- "845 Sherbrooke Ouest, Montréal, Quebec"

map_zoom <- 9.9
map_loc <- c(lat = -73.70, lon = 45.53)


# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(curbcut)
  
  library(DBI)
  library(RSQLite)

  library(cc.landing)
  library(cc.map)
})

# Shiny options -----------------------------------------------------------

options(shiny.fullstacktrace = TRUE)
options(shiny.useragg = TRUE)
shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "cache")))


# Data --------------------------------------------------------------------

# Load all .qs and .qsm files that are in the root of the data folder
data_files <- list.files("data", full.names = TRUE)
invisible(lapply(data_files[grepl("qsm$", data_files)],
  qs::qload,
  env = .GlobalEnv
))
invisible(lapply(
  data_files[grepl("qs$", data_files)],
  \(x) {
    object_name <- gsub("(data/)|(\\.qs)", "", x)
    assign(object_name, qs::qread(x), envir = .GlobalEnv)
  }
))


# Connect to the dbs ------------------------------------------------------

dbs <- list.files("data", full.names = TRUE)
dbs <- subset(dbs, grepl(".sqlite$", dbs))

lapply(dbs, \(x) {
  connection_name <- paste0(stringr::str_extract(x, "(?<=/).*?(?=\\.)"), "_conn")
  assign(connection_name, DBI::dbConnect(RSQLite::SQLite(), x), envir = .GlobalEnv)
}) |> invisible()


# Map defaults ------------------------------------------------------------

map_token <- paste0(
  "pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg"
)
map_base_style <- "mapbox://styles/curbcut/cljkciic3002h01qveq5z1wrp"

first_level_choropleth <-
  sapply(ls()[grepl("map_zoom_levels_", ls())], \(x) names(get(x)[1]),
    USE.NAMES = FALSE
  ) |> unique()

all_choropleths <-
  sapply(ls()[grepl("map_zoom_levels_", ls())], get, simplify = FALSE, USE.NAMES = TRUE) |>
  unname() |> 
  unlist() |> 
  names()


# Declare temporary folder ------------------------------------------------

temp_folder <- tempdir()
addResourcePath("temp_folder_shortcut", temp_folder)


# Create the UI and server functions for basic modules --------------------

curbcut::create_ui_server_mods(modules = modules)


# Set up fonts ------------------------------------------------------------

systemfonts::register_font(
  name = "acidgrotesk-book",
  plain = list("www/fonts/acidgrotesk-book.woff", 0)
)


# Source the R folder -----------------------------------------------------

# Curbcut works with global environment. Must source to the current global env
lapply(list.files("R", full.names = TRUE), source, verbose = FALSE)
