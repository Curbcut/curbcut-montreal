#### SUS GLOBALS ###############################################################

# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)

  library(rdeck)
  library(ggplot2)
  library(stringr)

  library(qs)
  library(glue)
  library(metathis)
  
  library(RSQLite)
})

# Shiny options -----------------------------------------------------------

options(shiny.trace = FALSE) # Set TRUE for debugging
options(shiny.useragg = TRUE)
shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "cache")))


# Data --------------------------------------------------------------------

variables <- qread("data/variables.qs")
title_text <- qread("data/title_text.qs")
dyk <- qread("data/dyk.qs")
qload("data/colours.qsm")
tile_lookup <- qread("data/tile_lookup.qs")

qload("data/census.qsm")
# qload("data/covid.qsm")
# green_space <- qread("data/green_space.qs")
qload("data/alley.qsm")
# crash <- qread("data/crash.qs")
# marketed_sustainability <- qread("data/marketed_sustainability.qs")
metro_lines <- qread("data/metro_lines.qs")

qload("data/stories.qsm")

qload("data/place_explorer.qsm")
postal_codes <- qread("data/postal_codes.qs")


# Global variables --------------------------------------------------------

census_min <- 
  variables$dates[variables$source == "census"] |> 
  unlist() |> 
  unique() |> 
  min() |> 
  as.numeric()

census_max <- 
  variables$dates[variables$source == "census"] |> 
  unlist() |> 
  unique() |> 
  max() |> 
  as.numeric()

island_CSDUID <- 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072", "2466023")


# Modules ready -----------------------------------------------------------

mods_rdy <- list(
  "Climate" = c(
    "Climate risk" = "climate_risk"
    ),
  # "Covid" = c(
  #   "Covid interventions" = "covid"
  #   ),
  "Housing" = c(
    "Housing system" = "housing"
  #   "Gentrification" = "gentrification", 
  #   "Permits" = "permits", 
  #   "Marketed Sustainability" = "marketed_sustainability"
    ),
  "Policy" = c(
    "Montréal climate plans" = "mcp"
    ),
  "Transport" = c(
    "Accessibility" = "access"#,
  #   "Road safety" = "crash"
    ),
  "Urban life" = c(
    "Active living potential" = "canale", 
    "Green alleys" = "alley"#,
    # "Green spaces" = "green_space"
    ),
  "Ecology" = c(
    "Natural infrastructure" = "natural_inf"
    )
  )

stand_alone_tabs <- c(
  "Montréal stories" = "stories",
  "Place explorer" = "place_explorer",
  "How to use" = "how_to_use",
  "About" = "about_sus",
  "Authors" = "authors"
  )


# Translation -------------------------------------------------------------

translation_fr <- qread("data/translation_fr.qs")


# Map defaults ------------------------------------------------------------

map_token <- paste0("pk.eyJ1Ijoic3VzLW1jZ2lsbCIsImEiOiJjbDBxMTcyNWwyNTl0M2",
                    "RtZzRremNxOHA3In0.V2Ah5lxy-3RZlF2QKOvIjg")
options(rdeck.mapbox_access_token = map_token)
map_base_style <- "mapbox://styles/sus-mcgill/cl0reqoz4000z15pekuh48ld6"
map_style_building <- "mapbox://styles/sus-mcgill/cl2bwtrsp000516rwyrkt9ior"
map_zoom <- 10.1
map_zoom_levels <- c("borough" = 0, "CT" = 10.5, "DA" = 12.5, "building" = 15.5)
map_loc <- c(-73.58, 45.53)


# Set up fonts ------------------------------------------------------------

systemfonts::register_font(
  name = "SourceSansPro",
  plain = "www/fonts/SourceSansPro-Regular.ttf",
  italic = "www/fonts/SourceSansPro-Italic.ttf",
  bold = "www/fonts/SourceSansPro-Bold.ttf",
  bolditalic = "www/fonts/SourceSansPro-BoldItalic.ttf")


# Error function ----------------------------------------------------------

options(shiny.fullstacktrace = TRUE)


# Connect to the db -------------------------------------------------------

db <- dbConnect(SQLite(), "data/sql_db.sqlite")
