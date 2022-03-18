#### STREET TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
library(future)
library(furrr)
source("dev/tiles/tile_functions.R")
# Load raw street network download
street <- qread("dev/data/street_network.qs")

# Filter the street network -----------------------------------------------

street <-
  street |>
  filter(highway %in% c("motorway", "trunk", "primary",
                        "secondary", "tertiary",
                        "motorway_link", "trunk_link",
                        "primary_link", "residential", "service"))


# Filter columns ----------------------------------------------------------

streets <- qread("dev/tiles/texture/streets.qs")

# Upload street tile source -----------------------------------------------

# Street testing bbox
# filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
#   bb <- sf::st_bbox(.data)
#   if (!is.null(xmin)) bb["xmin"] <- xmin
#   if (!is.null(xmax)) bb["xmax"] <- xmax
#   if (!is.null(ymin)) bb["ymin"] <- ymin
#   if (!is.null(ymax)) bb["ymax"] <- ymax
#   sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
# }
# streets <- 
#   filter_sf(streets, -73.63, -73.56, 45.45, 45.51)

streets |> 
  mutate(group = case_when(highway %in% c("motorway", "trunk", "primary",
                                      "secondary", "tertiary") ~ "low",
                           highway %in% c("motorway_link", "trunk_link",
                                       "primary_link", "residential") ~ "mid",
                           TRUE ~ "high")) |> 
  select(group, street_type = highway) |> 
  upload_tile_source("streets", "maxbdb2", .sus_token)


# Clean-up ----------------------------------------------------------------

rm(streets)

