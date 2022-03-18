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

# streets <- 
#   street |> 
#   select(name = name_2, street_type, CSDUID) |> 
#   filter(!is.na(CSDUID)) |> 
#   filter(st_is(geometry, "LINESTRING") | st_is(geometry, "MULTILINESTRING"))
# 
# CSDUID <- 
#   streets |> 
#   pull("CSDUID") |> 
#   unique()
# 
# streets <- 
#   map_dfr(CSDUID, ~{
#     streets[streets$CSDUID == .x, ] |> 
#       group_by(name, street_type) |>
#       summarize(.groups = "drop")
#   })
# 
# qsave(streets, file = "dev/tiles/texture/streets.qs")

streets <- qread("dev/tiles/texture/streets.qs")

# Upload street tile source -----------------------------------------------

# Street testing bbox
filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  bb <- sf::st_bbox(.data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
}
streets <- 
  filter_sf(streets, -73.63, -73.56, 45.45, 45.51)

streets |> 
  mutate(group = case_when(highway %in% c("motorway", "trunk", "primary",
                                      "secondary", "tertiary") ~ "low",
                           highway %in% c("motorway_link", "trunk_link",
                                       "primary_link", "residential") ~ "mid",
                           TRUE ~ "high")) |> 
  select(group, street_type = highway) |> 
  upload_tile_source("streets", "maxbdb2", .sus_token)

# streets |> 
  # filter(street_type %in% c("motorway", "trunk", "primary",
  #                           "secondary", "tertiary")) |> 
  # upload_tile_source("streets", "maxbdb2", .sus_token)

# streets |> 
#   filter(street_type %in% c("motorway_link", "trunk_link",
#                             "primary_link", "residential")) |> 
#   upload_tile_source("streets-mid", "maxbdb2", .sus_token)
# 
# streets |> 
#   filter(!street_type %in% c("motorway", "trunk", "primary",
#                              "secondary", "tertiary",
#                              "motorway_link", "trunk_link",
#                              "primary_link", "residential")) |> 
#   upload_tile_source("streets-high", "maxbdb2", .sus_token)


# Clean-up ----------------------------------------------------------------

rm(streets)

