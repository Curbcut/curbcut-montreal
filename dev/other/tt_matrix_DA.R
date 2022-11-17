#### Travel time matrix data setup #############################################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(qs)


# Get the population weighted centroid of DA on street network ------------

library(r5r)
options(java.parameters = '-Xmx16G')
r5r_core <- setup_r5(data_path = "dev/data/routing", verbose = FALSE)

old_plan <- future::plan()
future::plan(future::multisession)

# Get the closest street point of the centroid
streets_for_amenities <-
  qread("dev/data/street.qs") |>
  filter(!street_type %in% c("motorway", "motorway_link")) |>
  select(ID = DAUID)

# Look at all the streets of a DA, get the street centroid closest to the
# population weighted centroid of the DA
DA_pop_centroid <-
  read.csv("dev/data/da_centroids_popw.csv") |>
  filter(DAuid.ADidu %in% DA$ID) |>
  distinct(DAuid.ADidu, .keep_all = T) |>
  as_tibble() |>
  st_as_sf(coords = c("DArplong.ADlong", "DArplat.Adlat"),
           crs = 4326) |>
  transmute(id = as.character(DAuid.ADidu))

progressr::with_progress({
  p <- progressr::progressor(steps = length(DA$ID))

  DA_street_centroid <-
    furrr::future_map_dfr(DA$ID, function(DAUID) {

      data <- streets_for_amenities[
        streets_for_amenities$ID == DAUID, ]

      out <-
        data |>
        st_centroid() |>
        st_distance(DA_pop_centroid[DA_pop_centroid$id == DAUID, ]) |>
        (\(x) which(x == min(x)))() |>
        (\(x) slice(data, x))() |>
        st_centroid()

      # For DAs that don't have streets in, take a neighbouring street
      if (nrow(out) == 0) {
        out <-
          DA_pop_centroid[DA_pop_centroid$id == DAUID, ] |>
          st_nearest_feature(streets_for_amenities) |>
          (\(x) slice(streets_for_amenities[x, ]))() |>
          st_centroid()
      }

      p()

      transmute(out, ID = DAUID)

    })
})

# Go back to old plan
future::plan(old_plan)

qsave(DA_street_centroid, file = "dev/data/pop_weighted_centroid_DA_street.qs")

DA_street_centroid <- qread("dev/data/pop_weighted_centroid_DA_street.qs")
# Calculate travel time matrix per mode -----------------------------------

tt_matrix <- list()

timings <- 
  c(c("pwd" = as.POSIXct("2022-06-15 08:15:00")), 
    c("opwd" = as.POSIXct("2022-06-15 12:00:00")), 
    c("nwd" = as.POSIXct("2022-06-15 23:00:00")), 
    c("pwe" = as.POSIXct("2022-06-18 12:00:00")), 
    c("opwe" = as.POSIXct("2022-06-18 9:00:00")), 
    c("nwe" = as.POSIXct("2022-06-18 23:00:00")))

destinations <- DA_street_centroid |> rename(id = ID)
origins <- rename(DA_street_centroid, id = ID)

# Walk tt_matrix
tt_matrix$WALK <- 
    travel_time_matrix(r5r_core = r5r_core,
                       origins = origins,
                       destinations = destinations,
                       mode = "WALK",
                       max_trip_duration = 120, 
                       verbose = FALSE
                       ) |> 
      suppressMessages()


# Bicycle tt_matrix
tt_matrix$BICYCLE <- 
  travel_time_matrix(r5r_core = r5r_core,
                     origins = origins,
                     destinations = destinations,
                     mode = "BICYCLE",
                     max_trip_duration = 120, 
                     verbose = FALSE
  ) |> 
  suppressMessages()

# Transit tt_matrix
tt_matrix$TRANSIT <- 
  map(timings, function(date_time) {
    travel_time_matrix(r5r_core = r5r_core,
                       origins = origins,
                       destinations = destinations,
                       mode = "TRANSIT",
                       max_trip_duration = 120,
                       time_window = 60,
                       departure_datetime = date_time, 
                       verbose = FALSE) |> 
      suppressMessages()
  })

# Car tt_matrix
tt_matrix$CAR <- 
  travel_time_matrix(r5r_core = r5r_core,
                     origins = origins,
                     destinations = destinations,
                     mode = "CAR",
                     max_trip_duration = 120, 
                     verbose = FALSE
  ) |> 
  suppressMessages()


# Save --------------------------------------------------------------------

qsave(tt_matrix, file = "dev/data/tt_matrix_DA.qs")


# Clean -------------------------------------------------------------------

rm(r5r_core, streets_for_amenities, DA_pop_centroid, DA_street_centroid, 
   destinations, groups, old_plan)
