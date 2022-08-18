#### Travel time matrix data setup #############################################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(qs)
old_plan <- future::plan()
future::plan(future::multisession)


# Get the population weighted centroid of DA on street network ------------

progressr::handlers(progressr::handler_progress(
  format =
    ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
  width = 60,
  complete = "+"
))

library(r5r)
options(java.parameters = '-Xmx16G')
r5r_core <- setup_r5(data_path = "dev/data/routing",  elevation = "TOBLER",
                     verbose = FALSE)

# Get the closest street point of the centroid
streets_for_amenities <-
  qread("dev/data/street.qs") |>
  filter(!street_type %in% c("motorway", "motorway_link")) |>
  select(ID = DAUID)

# Look at all the streets of a DA, get the street centroid closest to the
# population weighted centroid of the DA
DA_pop_centroid <-
  read.csv("dev/data/da_centroids_popw.csv") |>
  filter(CMAuid.RMRidu == "462") |>
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


# Calculate travel time matrix per mode -----------------------------------

tt_matrix <- list()

destinations <- DA_street_centroid |> rename(id = ID)

# Walk tt_matrix
tt_matrix$WALK <- 
  travel_time_matrix(r5r_core = r5r_core,
                     origins = rename(DA_street_centroid, id = ID),
                     destinations = destinations,
                     mode = "WALK",
                     max_trip_duration = 120) |> 
  suppressMessages()

# Bicycle tt_matrix
groups <-
  cut(seq_len(nrow(DA_street_centroid)), ceiling(nrow(DA_street_centroid)/50))

progressr::with_progress({
  p <- progressr::progressor(steps = length(unique(groups)))
  
  tt_matrix$BICYCLE <- 
    map_dfr(unique(groups), function(gr) {
      p()
      
      origins <- 
        DA_street_centroid[groups == gr, ] |> 
        rename(id = ID)
      
      travel_time_matrix(r5r_core = r5r_core,
                         origins = origins,
                         destinations = destinations,
                         # all_to_all = TRUE,
                         mode = "BICYCLE",
                         max_trip_duration = 120) |> 
        suppressMessages()
    })
})

# Transit tt_matrix
groups <-
  cut(seq_len(nrow(DA_street_centroid)), ceiling(nrow(DA_street_centroid)/500))

progressr::with_progress({
  p <- progressr::progressor(steps = length(unique(groups)))
  
  tt_matrix$TRANSIT <- 
    map_dfr(unique(groups), function(gr) {
      p()
      
      origins <- 
        DA_street_centroid[groups == gr, ] |> 
        rename(id = ID)
      
      travel_time_matrix(r5r_core = r5r_core,
                         origins = origins,
                         destinations =
                           destinations,
                         mode = "TRANSIT",
                         departure_datetime = 
                           as.POSIXct("15-06-2022 08:00:00",
                                      format = "%d-%m-%Y %H:%M:%S"),
                         max_trip_duration = 120,
                         time_window = 60) |> 
        suppressMessages()
    })
})

# Car tt_matrix
groups <-
  cut(seq_len(nrow(DA_street_centroid)), ceiling(nrow(DA_street_centroid)/25))

progressr::with_progress({
  p <- progressr::progressor(steps = length(unique(groups)))
  
  tt_matrix$CAR <- 
    map_dfr(unique(groups), function(gr) {
      p()
      
      origins <- 
        DA_street_centroid[groups == gr, ] |> 
        rename(id = ID)
      
      travel_time_matrix(r5r_core = r5r_core,
                         origins = origins,
                         destinations = destinations,
                         mode = "CAR",
                         max_trip_duration = 120) |> 
        suppressMessages()
    })
})


# Save --------------------------------------------------------------------

qsave(tt_matrix, file = "dev/data/tt_matrix_DA.qs")


# Clean -------------------------------------------------------------------

rm(r5r_core, streets_for_amenities, DA_pop_centroid, DA_street_centroid, 
   destinations, groups, old_plan, p)
