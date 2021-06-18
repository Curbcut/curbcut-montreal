#### Pedestrian module data setup ##############################################

library(tidyverse)
library(sf)
library(qs)
library(future)
library(foreach)
library(doFuture)
library(progressr)

plan(multisession)
registerDoFuture()
handlers(global = TRUE)


# Sidewalks ---------------------------------------------------------------

# http://donnees.ville.montreal.qc.ca/dataset/voirie-trottoir-ilot
sidewalks <- 
  read_sf("dev/data/voi_trottoir_s_t12_shp/VOI_TROTTOIR_S_T12.shp") %>% 
  st_transform(32618)

pedestrian_streets <- 
  read_sf("dev/data/projetpietonnisation2017/projetpietonnisation2017.shp") %>%
  st_transform(32618)

# Helper function
measure_sidewalk_width <- function(df, total_tries = 15) {

  pb <- progressor(nrow(df))

  out <- foreach(i = seq_len(nrow(df)), .combine = "bind_rows") %dopar% {
    
    pb()
    min_width <- 10
    is_it_empty <- TRUE
    tries <- 0
    
    while (is_it_empty && tries < total_tries) {
      tries <- tries + 1
      max_width <- min_width
      min_width <- min_width * 0.5
      is_it_empty <- sf::st_is_empty(sf::st_buffer(df[i,], min_width * -1))
    }
    
    while (tries < total_tries) {
      tries <- tries + 1
      is_it_empty <-
        sf::st_is_empty(sf::st_buffer(df[i,], -1 * (max_width + min_width) / 2))
      if (is_it_empty) {
        max_width <- (max_width + min_width) / 2
      } else min_width <- (max_width + min_width) / 2
    }
    
    table <- df[i,]
    table$width <- max_width + min_width
    table
    
  }

  out
}

# Calculate sidewalk widths
sidewalks <- measure_sidewalk_width(sidewalks)
rm(measure_sidewalk_width)

# Create sidewalk width brackets
sidewalks <- 
  sidewalks %>% 
  mutate(rounded_width = as.factor(case_when(width <= 1 ~ "0-1 m",
                                             width <= 2 ~ "1-2 m",
                                             width <= 4 ~ "2-4 m",
                                             width <= 6 ~ "4-6 m",
                                             width <= 10 ~ "6-10 m",
                                             width > 10 ~ "10-20 m")))

# Select and rename fields, and convert to WGS 84
sidewalks <- 
  sidewalks %>% 
  select(ID = ID_VOI_TRO, width, rounded_width, geometry) %>% 
  st_transform(4326)

qsave(sidewalks, "sidewalks_temp.qs")







####### Get census variables #########
census <- cancensus::get_census("CA16",
                     regions = list(CMA = c("24462")),
                     level = "DA",
                     vectors = c("v_CA16_406", "v_CA16_5795", "v_CA16_5798", "v_CA16_5792",
                                 "v_CA16_5801", "v_CA16_5804", "v_CA16_5807", "v_CA16_2207",
                                 "v_CA16_2213", "v_CA16_3957",   "v_CA16_3966",
                                 "v_CA16_3411", "v_CA16_3846", "v_CA16_3996"),
                     geo_format = "sf")

#modify / add data
census <- census %>%
  select(3:5, 7:9, 11:27) %>%
  set_names("households", "dwellings", "GeoUID", "population", "CT_UID", "CD_UID", "name", "area(sqkm)",
            "pop_density(sqkm)", "driver", "passenger", "total_commute", "transit", "pedestrian",
            "cyclcist", "gross_median_income", "net_median_income", "visible_minority_pop",
            "black_minority_pop", "non_minority_pop", "immigrants", "refugees", "geometry") %>%
  st_transform(32620) %>%
  mutate(prop_walk = (pedestrian/total_commute)*100,
         prop_non_driving = ((pedestrian + cyclcist + transit)/total_commute)*100,
         prop_driving = ((driver + passenger) / total_commute)*100,
         minority_percent = visible_minority_pop / population,
         black_percent = black_minority_pop / population,
         immigrant_percent = immigrants / population,
         refugees_percent = refugees / population,
         non_minority_percent = non_minority_pop / population) %>%
  filter(CD_UID == 2466)

census <- 
  census %>% 
  as_tibble() %>% 
  st_as_sf()

census %>% 
  select(ID = GeoUID, immigrant_percent) %>% 
  slice(1:10) %>%
  left_join(st_drop_geometry(DA), by = "ID") %>% 
  select(ID, immigrant_percent, immigrant_prop)

