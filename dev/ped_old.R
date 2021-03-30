#### Pedestrian module data setup ##############################################

library(tidyverse)
library(sf)
library(qs)

# Load data for pedestrian realm
load(file = "dev/data/sidewalks_WSG.Rdata")
load(file = "dev/data/census_analysis_quantile.Rdata")
load(file = "dev/data/census_analysis_ct.Rdata")
load(file = "dev/data/original_VAS_plan.Rdata")
load(file = "dev/data/revised_VAS_plan.Rdata")


# Trim sidewalks
sidewalks_WSG <- 
  sidewalks_WSG %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  select(sidewalk_width)

census_analysis_quantile %>% 
  as_tibble() %>% 
  st_as_sf()

## Create VAS plans  -------------------------------------------------

# # May plan
st_crs(original_plan_disaggregated) = 32620
may_vas_plan <- original_plan_disaggregated %>%
  st_transform(4326)

# # July plan
st_crs(revised_plan) = 32620
july_vas_plan <- revised_plan %>%
  st_transform(4326)




# # Pedestrian_module_data
# 
# ################ Libraries #####################
# library(tidyverse)
# library(sf)
# library(sp)
# library(ggplot2)
# library(mapview)
# library(cancensus)
# library(cansim)
# library(osmdata)
# library(mapboxapi)
# library(classInt)
# library(scales)
# library(cowplot)
# library(ggsn)
# library(ggspatial)
# 
# ################ SIDEWALKS #####################
# 
# ## Load files
# # Data for Sidewalks taken from the Montreal Open Data Portal: 
# # http://donnees.ville.montreal.qc.ca/dataset/voirie-trottoir-ilot  
# sidewalks <- st_read("data", "VOI_TROTTOIR_S_T12") %>% 
#   st_transform(32620)
# 
# pedestrian_streets <- st_read("data", "projetpietonnisation2017") %>% 
#   st_transform(32620)
# 
# ## Function to Determine Sidewalk Width 
# 
# measure_sidewalk_width <- function(df, total_tries = 15) {
#   
#   sidewalk_width <- vector("numeric", length = nrow(df))
#   
#   for (i in seq_len(nrow(df))) {
#     
#     min_width <- 10
#     is_it_empty <- TRUE
#     tries <- 0
#     
#     while (is_it_empty && tries < total_tries) {
#       
#       tries <- tries + 1
#       max_width <- min_width
#       min_width <- min_width * 0.5
#       is_it_empty <- sf::st_is_empty(sf::st_buffer(df[i,], min_width * -1))
#       
#     }
#     
#     while (tries < total_tries) {
#       
#       tries <- tries + 1
#       
#       is_it_empty <- 
#         sf::st_is_empty(sf::st_buffer(df[i,], -1 * (max_width + min_width) / 2))
#       
#       if (is_it_empty) {
#         max_width <- (max_width + min_width) / 2
#       } else min_width <- (max_width + min_width) / 2
#       
#     }
#     
#     sidewalk_width[[i]] <- max_width + min_width
#     
#   }
#   
#   return(sidewalk_width)
# }
# 
# ## Join sidewalk_width to sidewalk shape file
# 
# sidewalk_width <- measure_sidewalk_width(sidewalks)
# 
# sidewalks <- sidewalks %>% 
#   mutate(sidewalk_width = sidewalk_width)
# #select only the sidewalk width column 
# 
# # Create new column
# sidewalks <- sidewalks %>% 
#   mutate(rounded_sidewalk_width = sidewalk_width)
# 
# # Create sidewalk width brackets
# sidewalks$rounded_sidewalk_width <- case_when(
#   sidewalks$rounded_sidewalk_width <= 1 ~ "0-1 m",
#   sidewalks$rounded_sidewalk_width > 1 & sidewalks$rounded_sidewalk_width <= 2 ~ "1-2 m",
#   sidewalks$rounded_sidewalk_width > 2 & sidewalks$rounded_sidewalk_width <= 4 ~ "2-4 m",
#   sidewalks$rounded_sidewalk_width > 4 & sidewalks$rounded_sidewalk_width <= 6 ~ "4-6 m",
#   sidewalks$rounded_sidewalk_width > 6 & sidewalks$rounded_sidewalk_width <= 10 ~ "6-10 m",
#   sidewalks$rounded_sidewalk_width > 10 ~ "10-20 m"
# )
# # as factor
# sidewalks <-  sidewalks %>% 
#   mutate(rounded_sidewalk_width = as.factor(rounded_sidewalk_width))
# 
# 
# ###################################### Main File with Census Data #######################################################
# 
# ####### Get census variables ######### 
# census <- get_census("CA16",
#                      regions = list(CMA = c("24462")),
#                      level = "DA",
#                      vectors = c("v_CA16_406", "v_CA16_5795", "v_CA16_5798", "v_CA16_5792",
#                                  "v_CA16_5801", "v_CA16_5804", "v_CA16_5807", "v_CA16_2207", 
#                                  "v_CA16_2213", "v_CA16_3957",   "v_CA16_3966",
#                                  "v_CA16_3411", "v_CA16_3846", "v_CA16_3996"),
#                      geo_format = "sf")
# 
# #modify / add data
# census <- census %>% 
#   select(3:5, 7:9, 11:27) %>% 
#   set_names("households", "dwellings", "GeoUID", "population", "CT_UID", "CD_UID", "name", "area(sqkm)",
#             "pop_density(sqkm)", "driver", "passenger", "total_commute", "transit", "pedestrian",
#             "cyclcist", "gross_median_income", "net_median_income", "visible_minority_pop", 
#             "black_minority_pop", "non_minority_pop", "immigrants", "refugees", "geometry") %>% 
#   st_transform(32620) %>% 
#   mutate(prop_walk = (pedestrian/total_commute)*100,
#          prop_non_driving = ((pedestrian + cyclcist + transit)/total_commute)*100,
#          prop_driving = ((driver + passenger) / total_commute)*100,
#          minority_percent = visible_minority_pop / population,
#          black_percent = black_minority_pop / population,
#          immigrant_percent = immigrants / population,
#          refugees_percent = refugees / population,
#          non_minority_percent = non_minority_pop / population) %>% 
#   filter(CD_UID == 2466)
# 
# 
# 
# ####### Create Intersection between sidewalk and census files ######### 
# 
# # Intersection
# census_sidewalks <- st_intersection(census, sidewalks)
# # add sidewalk surface area
# census_sidewalks <- census_sidewalks %>% 
#   mutate(da_sidewalk_sqm_area = st_area(census_sidewalks))
# # Remove unit
# attributes(census_sidewalks$da_sidewalk_sqm_area) = NULL
# # Group by DA
# census_sidewalks <- census_sidewalks %>%
#   group_by(GeoUID) %>% 
#   summarise(
#     #calculate total sidewalk surface area by DA in sqm
#     da_sidewalk_sqm_area = round(sum(da_sidewalk_sqm_area), 4),
#     #calculate average sidewalk width by da
#     average_sidewalk_width = round(mean(c(sidewalk_width)), 2)) %>% 
#   st_drop_geometry() %>% 
#   as.data.frame() %>% 
#   as.tibble()
# # Merge new sidewalk variables with census file
# census <- right_join(census, census_sidewalks, by = "GeoUID")
# 
# 
# ####### Include surface area for osm pedestrian features #########
# 
# ## Get montreal bbox
# mtl_bb <- getbb("montreal canada")
# 
# ## Get osm landuse features and convert to sf table
# osm_landuse <- opq(bbox = mtl_bb, timeout = 900) %>% 
#   add_osm_feature(key = "landuse") %>% 
#   osmdata_sf()
# #Extract Polygons & Multipolygons
# osm_landuse_features <-
#   rbind(osm_landuse$osm_polygons %>% select(landuse), 
#         osm_landuse$osm_multipolygons %>% select(landuse)) %>% 
#   st_set_agr("constant") %>% 
#   filter(!is.na(landuse)) %>% 
#   set_names("feature", "geometry") %>% 
#   #Make geometries valid
#   st_make_valid() %>% 
#   st_cast("POLYGON")  %>% 
#   st_as_sf() %>% 
#   st_transform(32620)
# 
# ## Get osm leisure features and convert to sf table
# osm_leisure <- opq(bbox = mtl_bb, timeout = 900) %>% 
#   add_osm_feature(key = "leisure") %>% 
#   osmdata_sf()
# #Extract Polygons & Multipolygons
# osm_leisure_features <-
#   rbind(osm_leisure$osm_polygons %>% select(leisure), 
#         osm_leisure$osm_multipolygons %>% select(leisure)) %>% 
#   st_set_agr("constant") %>% 
#   filter(!is.na(leisure)) %>% 
#   set_names("feature", "geometry") %>% 
#   #Make geometries valid
#   st_make_valid() %>%
#   st_cast("POLYGON") %>% 
#   st_as_sf() %>% 
#   st_transform(32620)
# 
# ## Combine landuse and leisure into one file and filter
# osm_features <- rbind(osm_landuse_features, osm_leisure_features) %>% 
#   filter(feature == "cemetery" | feature == "recreation_ground" | 
#            feature == "village_green"| feature == "garden" |feature == "park" |
#            feature == "playground" | feature == "dog_park") %>% 
#   st_difference()
# 
# osm_features <- st_intersection(census, osm_features) %>% 
#   # Only select GeoUID column
#   select(3)
# # calculate leisure space surface area
# osm_features <- osm_features %>% 
#   mutate(leisure_surfarea_sqm = round(st_area(osm_features), 2)) %>% 
#   group_by(GeoUID) %>% 
#   summarise(leisure_surfarea_sqm = sum(leisure_surfarea_sqm)) %>% 
#   # Drop geometry
#   st_drop_geometry() %>% 
#   as.data.frame() %>% 
#   as.tibble()
# # Remove unit
# attributes(osm_features$leisure_surfarea_sqm) = NULL
# 
# ## Left Join with census_analysis
# census <- left_join(census, osm_features, by = "GeoUID")
# #Transform NAs to 0
# census$leisure_surfarea_sqm <- census$leisure_surfarea_sqm %>% 
#   replace(is.na(census$leisure_surfarea_sqm), 0)
# 
# 
# 
# ####### Add pre-covid pedestrian street surf area #########
# 
# # Source taken from https://www.donneesquebec.ca/recherche/dataset/vmtl-rues-pietonnes 
# pedestrian_streets <- st_read("data", "projetpietonnisation2017") %>% 
#   st_transform(32620)
# 
# # Add street widths collected from Google Maps and get surface area in sqm
# pedestrian_streets <- pedestrian_streets %>% 
#   mutate(width_meters = c(5.86, 6.52, 2.56, 11.2, 7.59, 9, 16.77, 6.5, 10.74, 
#                           10.06, 10.33, 6.65, 3.8, 6.11, 5.72, 8.93, 4.77, 
#                           16.91, 2.89, 11, 10.46, 8.69, 3.11, 8.97, 6.41, 10, 
#                           9.84, 4.25, 11.87, 11.78, 5.50, 7, 9.39, 8.48, 15.3, 
#                           3.67, 5, 5.8, 6.88, 24, 8.65, 3.25, 8.8, 4.5, 2.5, 26, 
#                           10.75, 9.73, 10.07, 26.5, 11, 9.53, 11),
#          pedestrian_streets_area_sqm = LONGUEUR_T * width_meters)
# 
# # Create intersection between census and pedestrian_streets
# pedestrian_streets <- st_intersection(census, pedestrian_streets) %>% 
#   select(3, 63) %>% 
#   st_drop_geometry() %>% 
#   as.data.frame() %>% 
#   as.tibble() %>% 
#   # Group by GeoUID and sum pedestrian street total area
#   group_by(GeoUID) %>% 
#   summarise(pedestrian_streets_area_sqm = sum(pedestrian_streets_area_sqm))
# 
# # Left Join with census_analysis
# census <- left_join(census, pedestrian_streets, by = "GeoUID")
# #Transform NAs to 0
# census$pedestrian_streets_area_sqm <- census$pedestrian_streets_area_sqm %>% 
#   replace(is.na(census$pedestrian_streets_area_sqm), 0)
# 
# 
# ####### ADD NEW VARIABLES ########## 
# 
# census <- census %>%
#   mutate(
#     # combine sidewalk / leisure surface area / pedestrian street area
#     walkable_surface_area = da_sidewalk_sqm_area + leisure_surfarea_sqm + pedestrian_streets_area_sqm,
#     # pop density per walkable surface area (DA)
#     pop_density_per_walkable_area = round(population / walkable_surface_area, 2),
#     # sqm of walkable space per person (DA)
#     sqm_walkable_space_per_person = round(walkable_surface_area / population, 2), 
#     # 1 meter radius social distancing capacity (absolute terms)
#     soc_distance_pop_capacity_1m = round(walkable_surface_area / 3.14, 2),
#     # Deficit or surplus of capacity in absolute numbers for 1 meter social distancing
#     soc_distance_pop_balance_1m = round(soc_distance_pop_capacity_1m - population, 2),
#     # % of residential population that can walk at the same time while social distancing with 1 meter radius social distancing
#     social_distancing_capacity_pop_perc_1m = round((soc_distance_pop_capacity_1m / population) *100, 2),
#     # Total number of locals that can walk at the same time while respecting 2 meter radius social distancing
#     soc_distance_pop_capacity_2m = round(walkable_surface_area / 12.56, 2),
#     # Deficit or surplus of capacity in absolute numbers for 2 meter social distancing
#     soc_distance_pop_balance_2m = round(soc_distance_pop_capacity_2m - population, 2),
#     # % of residential population that can walk at the same time while social distancing with 2 meter radius social distancing
#     social_distancing_capacity_pop_perc_2m = round((soc_distance_pop_capacity_2m / population) *100, 2))
# 
# ####### ADD WALKABLE PROXIMITY COMPOSITE INDEX SCORE ########## 
# 
# # Data taken from: https://www150.statcan.gc.ca/n1/pub/17-26-0002/172600022020001-eng.htm#a1 
# 
# #can use cansim to load via API
# amenities <- read_csv("data/amenities_1.csv")
# 
# ## Select variables of interest
# amenities <- amenities %>% 
#   select(3, 22, 24, 28, 36) %>% 
#   rename("GeoUID" = "DAUID") %>% 
#   # Transform variables to numeric values
#   transform(prox_idx_pharma = as.numeric(prox_idx_pharma)) %>% 
#   transform(prox_idx_childcare = as.numeric(prox_idx_childcare)) %>% 
#   transform(prox_idx_grocery = as.numeric(prox_idx_grocery)) %>% 
#   transform(prox_idx_parks = as.numeric(prox_idx_parks))
# # replace NAs to 0
# amenities <- amenities %>%
#   replace(is.na(amenities), 0) %>% 
#   #group by DA
#   group_by(GeoUID) %>% 
#   summarise(indx_pharma = round(mean(c(prox_idx_pharma)), 2),
#             indx_grocery = round(mean(c(prox_idx_grocery)), 2),
#             indx_parks = round(mean(c(prox_idx_parks)), 2),
#             indx_childcare = round(mean(c(prox_idx_childcare)),2)) %>% 
#   rowwise() %>% 
#   # create composite index score indicating walkable proximity to parks/groceries/pharmacies/childcare
#   mutate(agg_proximity_score = round(mean(c(indx_pharma, indx_grocery,
#                                             indx_parks, indx_childcare), na.rm = T), 2)) %>% 
#   select(1, 6)
# 
# #Merge amenities and census_analysis
# 
# census <- merge(census, amenities, by = "GeoUID")
# 
# 
# ####### ADD PED TRIP FLOWS FROM TRAJETMTL DATA ##########
# # Data file taken from Qiao
# 
# # load data
# mtl_trajet_lines <- st_read("data", "w_path") %>% 
#   st_transform(32620)
# 
# # modify data
# mtl_trajet_lines <- mtl_trajet_lines %>% 
#   # select trips that were made for leisure / health
#   filter(purpose == 0 | purpose == 2 | purpose == 3 | purpose == 4 | purpose == 5 | purpose == 6) %>% 
#   rename("GeoUID" = "DAUID") %>% 
#   st_drop_geometry() %>% 
#   as.data.frame() %>% 
#   # group by DA
#   group_by(GeoUID) %>% 
#   # get the amount of trips by DA
#   summarise(trip_da_count = n())
# 
# #Merge with census
# census <- merge(census, mtl_trajet_lines, by="GeoUID", all.x=T)
# 
# # add variables using mutate
# census <- census %>% 
#   # Normalize by number of trips per sqm of walkable space
#   mutate(trips_per_walkable_area = round(trip_da_count / walkable_surface_area, 8),
#          # Create standardized value
#          trip_scale = scale(trips_per_walkable_area))
# 
# 
# ####### CREATE QUANTILES / COLOR SCHEMES / NEW VARIABLES ##########
# 
# # extract quantile column names
# quant_list <- c("social_distancing_capacity_pop_perc_2m", "agg_proximity_score", 
#                 "net_median_income", "minority_percent", "immigrant_percent")
# 
# # Create quantiles
# census_analysis_quantile <-
#   census %>%
#   mutate_at(quant_list,
#             funs(quant3 = ntile(.,3))
#   )
# 
# # Biv color scale for SUS app
# bivariate_color_scale <- tibble(
#   "3 - 3" = "#2A5A5B",
#   "2 - 3" = "#567994",
#   "1 - 3" = "#6C83B5",
#   "3 - 2" = "#5A9178",
#   "2 - 2" = "#90B2B3",
#   "1 - 2" = "#B5C0DA",
#   "3 - 1" = "#73AE80",
#   "2 - 1" = "#B8D6BE",
#   "1 - 1" = "#E8E8E8") %>%
#   gather("group", "fill")
# 
# # Modify census_analysis_quantile
# census_analysis_quantile <-  census_analysis_quantile %>% 
#   # create the binary quantile groups
#   mutate(agg_proximity_score_quant3_group = paste(as.numeric(social_distancing_capacity_pop_perc_2m_quant3), "-", 
#                                                   as.numeric(agg_proximity_score_quant3)),
#          net_median_income_quant3_group = paste(as.numeric(social_distancing_capacity_pop_perc_2m_quant3), "-",
#                                                 as.numeric(net_median_income_quant3)),
#          minority_percent_quant3_group = paste(as.numeric(social_distancing_capacity_pop_perc_2m_quant3), "-",
#                                                as.numeric(minority_percent_quant3)),
#          immigrant_percent_quant3_group = paste(as.numeric(social_distancing_capacity_pop_perc_2m_quant3), "-",
#                                                 as.numeric(immigrant_percent_quant3))
#   ) %>% 
#   # bring in HEX codes
#   left_join(bivariate_color_scale, by = c("agg_proximity_score_quant3_group" = "group")) %>%
#   left_join(bivariate_color_scale, by = c("net_median_income_quant3_group" = "group")) %>% 
#   left_join(bivariate_color_scale, by = c("minority_percent_quant3_group" = "group")) %>% 
#   left_join(bivariate_color_scale, by = c("immigrant_percent_quant3_group" = "group")) %>% 
#   rename("agg_proximity_score_quant3_fill" = "fill.x", "net_median_income_quant3_fill" = "fill.y",
#          "minority_percent_quant3_fill" = "fill.x.x", "immigrant_percent_quant3_fill" = "fill.y.y") %>% 
#   # add opacity to the fill
#   mutate(agg_proximity_score_quant3_fill_opacity = paste0(agg_proximity_score_quant3_fill, "99"),
#          net_median_income_quant3_fill_opacity = paste0(net_median_income_quant3_fill, "99"),
#          minority_percent_quant3_fill_opacity = paste0(minority_percent_quant3_fill, "99"),
#          immigrant_percent_quant3_fill_opacity = paste0(immigrant_percent_quant3_fill, "99")
#   )



