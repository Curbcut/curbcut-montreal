#### City amenities data setup ##############################################

# Get data ----------------------------------------------------------------

bike <- 
  read_sf("dev/data/city_accessibility/Indicateurs_accessibilite_velo.shp",
          crs = 4326) |> 
  st_drop_geometry() |> 
  relocate(ID = GeoUID, .before = Bibliotequ) |> 
  select(-Population) |> 
  set_names("ID", "library", "cultural", "Communita", "daycare", "preschool", 
           "primary", "secondary", "postsecondary", "Sport_inte", "Equi_Sante", "commercial_zone", 
           "alimentation", "pharmacy", "laundromat", "P_quartier", "big_park", "playground", 
           "cemeteries", "nature", "aquatic", "winter", "summer") |> 
  rename_with(~paste0("city_amenities_", str_to_lower(.x), "_bike_avg"), 
              where(is.numeric))

walk <- 
  read_sf("dev/data/city_accessibility/Indicateurs_accessibilite_marche.shp",
          crs = 4326) |> 
  st_drop_geometry() |> 
  relocate(ID = GeoUID, .before = Bibliotequ) |> 
  select(-Population) |> 
  set_names("ID", "library", "cultural", "Communita", "daycare", "preschool", 
            "primary", "secondary", "postsecondary", "Sport_inte", "Equi_Sante", "commercial_zone", 
            "alimentation", "pharmacy", "laundromat", "P_quartier", "big_park", "playground", 
            "cemeteries", "nature", "aquatic", "winter", "summer") |> 
  rename_with(~paste0("city_amenities_", str_to_lower(.x), "_walk_avg"), 
              where(is.numeric))

city_amenities <- left_join(bike, walk, by = "ID")
city_amenities <- city_amenities |> select(!contains(c("library", "communita")))

rm(bike, walk)

# Data testing ------------------------------------------------------------

data_testing(data = list("city_amenities" = city_amenities))


# Interpolate -------------------------------------------------------------

all_city_amenities <- 
  interpolate_scales(data = city_amenities, 
                     base_scale = "DB", 
                     all_tables = all_tables["city"],
                     crs = 32618,
                     weight_by = "population")


# Calculate breaks --------------------------------------------------------

all_city_amenities <- calculate_breaks(all_city_amenities)


# Assign to existing geographies ------------------------------------------

assign_tables(module_tables = all_city_amenities)


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add to variables table --------------------------------------------------

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(all_city_amenities$tables_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  imap_dfr(all_city_amenities$tables_q5, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:5,
                                 .before = 1)})

var_list <- 
  all_city_amenities$tables_list[[1]] |> 
  names() |> 
  str_subset("ID|_q3|_q5", negate = TRUE)

interpolation_keys <- 
  map_chr(set_names(names(all_city_amenities$tables_list)), ~{
    if (str_detect(.x, "_DB$")) FALSE else "dissemination block"
  })

new_rows <-
  map_dfr(var_list, function(var) {
    
    
    # TITLE
    category <- case_when(str_detect(var, "_library_") ~
                            "libraries",
                          str_detect(var, "_cultural_") ~
                            "cultural facilities",
                          str_detect(var, "_communita_") ~
                            "community facilities",
                          str_detect(var, "_daycare_") ~
                            "daycares",
                          str_detect(var, "_preschool_") ~ 
                            "preschools",
                          str_detect(var, "_primary_") ~ 
                            "primary scools",
                          str_detect(var, "_secondary_") ~ 
                            "secondary schools",
                          str_detect(var, "_postsecondary_") ~ 
                            "postsecondary schools",
                          str_detect(var, "_sport_inte_") ~ 
                            "indoor sports activities",
                          str_detect(var, "_equi_sante_") ~ 
                            "health and social services facilities",
                          str_detect(var, "_commercial_zone_") ~ 
                            "commercial zones",
                          str_detect(var, "_alimentation_") ~ 
                            "food stores",
                          str_detect(var, "_pharmacy_") ~ 
                            "pharmacies",
                          str_detect(var, "_laundromat_") ~ 
                            "laundromats",
                          str_detect(var, "_p_quartier_") ~ 
                            "neighbourhood parks",
                          str_detect(var, "_big_park_") ~ 
                            "big parks",
                          str_detect(var, "_playground_") ~ 
                            "playgrounds",
                          str_detect(var, "_cemeteries_") ~ 
                            "cemeteries",
                          str_detect(var, "_nature_") ~ 
                            "nature facilities",
                          str_detect(var, "_aquatic_") ~ 
                            "outdoor water activities",
                          str_detect(var, "_winter_") ~ 
                            "outdoor winter activities",
                          str_detect(var, "_summer_") ~ 
                            "outdoor summer activities")
    
    mode_title <- case_when(str_detect(var, "_walk_") ~
                            "(Walk)",
                          str_detect(var, "_bike_") ~
                            "(Cycling)")
    
    mode_exp <- case_when(str_detect(var, "_walk_") ~
                        "within a 15-minute walk",
                      str_detect(var, "_bike_") ~
                        "within a 20-minute cycling time")
    
    title <- paste0("Accessibility to ", category, " ", mode_title)
    short <- str_to_sentence(category)
    exp <- paste0("the average number of ", category, 
                    " a resident of the area can reach ", mode_exp)
    
    mode_group <- 
      case_when(
        str_detect(var, "_walk_") ~ "15-minute walking time",
        str_detect(var, "_bike_") ~ "20-minute cycling time")
    
    group_diff <- list("Mode of transport" = mode_group)
    
    # ADDED ROW
    out <-
      add_variables(variables,
                    var_code = var,
                    var_title = title,
                    var_short = short,
                    explanation = exp,
                    category = NA,
                    theme = "City amenities",
                    private = TRUE,
                    dates = NA,
                    scales = names(all_city_amenities$tables_list),
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, 
                                       var = all_of(var)),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, 
                                       var = all_of(var)),
                    source = "Open data from the City of Montreal, DMTI data, and OpenStreetMap data",
                    interpolated = list(interpolation_keys),
                    grouping = paste0("Accessibility to ", category),
                    group_diff = group_diff)
    
    out[out$var_code == var, ]
    
  })

variables <-
  bind_rows(variables, new_rows)


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(
    id = "city_amenities",
    metadata = TRUE,
    dataset_info = 
      paste0("The indicators of this module represent the number of ",
             "destinations accessible by walking and cycling from a ",
             "specified origin within a given time (cumulative-opportunities ",
             "method).",
             "<p>Each indicator is calculed at the dissemination blocks level, the smallest geographic area defined by Statistics Canada.",
             "<p>The defined cutoff times (15-minute walk ; 20-minute bike ride) do not vary throughout the study area, i.e., the City of Montreal. ",
             "<p>The characteristics and quality of the destinations are not taken into account.",
             "<p>Travel times are calculated using the r5r ('Rapid Realistic Routing with R5') package in R.",
             "<p>Calculations take into consideration walking and cycling infrastructures as well as streets' slopes.",
             "<p>The costs of turning and crossing an intersection are not considered.",
             "<p>The data used to calculate the indicators includes open data from the City of Montreal, DMTI data, and OpenStreetMap data."
             ))


# Additional disclaimer they want -----------------------------------------

city_amenities_disclaimer <- 
read_csv("dev/data/city_accessibility/disclaimer.csv") |> 
  mutate(var_code_walk = paste0("city_amenities_", tolower(var_code),
                                "_walk_avg"),
         var_code_bike = paste0("city_amenities_", tolower(var_code),
                                "_bike_avg")) |> 
  rowwise() |> 
  transmute(var_code = list(list(var_code_walk, var_code_bike)),
            disclaimer) |> 
  filter(!is.na(disclaimer)) |> 
  ungroup()

qs::qsave(city_amenities_disclaimer, "data/city_amenities_disclaimer.qs")


# Clean up ----------------------------------------------------------------

rm(all_city_amenities, breaks_q3_active, breaks_q5_active, var_list,
   interpolation_keys, new_rows)

# To save output, run dev/build_data.R, which calls this script
