#### Green space data setup #################################################### 

# This script relies on objects created in dev/build_data.R

# Get updated data from open data portal ----------------------------------

# dl_unzip <- function(shp_url, name) {
#   download.file(shp_url, destfile = paste0("dev/data/", "temp",
#                                            ".zip"))
#   
#   unzip(paste0("dev/data/", "temp", ".zip"),
#         exdir = "dev/data")
#   
#   unlink(paste0("dev/data/", "temp", ".zip"), recursive = TRUE)
# }
# 
# # DL Espace_Vert.shp
# dl_unzip(paste0("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-",
#                 "565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/",
#                 "download/shapefile.zip"))
# 
# rm(dl_unzip)

# Tidy and transform data -------------------------------------------------

green_space <- 
  read_sf("dev/data/Espace_Vert.shp") |>
  select(ID = OBJECTID, name = Nom, type_1 = TYPO1, type_2 = TYPO2, 
         property = PROPRIETE, management = GESTION, geometry) |>
  st_transform(4326) |> 
  st_set_agr("constant") |>
  st_cast("POLYGON") |> 
  group_by(ID, name, type_1, type_2, property, management) |> 
  summarize(geometry = st_combine(geometry), .groups = "drop") |> 
  ungroup() |> 
  st_set_agr("constant") |> 
  mutate(type = coalesce(type_1, "Autre espace vert"), 
         # This is paste0(col_left_5[4], "AA")
         fill = "#31A354",
         area = round(units::drop_units(st_area(geometry))), 
         .before = geometry) |> 
  mutate(type_1 = case_when(
    type_1 == "Autre espace vert" ~ "other",
    type_1 == "En cours de validation" ~ "under_validation",
    type_1 == "Espace voirie" ~ "road_space",
    type_1 == "Grand parc" ~ "large_park",
    type_1 == "Parc d'arrondissement" ~ "borough_park",
    is.na(type_1) ~ "other")) |> 
  st_set_agr("constant")

# Attach a CSDUID
linked_borough <- 
  green_space |> 
  st_transform(32618) |> 
  st_intersection(st_transform(select(borough, CSDUID) |> 
                                 st_set_agr("constant"), 32618)) |> 
  mutate(area_after = st_area(geometry)) |> 
  st_drop_geometry() |> 
  select(ID, CSDUID, area, area_after) |> 
  mutate(area_prop = area_after / area) |> 
  arrange(-area_prop) |> 
  group_by(ID) |> 
  slice(1) |> 
  select(ID, CSDUID)

green_space <- 
  green_space |> 
  left_join(linked_borough, by = "ID") |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")


# Process green space data ------------------------------------------------

process_gs <- function(df) {
  
  st_agr(df) <-  "constant"
  
  # Only keep Ville de Montreal
  df <- if (nrow(df) == nrow(borough)) {
    filter(df, str_starts(ID, "2466023")) 
  } else {
    filter(df, str_starts(CSDUID, "2466023")) 
  }
  
  df |> 
    st_transform(32618) |> 
    select(ID) |> 
    st_intersection(st_transform(green_space, 32618)) |> 
    filter(st_is(geometry, c("POLYGON", "MULTIPOLYGON"))) |>
    mutate(area = units::drop_units(st_area(geometry)), .before = geometry) |> 
    st_drop_geometry() |> 
    group_by(ID, type_1) |> 
    summarize(green_space_sqm = sum(area), .groups = "drop") |> 
    group_by(ID) |> 
    summarize(type = c(type_1, "total"), 
              area = c(green_space_sqm, sum(green_space_sqm, na.rm = TRUE)),
              .groups = "drop") |> 
    pivot_wider(id_cols = "ID", names_from = "type", 
                names_prefix = "green_space_", names_sep = "_", 
                values_from = area) |> 
    full_join(select(df, ID, population), by = "ID") |> 
    st_as_sf() |> 
    mutate(across(starts_with("green_space"), .fns = list(
      sqkm = ~{1000 * .x / units::drop_units(st_area(geometry))},
      per1k = ~{1000 * .x / population}), .names = "{.col}_{.fn}"), 
      .before = geometry) |>
    select(-c(green_space_other, green_space_road_space,
              green_space_borough_park, green_space_under_validation,
              green_space_large_park, green_space_total, population)) |>
    mutate(across(starts_with("green_space"), ~replace(., is.na(.), 0))) |> 
    mutate(across(starts_with("green_space"), 
                  ~replace(., is.infinite(.), 0))) |> 
    st_drop_geometry()
}

gs_results <- map(list("borough" = borough, "CT" = CT, "DA" = DA), process_gs)


# Add breaks --------------------------------------------------------------

gs_results <- map(gs_results, add_q3)
gs_q3 <- map(gs_results, get_breaks_q3)
gs_q5 <- map(gs_results, get_breaks_q5)
gs_results <- map2(gs_results, gs_q5, ~bind_cols(.x, add_q5(.x, .y)))


# Data testing ------------------------------------------------------------

data_testing(gs_results)


# Join data ---------------------------------------------------------------

walk(names(gs_results), ~{
  assign(.x, left_join(get(.x), gs_results[[.x]], by = "ID") |> 
           relocate(any_of(c("buffer", "centroid", "building", "geometry")), 
                    .after = last_col()), envir = globalenv())})

building <- 
  building |> 
  left_join(gs_results$DA, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

street <- 
  street |> 
  left_join(gs_results$DA, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")


# Check meta data ---------------------------------------------------------

# meta_testing()


# Add to variables table --------------------------------------------------

var_list <- 
  gs_results |> 
  map(~names(select(.x, -ID, -contains(c("q3", "q5"))))) |> 
  unlist() |> 
  unique()

# Get breaks_q3
breaks_q3_active <-
  map(set_names(var_list), ~{
    map2_dfr(gs_q3, names(gs_results), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                   .before = everything())}) |> 
      select(scale, date, rank, var = all_of(.x))})

# Get breaks_q5
breaks_q5_active <- 
  map(set_names(var_list), ~{
    map2_dfr(gs_q5, names(gs_results), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, rank = 0:5, 
                                   .before = everything())}) |> 
      select(scale, rank, var = all_of(.x))})

# Construct green space variables table
green_space_table <- 
  map_dfr(var_list, ~{
    type <- case_when(str_detect(.x, "borough_park") ~ "Borough park",
                      str_detect(.x, "large_park") ~ "Large park",
                      str_detect(.x, "other") ~ "Other park",
                      str_detect(.x, "under_validation") ~ "Under validation",
                      str_detect(.x, "total") ~ "Total green space",
                      str_detect(.x, "road_space") ~ "Road space")
    
    group <- if (str_detect(.x, "sqkm")) "per sq km" else "per 1,000"
    
    tibble(
      var_code = .x,
      var_title = paste(type, group),
      var_short = str_remove_all(paste(type, group), "per |green space ") |> 
        str_replace("sq km", "/sqkm") |> 
        str_replace("1,000", "/1,000"),
      explanation = paste("the number of square metres of", 
                          str_to_lower(type) |> 
                            str_replace("under validation", 
                                        "green space under validation"), 
                          str_replace(group, "sq km", "square kilometre") |> 
                            str_replace("1,000", "1,000 residents")),
      category = NA,
      theme = "Urban life",
      private = FALSE,
      dates = NA,
      scales = list(c("borough", "CT", "DA", "building", "street")),
      breaks_q3 = list(breaks_q3_active[[.x]]),
      breaks_q5 = list(breaks_q5_active[[.x]]),
      source = "VdM")
  })

# Join green space variable table to variables table
variables <-
  variables |>
  bind_rows(green_space_table)


# Clean up ----------------------------------------------------------------

rm(breaks_q3_active, breaks_q5_active, green_space_table, gs_q3, gs_q5, 
   gs_results, process_gs, var_list, linked_borough)
