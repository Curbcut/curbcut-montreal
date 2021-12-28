#### Green alley data setup #################################################### 

# This script relies on objects created in dev/census.R


# Load packages -----------------------------------------------------------
suppressPackageStartupMessages({
  library(future)
  library(foreach)
  library(progressr)
  library(doFuture)
  registerDoFuture()
  plan(multisession)
})

# Get updated data from open data portal ----------------------------------

dl_unzip <- function(shp_url, name) {
  download.file(shp_url, destfile = paste0("dev/data/", "temp",
                                           ".zip"))
  
  unzip(paste0("dev/data/", "temp", ".zip"),
        exdir = "dev/data")
  
  unlink(paste0("dev/data/", "temp", ".zip"), recursive = TRUE)
}

# DL Espace_Vert.shp
dl_unzip(paste0("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-",
                "565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/",
                "download/shapefile.zip"))

rm(dl_unzip)

# Tidy and transform data -------------------------------------------------

green_space <- 
  read_sf("dev/data/Espace_Vert.shp") |>
  select(ID = OBJECTID, name = Nom, type_1 = TYPO1, type_2 = TYPO2, geometry) |>
  st_transform(4326) |> 
  st_set_agr("constant") |>
  st_cast("POLYGON")

green_space <- 
green_space |> 
  mutate(type_1 = case_when(type_1 == "Autre espace vert" ~ "other",
                            is.na(type_1) ~ "other",
                            type_1 == "En cours de validation" ~ "under_validation",
                            type_1 == "Espace voirie" ~ "road_space",
                            type_1 == "Grand parc" ~ "large_park",
                            type_1 == "Parc d'arrondissement" ~ "borough_park"))
    


# Process green space data ------------------------------------------------

process_gs <- function(df) {

  x <- nrow(green_space)
  batch_size <- 200
  
  with_progress({
    pb <- progressor(x)
    handlers(list(
      handler_progress(
        format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        width    = 60,
        complete = "="
      )
    ))
    
    total_iterations <- ceiling(x / batch_size)
    iteration <- 1
    results <- vector("list", total_iterations)
    
    while (iteration <= total_iterations) {
      results[[iteration]] <- 
        foreach(i = ((iteration - 1) * batch_size + 1):min(iteration * batch_size, x), .combine = c) %dopar% {
          pb()
          st_intersection(select(df, ID), select(green_space[i, ], type_1))
        }  
      iteration <- iteration + 1
    }
  })
  
  intersected <- 
    map_dfr(results, ~{
      tibble(ID = .x[names(.x) == "ID"],
             type_1 = .x[names(.x) == "type_1"],
             geometry = .x[names(.x) == "geometry"])
    }) |> unnest(c(ID, type_1, geometry)) |> 
    st_as_sf()

  intersected |> 
    filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
    mutate(
      area = units::drop_units(st_area(geometry)),
      .before = geometry
    ) |> 
    st_drop_geometry() |> 
    group_by(ID, type_1) |> 
    summarize(green_space_sqm = sum(area),
              .groups = "drop") |> 
    group_by(ID) |> 
    summarize(type = c(type_1, "total"), area = c(green_space_sqm, sum(green_space_sqm, na.rm = TRUE)),
              .groups = "drop") |> 
    pivot_wider(id_cols = "ID",
                names_from = "type",
                names_prefix = "green_space_",
                names_sep = "_",
                values_from = area) |> 
    left_join(select(df, ID, population), ., by = "ID") |> 
    st_as_sf() |> 
    mutate(across(starts_with("green_space"), 
                  .fns = list(
                    sqkm = ~{1000 * .x / 
                        units::drop_units(st_area(geometry))},
                    per1k = ~{1000 * .x / population}),
                  .names = "{.col}_{.fn}"), .before = geometry) |>
    select(-c("green_space_other", "green_space_road_space",
              "green_space_borough_park", "green_space_under_validation",
              "green_space_large_park", "green_space_total"),
           -population, everything()) |>
    mutate(across(starts_with("green_space"), ~replace(., is.na(.), 0))) |> 
    mutate(across(starts_with("green_space"), ~replace(., is.infinite(.), 0))) |> 
    mutate(across(starts_with("green_space"), ntile, n = 3, .names = "{.col}_q3"), 
           .before = geometry) |> 
    st_drop_geometry()
}

gs_results <- map(list("borough" = borough, 
                       "CT" = CT, "DA" = DA, "grid" = grid), process_gs)


# Data testing ------------------------------------------------------------

data_testing(gs_results)


# Variable explanations ---------------------------------------------------

variables <-
  variables |>
  add_variables(
    var_code = "green_alley_sqkm",
    var_title = "Green alleys per sq km",
    var_short = "Alleys sqkm",
    explanation = paste0("the number of square meters of green alley per ",
                         "square kilometers"),
    category = NA,
    private = FALSE,
    dates = "2021",
    scales = c("borough", "CT", "DA"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "VdM") |> 
  add_variables(
    var_code = "green_alley_per1k",
    var_title = "Green alleys per 1,000",
    var_short = "Alleys 1,000",
    explanation = paste0("the number of square meters of green alley per ",
                         "1,000 residents"),
    category = NA,
    private = FALSE,
    dates = "2021",
    scales = c("borough", "CT", "DA"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "VdM")
