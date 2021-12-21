#### Census data setup #########################################################

# This script relies on objects created in dev/build_data.R


# Global variables and functions ------------------------------------------

# Turn on progress bars
progressr::handlers(global = TRUE)

# Turn on parallel processing
suppressPackageStartupMessages(library(future))
plan(multisession)

# Lists of scales and years
scales <- c("CSD", "CT", "DA")
years <- c(1996, 2001, 2006, 2011, 2016)

# Load functions
source("dev/modules/census/01_download_prep.R")
source("dev/modules/census/02_interpolate.R")
source("dev/modules/census/03_process_and_breaks.R")
source("dev/modules/census/04_add_variables.R")
source("dev/modules/census/05_add_census_data.R")


# Add census data by topic ------------------------------------------------

census_vec <- tibble(
  var_code = character(),
  vec_2016 = list(),
  vec_2011 = list(),
  vec_2006 = list(),
  vec_2001 = list(),
  vec_1996 = list(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
)

add_row_census_vec <- function(data, var_code, vec_2016, vec_2011, vec_2006, 
                               vec_2001, vec_1996, var_title, var_short, 
                               explanation, private) {
  add_row(data,
          var_code = var_code,
          vec_2016 = list(vec_2016),
          vec_2011 = list(vec_2011),
          vec_2006 = list(vec_2006),
          vec_2001 = list(vec_2001),
          vec_1996 = list(vec_1996),
          var_title = var_title,
          var_short = var_short,
          explanation = explanation,
          private = private)
}

parent_vectors <- c()

source("dev/modules/census/census_housing.R")
source("dev/modules/census/census_income.R")
source("dev/modules/census/census_identity.R")
source("dev/modules/census/census_transport.R")
source("dev/modules/census/census_employment.R")
source("dev/modules/census/census_family.R")
source("dev/modules/census/census_language.R")
source("dev/modules/census/census_age.R")
source("dev/modules/census/census_education.R")

census_vec <- 
  census_vec |> 
  mutate(source = "census")


# Gather data -------------------------------------------------------------

data_to_add <- 
  add_census_data(census_vec, scales, years, parent_vectors)

# Assign data -------------------------------------------------------------

borough <- 
  borough |> 
  left_join(data_to_add[[1]]$borough, by = "ID") |> 
  relocate(geometry, .after = last_col())

CT <- 
  CT |> 
  left_join(data_to_add[[1]]$CT, by = "ID") |> 
  relocate(geometry, .after = last_col())

DA <- 
  DA |> 
  left_join(data_to_add[[1]]$DA, by = "ID") |> 
  relocate(centroid, buffer, geometry, .after = last_col())

grid <-
  grid |>
  left_join(data_to_add[[1]]$grid, by = "ID") |>
  relocate(geometry, .after = last_col())


# Assign DA data to streets and buildings ---------------------------------

DA_census <- 
  DA[, str_detect(names(DA), paste0(census_vec$var_code, collapse = "|"))] |> 
  mutate(ID = DA$ID) |> 
  st_drop_geometry()

street <- 
  street |> 
  left_join(DA_census, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col())

building <- 
  building |> 
  left_join(DA_census, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

variables <- bind_rows(variables, data_to_add[[2]])


# Clean up ----------------------------------------------------------------

rm(scales, years, add_census_data, add_q3, add_q5, add_vars, add_years, agg_add,
   agg_avg, drop_vars, find_breaks_q5, get_agg_type, get_breaks_q3, 
   get_breaks_q5, get_categories_q5, get_census_vectors, get_empty_geometries,
   get_unit_type, interpolate, interpolate_other, merge_breaks, normalize,
   reduce_years, swap_csd_to_borough, weighted_mean, census_vec, parent_vectors,
   DA_census)

# To save output, run dev/build_geometries.R, which calls this script
