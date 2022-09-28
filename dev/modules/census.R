#### Census data setup #########################################################

# This script relies on objects created in dev/build_data.R


# Global variables and functions ------------------------------------------

# Turn on progress bars
progressr::handlers(global = TRUE)

# Turn on parallel processing
suppressPackageStartupMessages(library(future))
# plan(multisession)

# Lists of scales and years
scales <- unique(unlist(all_tables, use.names = FALSE))
scales <- scales[!scales %in% c("building")]

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
  mutate(source = "Canadian census")


# Kepp track of vector and their parents for data explanation -------------

census_variables <- 
  map(years, function(year) {
    
    census_dataset <- paste0("CA", sub("20", "", year))
    raw_vecs <- cancensus::list_census_vectors(census_dataset)
    
    # Already has a parent vector
    other_parent_vec <- 
      parent_vectors[str_which(parent_vectors, census_dataset)]
    names(other_parent_vec) <- str_remove(names(other_parent_vec), "\\d$")
    
    out <- 
      census_vec |> 
      select(var_code, all_of(paste0("vec_", year))) |> 
      rowwise() |> 
      mutate(vec_label = list(
        map_chr(.data[[paste0("vec_", year)]], function(row) {
          map_chr(row, function(vec) {
            if (is.na(vec)) return(NA)
            raw_vecs[raw_vecs$vector == vec, ]$label[1]
          })
        }) |> unique())) |> 
      mutate(parent_vec = 
               if_else(var_code %in% names(other_parent_vec), 
                       list(unname(other_parent_vec)[
                         names(other_parent_vec) == var_code]),
                       list(
                         map_chr(.data[[paste0("vec_", year)]], function(row) {
                           map_chr(row, function(vec) {
                             if (is.na(vec)) return(NA)
                             raw_vecs[raw_vecs$vector == vec, ]$parent_vector
                           })
                         }) |> unique())),
             aggregation = 
               map_chr(.data[[paste0("vec_", year)]], function(row) {
                 map_chr(row, function(vec) {
                   if (is.na(vec)) return(NA)
                   raw_vecs[raw_vecs$vector == vec, ]$aggregation
                 })
               }) |> unique()) |> 
      mutate(parent_vec = if_else(length(parent_vec) == 1 &&
                                    is.na(parent_vec) &&
                                    !is.na(aggregation), 
                                  list(str_extract(aggregation, "v_CA.*$")),
                                  list(parent_vec))) |>
      mutate(parent_vec_label = list(
        map_chr(parent_vec, function(row) {
          map_chr(row, function(vec) {
            if (is.na(vec)) return(NA)
            raw_vecs[raw_vecs$vector == vec, ]$label[1]
          })
        }) |> unique())) |> 
      ungroup()
    
    names(out)[3] <- paste0("vec_label_", year)
    names(out)[4] <- paste0("parent_vec_", year)
    names(out)[5] <- paste0("aggregation_", year)
    names(out)[6] <- paste0("parent_vec_label_", year)
    
    out
    
  }) |> reduce(left_join, by = "var_code")

# Arrange more easily
census_variables <- 
  map_dfr(years, function(year) {
    map_dfr(census_variables$var_code, function(var) {
      
      row <- 
        census_variables |> 
        select(var_code, ends_with(as.character(year))) |> 
        filter(var_code == var)
      
      tibble(var_code = paste0(var, "_", year),
             vec = row[[paste0("vec_", year)]],
             vec_label = row[[paste0("vec_label_", year)]],
             parent_vec = row[[paste0("parent_vec_", year)]],
             aggregation = row[[paste0("aggregation_", year)]],
             parent_vec_label = row[[paste0("parent_vec_label_", year)]])
    })
  })


# Gather data -------------------------------------------------------------

# data_to_add <-
#   add_census_data(census_vec, scales,  all_tables, years, parent_vectors)
# qsave(data_to_add, file = "dev/data/modules_raw_data/census.qs")

data_to_add <- qread("dev/data/modules_raw_data/census.qs")


# Data testing ------------------------------------------------------------

data_testing(data_to_add$scales)


# Assign data -------------------------------------------------------------

assign_tables(data_to_add$scales)


# Add to variables table --------------------------------------------------

variables <- bind_rows(variables, data_to_add$variables)


# Add to modules table ----------------------------------------------------

# Only housing is its own module from the census
modules <- 
  modules |> 
  add_modules(id = "housing",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>This module presents <a href = 'https://www.statcan",
                       ".gc.ca/en/census/census-engagement/about'>housing data",
                       " from the 1996 to ", years[length(years)], 
                       " Canadian Censuses</a></p>"))


# Clean up ----------------------------------------------------------------

rm(scales, years, add_census_data, add_q3_list, add_q5_list, 
   add_vars, add_years, agg_add, agg_avg, drop_vars, 
   get_agg_type, get_breaks_q3_list, 
   get_breaks_q5_list, get_categories_q5, get_categories_q5_list, 
   get_census_vectors, get_empty_geometries, get_unit_type, interpolate, 
   interpolate_other, merge_breaks, normalize, reduce_years, 
   swap_csd_to_borough, weighted_mean, census_vec, parent_vectors,  
   add_row_census_vec, data_to_add)

# To save output, run dev/build_geometries.R, which calls this script
