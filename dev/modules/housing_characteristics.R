#### Vulnerable population data setup ##########################################

# This script relies on objects created in dev/census.R


# Load libraries and data -------------------------------------------------

library(tidyverse)
library(qs)
source("dev/other/crosstabs_fun.R")
library(future)
library(furrr)
library(progressr)
library(sf)

# Load data ---------------------------------------------------------------

# table1 <- 
#   read.csv("data/StatCan_Recensement2016/Fichiers_Sources/tableau1.csv",
#            header = FALSE) |> as_tibble()
# 
# table2 <- 
#   read.csv("data/StatCan_Recensement2016/Fichiers_Sources/tableau2.csv",
#            header = FALSE) |> as_tibble()
# 
# qsavem(table1, table2, 
#        file = "data/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")

qload("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")

rm(table1)

# Prepare variables --------------------------------------------------------

tenure_statuses <- list("total" = "total", 
                     "tenant" = "tenant",
                     "owner" = "owner")
shelter_costs <- list("total" = "total", 
                      "more_30_per" = c("30-50%", "50%-80%", ">80%"),
                      "more_50_per" = c("50%-80%", ">80%"),
                      "more_80_per" = ">80%")
characteristics <- list("total" = "total",
                        "kids_3_plus" = "Families with 3 or more children",
                        "unsuitable" = "unsuitable",
                        "core_need" = "in core need",
                        "repairs" = "major repairs needed")


# Iteration of the retrieval function -------------------------------------

ordered_ID <- get_housing_characteristics()[, "ID"]

# With progress!
progressr::handlers(progressr::handler_progress(
  format = 
    ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
  width = 60,
  complete = "+"
))

with_progress({
  
  p <- 
    progressr::progressor(steps = length(tenure_statuses) *
                            length(shelter_costs) *
                            length(characteristics))
  
  housing_characteristics <- 
    map_dfc(names(tenure_statuses), function(tenure_status_name) {
      
      tenure_status <- tenure_statuses[[tenure_status_name]]
      
      map_dfc(names(shelter_costs), function(shelter_cost_name) {
        
        shelter_cost_f <- shelter_costs[[shelter_cost_name]]
        
        shelter_cost_sum_rows <- 
          map(shelter_cost_f, function(shelter_c) {
            map_dfc(names(characteristics), function(characteristic_name) {
              
              characteristic <- characteristics[[characteristic_name]]
              
              out <- 
                get_housing_characteristics(tenure = tenure_status, 
                                            shelter_cost = shelter_c,
                                            characteristics = characteristic)[
                                              , "var"]
              
              p()
              
              names(out) <- paste(tenure_status_name,
                                  shelter_cost_name,
                                  characteristic_name, 
                                  sep = "_")
              
              out
              
            })
          })
        
        if (length(shelter_cost_sum_rows) > 1) {
          shelter_cost_sum_rows <- 
            map(shelter_cost_sum_rows, mutate, row_n = row_number()) |> 
            reduce(bind_rows) |> 
            group_by(row_n) |> 
            summarize_all(sum) |> 
            select(-row_n)
        }
        
        shelter_cost_sum_rows
        
      })
    })
})

housing_characteristics <- 
  bind_cols(ordered_ID, housing_characteristics) |>
  rename_with(~paste0("housing_characteristics_", .x, "_2016"), 
              total_total_total:last_col())


# Filter only the CMA -----------------------------------------------------

housing_characteristics <- 
  select(CT, ID) |> 
  st_drop_geometry() |> 
  left_join(housing_characteristics, by = "ID")


# Calculate breaks --------------------------------------------------------

housing_characteristics <- add_q3(housing_characteristics)

housing_characteristics_q3 <- get_breaks_q3(housing_characteristics) 
housing_characteristics_q5 <- get_breaks_q5(housing_characteristics)

housing_characteristics <-
  bind_cols(housing_characteristics, add_q5(housing_characteristics, housing_characteristics_q5))


# Add to variables table --------------------------------------------------

var_list <- 
  housing_characteristics |> 
  select(-ID, -contains(c("q3", "q5"))) |> 
  names()

# Get breaks_q3
breaks_q3_active <-
  map(set_names(var_list), ~{
    if (nrow(housing_characteristics_q3) > 0) 
      housing_characteristics_q3 |> 
      mutate(scale = "CT", date = "2016", rank = 0:3,
                                  .before = everything()) |> 
      select(scale, date, rank, var = all_of(.x))})

# Get breaks_q5
breaks_q5_active <-
  map(set_names(var_list), ~{
    if (nrow(housing_characteristics_q5) > 0) 
      housing_characteristics_q5 |> mutate(scale = "CT", date = "2016", rank = 0:5,
                                  .before = everything()) |> 
      select(scale, date, rank, var = all_of(.x))})

new_rows <- 
  map_dfr(var_list, function(var) {
    
    out <- 
      add_variables(variables,
                    var_code = str_remove(var, "_\\d{4}$"),
                    var_title = str_remove(var, "_\\d{4}$"),
                    var_short = str_remove(var, "_\\d{4}$"),
                    explanation = str_remove(var, "_\\d{4}$"),
                    category = NA,
                    theme = "Housing",
                    private = TRUE,
                    dates = "2016",
                    scales = "CT",
                    breaks_q3 = breaks_q3_active[[var]],
                    breaks_q5 = breaks_q5_active[[var]],
                    source = "Centraide")
    
    out[out$var_code == str_remove(var, "_\\d{4}$"), ]
    
  })

variables <- 
  bind_rows(variables, new_rows)


# Join housing_characteristics to CT -----------------------------------------------

CT <- 
  left_join(CT, housing_characteristics, by = "ID")

# Clean up ----------------------------------------------------------------

rm(tenure_statuses, characteristics, shelter_costs, ordered_ID,
   var_list, breaks_q3_active, breaks_q5_active, new_rows, housing_characteristics,
   housing_characteristics_q3, housing_characteristics_q5)
