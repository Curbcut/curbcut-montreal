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

# With progress!
progressr::handlers(progressr::handler_progress(
  format = 
    ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
  width = 60,
  complete = "+"
))

with_progress({
  
  p <- 
    progressr::progressor(sum(map_int(tenure_statuses, length)) *
                            sum(map_int(shelter_costs, length)) *
                            sum(map_int(characteristics, length)) * 
                            2)
  
  housing_characteristics <- 
    map(set_names(c("CT", "centraide")), function(scale) {
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
                                              characteristics = characteristic)[[
                                                scale]][, "var"]
                
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
})

housing_characteristics <- 
  map2(housing_characteristics, names(housing_characteristics), function(df, scale) {
    bind_cols(get_housing_characteristics()[[scale]][, "ID"], df) |>
      rename_with(~paste0("housing_characteristics_", .x, "_2016"), 
                  total_total_total:last_col())
  })


# Filter only the CMA -----------------------------------------------------

housing_characteristics <- list(
  CT = 
    select(CT, ID) |> 
    st_drop_geometry() |> 
    left_join(housing_characteristics$CT, by = "ID"),
  centraide = 
    select(centraide, name) |> 
    st_drop_geometry() |> 
    left_join(housing_characteristics$centraide, by = c("name" = "ID"))
)

housing_characteristics <- 
  map(housing_characteristics, ~{
    .x |> 
      select(-contains("q3"), -contains("q5"))
  })

# Calculate breaks --------------------------------------------------------

housing_characteristics <- map(housing_characteristics, add_q3)

housing_characteristics_q3 <- map(housing_characteristics, get_breaks_q3)
housing_characteristics_q5 <- map(housing_characteristics, get_breaks_q5)

housing_characteristics <-
  map2(housing_characteristics, housing_characteristics_q5, 
       ~{bind_cols(.x, add_q5(.x, .y))})

# Add to variables table --------------------------------------------------

var_list <-
  housing_characteristics$CT |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  map2_dfr(housing_characteristics_q3, c("CT", "centraide"), \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  map2_dfr(housing_characteristics_q5, c("CT", "centraide"), \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:5,
                                 .before = 1)})

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
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, var = all_of(var)),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, var = all_of(var)),
                    source = "Centraide")
    
    out[out$var_code == str_remove(var, "_\\d{4}$"), ]
    
  })

variables <-
  bind_rows(variables, new_rows)

# Join vulnerable_pop to CT -----------------------------------------------

CT <-
  left_join(CT, housing_characteristics$CT, by = "ID")


# Join vulnerable_pop to centraide ----------------------------------------

centraide <-
  left_join(centraide, housing_characteristics$centraide, by = "name") |> 
  relocate(geometry, .after = last_col())

# Clean up ----------------------------------------------------------------

rm(tenure_statuses, characteristics, shelter_costs,
   var_list, breaks_q3_active, breaks_q5_active, new_rows, housing_characteristics,
   housing_characteristics_q3, housing_characteristics_q5)
