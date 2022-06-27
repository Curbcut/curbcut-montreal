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

rm(table2)

# Prepare variables --------------------------------------------------------

imm_statuses <- list("total" = "total", "immigrants" = "immigrants",
                     "non_immigrants" = c("non-immigrants", "non-permanent"))
household_statuses <- list("total" = "total", 
                           "lone_parents" = "Lone parents (lone-parent families)",
                           "living_alone" = "Persons living alone")
shelter_costs <- list("total" = "total", 
                      "more_30_per" = c("30-50%", "50%-80%", ">80%"),
                      "more_50_per" = c("50%-80%", ">80%"),
                      "more_80_per" = ">80%")
sexes <- list("total" = "total", 
              "female" = "female", 
              "male" = "male")


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
    progressr::progressor(steps = length(imm_statuses) *
                            length(household_statuses) *
                            length(shelter_costs) *
                            length(sexes) * 2)
  
  vulnerable_pop <- 
    map(set_names(c("CT", "centraide")), function(scale) {
      map_dfc(names(imm_statuses), function(imm_status_name) {
        
        imm_status <- imm_statuses[[imm_status_name]]
        
        # Non-immigrant includes also non-permanent resident. Get the multiple
        # columns, and sum them later.
        imm_sum_rows <- 
          map(imm_status, function(imm_stat) {
            map_dfc(names(household_statuses), function(household_status_name) {
              
              household_status <- household_statuses[[household_status_name]]
              
              map_dfc(names(shelter_costs), function(shelter_cost_name) {
                
                shelter_cost_f <- shelter_costs[[shelter_cost_name]]
                
                shelter_cost_sum_rows <- 
                  map(shelter_cost_f, function(shelter_c) {
                    map_dfc(names(sexes), function(sex_name) {
                      
                      se <- sexes[[sex_name]]
                      
                      out <- 
                        get_vulnerable_pop(sex = se, 
                                           shelter_cost = shelter_c,
                                           immigrant_status = imm_stat,
                                           characteristics = household_status)[[
                                             scale]][, "var"]
                      
                      p()
                      
                      names(out) <- paste(imm_status_name,
                                          household_status_name,
                                          shelter_cost_name,
                                          sex_name, 
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
        
        # In the case of Non-immigrant including two columns (non-immigrant
        # and non-permanent resident), sum the two columns.
        if (length(imm_sum_rows) > 1) {
          imm_sum_rows <- 
            map(imm_sum_rows, mutate, row_n = row_number()) |> 
            reduce(bind_rows) |> 
            group_by(row_n) |> 
            summarize_all(sum) |> 
            select(-row_n)
        }
        
        imm_sum_rows
        
      })
      
    })
})

vulnerable_pop <- 
map2(vulnerable_pop, names(vulnerable_pop), function(df, scale) {
  bind_cols(get_vulnerable_pop()[[scale]][, "ID"], df) |>
    rename_with(~paste0("vulnerable_pop_", .x, "_2016"), 
                total_total_total_total:last_col())
  
})


# Filter only the CMA -----------------------------------------------------

vulnerable_pop <- list(
  CT = 
    select(CT, ID) |> 
    st_drop_geometry() |> 
    left_join(vulnerable_pop$CT, by = "ID"),
  centraide = 
    select(centraide, name) |> 
    st_drop_geometry() |> 
    left_join(vulnerable_pop$centraide, by = c("name" = "ID"))
)


# Calculate breaks --------------------------------------------------------

vulnerable_pop <- map(vulnerable_pop, add_q3)

vulnerable_pop_q3 <- map(vulnerable_pop, get_breaks_q3)
vulnerable_pop_q5 <- map(vulnerable_pop, get_breaks_q5)

vulnerable_pop <-
  map2(vulnerable_pop, vulnerable_pop_q5, ~{bind_cols(.x, add_q5(.x, .y))})


  bind_cols(vulnerable_pop, add_q5(vulnerable_pop, vulnerable_pop_q5))


# Add to variables table --------------------------------------------------

var_list <-
  vulnerable_pop |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  map(set_names(var_list), ~{
    if (nrow(vulnerable_pop_q3) > 0)
      vulnerable_pop_q3 |>
      mutate(scale = "CT", date = "2016", rank = 0:3,
                                  .before = everything()) |>
      select(scale, date, rank, var = all_of(.x))})

# Get breaks_q5
breaks_q5_active <-
  map(set_names(var_list), ~{
    if (nrow(vulnerable_pop_q5) > 0)
      vulnerable_pop_q5 |> mutate(scale = "CT", date = "2016", rank = 0:5,
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


# Join vulnerable_pop to CT -----------------------------------------------

CT <-
  left_join(CT, vulnerable_pop, by = "ID")

# Clean up ----------------------------------------------------------------

rm(imm_statuses, household_statuses, shelter_costs, sexes, ordered_ID,
   var_list, breaks_q3_active, breaks_q5_active, new_rows, vulnerable_pop,
   vulnerable_pop_q3, vulnerable_pop_q5)
