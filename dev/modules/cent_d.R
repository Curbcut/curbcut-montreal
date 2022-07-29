#### Centraide DWELLINGS type data setup #######################################

# This script relies on objects created in dev/census.R


# Load libraries and data -------------------------------------------------

library(tidyverse)
library(qs)
library(future)
library(furrr)
library(progressr)
library(sf)

source("dev/other/crosstabs_fun.R")

# table1 <-
#   read.csv("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tableau1.csv",
#            header = FALSE) |> as_tibble()
# 
# table2 <-
#   read.csv("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tableau2_amend.csv",
#            header = FALSE) |> as_tibble()
# 
# qsavem(table1, table2,
#        file = "data/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")

qload("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")

rm(table1)


# Prepare variables --------------------------------------------------------

# Tenure status
tenure_statuses <- list("total" = "total", 
                        "tenant" = "tenant",
                        "owner" = "owner")

# Shelter cost burden
shelter_costs <- list("total" = "total", 
                      "more_30_per" = c("30-50%", "50%-80%", ">80%"),
                      "more_50_per" = c("50%-80%", ">80%"),
                      "more_80_per" = ">80%")

add_characteristics <- 
  # Dwelling characteristics
  list("total" = "total", 
       "single_detached" = "single-detached house",
       "semi_detached" = "semi-detached house",
       "row_house" = "row house",
       "in_duplex" = "apartment or flat in a duplex",
       "in_5plus_storeys" = "apartment in a building that has five or more storeys",
       "in_less5_storeys" = "apartment in a building that has fewer than five storeys",
       "other_single_attached" = "other single-attached house",
       "mobile_homes" = "mobile homes and other movable dwellings",
       # OR
       # Family characteristics
       "kids_3_plus" = "Families with 3 or more children",
       "low_inc" = "low income after tax",
       "unsuitable" = "unsuitable",
       "repairs" = "major repairs needed")


# Compare -----------------------------------------------------------------

# Amenity access: food, parks, healthcare facilities
# Accessibility to total jobs on peak weekday
# canale_ind
# climate_heat_waves


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
                            sum(map_int(add_characteristics, length)) * 
                            2)
  
  cent_d <- 
    map(set_names(c("CT", "centraide")), function(scale) {
      map_dfc(names(tenure_statuses), function(tenure_status_name) {
        
        tenure_status <- tenure_statuses[[tenure_status_name]]
        
        map_dfc(names(shelter_costs), function(shelter_cost_name) {
          
          shelter_cost_f <- shelter_costs[[shelter_cost_name]]
          
          shelter_cost_sum_rows <- 
            map(shelter_cost_f, function(shelter_c) {
              map_dfc(names(add_characteristics), function(characteristic_name) {
                
                add_characteristics <- add_characteristics[[characteristic_name]]
                
                out <- 
                  get_housing_char(tenure = tenure_status, 
                                              shelter_cost = shelter_c,
                                              characteristics = add_characteristics)[[
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

cent_d <- 
  map2(cent_d, names(cent_d), function(df, scale) {
    bind_cols(get_housing_char()[[scale]][, "ID"], df) |>
      rename_with(~paste0("cent_d_", .x, "_count_2016"), 
                  total_total_total:last_col())
  })


# Filter only the CMA -----------------------------------------------------

cent_d <- list(
  CT = 
    select(CT, ID) |> 
    st_drop_geometry() |> 
    left_join(cent_d$CT, by = "ID"),
  centraide = 
    select(centraide, name) |> 
    st_drop_geometry() |> 
    left_join(cent_d$centraide, by = c("name" = "ID"))
)

# Add interpolated boroughs
cent_d$borough <-
  cent_d$CT |> 
  (\(x) left_join(select(st_drop_geometry(CT), ID, CSDUID), x, 
                  by = "ID"))()  |> 
  group_by(CSDUID) |>
  summarize(across(starts_with("cent_d"), ~sum(.x, na.rm = T))) |> 
  (\(x) left_join(select(st_drop_geometry(borough), ID), x, 
                  by = c("ID" = "CSDUID")))()


# Count and percentage ----------------------------------------------------

cent_d <- 
  imap(cent_d, function(scale, df) {
    
    # Switch centraide name to ID
    if (df == "centraide") 
      scale <- left_join(select(st_drop_geometry(centraide), name, ID),
                         scale, by = "name") |> select(-name)
    
    # Add _pct
    with_pct <- 
      scale |> 
      mutate(across(starts_with("cent_d"), ~{.x / 
          cent_d_total_total_total_count_2016}))
    
    names(with_pct)[str_starts(names(with_pct), "cent_d")] <- 
      names(with_pct)[str_starts(names(with_pct), "cent_d")] |> 
      str_replace("count_2016$", "pct_2016")
    
    # Combine count and pct
    left_join(scale, with_pct, by = "ID")
    
  })

# Calculate breaks --------------------------------------------------------

cent_d <- map(cent_d, add_q3)

cent_d_q3 <- map(cent_d, get_breaks_q3)
cent_d_q5 <- map(cent_d, get_breaks_q5)

cent_d <-
  map2(cent_d, cent_d_q5, 
       ~{bind_cols(.x, add_q5(.x, .y))})

# Add to variables table --------------------------------------------------

var_list <-
  cent_d$CT |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  map2_dfr(cent_d_q3, c("CT", "centraide", "borough"), \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  map2_dfr(cent_d_q5, c("CT", "centraide", "borough"), \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:5,
                                 .before = 1)})

new_rows <-
  map_dfr(var_list, function(var) {
    
    var <- str_remove(var, "_\\d{4}$")
    
    # # TITLE
    # post_title <- 
    #   case_when(str_ends(var, "_count$") ~ "",
    #             str_ends(var, "_pct$") ~ "%")
    # 
    # tenure_title <- 
    #   case_when(str_detect(var, "tenant") ~ "Tenant",
    #             str_detect(var, "owner") ~ "Owner",
    #             TRUE ~ "Household")
    # 
    # shelter_title <- 
    #   case_when(str_detect(var, "more_30_per") ~ 
    #               ">30% of revenue on shelter",
    #             str_detect(var, "more_50_per") ~ 
    #               ">50% of revenue on shelter",
    #             str_detect(var, "more_80_per") ~ 
    #               ">80% of revenue on shelter",
    #             TRUE ~ "")
    # 
    # characteristics_title <- 
    #   case_when(str_detect(var, "kids_3_plus") ~ 
    #               "Family with 3+ children",
    #             str_detect(var, "unsuitable") ~ 
    #               "Unsuitable housing",
    #             str_detect(var, "core_need") ~ 
    #               "Core housing need",
    #             str_detect(var, "repairs") ~ 
    #               "Major repairs needed",
    #             TRUE ~ "")
    # 
    # title <-
    #   paste(tenure_title, shelter_title, characteristics_title, 
    #         post_title, sep = " / ") |> 
    #   str_remove_all("(/  )|(/ $)") |> 
    #   str_trim()
    # 
    # # SHORT TITLE
    # post_short <- 
    #   case_when(str_ends(var, "_count$") ~ "",
    #             str_ends(var, "_pct$") ~ "%")
    # 
    # tenure_short <- 
    #   case_when(str_detect(var, "tenant") ~ "Ten.",
    #             str_detect(var, "owner") ~ "Own.",
    #             TRUE ~ "")
    # 
    # shelter_short <- 
    #   case_when(str_detect(var, "more_30_per") ~ 
    #               ">30%.",
    #             str_detect(var, "more_50_per") ~ 
    #               ">50%.",
    #             str_detect(var, "more_80_per") ~ 
    #               ">80%.",
    #             TRUE ~ "")
    # 
    # characteristics_short <- 
    #   case_when(str_detect(var, "kids_3_plus") ~ 
    #               "3+ child.",
    #             str_detect(var, "unsuitable") ~ 
    #               "Unsuit.",
    #             str_detect(var, "core_need") ~ 
    #               "Core need.",
    #             str_detect(var, "repairs") ~ 
    #               "Repairs.",
    #             TRUE ~ "")
    # 
    # short <-
    #   paste(tenure_short, shelter_short, characteristics_short, 
    #         post_short, sep = " ") |> 
    #   str_replace("\\s*", " ") |> 
    #   str_trim()
    # 
    # # EXPLANATION
    # pre_explanation <- 
    #   case_when(str_ends(var, "_count$") ~ "the count of",
    #             str_ends(var, "_pct$") ~ "the percentage of")
    # 
    # tenure_explanation <- 
    #   case_when(str_detect(var, "tenant") ~ "tenant households",
    #             str_detect(var, "owner") ~ "owner households",
    #             TRUE ~ "households")
    # 
    # shelter_explanation <- 
    #   case_when(str_detect(var, "more_30_per") ~ 
    #               "spending more than 30% of their revenue on shelter cost",
    #             str_detect(var, "more_50_per") ~ 
    #               "spending more than 50% of their revenue on shelter cost",
    #             str_detect(var, "more_80_per") ~ 
    #               "spending more than 50% of their revenue on shelter cost",
    #             TRUE ~ "")
    # 
    # characteristics_explanation <- 
    #   case_when(str_detect(var, "kids_3_plus") ~ 
    #               "in a family with 3 or more children",
    #             str_detect(var, "unsuitable") ~ 
    #               "in unsuitable housing",
    #             str_detect(var, "core_need") ~ 
    #               "in core housing need",
    #             str_detect(var, "repairs") ~ 
    #               "in housing with major repairs needed",
    #             TRUE ~ "")
    # 
    # exp <- 
    #   paste(pre_explanation, tenure_explanation,
    #         shelter_explanation,
    #         characteristics_explanation) |> 
    #   str_replace("\\s*", " ") |> 
    #   str_trim()
    
    # ADDED ROW
    out <-
      add_variables(variables,
                    var_code = var,
                    var_title = var,
                    var_short = var,
                    explanation = var,
                    category = NA,
                    theme = "Housing",
                    private = TRUE,
                    dates = "2016",
                    scales = c("CT", "borough", "centraide"),
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, 
                                       var = all_of(paste0(var, "_2016"))),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, 
                                       var = all_of(paste0(var, "_2016"))),
                    source = "Centraide",
                    interpolated = list(c(CT = FALSE,
                                          borough = "census tracts",
                                          centraide = FALSE)))
    
    out[out$var_code == var, ]
    
  }) #|> 
  # mutate(var_short = if_else(var_code == "cent_d_total_total_total_count",
  #                             "Household", var_short),
  #        var_short = if_else(var_code == "cent_d_total_total_total_pct",
  #                            "Household %", var_short))

variables <-
  bind_rows(variables, new_rows)

# Join vulnerable_pop to CT -----------------------------------------------

CT <-
  left_join(CT, cent_d$CT, by = "ID") |>
  relocate(geometry, .after = last_col())


# Join vulnerable_pop to borough ------------------------------------------

borough <-
  left_join(borough, cent_d$borough, by = "ID") |>
  relocate(geometry, .after = last_col())


# Join vulnerable_pop to centraide ----------------------------------------

centraide <-
  left_join(centraide, cent_d$centraide, by = "ID") |>
  relocate(geometry, .after = last_col())


# Clean up ----------------------------------------------------------------

rm(tenure_statuses, add_characteristics, shelter_costs,
   var_list, breaks_q3_active, breaks_q5_active, new_rows,
   cent_d_q3, cent_d_q5)
