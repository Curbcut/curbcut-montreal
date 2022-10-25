#### Centraide DWELLINGS type data setup #######################################

# This script relies on objects created in dev/census.R


# # Load libraries and data -------------------------------------------------
# 
# library(progressr)
# 
# source("dev/other/crosstabs_fun.R")
# 
# # table1 <-
# #   read.csv("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tableau1.csv",
# #            header = FALSE) |> as_tibble()
# #
# # table2 <-
# #   read.csv("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tableau2_amend.csv",
# #            header = FALSE) |> as_tibble()
# #
# # qsavem(table1, table2,
# #        file = "dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")
# 
# qload("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")
# 
# rm(table1)
# 
# 
# # Prepare variables --------------------------------------------------------
# 
# # Tenure status
# tenure_statuses <- list("total" = "total",
#                         "tenant" = "tenant",
#                         "owner" = "owner")
# 
# # Shelter cost burden
# shelter_costs <- list("total" = "total",
#                       "more_30_per" = c("30-50%", "50%-80%", ">80%"),
#                       "more_50_per" = c("50%-80%", ">80%"),
#                       "more_80_per" = ">80%")
# 
# add_characteristics <-
#   # Dwelling characteristics
#   list("total" = "total",
#        "single_detached" = "single-detached house",
#        "semi_detached" = "semi-detached house",
#        "row_house" = "row house",
#        "in_duplex" = "apartment or flat in a duplex",
#        "in_5plus_storeys" = "apartment in a building that has five or more storeys",
#        "in_less5_storeys" = "apartment in a building that has fewer than five storeys",
#        "other_single_attached" = "other single-attached house",
#        "mobile_homes" = "movable dwelling",
#        # OR
#        # Family characteristics
#        "kids_3_plus" = "Families with 3 or more children",
#        "low_inc" = "low income after tax",
#        "unsuitable" = "unsuitable",
#        "repairs" = "major repairs needed")
# 
# 
# # Iteration of the retrieval function -------------------------------------
# 
# # With progress!
# progressr::handlers(progressr::handler_progress(
#   format =
#     ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
#   width = 60,
#   complete = "+"
# ))
# 
# with_progress({
# 
#   p <-
#     progressr::progressor(sum(map_int(tenure_statuses, length)) *
#                             sum(map_int(shelter_costs, length)) *
#                             sum(map_int(add_characteristics, length)) *
#                             2)
# 
#   cent_d <-
#     map(set_names(c("CT", "centraide")), function(scale) {
#       map_dfc(names(tenure_statuses), function(tenure_status_name) {
# 
#         tenure_status <- tenure_statuses[[tenure_status_name]]
# 
#         map_dfc(names(shelter_costs), function(shelter_cost_name) {
# 
#           shelter_cost_f <- shelter_costs[[shelter_cost_name]]
# 
#           shelter_cost_sum_rows <-
#             map(shelter_cost_f, function(shelter_c) {
#               map_dfc(names(add_characteristics), function(characteristic_name) {
# 
#                 add_characteristics <- add_characteristics[[characteristic_name]]
# 
#                 out <-
#                   get_housing_char(tenure = tenure_status,
#                                               shelter_cost = shelter_c,
#                                               characteristics = add_characteristics)[[
#                                                 scale]][, "var"]
# 
#                 p()
# 
#                 names(out) <- paste(tenure_status_name,
#                                     shelter_cost_name,
#                                     characteristic_name,
#                                     sep = "_")
# 
#                 out
# 
#               })
#             })
# 
#           if (length(shelter_cost_sum_rows) > 1) {
#             shelter_cost_sum_rows <-
#               map(shelter_cost_sum_rows, mutate, row_n = row_number()) |>
#               reduce(bind_rows) |>
#               group_by(row_n) |>
#               summarize_all(sum) |>
#               select(-row_n)
#           }
# 
#           shelter_cost_sum_rows
# 
#         })
#       })
#     })
# })
# 
# cent_d <-
#   imap(cent_d, function(df, scale) {
#     bind_cols(get_housing_char()[[scale]][, "ID"], df) |>
#       rename_with(~paste0("cent_d_", .x, "_count_2016"),
#                   total_total_total:last_col())
#   })
# qsave(cent_d, file = "dev/data/modules_raw_data/cent_d.qs")

cent_d <- qread("dev/data/modules_raw_data/cent_d.qs")


# Filter and interpolate --------------------------------------------------

all_cent_d <- 
  interpolate_scales(data = cent_d$CT, 
                     base_scale = "CT", 
                     all_tables = all_tables,
                     weight_by = "households",
                     crs = 32618)

# Use Centraide coming from the real data, no interpolation needed
all_cent_d$centraide_centraide <- 
  left_join(cent_d$centraide |> 
              rename(name = ID),
            centraide_centraide |> 
              st_drop_geometry() |> 
              select(ID, name),
            by = "name") |> 
  relocate(ID, .before = name) |> 
  select(-name)


# Count and percentage ----------------------------------------------------

all_cent_d <- 
  imap(all_cent_d, function(scale, df) {
    
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

all_cent_d <- calculate_breaks(all_cent_d)


# Assign to existing geographies ------------------------------------------

assign_tables(module_tables = all_cent_d)


# Add to variables table --------------------------------------------------

var_list <-
  all_cent_d$tables_list[[1]] |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(all_cent_d$tables_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  imap_dfr(all_cent_d$tables_q5, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:5, 
                                 .before = 1)})

new_rows <-
  map_dfr(var_list, function(var) {

    var <- str_remove(var, "_\\d{4}$")
    
    # TITLE
    post_title <-
      case_when(str_ends(var, "_count$") ~ "",
                str_ends(var, "_pct$") ~ " (%)")

    tenure_title <-
      case_when(str_detect(var, "tenant") ~ "Tenant households",
                str_detect(var, "owner") ~ "Owner households",
                TRUE ~ "Households")

    shelter_title <-
      case_when(str_detect(var, "more_30_per") ~
                  " spending >30% of income on shelter",
                str_detect(var, "more_50_per") ~
                  " spending >50% of income on shelter",
                str_detect(var, "more_80_per") ~
                  " spending >80% of income on shelter",
                TRUE ~ "")
    
    characteristics_title <-
      case_when(str_detect(var, "kids_3_plus") ~
                  " with a family of 3+ children",
                str_detect(var, "unsuitable") ~
                  " in unsuitable housing",
                str_detect(var, "repairs") ~
                  " in housing with major repairs needed",
                str_detect(var, "low_inc") ~
                  "Low income ",
                str_detect(var, "single_detached") ~ 
                  " in single-detached houses",
                str_detect(var, "semi_detached") ~ 
                  " in semi-detached houses",
                str_detect(var, "row_house") ~ 
                  " in row houses",
                str_detect(var, "in_duplex") ~ 
                  " in apartments or flats in a duplex",
                str_detect(var, "in_5plus_storeys") ~ 
                  " in apartments in buildings that has five or more storeys",
                str_detect(var, "in_less5_storeys") ~ 
                  " in apartments in buildings that has fewer than five storeys",
                str_detect(var, "other_single_attached") ~ 
                  " in other single-attached houses",
                str_detect(var, "mobile_homes") ~ 
                  " in mobile homes and other movable dwellings",
                TRUE ~ "")
    
    title <- if (!str_detect(var, "low_inc")) {
      paste0(tenure_title, shelter_title, characteristics_title, 
                    post_title)
    } else {
      paste0(characteristics_title, tolower(tenure_title), shelter_title, 
             post_title)
    }

    # SHORT TITLE
    post_short <-
      case_when(str_ends(var, "_count$") ~ "",
                str_ends(var, "_pct$") ~ " (%)")
    
    tenure_short <-
      case_when(str_detect(var, "tenant") ~ "Ten.",
                str_detect(var, "owner") ~ "Own.",
                TRUE ~ "Hou.")
    
    shelter_short <-
      case_when(str_detect(var, "more_30_per") ~
                  " >30%",
                str_detect(var, "more_50_per") ~
                  " >50%",
                str_detect(var, "more_80_per") ~
                  " >80%",
                TRUE ~ "")
    
    characteristics_short <-
      case_when(str_detect(var, "kids_3_plus") ~
                  " 3+ child.",
                str_detect(var, "unsuitable") ~
                  " Uns.",
                str_detect(var, "repairs") ~
                  " Rep.",
                str_detect(var, "low_inc") ~
                  " Low inc.",
                str_detect(var, "single_detached") ~ 
                  " Detached",
                str_detect(var, "semi_detached") ~ 
                  " Semi",
                str_detect(var, "row_house") ~ 
                  " Row",
                str_detect(var, "in_duplex") ~ 
                  " Duplex",
                str_detect(var, "in_5plus_storeys") ~ 
                  " 5+ storeys",
                str_detect(var, "in_less5_storeys") ~ 
                  " -5 storeys",
                str_detect(var, "other_single_attached") ~ 
                  " Othr att.",
                str_detect(var, "mobile_homes") ~ 
                  " Movable",
                TRUE ~ "")
    
    short <-
      paste0(tenure_short, shelter_short, characteristics_short,
             post_short)
    
    # EXPLANATION
    pre_explanation <-
      case_when(str_ends(var, "_count$") ~ "the count of",
                str_ends(var, "_pct$") ~ "the percentage of")
    
    tenure_explanation <-
      case_when(str_detect(var, "tenant") ~ " tenant households",
                str_detect(var, "owner") ~ " owner households",
                TRUE ~ " households")
    
    shelter_explanation <-
      case_when(str_detect(var, "more_30_per") ~
                  " spending more than 30% of their income on shelter cost",
                str_detect(var, "more_50_per") ~
                  " spending more than 50% of their income on shelter cost",
                str_detect(var, "more_80_per") ~
                  " spending more than 80% of their income on shelter cost",
                TRUE ~ "")
    
    characteristics_explanation <-
      case_when(str_detect(var, "kids_3_plus") ~
                  " in families with 3 or more children",
                str_detect(var, "unsuitable") ~
                  " in unsuitable housing",
                str_detect(var, "repairs") ~
                  " in housing with major repairs needed",
                str_detect(var, "low_inc") ~
                  " low income",
                str_detect(var, "single_detached") ~ 
                  " in single-detached houses",
                str_detect(var, "semi_detached") ~ 
                  " in semi-detached houses",
                str_detect(var, "row_house") ~ 
                  " in row houses",
                str_detect(var, "in_duplex") ~ 
                  " in apartments or flats in a duplex",
                str_detect(var, "in_5plus_storeys") ~ 
                  " in apartments in buildings that has five or more storeys",
                str_detect(var, "in_less5_storeys") ~ 
                  " in apartments in buildings that has fewer than five storeys",
                str_detect(var, "other_single_attached") ~ 
                  " in other single-attached houses",
                str_detect(var, "mobile_homes") ~ 
                  " in mobile homes and other movable dwellings",
                TRUE ~ "")
    
    exp <- if (!str_detect(var, "low_inc")) {
      paste0(pre_explanation, tenure_explanation, shelter_explanation,
             characteristics_explanation)
    } else {
      paste0(pre_explanation, characteristics_explanation, tenure_explanation, 
             shelter_explanation)
    }
    
    interpolated_key <- 
      map_chr(set_names(names(all_cent_d$tables_list)), function(x) {
        if (str_detect(x, "_CT$")) return(FALSE)
        return("census tracts")
      })
    
    # ADDED ROW
    out <-
      add_variables(variables,
                    var_code = var,
                    var_title = title,
                    var_short = short,
                    explanation = exp,
                    category = NA,
                    theme = "Housing",
                    private = FALSE,
                    dates = "2016",
                    scales = names(all_cent_d$tables_list),
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, 
                                       var = all_of(paste0(var, "_2016"))),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, 
                                       var = all_of(paste0(var, "_2016"))),
                    source = "Centraide of Greater Montreal",
                    interpolated = list(interpolated_key))
    
    out[out$var_code == var, ]
    
  }) |>
  mutate(var_short = if_else(var_code == "cent_d_total_total_total_count",
                              "Households", var_short),
         var_short = if_else(var_code == "cent_d_total_total_total_pct",
                             "Households (%)", var_short))

variables <-
  bind_rows(variables, new_rows)


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(id = "afford",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>The census data (2016) in this module comes from custom tabulations ",
                       "ordered by Centraide of Greater Montreal to Statistics ",
                       "Canada.</p>")) |> 
  add_modules(id = "tenure",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>The census data (2016) in this module comes from custom tabulations ",
                       "ordered by Centraide of Greater Montreal to Statistics ",
                       "Canada.</p>")) |> 
  add_modules(id = "dw_types",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>The census data (2016) in this module comes from custom tabulations ",
                       "ordered by Centraide of Greater Montreal to Statistics ",
                       "Canada.</p>"))


# Clean up ----------------------------------------------------------------

rm(tenure_statuses, add_characteristics, shelter_costs, cent_d, all_cent_d,
   var_list, breaks_q3_active, breaks_q5_active, new_rows, table2)
