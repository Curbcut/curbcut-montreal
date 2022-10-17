#### Centraide POPULATION data setup ###########################################

# This script relies on objects created in dev/census.R


# Load libraries and data -------------------------------------------------

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

rm(table2)


# Prepare variables --------------------------------------------------------

sexes <- list("total" = "total", 
              "female" = "female", 
              "male" = "male")

imm_statuses <- list("total" = "total", 
                     "immigrants" = "immigrants",
                     "non_immigrants" = c("non-immigrants", "non-permanent"))

shelter_costs <- list("total" = "total", 
                      "more_30_per" = c("30-50%", "50%-80%", ">80%"),
                      "more_50_per" = c("50%-80%", ">80%"),
                      "more_80_per" = ">80%")

add_characteristics <- 
  list("total" = "total",
       # Immigration characteristics
       # Appears and disappears depending if immigrant is selected
       "before_2001" = "Before 2001",
       "2001_to_2010" = "2001 to 2010",
       "2011_to_2016" = "2011 to 2016",
       "eco_imm" = "Economic immigrants",
       "sponsored_imm" = "Immigrants sponsored by family",
       "refugees_imm" = "Refugees",
       "other_imm" = "Other immigrants",
       # Visible minority / Indigenous
       "visible_min" = "Visible minority",
       "not_visible_min" = "Does not belong to a visible minority group",
       "aboriginal" = "Aboriginal",
       # Family characteristics
       "lone_parents" = "Lone parents (lone-parent families)",
       "living_alone" = "Persons living alone",
       "low_inc" = "low income after tax")


# Iteration of the retrieval function -------------------------------------

# With progress!
progressr::handlers(progressr::handler_progress(
  format = 
    ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
  width = 60,
  complete = "+"
))

# library(future)
# old_plan <- plan()
# plan(list(tweak(multisession, workers = 2), 
#           tweak(multisession, workers = 3),
#           tweak(multisession, workers = 2),
#           tweak(multisession, workers = 2)))
# 
# with_progress({
#   
#   p <- 
#     progressr::progressor(steps = sum(map_int(imm_statuses, length)) *
#                             sum(map_int(add_characteristics, length)) *
#                             sum(map_int(shelter_costs, length)) *
#                             sum(map_int(sexes, length)) * 
#                             2)
#   
#   cent_p <- 
#     furrr::future_map(set_names(c("CT", "centraide")), function(scale) {
#       furrr::future_map_dfc(names(imm_statuses), function(imm_status_name) {
#         
#         imm_status <- imm_statuses[[imm_status_name]]
#         
#         # Non-immigrant includes also non-permanent resident. Get the multiple
#         # columns, and sum them later.
#         imm_sum_rows <- 
#           furrr::future_map(imm_status, function(imm_stat) {
#             furrr::future_map_dfc(names(add_characteristics), function(add_characteristics_name) {
#               
#               household_status <- add_characteristics[[add_characteristics_name]]
#               
#               map_dfc(names(shelter_costs), function(shelter_cost_name) {
#                 
#                 shelter_cost_f <- shelter_costs[[shelter_cost_name]]
#                 
#                 shelter_cost_sum_rows <- 
#                   map(shelter_cost_f, function(shelter_c) {
#                     map_dfc(names(sexes), function(sex_name) {
#                       
#                       se <- sexes[[sex_name]]
#                       
#                       out <- 
#                         get_vulnerable_pop(sex = se, 
#                                            shelter_cost = shelter_c,
#                                            immigrant_status = imm_stat,
#                                            characteristics = household_status)[[
#                                              scale]][, "var"]
#                       
#                       p()
#                       
#                       names(out) <- paste(imm_status_name,
#                                           add_characteristics_name,
#                                           shelter_cost_name,
#                                           sex_name, 
#                                           sep = "_")
#                       
#                       out
#                       
#                     })
#                   })
#                 
#                 if (length(shelter_cost_sum_rows) > 1) {
#                   shelter_cost_sum_rows <- 
#                     map(shelter_cost_sum_rows, mutate, row_n = row_number()) |> 
#                     reduce(bind_rows) |> 
#                     group_by(row_n) |> 
#                     summarize_all(sum) |> 
#                     select(-row_n)
#                 }
#                 
#                 shelter_cost_sum_rows
#                 
#               })
#             })
#           })
#         
#         # In the case of Non-immigrant including two columns (non-immigrant
#         # and non-permanent resident), sum the two columns.
#         if (length(imm_sum_rows) > 1) {
#           imm_sum_rows <- 
#             map(imm_sum_rows, mutate, row_n = row_number()) |> 
#             reduce(bind_rows) |> 
#             group_by(row_n) |> 
#             summarize_all(sum) |> 
#             select(-row_n)
#         }
#         
#         imm_sum_rows
#         
#       })
#       
#     })
# })
# 
# plan(old_plan)
# 
# # Filter out impossible combinations
# cent_p <- 
# map(cent_p, function(df) {
#   df[, !names(df) %in% names(df)[{
#     str_detect(names(df), paste0("before_2001|2001_to_2010|2011_to_2016|",
#                                  "eco_imm|sponsored_imm|refugees_imm|",
#                                  "refugees_imm|other_imm")) &
#       str_detect(names(df), "^non_immigrants_")}]]
# })
# 
# cent_p <- 
#   imap(cent_p, function(df, scale) {
#     bind_cols(get_vulnerable_pop()[[scale]][, "ID"], df) |>
#       rename_with(~paste0("cent_p_", .x, "_count_2016"), 
#                   total_total_total_total:last_col())
#   })
# 
# qsave(cent_p, file = "dev/data/modules_raw_data/cent_p.qs")
cent_p <- qread("dev/data/modules_raw_data/cent_p.qs")

# Filter and interpolate --------------------------------------------------

all_cent_p <- 
  interpolate_scales(data = cent_p$CT, 
                     base_scale = "CT", 
                     all_tables = all_tables,
                     weight_by = "population",
                     crs = 32618)

# Use Centraide coming from the real data, no interpolation needed
all_cent_p$centraide_centraide <- 
  left_join(cent_p$centraide |> 
              rename(name = ID),
            centraide_centraide |> 
              st_drop_geometry() |> 
              select(ID, name),
            by = "name") |> 
  relocate(ID, .before = name) |> 
  select(-name)


# Count, percentage and density ------------------------------------------

# Percentage
all_cent_p <- 
  imap(all_cent_p, function(scale, df) {
    
    # Switch centraide name to ID
    if (df == "centraide") 
      scale <- left_join(select(st_drop_geometry(centraide), name, ID),
                         scale, by = "name") |> select(-name)
    # Add _pct
    with_pct <- 
      scale |> 
      mutate(across(starts_with("cent_p"), ~{.x / 
          cent_p_total_total_total_total_count_2016}))
    
    names(with_pct)[str_starts(names(with_pct), "cent_p")] <- 
      names(with_pct)[str_starts(names(with_pct), "cent_p")] |> 
      str_replace("count_2016$", "pct_2016")
    
    # Combine count and pct
    left_join(scale, with_pct, by = "ID")
    
  })

# Sqkm
all_cent_p <-
  imap(all_cent_p, function(scale, df) {
    
    # Add geometry to calculate area
    scale_geom <- 
      left_join(select(get(df), ID), scale, by = "ID")
    
    # Add _pct
    with_sqkm <- 
      scale_geom |> 
      select(ID, ends_with("_count_2016")) |> 
      mutate(area_km = units::drop_units(st_area(geometry)) / 1e+6) |> 
      mutate(across(ends_with("_count_2016"), ~{.x / area_km})) |> 
      select(-area_km) |> 
      st_drop_geometry()
    
    names(with_sqkm)[str_starts(names(with_sqkm), "cent_p")] <- 
      names(with_sqkm)[str_starts(names(with_sqkm), "cent_p")] |> 
      str_replace("count_2016$", "sqkm_2016")
    
    # Combine count and pct
    left_join(scale, with_sqkm, by = "ID")
    
  })


# Calculate breaks --------------------------------------------------------

all_cent_p <- calculate_breaks(all_cent_p)


# Assign to existing geographies ------------------------------------------

assign_tables(module_tables = all_cent_p)


# Add to variables table --------------------------------------------------

var_list <-
  all_cent_p$tables_list[[1]] |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(all_cent_p$tables_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  imap_dfr(all_cent_p$tables_q5, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:5, 
                                 .before = 1)})

new_rows <-
  map_dfr(var_list, function(var) {
    
    var <- str_remove(var, "_\\d{4}$")
    
    # TITLE
    post_title <-
      case_when(str_ends(var, "_count$") ~ "",
                str_ends(var, "_pct$") ~ " (%)",
                str_ends(var, "_sqkm$") ~ " (per sq. km)")
    
    sex_imm_title <- 
      case_when(str_detect(var, "cent_p_immigrants") && str_detect(var, "female") ~ 
                  "Female immigrants",
                str_detect(var, "cent_p_immigrants") && str_detect(var, "male") ~ 
                  "Male immigrants",
                str_detect(var, "non_immigrants") && str_detect(var, "female") ~ 
                  "Non-immigrant females",
                str_detect(var, "non_immigrants") && str_detect(var, "male") ~ 
                  "Non-immigrant males",
                !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "female") ~ 
                  "Female population",
                !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "male") ~ 
                  "Male population",
                str_detect(var, "cent_p_immigrants") && !str_detect(var, "male|female") ~ 
                  "Immigrant population",
                str_detect(var, "non_immigrants") && !str_detect(var, "male|female") ~ 
                  "Non-immigrant population",
                TRUE ~ "Population")
    
    shelter_title <-
      case_when(str_detect(var, "more_30_per") ~
                  " spending >30% of income on shelter",
                str_detect(var, "more_50_per") ~
                  " spending >50% of income on shelter",
                str_detect(var, "more_80_per") ~
                  " spending >80% of income on shelter",
                TRUE ~ "")
    
    characteristics_title <-
      case_when(str_detect(var, "before_2001") ~
                  " who immigrated before 2001",
                str_detect(var, "2001_to_2010") ~
                  " who immigrated between 2001 and 2010",
                str_detect(var, "2011_to_2016") ~
                  " who immigrated between 2011 and 2016",
                str_detect(var, "eco_imm") ~
                  " who are economic immigrants",
                str_detect(var, "sponsored_imm") ~
                  " who are immigrants sponsored by family",
                str_detect(var, "refugees_imm") ~
                  " who are refugees",
                str_detect(var, "other_imm") ~
                  " who are other immigrants",
                str_detect(var, "visible_min") ~
                  " who are members of a visible minority group",
                str_detect(var, "not_visible_min") ~
                  " who are not members of a visible minority group",
                str_detect(var, "aboriginal") ~
                  " who are aboriginals",
                str_detect(var, "lone_parents") ~
                  " who are lone parents",
                str_detect(var, "living_alone") ~
                  " who are living alone",
                str_detect(var, "low_inc") ~
                  "Low income ",
                TRUE ~ "")
    
    
    title <- if (!str_detect(var, "low_inc")) {
      paste0(sex_imm_title, shelter_title, characteristics_title, 
                    post_title)
    } else {
      paste0(characteristics_title, tolower(sex_imm_title), shelter_title, 
             post_title)
    }
    
    # EXPLANATION
    pre_exp <-
      case_when(str_ends(var, "_count$") ~ "the number of",
                str_ends(var, "_pct$") ~ "the percentage of",
                str_ends(var, "_sqkm$") ~ "the number of")
    
    post_exp <- 
      case_when(str_ends(var, "_sqkm$") ~ " per square kilometer",
                TRUE ~ "")
    
    sex_imm_exp <- 
      case_when(str_detect(var, "cent_p_immigrants") && str_detect(var, "female") ~ 
                  " female immigrants",
                str_detect(var, "cent_p_immigrants") && str_detect(var, "male") ~ 
                  " male immigrants",
                str_detect(var, "non_immigrants") && str_detect(var, "female") ~ 
                  " non-immigrant females",
                str_detect(var, "non_immigrants") && str_detect(var, "male") ~ 
                  " non-immigrant males",
                !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "female") ~ 
                  " females",
                !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "male") ~ 
                  " males",
                str_detect(var, "cent_p_immigrants") && !str_detect(var, "male|female") ~ 
                  " immigrants",
                str_detect(var, "non_immigrants") && !str_detect(var, "male|female") ~ 
                  " non-immigrants",
                TRUE ~ " individuals")
    
    shelter_exp <-
      case_when(str_detect(var, "more_30_per") ~
                  " spending >30% of income on shelter",
                str_detect(var, "more_50_per") ~
                  " spending >50% of income on shelter",
                str_detect(var, "more_80_per") ~
                  " spending >80% of income on shelter",
                TRUE ~ "")
    
    characteristics_exp <-
      case_when(str_detect(var, "before_2001") ~
                  " who immigrated before 2001",
                str_detect(var, "2001_to_2010") ~
                  " who immigrated between 2001 and 2010",
                str_detect(var, "2011_to_2016") ~
                  " who immigrated between 2011 and 2016",
                str_detect(var, "eco_imm") ~
                  " who are economic immigrants",
                str_detect(var, "sponsored_imm") ~
                  " who are immigrants sponsored by family",
                str_detect(var, "refugees_imm") ~
                  " who are refugees",
                str_detect(var, "other_imm") ~
                  " who are other immigrants",
                str_detect(var, "visible_min") ~
                  " who are members of a visible minority group",
                str_detect(var, "not_visible_min") ~
                  " who are not members of a visible minority group",
                str_detect(var, "aboriginal") ~
                  " who are aboriginals",
                str_detect(var, "lone_parents") ~
                  " who are lone parents",
                str_detect(var, "living_alone") ~
                  " who are living alone",
                str_detect(var, "low_inc") ~
                  "low income",
                TRUE ~ "")
    
    exp <- if (!str_detect(var, "low_inc")) {
      paste0(pre_exp, sex_imm_exp, shelter_exp, characteristics_exp,
                  post_exp)
    } else {
      paste0(pre_exp, characteristics_exp, sex_imm_exp, shelter_exp,
             post_exp)      
    }
    
    # SHORT
    post_short <-
      case_when(str_ends(var, "_count$") ~ "",
                str_ends(var, "_pct$") ~ " (%)",
                str_ends(var, "_sqkm$") ~ " (sqkm)")
    
    sex_imm_short <- 
      case_when(str_detect(var, "cent_p_immigrants") && str_detect(var, "female") ~ 
                  "F. Imm.",
                str_detect(var, "cent_p_immigrants") && str_detect(var, "male") ~ 
                  "M. Imm.",
                str_detect(var, "non_immigrants") && str_detect(var, "female") ~ 
                  "F. N-Imm.",
                str_detect(var, "non_immigrants") && str_detect(var, "male") ~ 
                  "M. N-Imm.",
                !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "female") ~ 
                  "F.",
                !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "male") ~ 
                  "M.",
                str_detect(var, "cent_p_immigrants") && !str_detect(var, "male|female") ~ 
                  "Imm.",
                str_detect(var, "non_immigrants") && !str_detect(var, "male|female") ~ 
                  "N-Imm.",
                TRUE ~ "Pop.")
    
    shelter_short <-
      case_when(str_detect(var, "more_30_per") ~
                  " >30%",
                str_detect(var, "more_50_per") ~
                  " >50%",
                str_detect(var, "more_80_per") ~
                  " >80%",
                TRUE ~ "")
    
    characteristics_short <-
      case_when(str_detect(var, "before_2001") ~
                  " <2001",
                str_detect(var, "2001_to_2010") ~
                  " >2001<2010",
                str_detect(var, "2011_to_2016") ~
                  " >2011<2016",
                str_detect(var, "eco_imm") ~
                  " Eco.",
                str_detect(var, "sponsored_imm") ~
                  " Spon.",
                str_detect(var, "refugees_imm") ~
                  " Ref.",
                str_detect(var, "other_imm") ~
                  " Oth.",
                str_detect(var, "visible_min") ~
                  " Vis.",
                str_detect(var, "not_visible_min") ~
                  " Non-V.",
                str_detect(var, "aboriginal") ~
                  " Abo.",
                str_detect(var, "lone_parents") ~
                  " Lone-P.",
                str_detect(var, "living_alone") ~
                  " Living A.",
                str_detect(var, "low_inc") ~
                  " Low inc.",
                TRUE ~ "")
    
    
    short <- paste0(sex_imm_short, shelter_short, characteristics_short, 
                    post_short)
    
    interpolated_key <- 
      map_chr(set_names(names(all_cent_p$tables_list)), function(x) {
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
                    scales = names(all_cent_p$tables_list),
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, 
                                       var = all_of(paste0(var, "_2016"))),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, 
                                       var = all_of(paste0(var, "_2016"))),
                    source = "Centraide of Greater Montreal",
                    interpolated = list(interpolated_key))
    
    out[out$var_code == var, ]
    
  })

variables <-
  bind_rows(variables, new_rows)


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(id = "demographics",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>The census data (2016) in this module comes from ",
                       "custom tabulations ordered by Centraide of Greater ",
                       "Montreal to Statistics Canada.</p>"))

# Clean up ----------------------------------------------------------------

rm(imm_statuses, add_characteristics, shelter_costs, sexes, cent_p, all_cent_p,
   var_list, breaks_q3_active, breaks_q5_active, new_rows, table1)
