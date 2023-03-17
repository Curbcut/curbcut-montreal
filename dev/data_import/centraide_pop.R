## BUILD AND APPEND CENTRAIDE_POP DATA #########################################

build_and_append_centraide_pop <- function(scales_variables_modules, 
                                           CT_table, region_CT_IDs, crs) {

  # # Read and prepare data ---------------------------------------------------
  # 
  # table1 <-
  #   qs::qread("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/table1.qs")
  # 
  # source("dev/other/crosstabs_fun.R")
  # sexes <- list("total" = "total",
  #               "female" = "female",
  #               "male" = "male")
  # 
  # imm_statuses <- list("total" = "total",
  #                      "immigrants" = "immigrants",
  #                      "non_immigrants" = c("non-immigrants", "non-permanent"))
  # 
  # shelter_costs <- list("total" = "total",
  #                       "more_30_per" = c("30-50%", "50%-80%", ">80%"),
  #                       "more_50_per" = c("50%-80%", ">80%"),
  #                       "more_80_per" = ">80%")
  # 
  # add_characteristics <-
  #   list("total" = "total",
  #        # Immigration characteristics
  #        # Appears and disappears depending if immigrant is selected
  #        "before_2001" = "Before 2001",
  #        "2001_to_2010" = "2001 to 2010",
  #        "2011_to_2016" = "2011 to 2016",
  #        "eco_imm" = "Economic immigrants",
  #        "sponsored_imm" = "Immigrants sponsored by family",
  #        "refugees_imm" = "Refugees",
  #        "other_imm" = "Other immigrants",
  #        # Visible minority / Indigenous
  #        "visible_min" = "Visible minority",
  #        "not_visible_min" = "Does not belong to a visible minority group",
  #        "aboriginal" = "Aboriginal",
  #        # Family characteristics
  #        "lone_parents" = "Lone parents (lone-parent families)",
  #        "living_alone" = "Persons living alone",
  #        "low_inc" = "low income after tax")
  # 
  # 
  # # Iteration of the retrieval function -------------------------------------
  # 
  # old_plan <- future::plan()
  # future::plan(list(future::tweak(future::multisession, workers = 2),
  #                   future::tweak(future::multisession, workers = 3),
  #                   future::tweak(future::multisession, workers = 3),
  #                   future::tweak(future::multisession, workers = 2)))
  # 
  # progressr::with_progress({
  #   pb <-
  #     progressr::progressor(steps = sum(purrr::map_int(imm_statuses, length)) *
  #                             sum(purrr::map_int(add_characteristics, length)) *
  #                             sum(purrr::map_int(shelter_costs, length)) *
  #                             sum(purrr::map_int(sexes, length)) *
  #                             2)
  # 
  #   data <-
  #     furrr::future_map(purrr::set_names(c("CT", "centraide")), function(scale) {
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
  #               purrr::map_dfc(names(shelter_costs), function(shelter_cost_name) {
  # 
  #                 shelter_cost_f <- shelter_costs[[shelter_cost_name]]
  # 
  #                 shelter_cost_sum_rows <-
  #                   purrr::map(shelter_cost_f, function(shelter_c) {
  #                     purrr::map_dfc(names(sexes), function(sex_name) {
  # 
  #                       se <- sexes[[sex_name]]
  # 
  #                       out <-
  #                         get_vulnerable_pop(sex = se,
  #                                            shelter_cost = shelter_c,
  #                                            immigrant_status = imm_stat,
  #                                            characteristics = household_status,
  #                                            region_CT_IDs = region_CT_IDs)[[
  #                                              scale]][, "var"]
  # 
  #                       pb()
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
  #                     purrr::map(shelter_cost_sum_rows,
  #                                dplyr::mutate, row_n = dplyr::row_number()) |>
  #                     purrr::reduce(dplyr::bind_rows) |>
  #                     dplyr::group_by(row_n) |>
  #                     dplyr::summarize_all(sum) |>
  #                     dplyr::select(-row_n)
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
  #             purrr::map(imm_sum_rows, dplyr::mutate, row_n = dplyr::row_number()) |>
  #             purrr::reduce(dplyr::bind_rows) |>
  #             dplyr::group_by(row_n) |>
  #             dplyr::summarize_all(sum) |>
  #             dplyr::select(-row_n)
  #         }
  # 
  #         imm_sum_rows
  # 
  #       })
  # 
  #     })
  # })
  # 
  # future::plan(old_plan)
  # # Filter out impossible combinations
  # data <-
  #   purrr::map(data, function(df) {
  #     df[, !names(df) %in% names(df)[{
  #       stringr::str_detect(names(df),
  #                           paste0("before_2001|2001_to_2010|2011_to_2016|",
  #                                  "eco_imm|sponsored_imm|refugees_imm|",
  #                                  "refugees_imm|other_imm")) &
  #         stringr::str_detect(names(df), "^non_immigrants_")}]]
  #   })
  # 
  # data <-
  #   purrr::imap(data, function(df, scale) {
  #     dplyr::bind_cols(get_vulnerable_pop(
  #       region_CT_IDs = region_CT_IDs)[[scale]][, "ID"], df) |>
  #       dplyr::rename_with(~paste0("cent_p_", .x, "count_2016"),
  #                          total_total_total_total:last_col())
  #   })
  # 
  # 
  #   qs::qsave(data, file = "dev/data/built/centraide_pop.qs")
  data <- qs::qread("dev/data/built/centraide_pop.qs")
  
  
  # Normalize (percentage) --------------------------------------------------
  
  data <- 
    lapply(data, \(df) {
      # Add _pct
      with_pct <- 
        df |> 
        dplyr::mutate(dplyr::across(dplyr::starts_with("cent_p"), ~{.x / 
            cent_p_total_total_total_total_count_2016}))
      
      names(with_pct)[stringr::str_starts(names(with_pct), "cent_p")] <- 
        names(with_pct)[stringr::str_starts(names(with_pct), "cent_p")] |> 
        stringr::str_replace("count_2016$", "pct_2016")
      
      # Combine count and pct
      merge(df, with_pct)
    })
  
  
  # Normalize (sqkm) --------------------------------------------------------
  
  data$CT <- merge(data$CT, CT_table["ID"])
  names(data$centraide)[1] <- "name"
  data$centraide <- merge(data$centraide, 
                          scales_variables_modules$scales$centraide$centraide["name"],
                          by = "name")
  names(data$centraide)[1] <- "ID"
  data <-
    lapply(data, \(df) {
      
      area <- sf::st_area(df)
      units(area) <- "km^2"
      df$area <- units::drop_units(area)
      
      with_sqkm <-
        df |>
        dplyr::mutate(dplyr::across(dplyr::starts_with("cent_p"), ~{.x /
            area}))
      
      names(with_sqkm)[str_starts(names(with_sqkm), "cent_p")] <-
        names(with_sqkm)[str_starts(names(with_sqkm), "cent_p")] |>
        str_replace("count_2016$", "sqkm_2016")
      
      with_sqkm <- with_sqkm[c(1, which(grepl("sqkm_2016", names(with_sqkm))))]
      
      # Combine count and pct
      merge(sf::st_drop_geometry(df), 
            sf::st_drop_geometry(with_sqkm),
            by = "ID")
    })
  
  # Get list of data variables ----------------------------------------------
  
  # Build a character vector of all data variables that will be added to all
  # scales.
  add_vars <- names(data$CT)[!grepl("ID$", names(data$CT)) &
                               grepl("count_2016$", names(data$CT))]
  avg_vars <- names(data$CT)[!grepl("ID$", names(data$CT)) &
                               grepl("pct_2016$|sqkm_2016", names(data$CT))]
  
  
  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  names(data$CT)[1] <- "CT_ID"
  data_interpolated <-
    interpolate_from_census_geo(
      data = data$CT,
      base_scale = "CT",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      crs = crs,
      average_vars = avg_vars,
      additive_vars = add_vars
    )
  
  # Merge the data that is already for Centraide
  names(data$centraide)[1] <- "name"
  data_interpolated$scales$centraide$centraide <- 
    merge(scales_variables_modules$scales$centraide$centraide,
          data$centraide, by = "name")
  
  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$df == "centraide_centraide"
  ] <- FALSE
  

  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = c(add_vars, avg_vars)
    )


  # Variables table ---------------------------------------------------------
  
  var_in_place_explorer <- 
    c("cent_p_immigrants_total_more_30_per_total_pct_2016",
      "cent_p_total_lone_parents_more_30_per_total_pct_2016",
      "cent_p_total_visible_min_more_30_per_total_pct_2016")
  
  progressr::with_progress({
  pb <- progressr::progressor(steps = length( c(add_vars, avg_vars)))
  new_rows <-
    lapply(c(add_vars, avg_vars), function(var) {
      
      var <- str_remove(var, "_\\d{4}$")
      
      # TITLE
      post_title <-
        case_when(str_ends(var, "_count$") ~ "",
                  str_ends(var, "_pct$") ~ " (%)",
                  str_ends(var, "_sqkm$") ~ " (per sq. km)")
      
      sex_imm_title <- 
        case_when(str_detect(var, "cent_p_immigrants") && str_detect(var, "female") ~ 
                    "Immigrant women",
                  str_detect(var, "cent_p_immigrants") && str_detect(var, "male") ~ 
                    "Immigrant men",
                  str_detect(var, "non_immigrants") && str_detect(var, "female") ~ 
                    "Non-immigrant women",
                  str_detect(var, "non_immigrants") && str_detect(var, "male") ~ 
                    "Non-immigrant men",
                  !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "female") ~ 
                    "Women",
                  !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "male") ~ 
                    "Men",
                  str_detect(var, "cent_p_immigrants") && !str_detect(var, "male|female") ~ 
                    "Immigrants",
                  str_detect(var, "non_immigrants") && !str_detect(var, "male|female") ~ 
                    "Non-immigrants",
                  TRUE ~ "People")
      
      shelter_title <-
        case_when(str_detect(var, "more_30_per") ~
                    " who are spending >30% of income on shelter",
                  str_detect(var, "more_50_per") ~
                    " who are spending >50% of income on shelter",
                  str_detect(var, "more_80_per") ~
                    " who are spending >80% of income on shelter",
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
                    " who are classified as 'other immigrants'",
                  str_detect(var, "visible_min") ~
                    " who are members of a visible minority group",
                  str_detect(var, "not_visible_min") ~
                    " who are not members of a visible minority group",
                  str_detect(var, "aboriginal") ~
                    " who are aboriginals",
                  str_detect(var, "lone_parents") ~
                    " lone parents",
                  str_detect(var, "living_alone") ~
                    " who are living alone",
                  str_detect(var, "low_inc") ~
                    "Low income ",
                  TRUE ~ "")
      
      
      title <- if (str_detect(var, "low_inc")) {
        paste0(characteristics_title, tolower(sex_imm_title), shelter_title, 
               post_title)
      } else if (str_detect(var, "lone_parents")) {
        paste0(gsub("s$", "", sex_imm_title), characteristics_title, shelter_title, 
               post_title)
      } else {
        paste0(sex_imm_title, shelter_title, 
               characteristics_title, 
               if (characteristics_title == "") post_title else gsub(" who ", " and ", post_title))
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
                    " immigrant women",
                  str_detect(var, "cent_p_immigrants") && str_detect(var, "male") ~ 
                    " immigrant men",
                  str_detect(var, "non_immigrants") && str_detect(var, "female") ~ 
                    " non-immigrant women",
                  str_detect(var, "non_immigrants") && str_detect(var, "male") ~ 
                    " non-immigrant men",
                  !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "female") ~ 
                    " women",
                  !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "male") ~ 
                    " men",
                  str_detect(var, "cent_p_immigrants") && !str_detect(var, "male|female") ~ 
                    " immigrants",
                  str_detect(var, "non_immigrants") && !str_detect(var, "male|female") ~ 
                    " non-immigrants",
                  TRUE ~ " people")
      
      shelter_exp <-
        case_when(str_detect(var, "more_30_per") ~
                    " who are spending >30% of income on shelter",
                  str_detect(var, "more_50_per") ~
                    " who are spending >50% of income on shelter",
                  str_detect(var, "more_80_per") ~
                    " who are spending >80% of income on shelter",
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
                    " lone parents",
                  str_detect(var, "living_alone") ~
                    " who are living alone",
                  str_detect(var, "low_inc") ~
                    " low income",
                  TRUE ~ "")
      
      exp <- if (str_detect(var, "lone_parents")) {
        paste0(pre_exp, gsub("s$", "", sex_imm_exp), characteristics_exp, shelter_exp,
               post_exp)
      } else if (str_detect(var, "low_inc")) {
        paste0(pre_exp, characteristics_exp, sex_imm_exp, shelter_exp,
               post_exp)
      } else {
        paste0(pre_exp, sex_imm_exp, characteristics_exp, 
               if (characteristics_exp == "") shelter_exp else gsub(" who ", " and ", shelter_exp),
               post_exp)
      }
      
      # SHORT
      post_short <-
        case_when(str_ends(var, "_count$") ~ "",
                  str_ends(var, "_pct$") ~ " (%)",
                  str_ends(var, "_sqkm$") ~ " (sqkm)")
      
      sex_imm_short <- 
        case_when(str_detect(var, "cent_p_immigrants") && str_detect(var, "female") ~ 
                    "W. Imm.",
                  str_detect(var, "cent_p_immigrants") && str_detect(var, "male") ~ 
                    "M. Imm.",
                  str_detect(var, "non_immigrants") && str_detect(var, "female") ~ 
                    "W. N-Imm.",
                  str_detect(var, "non_immigrants") && str_detect(var, "male") ~ 
                    "M. N-Imm.",
                  !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "female") ~ 
                    "W.",
                  !str_detect(var, "cent_p_immigrants|non_immigrants") && str_detect(var, "male") ~ 
                    "M.",
                  str_detect(var, "cent_p_immigrants") && !str_detect(var, "male|female") ~ 
                    "Imm.",
                  str_detect(var, "non_immigrants") && !str_detect(var, "male|female") ~ 
                    "N-Imm.",
                  TRUE ~ "Peo.")
      
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
      
      
      type <- case_when(str_ends(var, "_count$") ~ "count",
                        str_ends(var, "_pct$") ~ "pct",
                        str_ends(var, "_sqkm$") ~ "sqkm")
      
      
      # ADDED ROW
      out <-
        add_variable(
          variables = scales_variables_modules$variables,
          var_code = var,
          type = type,
          var_title = title,
          var_short = short,
          explanation = exp,
          theme = "Housing",
          private = FALSE,
          pe_include = {var %in% var_in_place_explorer},
          dates = with_breaks$avail_dates[[var]],
          avail_df = data_interpolated$avail_df,
          breaks_q3 = with_breaks$q3_breaks_table[[var]],
          breaks_q5 = with_breaks$q5_breaks_table[[var]],
          source = "Centraide of Greater Montreal",
          interpolated = data_interpolated$interpolated_ref
        )
      
      pb()
      out[out$var_code == var, ]
      
    })
  })
  
  new_rows <- data.table::rbindlist(new_rows)
  variables <- rbind(scales_variables_modules$variables, new_rows)

  
  # Modules table -----------------------------------------------------------
  
  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "demographics",
      theme = "Urban life",
      nav_title = "Demographics",
      title_text_title = "Demographics",
      title_text_main = paste0(
        "Certain factors about a population can reveal interesting socioeconomic ",
        "information. In this module, learn and explore different demographics ",
        "statistically expressed per square kilometre, percentage of population, ",
        "or simply by count. Choose various factors to examine such as gender, ",
        "immigration status, shelter cost, and additional immigration, visible ",
        "minority and family characteristics."),
      title_text_extra = paste0(
        "<p>The comparative analysis that you see in this module is based on ", 
        "demographic data from the 2016 Census. In selecting different options ",
        "from the drop-down menus, insights can be gained on the prevalence of ",
        "different factors by the three types of groupings. Using the panel on ",
        "the right, you can compare these demographic variables with access to ",
        "different amenities by mode of transportation. Examining and comparing ",
        "demographic variables can provide valuable information about the ",
        "Montreal population. </ul>"),
      regions = data_interpolated$regions,
      metadata = TRUE,
      dataset_info = paste0(
        "<p>The census data (2016) in this module comes from ",
        "custom tabulations ordered by Centraide of Greater ",
        "Montreal to Statistics Canada.</p>")
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}
