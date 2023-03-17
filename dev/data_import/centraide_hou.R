## BUILD AND APPEND CENTRAIDE_HOU DATA #########################################

build_and_append_centraide_hou <- function(scales_variables_modules, 
                                           CT_table, region_CT_IDs, crs) {
  
  # # Read and prepare data ---------------------------------------------------
  # 
  # table2 <-
  #   qs::qread("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/table2.qs")
  # 
  # source("dev/other/crosstabs_fun.R")
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
  # old_plan <- future::plan()
  # future::plan(list(future::tweak(future::multisession, workers = 2),
  #                   future::tweak(future::multisession, workers = 3),
  #                   future::tweak(future::multisession, workers = 3),
  #                   future::tweak(future::multisession, workers = 2)))
  # 
  # progressr::with_progress({
  #   pb <-
  #     progressr::progressor(sum(purrr::map_int(tenure_statuses, length)) *
  #                             sum(purrr::map_int(shelter_costs, length)) *
  #                             sum(purrr::map_int(add_characteristics, length)) *
  #                             2)
  # 
  #   data <-
  #     furrr::future_map(purrr::set_names(c("CT", "centraide")), function(scale) {
  #       furrr::future_map_dfc(names(tenure_statuses), function(tenure_status_name) {
  # 
  #         tenure_status <- tenure_statuses[[tenure_status_name]]
  # 
  #         furrr::future_map_dfc(names(shelter_costs), function(shelter_cost_name) {
  # 
  #           shelter_cost_f <- shelter_costs[[shelter_cost_name]]
  # 
  #           shelter_cost_sum_rows <-
  #             furrr::future_map(shelter_cost_f, function(shelter_c) {
  #               purrr::map_dfc(names(add_characteristics), function(characteristic_name) {
  # 
  #                 add_characteristics <- add_characteristics[[characteristic_name]]
  # 
  #                 out <-
  #                   get_housing_char(tenure = tenure_status,
  #                                    shelter_cost = shelter_c,
  #                                    characteristics = add_characteristics,
  #                                    region_CT_IDs = region_CT_IDs)[[
  #                                      scale]][, "var"]
  # 
  #                 pb()
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
  #               purrr::map(shelter_cost_sum_rows, dplyr::mutate,
  #                          row_n = dplyr::row_number()) |>
  #               purrr::reduce(bind_rows) |>
  #               dplyr::group_by(row_n) |>
  #               dplyr::summarize_all(sum) |>
  #               dplyr::select(-row_n)
  #           }
  # 
  #           shelter_cost_sum_rows
  # 
  #         })
  #       })
  #     })
  # })
  # 
  # data <-
  #   purrr::imap(data, function(df, scale) {
  #     dplyr::bind_cols(get_housing_char(
  #       region_CT_IDs = region_CT_IDs)[[scale]][, "ID"], df) |>
  #       dplyr::rename_with(~paste0("cent_d_", .x, "_count_2016"),
  #                          total_total_total:last_col())
  #   })
  # 
  # qsave(data, file = "dev/data/built/centraide_hou.qs")
  data <- qs::qread("dev/data/built/centraide_hou.qs")
  
  
  # Normalize (percentage) --------------------------------------------------
  
  data <- 
    lapply(data, \(df) {
      # Add _pct
      with_pct <- 
        df |> 
        dplyr::mutate(dplyr::across(dplyr::starts_with("cent_d"), ~{.x / 
            cent_d_total_total_total_count_2016}))
      
      names(with_pct)[stringr::str_starts(names(with_pct), "cent_d")] <- 
        names(with_pct)[stringr::str_starts(names(with_pct), "cent_d")] |> 
        stringr::str_replace("count_2016$", "pct_2016")
      
      # Combine count and pct
      merge(df, with_pct)
    })

  
  # Get list of data variables ----------------------------------------------
  
  # Build a character vector of all data variables that will be added to all
  # scales.
  add_vars <- names(data$CT)[!grepl("ID$", names(data$CT)) &
                               grepl("count_2016$", names(data$CT))]
  avg_vars <- names(data$CT)[!grepl("ID$", names(data$CT)) &
                               grepl("pct_2016$", names(data$CT))]
  
  
  # Interpolate data to all possible scales ---------------------------------
  
  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  names(data$CT)[1] <- "CT_ID"
  data_interpolated <-
    interpolate_from_census_geo(
      data = data$CT,
      base_scale = "CT",
      all_scales = scales_variables_modules$scales,
      weight_by = "households",
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
    c("cent_d_tenant_more_30_per_total_pct_2016",
      "cent_d_tenant_more_30_per_total_count_2016",
      "cent_d_tenant_more_30_per_low_inc_pct_2016",
      "cent_d_tenant_total_low_inc_pct_2016")
  
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(c(add_vars, avg_vars)))
    new_rows <-
      lapply(c(add_vars, avg_vars), function(var) {
        
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
                      " living in unsuitable housing",
                    str_detect(var, "repairs") ~
                      " living in housing with major repairs needed",
                    str_detect(var, "low_inc") ~
                      "Low income ",
                    str_detect(var, "single_detached") ~ 
                      " living in single-detached houses",
                    str_detect(var, "semi_detached") ~ 
                      " living in semi-detached houses",
                    str_detect(var, "row_house") ~ 
                      " living in row houses",
                    str_detect(var, "in_duplex") ~ 
                      " living in apartments or flats in a duplex",
                    str_detect(var, "in_5plus_storeys") ~ 
                      " living in apartments in buildings that has five or more storeys",
                    str_detect(var, "in_less5_storeys") ~ 
                      " living in apartments in buildings that has fewer than five storeys",
                    str_detect(var, "other_single_attached") ~ 
                      " living in other single-attached houses",
                    str_detect(var, "mobile_homes") ~ 
                      " living in mobile homes and other movable dwellings",
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
                      " living in unsuitable housing",
                    str_detect(var, "repairs") ~
                      " living in housing with major repairs needed",
                    str_detect(var, "low_inc") ~
                      " low income",
                    str_detect(var, "single_detached") ~ 
                      " living in single-detached houses",
                    str_detect(var, "semi_detached") ~ 
                      " living in semi-detached houses",
                    str_detect(var, "row_house") ~ 
                      " living in row houses",
                    str_detect(var, "in_duplex") ~ 
                      " living in apartments or flats in a duplex",
                    str_detect(var, "in_5plus_storeys") ~ 
                      " living in apartments in buildings that has five or more storeys",
                    str_detect(var, "in_less5_storeys") ~ 
                      " living in apartments in buildings that has fewer than five storeys",
                    str_detect(var, "other_single_attached") ~ 
                      " living in other single-attached houses",
                    str_detect(var, "mobile_homes") ~ 
                      " living in mobile homes and other movable dwellings",
                    TRUE ~ "")
        
        exp <- if (!str_detect(var, "low_inc")) {
          paste0(pre_explanation, tenure_explanation, shelter_explanation,
                 characteristics_explanation)
        } else {
          paste0(pre_explanation, characteristics_explanation, tenure_explanation, 
                 shelter_explanation)
        }
        
        type <- case_when(str_ends(var, "_count$") ~ "count",
                          str_ends(var, "_pct$") ~ "pct")
        
        
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
      id = "afford",
      theme = "Housing",
      nav_title = "Housing affordability",
      title_text_title = "Housing Affordability",
      title_text_main = paste0(
        "Having access to affordable and equitable shelter is essential. ",
        "Affordable housing is often broadly defined as spending less than 30% of ",
        "household income on shelter costs. The reality of each household ",
        "consists of many different factors and characteristics, such as, the ",
        "people living there, their income, their shelter costs, and their tenure ",
        "status. In this module, explore and compare housing affordability by ",
        "city or borough, or census tract."),
      title_text_extra = paste0(
        "<p> The comparative analysis that you see in this module is based on ",
        "housing data from the 2016 Census. In selecting different options from ",
        "the drop-down menus, insights can be gained on how affordability varies ",
        "by household or individuals, their shelter costs, whether they are ",
        "tenants or owners and various family, immigration, and dwelling ",
        "characteristics. Using the panel on the right, you can compare these ",
        "housing affordability variables with access to different amenities by ",
        "mode of transportation. It is important to understand the geographic and ",
        "socioeconomic patterns associated with housing affordability to inform ",
        "policies and actions to best address housing needs. </ul>"),
      regions = data_interpolated$regions,
      metadata = TRUE,
      dataset_info = paste0(
        "<p>The census data (2016) in this module comes from ",
        "custom tabulations ordered by Centraide of Greater ",
        "Montreal to Statistics Canada.</p>")
    )
  
  modules <-
    modules |>
    add_module(
      id = "tenure",
      theme = "Housing",
      nav_title = "Tenure status",
      title_text_title = "Tenure Status",
      title_text_main = paste0(
        "The categorization of housing by tenure status, tenancy or ownership, ",
        "helps to gain a clearer picture of the housing landscape in Montreal. ",
        "This is especially the case when compared with other factors. In this ",
        "module, tenure status can be explored in relation to shelter costs and ",
        "additional variables such as family characteristics and dwelling ",
        "types."),
      title_text_extra = paste0(
        "<p> The comparative analysis that you see in this module is based on ", 
        "housing data from the 2016 Census. In selecting different options from ",
        "the drop-down menus, insights can be gained on how tenure status ", 
        "interacts with shelter costs, and various family, immigration, and ",
        "dwelling characteristics. Using the panel on the right, you can compare ",
        "these tenure status variables with access to different amenities by mode ",
        "of transportation. Understanding housing needs by tenure status can help ",
        "to inform what is to be improved specifically for tenants or owners as ",
        "they might be experiencing different difficulties and ",
        "advantages. </ul>"),
      regions = data_interpolated$regions,
      metadata = TRUE,
      dataset_info = paste0(
        "<p>The census data (2016) in this module comes from ",
        "custom tabulations ordered by Centraide of Greater ",
        "Montreal to Statistics Canada.</p>")
    )
  
  modules <-
    modules |>
    add_module(
      id = "dw_types",
      theme = "Housing",
      nav_title = "Dwelling Types",
      title_text_title = "Dwelling Types",
      title_text_main = paste0(
        "Whether a household is living in a single-detached house or an ",
        "apartment in a building of 5+ stories is an important aspect of ", 
        "understanding people’s housing realities. In selecting different types ",
        "of dwellings, you can simply explore and compare them in relation to ",
        "tenure status and shelter cost."),
      title_text_extra = paste0(
        "<p> The comparative analysis that you see in this module is based on ",
        "housing data from the 2016 Census. In selecting different options from ",
        "the drop-down menus, insights can be gained on how dwelling types ",
        "interact with tenure status and shelter costs. Using the panel on the ",
        "right, you can compare the dwelling type variables with access to ",
        "different amenities by mode of transportation. Exploring the housing ",
        "system in Montreal through dwelling types adds a level of understanding ",
        "to the overall housing situation. </ul>"),
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