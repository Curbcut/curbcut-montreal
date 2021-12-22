#### Gentrification alley data setup ###########################################

# This script relies on objects created in dev/census.R

# Index function ----------------------------------------------------------

vars <- c(
  "edu_bachelor_above_pct", "inc_median_dollar", "housing_value_avg_dollar",
  "housing_rent_avg_dollar", "housing_tenant_pct", "iden_vm_pct")

weight <- c(0.143, 0.054, 0.054, 0.054, 0.347, 0.347)

neg_vars <- c("housing_tenant_pct", "iden_vm_pct")

dates_list <- c(1996, 2001, 2006, 2011, 2016)

index_fun <- function(df) {

  # Get dfs of all the vars across census
  list_dfs <-
    purrr::map(dates_list, ~ {
      df |> 
        st_drop_geometry() |> 
        select(starts_with(vars)) |> 
        select(ends_with(as.character(.x))) |> 
        select(!contains(c("q3", "q5"))) |> 
        rename_with(~paste0(str_remove(.x, "_\\d{4}$")), everything())
    })

  # Bind the previous dfs together, to bring all values and years in one big df
  binded_dfs <- purrr::reduce(list_dfs, rbind)

  # Give a position to all values, to enable possibility of creating an index
  vars_ranked <-
    binded_dfs |>
    mutate(across(everything(), ~(. - mean(., na.rm = TRUE)) / 
                    sd(., na.rm = TRUE), .names = "{.col}_zscore")) |>
    # mutate(across(everything(), percent_rank, .names = "{.col}_rank")) %>%
    # Invert the zscore for the 2 variables that have reverse impact
    mutate(across(paste0(neg_vars, "_zscore"), ~ . * -1)) |>
    distinct()

  pre_index_fun <- purrr::map(dates_list, function(year) {

    # Apply these ranks for a given year to the right df ID
    ranked_per_year <-
      purrr::map(paste0(vars, "_", year), ~{
        rank <-
          vars_ranked |>
          select(starts_with(str_remove(.x, "_\\d{4}$"))) |>
          rename_with(~paste0(.x, "_", year), everything())

        df |>
          st_drop_geometry() |> 
          select(ID, all_of(.x)) |> 
          left_join(rank, by = all_of(.x)) |>
          select(-all_of(.x)) |>
          distinct()
      })

    reduced <- purrr::reduce(ranked_per_year, left_join, by = "ID")

    # Create the index for that year
    id_index <-
      reduced %>%
      select(-ID) %>%
      as.matrix() %>%
      rowMeans() %>%
      # matrixStats::rowWeightedMeans(., w = weight) %>%
      cbind(select(reduced, ID), .) %>%
      as_tibble() %>%
      rename_with(~ paste0("gentrification_ind", "_", year), 2)

    id_index
  })


  reduced <- purrr::reduce(pre_index_fun, left_join, by = "ID")
  left_join(df, reduced, by = "ID")
}


# Apply function ----------------------------------------------------------

borough <- index_fun(borough)
CT <- index_fun(CT)
DA <- index_fun(DA)
grid <- index_fun(grid)
street <- index_fun(street)
building <- index_fun(building)


# Add variable explanations -----------------------------------------------

variables <-
  variables |>
  add_variables(
    var_code = "gentrification_ind",
    var_title = "Gentrification index",
    var_short = "Gentrification",
    explanation = "the gentrification pressure an area is experiencing",
    category = NA,
    private = FALSE,
    dates = c("1996", "2001", "2006", "2011", "2016"),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "census")


# Clean-up ----------------------------------------------------------------

rm(vars, neg_vars, dates_list, index_fun)
