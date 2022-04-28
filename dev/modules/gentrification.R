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

  purrr::reduce(pre_index_fun, left_join, by = "ID")
}

gen_to_join <- map(list("borough" = borough, "CT" = CT, "DA" = DA, 
                        "grid" = grid), index_fun)


# Add empty q3 and q5 -----------------------------------------------------
# gen_to_join <- map(gen_to_join, ~{
#   mutate(.x, across(c(-any_of("ID")), \(x) NA_real_, .names = "{.col}_q3")) |> 
#     rename_with(~paste0(str_remove(., "_\\d{4}"),
#                         str_extract(., "_\\d{4}")), matches("_\\d{4}"))
# })
# 
# gen_to_join <- map(gen_to_join, ~{
#   mutate(.x, across(c(-any_of("ID")), \(x) NA_real_, .names = "{.col}_q5")) |> 
#     rename_with(~paste0(str_remove(., "_\\d{4}"),
#                         str_extract(., "_\\d{4}")), matches("_\\d{4}"))
# })

gen_results <- map(gen_to_join, add_q3)
gen_q3 <- map(gen_results, get_breaks_q3)
gen_q5 <- map(gen_results, get_breaks_q5)
gen_results <- map2(gen_results, gen_q5, ~bind_cols(.x, add_q5(.x, .y)))

# Data testing ------------------------------------------------------------

data_testing(gen_to_join, ignore_year_diff = TRUE)
# Wranings on absolute average difference between years. In the case of
# gentrification ind., these warnings should be taken lightly.


# Apply function ----------------------------------------------------------

# Join data ---------------------------------------------------------------

walk(names(gen_results), ~{
  assign(.x, left_join(get(.x), gen_results[[.x]], by = "ID") |> 
           relocate(any_of(c("buffer", "centroid", "building", "geometry")), 
                    .after = last_col()), envir = globalenv())})

building <- 
  building |> 
  left_join(gen_results$DA, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")

street <- 
  street |> 
  left_join(gen_results$DA, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col()) |> 
  st_set_agr("constant")


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add variable explanations -----------------------------------------------

var_list <- 
  gen_results |> 
  map(~names(select(.x, -ID, -contains(c("q3", "q5"))))) |> 
  unlist() |> 
  unique()

years <- str_extract(var_list, "\\d{4}$")

# Get breaks_q3
breaks_q3_active <-
  map(set_names(var_list), ~{
    map2_dfr(gen_q3, names(gen_results), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                   .before = everything())}) |> 
      select(scale, date, rank, var = all_of(.x))}) |> 
  map2(years, ~{
    .x |> mutate(date = .y)
  }) |> 
  reduce(bind_rows)

# Get breaks_q5
breaks_q5_active <- 
  map(set_names(var_list), ~{
    map2_dfr(gen_q5, names(gen_results), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, rank = 0:5, 
                                   .before = everything())}) |> 
      select(scale, rank, var = all_of(.x))}) |> 
  map2(years, ~{
    .x |> mutate(date = .y)
  }) |> 
  reduce(bind_rows)

variables <-
  variables |>
  add_variables(
    var_code = "gentrification_ind",
    var_title = "Gentrification index",
    var_short = "Gentrification",
    explanation = "the gentrification pressure an area is experiencing",
    category = NA,
    theme = "Housing",
    private = FALSE,
    dates = c("1996", "2001", "2006", "2011", "2016"),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active,
    breaks_q5 = breaks_q5_active,
    source = "sus_team")


# Clean-up ----------------------------------------------------------------

rm(vars, neg_vars, dates_list, index_fun, weight, gen_to_join)
