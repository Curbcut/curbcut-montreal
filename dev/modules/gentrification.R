#### Gentrification alley data setup ########################################### 

# This script relies on objects created in dev/census.R

# Index function ----------------------------------------------------------

vars <- c("edu_bachelor_above_prop", "inc_median_dollar", "housing_value_avg_dollar",
          "housing_rent_avg_dollar", "housing_tenant_prop", "iden_vm_prop")

neg_vars <- c("housing_tenant_prop", "iden_vm_prop")

dates_list <- c(1996,2001,2006,2011,2016)

index_fun <- function(df){
  
  pre_index_fun <- function(df, year) {
    
    # Get dfs of all the vars across census
    list_dfs <- 
      purrr::map(dates_list, ~{
        df %>% 
          st_drop_geometry() %>% 
          select(starts_with(vars)) %>%
          select(ends_with(as.character(.x))) %>% 
          select(!contains("q3")) %>% 
          rename_with(~paste0(str_remove(.x, "_\\d{4}$")), everything())
      })
    
    # Bind the previous dfs together, to bring all values of all years in one big df
    binded_dfs <- purrr::reduce(list_dfs, rbind)
    
    # Give a position to all values, to enable to possibility of creating an indice
    vars_ranked <- 
      binded_dfs %>% 
      mutate(across(everything(), ~ (.- mean(., na.rm = T))/sd(., na.rm = T), .names = "{.col}_zscore")) #%>% 
      # mutate(across(everything(), percent_rank, .names = "{.col}_rank")) %>% 
      # Invert the rank for the 2 variables that have negative impact
      # mutate(across(paste0(neg_vars, "_rank"), ~ 1 - .))
    
    # Apply these ranks for a given year to the right df ID
    ranked_per_year <- 
      purrr::map(paste0(vars, "_", year), ~{df %>% 
          st_drop_geometry() %>% 
          select(ID, .x) %>%
          inner_join((vars_ranked %>% 
                        rename_with(~paste0(.x, "_", year), everything()) %>% 
                        select(contains(str_remove(.x, "_\\d{4}$"))) %>% 
                        distinct())) %>%
          select(-.x)
      })
    
    reduced <- 
      purrr::reduce(ranked_per_year, left_join)
    
    # Create the indice for that year
    id_index <- 
      reduced %>% 
      select(-ID) %>% 
      summarize(gentrification_ind = rowMeans(.)) %>% 
      cbind(select(reduced, ID), .) %>% 
      as_tibble() %>% 
      rename_with(~paste0(.x, "_", year), gentrification_ind)
    
    id_index
    
  }
  
  left_join(df, purrr::reduce(purrr::map(dates_list, ~{pre_index_fun(df, .x)}), left_join))
  
}

index_fun(DA) %>%
  mutate(change = (gentrification_ind_2016-gentrification_ind_2006)/gentrification_ind_2006) %>%
  ggplot()+
  geom_sf(aes(fill = change), color = "transparent") +
  scale_fill_continuous(#limits = c(-1,2), 
    type = "viridis")


# Apply function ----------------------------------------------------------

borough <- index_fun(borough)
CT <- index_fun(CT)
DA <- index_fun(DA)
grid <- index_fun(grid)

street <- index_fun(street)
building <- index_fun(building)


# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "gentrification_ind",
    var_name = "Gentrification index",
    explanation = "the experienced gentrification pressure")

# Clean-up ----------------------------------------------------------------

rm(vars, neg_vars, dates_list, index_fun)


# #### Gentrification data setup ############################################### 
# 
# # This script relies on objects created in dev/census.R
# 
# # Index function ----------------------------------------------------------
# 
# process_gi <- function(df, dates) {
#   vars <- c("edu_bachelor_above_prop", "inc_median_dollar", "housing_value_avg_dollar",
#             "housing_rent_avg_dollar", "housing_tenant_prop", "iden_vm_prop")
#   
#   new_cols <- 
#     map_dfc(dates, function(x) {
#       map_dfc(vars, ~{
#         df %>%  
#           st_drop_geometry() %>%  
#           select(all_of(paste(.x, x, sep = "_"))) %>%
#           set_names(c("var_1", "var_2")) %>%
#           transmute(
#             new_var = (var_2 - var_1) / abs(var_1)) %>%  
#           set_names(paste(.x, x[1], x[2], sep = "_"))
#       })
#     })
#   
#   new_cols <- 
#     df %>% 
#     st_drop_geometry() %>% 
#     select(ID) %>% 
#     cbind(new_cols) %>% 
#     mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>%
#     mutate(across(where(is.numeric), ~replace(., is.nan(.), 0)))
#   
#   ids <- new_cols$ID
#   
#   new_cols <- new_cols %>%
#     select(-ID) %>%
#     scale()
#   
#   new_cols <- cbind(ids, new_cols) %>% 
#     as_tibble() %>% 
#     mutate(across(contains("_"), ~as.numeric(.))) 
#   
#   indexes <- 
#     map_dfc(dates, ~{
#       new_cols %>%  
#         select(contains(paste(.x[1], .x[2], sep = "_"))) %>%
#         set_names(c("var_1", "var_2", "var_3", "var_4", "var_5", "var_6")) %>%
#         transmute(
#           new_var = var_1 + var_2 + var_3 + var_4 + (var_5 * -1) + (var_6 * -1)) %>%  
#         set_names(paste("gentrification_ind", .x[1], .x[2], sep = "_")) 
#     })
#   
#   indexes <- cbind(ids, indexes) %>%
#     as_tibble() %>%
#     rename(ID = ids)
# 
#   indexes
#   
# }
# 
# # Census years to date
# dates_list <- list(c(1996,2001,2006,2011,2016))
# 
# unique_dates_combination <- 
# (tibble(.rows = 1) %>% 
#   mutate(pairs = map2(dates_list, dates_list, expand.grid, stringsAsFactors = FALSE)) %>%
#   unnest(pairs) %>%
#   transmute(pairs = map2(Var1, Var2, c)) %>%
#   mutate(pairs = map(pairs, sort)) %>%
#   rowwise() %>%
#   filter(pairs[1] != pairs[2]) %>%
#   ungroup() %>% 
#   distinct() %>% 
#   as.list())[[1]]
# 
# 
# # Apply the function ------------------------------------------------------
# 
# add_indexes <- function(df) {
#   
#   list_of_gis_index <- map(map(unique_dates_combination, list), ~{process_gi(df, .x)})
#   
#   var_list <- map(list_of_gis_index, names) %>% 
#     unlist() %>% 
#     str_subset("gentrification_ind")
#   
#   list_of_gis_index <- append(list_of_gis_index, list(df), 0)
#   
#   purrr::reduce(list_of_gis_index, left_join, by = "ID") %>% 
#     mutate(across(all_of(var_list), ntile, 3, 
#                   .names = '{paste0(str_extract({.col}, "gentrification_ind"), "_q3", str_extract({.col}, ".{10}$"))}')) %>% 
#     relocate(geometry, .after = last_col())
#   
# }
# 
# borough <- add_indexes(borough) %>% 
#   st_as_sf() %>% 
#   st_set_agr("constant")
# 
# CT <- add_indexes(CT) %>% 
#   st_as_sf() %>% 
#   st_set_agr("constant")
# 
# DA <- add_indexes(DA) %>% 
#   st_as_sf() %>% 
#   st_set_agr("constant")
# 
# grid <- add_indexes(grid) %>% 
#   st_as_sf() %>% 
#   st_set_agr("constant")
# 
# street <- add_indexes(street)
# building <- add_indexes(building)
# 
# 
# # Clean-up ----------------------------------------------------------------
# 
# 
# rm(process_gi, dates_list, unique_dates_combination, add_indexes)