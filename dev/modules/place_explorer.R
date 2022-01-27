#### Place explorer data setup #################################################

# This script relies on objects created in dev/census.R


# Import postal codes -----------------------------------------------------

postal_codes <- 
  read_csv("dev/data/ZipCodeFiles/CanadianPostalCodes202103.csv")

postal_codes <- 
  postal_codes |>  
  filter(PROVINCE_ABBR == "QC") |>
  select(-PROVINCE_ABBR, -TIME_ZONE) |> 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |> 
  setNames(c("postal_code", "city", "geometry")) |> 
  st_filter(borough) |> 
  as_tibble() |> 
  st_as_sf() |> 
  mutate(postal_code = str_remove(str_to_lower(postal_code), "\\s"))




# Get percentile of variables, to order in place ex -----------------------

# Percentile retrieval
# Which variables should have a percentile attached?
basic_percentile_retrieval <- 
  variables |> 
  filter(source == "census" |
           str_starts(var_code, "climate") |
           str_starts(var_code, "canale"))

pe_var_hierarchy <- 
  map(set_names(c("borough", "CT", "DA")), function(scale) {
    map(set_names(basic_percentile_retrieval$var_code), function(variable_code) {
      
      var_row <- variables[variables$var_code == variable_code, ]
      var <- var_row$var_code
      max_date <- unlist(var_row$dates)[length(unlist(var_row$dates))]
      
      if (!is.na(max_date)) {
        var <- paste(var, max_date, sep = "_")
      }
      
      out <- 
        get(scale) |> 
        st_drop_geometry() |> 
        select(ID, var = all_of(var)) |> 
        mutate(var_percentile = percent_rank(var))
      
      names(out) <- c("ID", str_remove(var, paste0("_", max_date)),
                      paste0(str_remove(var, paste0("_", max_date)), 
                             "_percentile"))
      
      out
      
    }) |> reduce(left_join, by = "ID")
  })

# Add gentrification from the minimum date to its maximum.
gentrification_min_max <- 
  borough |> 
  st_drop_geometry() |> 
  select(starts_with("gentrification")) |> 
  names() |> 
  str_extract("\\d{4}$") |> 
  unique() |> 
  (\(x) list(min = min(x), max = max(x)))()

pe_var_hierarchy <-
  map(set_names(names(pe_var_hierarchy)), ~{
    
    id_gentrification_ind <- 
      get(.x) |> 
      st_drop_geometry() |> 
      select(ID, starts_with("gentrification") & 
               ends_with(c(gentrification_min_max$min, 
                           gentrification_min_max$max)),
             -contains(c("q3", "q5"))) |> 
      (\(x) mutate(x, gentrification_ind = (x[[3]] - x[[2]]) /
                     x[[2]]))() |> 
      select(ID, gentrification_ind) |> 
      mutate(gentrification_ind_percentile = 
               percent_rank(gentrification_ind))
    
    left_join(pe_var_hierarchy[[.x]], id_gentrification_ind, by = "ID")
    
  })

# Retrieve access average values
min_access_var_code <- 
  variables |> 
  filter(str_starts(var_code, "access")) |>
  mutate(var_code = case_when(str_starts(var_code, "access_jobs") ~ 
                                str_extract(var_code, "access_jobs_[^_]*"),
                              TRUE ~ str_extract(var_code, "access_[^_]*"))) |> 
  pull(var_code) |> 
  unique()

pe_var_hierarchy[["CT"]] <- 
  left_join(pe_var_hierarchy[["CT"]], 
            map(set_names(min_access_var_code), ~{
              out <- 
                CT |> 
                st_drop_geometry() |> 
                select(ID, starts_with(.x)) |> 
                pivot_longer(-ID) |> 
                group_by(ID) |> 
                summarize(var = mean(value)) |> 
                mutate(percentile = 
                         percent_rank(var))
              
              names(out) <- c("ID", .x, paste0(.x, "_percentile"))
              
              out
            }) |> reduce(left_join, by = "ID"),
            by = "ID")


# Put hierarchy in place --------------------------------------------------

# For each geometry, each ID will have to order both THEMES together to know
# which theme to show up first, + intra-theme which VARIBLES to show first
pe_theme_order <-
  map(set_names(names(pe_var_hierarchy)), ~{
    
    place_ex_variables <- 
      rbind(filter(variables, !str_starts(var_code, "access")),
            variables |> 
              filter(str_starts(var_code, "access")) |>
              mutate(var_code = case_when(str_starts(var_code, "access_jobs") ~ 
                                            str_extract(var_code, "access_jobs_[^_]*"),
                                          TRUE ~ str_extract(var_code, "access_[^_]*")))
      )
    
    pe_var_hierarchy[[.x]] |> 
      pivot_longer(-ID) |> 
      filter(str_ends(name, "percentile")) |> 
      transmute(ID, 
                var_code = str_remove(name, "_percentile"), 
                percentile = value) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme) |> 
      summarize(theme_order = mean(max_or_min), .groups = "drop") |> 
      group_by(ID) |> 
      arrange(-theme_order) |> 
      mutate(theme_order = row_number()) |> 
      ungroup()
  })

pe_variable_order <- 
  map(set_names(names(pe_var_hierarchy)), ~{
    
    place_ex_variables <- 
      rbind(filter(variables, !str_starts(var_code, "access")),
            variables |> 
              filter(str_starts(var_code, "access")) |>
              mutate(var_code = case_when(str_starts(var_code, "access_jobs") ~ 
                                            str_extract(var_code, "access_jobs_[^_]*"),
                                          TRUE ~ str_extract(var_code, "access_[^_]*")))
      )
    
    pe_var_hierarchy[[.x]] |> 
      pivot_longer(-ID) |> 
      filter(str_ends(name, "percentile")) |> 
      transmute(ID, 
                var_code = str_remove(name, "_percentile"), 
                percentile = value) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme) |> 
      arrange(-max_or_min) |> 
      mutate(variable_order = row_number()) |> 
      ungroup() |> 
      select(ID, theme, var_code, variable_order, theme)
  })

# Cleanup -----------------------------------------------------------------

rm(basic_percentile_retrieval, gentrification_min_max, min_access_var_code)

