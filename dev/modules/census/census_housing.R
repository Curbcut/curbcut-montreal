#### Census housing data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_housing <- tibble(
  var_code = character(),
  vec_2016 = character(),
  vec_2011 = character(),
  vec_2006 = character(),
  vec_2001 = character(),
  vec_1996 = character(),
  include = logical(),
  interpolation = character(),
  denominator = character(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
  )
    
census_housing <- 
  census_housing |> 
  add_row(
    var_code = "tenure_households",
    vec_2016 = "v_CA16_4836",
    vec_2011 = "v_CA11N_2252",
    vec_2006 = "v_CA06_101",
    vec_2001 = "v_CA01_96",
    vec_1996 = "v_CA1996_1678",
    include = FALSE,
    interpolation = "count") |> 
  add_row(
    var_code = "housing_tenant_pct",
    vec_2016 = "v_CA16_4838",
    vec_2011 = "v_CA11N_2254",
    vec_2006 = "v_CA06_103",
    vec_2001 = "v_CA01_100",
    vec_1996 = "v_CA1996_1683",
    include = TRUE,
    interpolation = "count",
    denominator = "tenure_households",
    var_title = "Tenant-occupied (%)",
    var_short = "Tenant (%)",
    explanation = "the percentage of private dwellings occupied by tenants",
    private = FALSE)
  

# Get empty geometries ----------------------------------------------------

geoms <- get_empty_geometries(scales, years)


# Download data -----------------------------------------------------------

data_raw <- get_census_vectors(census_housing, geoms, scales, years)


# Interpolate -------------------------------------------------------------

var_count <- 
  census_housing |> 
  filter(interpolation == "count") |> 
  pull(var_code)

var_avg <- 
  census_housing |> 
  filter(interpolation == "average") |> 
  pull(var_code)

data_inter <- interpolate(data_raw, scales, years)


# Swap CSD to borough -----------------------------------------------------

# TKTK THIS NEEDS TO HAPPEN HERE, BEFORE NORMALIZATION!


# Interpolate to building, grid & street ----------------------------------

# TKTK THIS SHOULD HAPPEN NOW AS WELL, SO THAT NORMALIZATION IS MORE PRECISE


# Normalize pct variables -------------------------------------------------

data_norm <- normalize(data_inter, census_housing)


# Drop variables which aren't included in final tables --------------------

data_final <- drop_vars(data_norm, census_housing)


# Add q3 and q5 versions --------------------------------------------------

cat_q5 <- get_categories_q5(data_final, census_housing)
data_q3 <- add_q3(data_final)
breaks_q3 <- get_breaks_q3(data_q3, census_housing)
breaks_q5 <- get_breaks_q5(data_final, cat_q5)
data_q5 <- add_q5(data_final, breaks_q5)
data_breaks <- merge_breaks(data_final, data_q3, data_q5)


# Add years ---------------------------------------------------------------

data_years <- add_years(data_breaks, years)


# Finalize output ---------------------------------------------------------

data_to_add <- reduce_years(data_years)

# building <- 
#   building |> 
#   left_join(data_to_add[[1]], by = "ID") |> 
#   relocate(geometry, .after = last_col())

## TKTK DOESN'T WORK UNTIL CSD IS SWITCHED TO BOROUGH!
borough <- 
  borough |> 
  left_join(data_to_add[[1]], by = "ID") |> 
  relocate(geometry, .after = last_col())

CT <- 
  CT |> 
  left_join(data_to_add[[2]], by = "ID") |> 
  relocate(geometry, .after = last_col())

DA <- 
  DA |> 
  left_join(data_to_add[[3]], by = "ID") |> 
  relocate(centroid, buffer, geometry, .after = last_col())

# grid <- 
#   grid |> 
#   left_join(data_to_add[[1]], by = "ID") |> 
#   relocate(geometry, .after = last_col())

# street <- 
#   street |> 
#   left_join(data_to_add[[1]], by = "ID") |> 
#   relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

new_vars <- add_vars(data_to_add, census_housing, breaks_q3, breaks_q5)
variables <- bind_rows(variables, new_vars)
