#### Census identity data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_identity <- tibble(
  var_code = character(),
  vec_2016 = character(),
  vec_2011 = character(),
  vec_2006 = character(),
  vec_2001 = character(),
  vec_1996 = character(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
  )
    
census_identity <- 
  census_identity |> 
  add_row(
    var_code = "iden_imm_pct",
    vec_2016 = "v_CA16_3411",
    vec_2011 = "v_CA11N_22",
    vec_2006 = "v_CA06_478",
    vec_2001 = "v_CA01_406",
    vec_1996 = "v_CA1996_128",
    var_title = "Immigrants (%)",
    var_short = "Immigrants",
    explanation = "the percentage of residents who are foreign-born",
    private = FALSE) |> 
  add_row(
    var_code = "iden_imm_new_pct",
    vec_2016 = "v_CA16_3432",
    vec_2011 = "v_CA11N_43",
    vec_2006 = "v_CA06_553",
    vec_2001 = "v_CA01_507",
    vec_1996 = "v_CA1996_228",
    var_title = "New immigrants (%)",
    var_short = "New immigrants",
    explanation = paste0("the percentage of people who have immigrated in ",
                         "the last five years"),
    private = FALSE) |>
  add_row(
    var_code = "iden_vm_pct",
    vec_2016 = "v_CA16_3957",
    vec_2011 = "v_CA11N_460",
    vec_2006 = "v_CA06_1303",
    vec_2001 = "v_CA01_703",
    vec_1996 = "v_CA1996_784",
    var_title = "Visible minorities (%)",
    var_short = "Vis. minorities",
    explanation = paste0("the percentage of people who identify as part of ",
                         "one or more visible minority groups"),
    private = FALSE) |>
  add_row(
    var_code = "iden_aboriginal_pct",
    vec_2016 = "v_CA16_3855",
    vec_2011 = "v_CA11N_1354",
    vec_2006 = "v_CA06_565",
    vec_2001 = "v_CA01_718",
    vec_1996 = "v_CA1996_473",
    var_title = "Aboriginal (%)",
    var_short = "Aboriginal",
    explanation = "the percentage of people who are of aboriginal identity",
    private = FALSE)
  

# Gather data -------------------------------------------------------------

data_to_add <- 
  add_census_data(census_identity, scales, years)


# Assign data -------------------------------------------------------------

borough <- 
  borough |> 
  left_join(data_to_add[[1]]$borough, by = "ID") |> 
  relocate(geometry, .after = last_col())

CT <- 
  CT |> 
  left_join(data_to_add[[1]]$CT, by = "ID") |> 
  relocate(geometry, .after = last_col())

DA <- 
  DA |> 
  left_join(data_to_add[[1]]$DA, by = "ID") |> 
  relocate(centroid, buffer, geometry, .after = last_col())

grid <-
  grid |>
  left_join(data_to_add[[1]]$grid, by = "ID") |>
  relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

variables <- bind_rows(variables, data_to_add[[2]])
rm(census_identity)
