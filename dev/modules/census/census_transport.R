#### Census housing data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_transport <- tibble(
  var_code = character(),
  vec_2016 = list(),
  vec_2011 = list(),
  vec_2006 = list(),
  vec_2001 = list(),
  vec_1996 = list(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
)

add_row_trans <- function(data, var_code, vec_2016, vec_2011, vec_2006, 
                          vec_2001, vec_1996, var_title, var_short, explanation, 
                          private) {
  add_row(data,
          var_code = var_code,
          vec_2016 = list(vec_2016),
          vec_2011 = list(vec_2011),
          vec_2006 = list(vec_2006),
          vec_2001 = list(vec_2001),
          vec_1996 = list(vec_1996),
          var_title = var_title,
          var_short = var_short,
          explanation = explanation,
          private = private)
}

census_transport <-
  census_transport |>
  add_row_trans(
    var_code = "trans_car_pct",
    vec_2016 = c("v_CA16_5795", "v_CA16_5798"),
    vec_2011 = c("v_CA11N_2194", "v_CA11N_2197"),
    vec_2006 = c("v_CA06_1101", "v_CA06_1102"),
    vec_2001 = c("v_CA01_1255", "v_CA01_1256", "v_CA01_1264", "v_CA01_1265"),
    vec_1996 = c("v_CA1996_1326", "v_CA1996_1327", "v_CA1996_1335", 
                 "v_CA1996_1336"),
    var_title = "Drive to work (%)",
    var_short = "Drive",
    explanation = paste0("the percentage of people who drive a privately ",
                         "owned car or truck to work"),
    private = FALSE) |>
  add_row_trans(
    var_code = "trans_walk_or_bike_pct",
    vec_2016 = c("v_CA16_5804", "v_CA16_5807"),
    vec_2011 = c("v_CA11N_2203", "v_CA11N_2206"),
    vec_2006 = c("v_CA06_1104", "v_CA06_1105"),
    vec_2001 = c("v_CA01_1258", "v_CA01_1259", "v_CA01_1267", "v_CA01_1268"),
    vec_1996 = c("v_CA1996_1329", "v_CA1996_1330", "v_CA1996_1338", 
                 "v_CA1996_1339"),
    var_title = "Walk or cycle to work (%)",
    var_short = "Walk or cycle",
    explanation = "the percentage of people who walk or cycle to work",
    private = FALSE) |>
  add_row_trans(
    var_code = "trans_transit_pct",
    vec_2016 = "v_CA16_5801",
    vec_2011 = "v_CA11N_2200",
    vec_2006 = "v_CA06_1103",
    vec_2001 = c("v_CA01_1266", "v_CA01_1257"),
    vec_1996 = c("v_CA1996_1337", "v_CA1996_1328"),
    var_title = "Public transit to work (%)",
    var_short = "Transit",
    explanation = paste0("the percentage of people who use public transit ",
                         "to get to work"),
    private = FALSE) |>
  add_row_trans(
    var_code = "trans_t_15_pct",
    vec_2016 = "v_CA16_5816",
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Commute under 15 minutes (%)",
    var_short = "Commute <15m",
    explanation = paste0("the percentage of people whose commute time is ",
                         "less than 15 minutes"),
    private = FALSE) |>
  add_row_trans(
    var_code = "trans_t_45_pct",
    vec_2016 = c("v_CA16_5819", "v_CA16_5822"),
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Commute 15-45 minutes (%)",
    var_short = "Commute 14-45m",
    explanation = paste0("the percentage of people whose commute time is ",
                         "between 15 and 45 minutes"),
    private = FALSE) |>
  add_row_trans(
    var_code = "trans_t_45_plus_pct",
    vec_2016 = c("v_CA16_5825", "v_CA16_5828"),
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Commute more than 45 minutes (%)",
    var_short = "Commute >45m",
    explanation = paste0("the percentage of people whose commute time is ",
                         "longer than 45 minutes"),
    private = FALSE)


# Gather data -------------------------------------------------------------

data_to_add <- add_census_data(
  census_transport, scales, years, parent_vectors = c(
    "trans_car_pct" = "v_CA01_1253", "trans_car_pct" = "v_CA1996_1324", 
    "trans_walk_or_bike_pct" = "v_CA01_1253", 
    "trans_walk_or_bike_pct" = "v_CA1996_1324", 
    "trans_transit_pct" = "v_CA01_1253", "trans_transit_pct" = "v_CA1996_1324"))


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
rm(census_transport)
