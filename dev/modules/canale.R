#### CanALE data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

reserves <- c("24670285", "24720184", "24720186", "24720187", "24720188", 
              "24720190", "24720191", "24720192", "24720193", "24720194", 
              "24720195", "24720196", "24720200", "24720201")

canale <- 
  read_csv("dev/data/canale/CanALE_Canada.csv") |> 
  filter(DAUID %in% !!DA$ID) |> 
  transmute(DAUID = as.character(DAUID), canale_ind_2016 = ale_index) |> 
  mutate(canale_ind_2016 = if_else(
    DAUID %in% reserves, NA_real_, canale_ind_2016))

# Data testing ------------------------------------------------------------

data_testing(data = list("canale" = canale))


# Interpolate -------------------------------------------------------------

all_canale <- 
  interpolate_scales(data = canale, 
                     base_scale = "DA", 
                     all_tables = all_tables, 
                     add_to_grid = FALSE)


# Calculate breaks --------------------------------------------------------

all_canale <- 
  calculate_breaks(all_canale)


# Assign to existing geographies ------------------------------------------

assign_tables(module_tables = all_canale)


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add to variables table --------------------------------------------------

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(all_canale$tables_q3, function(x, scale) {
   if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                .before = canale_ind_2016)}) |> 
  rename(var = canale_ind_2016)

# Get breaks_q5
breaks_q5_active <- 
  imap_dfr(all_canale$tables_q5, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, rank = 0:5, 
                                 .before = canale_ind_2016)}) |> 
  rename(var = canale_ind_2016)


variables <- 
  variables |>
  add_variables(
    var_code = "canale_ind",
    var_title = "CanALE index",
    var_short = "CanALE",
    explanation = "the potential for active living",
    category = NA,
    theme = "Urban life",
    private = FALSE,
    dates = c("2016"),
    scales = names(all_canale$tables_list),
    breaks_q3 = breaks_q3_active,
    breaks_q5 = breaks_q5_active,
    source = "McGill Geo-Social Determinants of Health Research Group",
    interpolated = list(c(DA = FALSE,
                          CT = "dissemination area",
                          borough = "dissemination area",
                          centraide = "dissemination area")))


# Clean up ----------------------------------------------------------------

rm(canale, all_canale, breaks_q3_active, breaks_q5_active, reserves)

# To save output, run dev/build_data.R, which calls this script
