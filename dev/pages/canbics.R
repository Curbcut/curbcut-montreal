#### CANbics data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

reserves <- c("24670285", "24720184", "24720186", "24720187", "24720188", 
              "24720190", "24720191", "24720192", "24720193", "24720194", 
              "24720195", "24720196", "24720200", "24720201")

canbics <- cc.data::canbics_data 
canbics <- canbics[canbics$canbics_2021 != -9999.00, ]

canbics <- 
  canbics |> 
  filter(DA_ID %in% !!DA$ID) |> 
  transmute(ID = as.character(DA_ID), canbics_ind_2016 = canbics_2021) |> 
  mutate(canbics_ind_2016 = if_else(ID %in% reserves, NA_real_, canbics_ind_2016))

# Interpolate -------------------------------------------------------------

all_canbics <- 
  interpolate_scales(data = canbics, 
                     base_scale = "DA", 
                     all_tables = all_tables,
                     crs = 32618)


# Calculate breaks --------------------------------------------------------

all_canbics <- calculate_breaks(all_canbics)


# Assign to existing geographies ------------------------------------------

assign_tables(module_tables = all_canbics)


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add to variables table --------------------------------------------------

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(all_canbics$tables_q3, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:3,
                                 .before = canbics_ind_2016)}) |> 
  rename(var = canbics_ind_2016)

# Get breaks_q5
breaks_q5_active <- 
  imap_dfr(all_canbics$tables_q5, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = 2016, rank = 0:5, 
                                 .before = canbics_ind_2016)}) |> 
  rename(var = canbics_ind_2016)

interpolation_keys <- 
  map_chr(set_names(names(all_canbics$tables_list)), ~{
    if (str_detect(.x, "_DA$")) FALSE else "dissemination area"
  })

variables <- 
  variables |>
  add_variables(
    var_code = "canbics_ind",
    var_title = "Can-BICS metric",
    var_short = "Can-BICS",
    explanation = "the bikeway comfort and safety classification system",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = c("2016"),
    scales = names(all_canbics$tables_list),
    breaks_q3 = breaks_q3_active,
    breaks_q5 = breaks_q5_active,
    source = "Meghan Winters at Faculty of Health Sciences, Simon Fraser University",
    interpolated = interpolation_keys)


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(
    id = "canbics",
    metadata = TRUE,
    dataset_info = paste0("<a href = 'https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html'>",
    "Can-BICS, or Canadian Bikeway Comfort and Safety,</a> is a classification ",
    "system for cycling infrastructure in Canada. This system is based on ",
    "three tiers that considers safety and user comfort: high-comfort bikeways, ",
    "medium-comfort bikeways, and low-comfort bikeways."))


# Clean up ----------------------------------------------------------------

rm(canbics, all_canbics, breaks_q3_active, breaks_q5_active, reserves)

# To save output, run dev/build_data.R, which calls this script