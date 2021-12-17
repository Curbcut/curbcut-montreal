#### Census data setup #########################################################

# This script relies on objects created in dev/build_data.R


# Global needed variables and functions -----------------------------------

# Turn on progress bars
progressr::handlers(global = TRUE)

# Turn on parallel processing
library(future)
plan(multisession)

# Lists of scales and years
scales <- c("CSD", "CT", "DA")
years <- c(1996, 2001, 2006, 2011, 2016)

# Load functions
source("dev/modules/census/01_download_prep.R")
source("dev/modules/census/02_interpolate.R")
source("dev/modules/census/03_process_and_breaks.R")
source("dev/modules/census/04_add_variables.R")
source("dev/modules/census/05_add_census_data.R")


# Add census data by topic ------------------------------------------------

source("dev/modules/census/census_housing.R")
source("dev/modules/census/census_income.R")
source("dev/modules/census/census_identity.R")
source("dev/modules/census/census_transport.R")
source("dev/modules/census/census_employment.R")
# source("dev/modules/census/census_family.R")
# source("dev/modules/census/census_language.R")
# source("dev/modules/census/census_age.R")
# source("dev/modules/census/census_education.R")


# Add variable explanations -----------------------------------------------

# var_exp <-
#   add_row(
#     var_code = "inc_median_dollar",
#     var_name = "Median household income ($)",
#     explanation = "median before-tax household income") %>%
#   add_row(
#     var_code = "inc_50_prop",
#     var_name = "Income under $50k (%)",
#     explanation = paste0("the percentage of households with an income less ",
#                          "then $50,000")) %>%
#   add_row(
#     var_code = "inc_100_prop",
#     var_name = "Income beetween $50k-$100k (%)",
#     explanation = paste0("the percentage of households with an income between ",
#                          "$50,000 and $100,000")) %>%
#   add_row(
#     var_code = "inc_high_prop",
#     var_name = "Income above $100k (%)",
#     explanation = paste0("the percentage of households with an income higher ",
#                          "than $100,000")) %>%
#   add_row(
#     var_code = "inc_limat_prop",
#     var_name = "Prevalence of low income (after-tax) (%)",
#     explanation = paste0("the prevalence of low income in private households ",
#                          "based on the Low income measure, after-tax",
#                          "(LIM-AT)")) %>%
#   add_row(
#     var_code = "iden_imm_prop",
#     var_name = "Immigrants (%)",
#     explanation = "the percentage of residents who are foreign-born") %>%
#   add_row(
#     var_code = "iden_imm_new_prop",
#     var_name = "New immigrants (%)",
#     explanation = paste0("the percentage of people who have immigrated in ",
#                          "the last five years")) %>%
#   add_row(
#     var_code = "iden_vm_prop",
#     var_name = "Visible minorities (%)",
#     explanation = paste0("the percentage of people who identify as part ",
#                          "of one or more visible minority groups")) %>%
#   add_row(
#     var_code = "iden_aboriginal_prop",
#     var_name = "Aboriginal (%)",
#     explanation = paste0("the percentage of people who are of ",
#                          "aboriginal identity")) %>%
#   add_row(
#     var_code = "trans_car_prop",
#     var_name = "Drive to work (%)",
#     explanation = paste0("the percentage of people who drive a privately ",
#                          "owned car or truck to work")) %>%
#   add_row(
#     var_code = "trans_walk_or_bike_prop",
#     var_name = "Walk or cycle to work (%)",
#     explanation = "the percentage of people who walk or cycle to work") %>%
#   add_row(
#     var_code = "trans_transit_prop",
#     var_name = "Public transit to work (%)",
#     explanation = paste0("the percentage of people who use public transit to ",
#                          "get to work")) %>%
#   add_row(
#     var_code = "trans_t_15_prop",
#     var_name = "15 minutes to work (%)",
#     explanation = paste0("the percentage of people whose commute time is less ",
#                          "than 15 minutes")) %>%
#   add_row(
#     var_code = "trans_t_45_prop",
#     var_name = "15-45 minutes to work (%)",
#     explanation = paste0("the percentage of people whose commute time is ",
#                          "between 15 and 45 minutes")) %>%
#   add_row(
#     var_code = "trans_t_45_plus_prop",
#     var_name = "More than 45 minutes to work (%)",
#     explanation = paste0("the percentage of people whose commute time is ",
#                          "longer than 45 minutes")) %>%
#   add_row(
#     var_code = "emp_professional_prop",
#     var_name = "Managerial and professional occupations (%)",
#     explanation = paste0("the percentage of the workforce in professional ",
#                          "and managerial occupations, based on the ",
#                          "North American Industry Classification System")) %>%
#   add_row(
#     var_code = "emp_professional_prop",
#     var_name = "Creative occupations (%)",
#     explanation = paste0("the percentage of the workforce in artistic ",
#                          "and cultural occupations, based on the ",
#                          "North American Industry Classification System")) %>%
#   add_row(
#     var_code = "family_children_prop",
#     var_name = "Families with children (%)",
#     explanation = paste0("the percentage of census families with children ",
#                          "out of total households")) %>%
#   add_row(
#     var_code = "family_one_person_prop",
#     var_name = "One person households (%)",
#     explanation = paste0("the percentage of one person households out ",
#                          "of total households")) %>%
#   add_row(
#     var_code = "lang_french_only_prop",
#     var_name = "French only (%)",
#     explanation = paste0("the percentage of individuals that only ",
#                          "know French as an official language")) %>%
#   add_row(
#     var_code = "lang_eng_only_prop",
#     var_name = "English only (%)",
#     explanation = paste0("the percentage of individuals that only ",
#                          "know English as an official language")) %>%
#   add_row(
#     var_code = "lang_french_eng_prop",
#     var_name = "French and English (%)",
#     explanation = paste0("the percentage of individuals that know both ",
#                          "official languages (French and English)")) %>%
#   add_row(
#     var_code = "lang_no_official_prop",
#     var_name = "Neither French nor English (%)",
#     explanation = paste0("the percentage of individuals that do not ",
#                          "know either of the official languages ",
#                          "(French or English)")) %>%
#   add_row(
#     var_code = "age_0_14_prop",
#     var_name = "Aged between 0 and 14 (%)",
#     explanation = paste0("the percentage of the population aged between ",
#                          "0 and 14 years old")) %>%
#   add_row(
#     var_code = "age_15_64_prop",
#     var_name = "Aged between 15 and 64 (%)",
#     explanation = paste0("the percentage of the population aged between ",
#                          "15 and 64 years old")) %>%
#   add_row(
#     var_code = "age_65_plus_prop",
#     var_name = "Aged 65 and above (%)",
#     explanation = paste0("the percentage of the population aged 65 ",
#                          "and above")) %>%
#   add_row(
#     var_code = "edu_bachelor_above_prop",
#     var_name = "Bachelor and above (%)",
#     explanation = paste0("the percentage of the population aged 15 ",
#                          "and over holding a degree at bachelor level ",
#                          "or above")) %>%
#   add_row(
#     var_code = "edu_no_degree_prop",
#     var_name = "No certificate, diploma or degree (%)",
#     explanation = paste0("the percentage of the population aged 15 ",
#                          "and over with no certificate, diploma or degree"))


# Cleanup -----------------------------------------------------------------

# rm(newest_census_year, vars_to_remove, census_retrieval)


# To save output, run dev/build_geometries.R, which calls this script
