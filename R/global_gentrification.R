### GI MODULE GLOBALS ######################################################

# Map token
token_gentrification <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                       "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
gentrification_zoom <- c("borough" = 0, "CT" = 10.5, "DA" = 12, "building" = 14)

rv_gentrification <- reactiveValues(poly_selected = NA)

# Time slider values
gentrification_slider <- list(
  min = 1996,
  max = 2016,
  interval = 5,
  init = c(2006,2016))

# Left-side dropdown menu
var_list_left_gentrification <- 
  list(#"----" = " ", 
    "Bachelor and above (%)" = "edu_bachelor_above_prop", 
    "Median household income ($)" = "inc_median_dollar", 
    "Average property value ($)" = "housing_value_avg_dollar",
    "Average rent ($)" = "housing_rent_avg_dollar", 
    "Tenant-occupied (%)" = "housing_tenant_prop", 
    "Visible minorities (%)" = "iden_vm_prop")

# Compare dropdown menu

# The commenting are for variables only available in one census.
# While this module only compares between two census years
var_list_right_gentrification <- 
  list("----" = " ", 
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
         # "Prevalence of low income (after-tax) (%)" = "inc_limat_prop"),
       "Education and occupation" = list(
         "Bacholar degree and above (%)" = "edu_bachelor_above_prop",
         "Professional occupation (%)" = "emp_professional_prop",
         "Creative occupation (%)" = "emp_creative_prop"),
       # "Family structure" = list(
       #   "Family with child (%)" = "family_children_prop",
       #   "Single person family (%)" = "family_one_person_prop"),       
       "Immigration and ethnicity" = list(
         "Immigrants (%)" =  "iden_imm_prop",
         "New immigrants (%)" = "iden_imm_new_prop",
         # "Visible minorities (%)" = "iden_imm_vm_prop",
         "Indeginous population (%)" = "iden_aboriginal_prop"),
       # "Language used" = list(
       #   "French only (%)" =  "lang_french_only_prop",
       #   "English only (%)" = "lang_eng_only_prop",
       #   "Both french and English (%)" = "lang_french_eng_prop",
       #   "Neither french and English (%)" = "lang_no_official_prop"),
       # "Living environment" = list(
       # "Active living potential (CanALE Index)" = "canale_ind"),
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         # "Average property value ($)" = "housing_prop_value_avg_dollar",
         # "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop",
         "Moved within 1 years(%)" = "housing_mobility_one_prop",
         "Moved within 5 years(%)" = "housing_mobility_five_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop"))
         # "15 minutes to work (%)" = "trans_t_15_prop",
         # "15-45 minutes to work (%)" = "trans_t_45_prop",
         # "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"))


# var_right_gentrification_shared <- 
#   borough %>% 
#   st_drop_geometry() %>% 
#   select(contains(all_of(as.character(unlist(var_list_right_gentrification)))),
#          -contains("_q3")) %>% 
#   names() %>% 
#   str_remove(., "_\\d{4}$") %>% 
#   as_tibble() %>% 
#   count(value) %>% 
#   filter(n == max(n)) %>% 
#   pull(value)
# 
# disabled_var_list_gentrification_right <- 
#   (!unlist(var_list_right_gentrification) %in% var_right_gentrification_shared) %>% 
#   replace(1, FALSE)
