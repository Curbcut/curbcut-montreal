### CRASH MODULE GLOBALS #######################################################

# Map token
map_zoom_crash_levels = c("borough" = 0, "CT" = 10.5, "DA" = 12, "street" = 14)

# Time slider values
crash_slider <- list(
  min = lubridate::year(min(crash$date)),
  max = lubridate::year(max(crash$date)),
  interval = 1,
  init = lubridate::year(max(crash$date)))

# Dropdown menu
var_list_left_crash_1 <- 
  list("Count" = "count",
       "Per sq km" = "sqkm",
       "Per 1,000 residents" = "per1k")

var_list_left_crash_2 <- 
  list("Total" = "total",
       "Pedestrian" = "ped",
       "Cyclist" = "cyc",
       "Other" = "other")

var_list_right_crash <- 
  list("----" = " ", 
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop"),
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
       "Immigration and ethnicity" = list(
         "Immigrants (%)" =  "iden_imm_prop",
         "New immigrants (%)" = "iden_imm_new_prop",
         "Visible minorities (%)" = "iden_vm_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"),
       "Employment" = list(
         "Managerial and professional occupations (%)" = "emp_professional_prop",
         "Creative occupations (%)" = "emp_professional_prop"),
       "Family" = list(
         "Families with children (%)" = "family_children_prop",
         "One person households (%)" = "family_one_person_prop"),
       "Language" = list(
         "French only (%)" = "lang_french_only_prop",
         "English only (%)" = "lang_eng_only_prop",
         "French and English (%)" = "lang_french_eng_prop",
         "Neither French nor English (%)" = "lang_no_official_prop"),
       "Age" = list(
         "Aged between 0 and 14 (%)" = "age_0_14_prop",
         "Aged between 15 and 64 (%)" = "age_15_64_prop",
         "Aged 65 and above (%)" = "age_65_plus_prop"),
       "Education" = list(
         "Bachelor and above (%)" = "edu_bachelor_above_prop",
         "No certificate, diploma or degree (%)" = "edu_no_degree_prop"))

# Mapdeck point cloud legend
crash_legend_en <- 
  mapdeck_legend(
    legend_element(
      variables = c("Pedestrian", "Cyclist", "Other", "Unknown"),
      colours = c("#91BD9AEE","#6C83B5EE", "#F39D60EE", "#E8E8E8EE"),
      colour_type = "fill",
      variable_type = "category",
      title = "Crash type")
  )

crash_legend_fr <- 
  mapdeck_legend(
    legend_element(
      variables = c("Piéton", "Cycliste", "Autre", "Inconnu"),
      colours = c("#91BD9AEE","#6C83B5EE", "#F39D60EE", "#E8E8E8EE"),
      colour_type = "fill",
      variable_type = "category",
      title = "Type d'accident")
  )
