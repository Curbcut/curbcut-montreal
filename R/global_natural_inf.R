
## NATURAL INFRASTRUCTURE GLOBALS ##############################################


# Dropdown list -----------------------------------------------------------

vars_natural_inf_left <- 
  list("Conservation priority" = "c_priority",
       "Biodiversity" = "c_bio",
       "Flood" = "c_flood", 
       "Heat island" = "c_heat")

vars_natural_inf_left_c_bio <- 
  list("Contribution to biodiversity conservation" = " ",
       "Habitat quality" = "habitat_qual", 
       "Habitat connectivity" = "habitat_con", 
       "Favourable climatic conditions" = "favorable_cc")

vars_natural_inf_left_c_flood <- 
  list("Contribution to flood prevention" = " ",
       "Flood risk areas" = "flood")

vars_natural_inf_left_c_heat <- 
  list("Contribution to heat island reduction" = " ",
       "Heat islands" = "heat",
       "Cool islands" = "cool")

custom_slider_choices <- 
  c("Not important", "Somewhat important", 
    "Important", "Very important", 
    "Extremely important")