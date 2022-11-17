### HOUSING AFFORDABILITY GLOBALS ##############################################

# Grouping
var_left_list_1_afford <- 
  list("Households" = "cent_d", 
       "Individuals" = "cent_p")

var_left_list_2_afford <-
  list("Total" = "total",
       "Spending >30% of income on shelter" = "more_30_per",
       "Spending >50% of income on shelter" = "more_50_per",
       "Spending >80% of income on shelter" = "more_80_per")


# If households is selected
var_left_list_3_afford <-
  list("Total" = "total",
       "Tenant" = "tenant",
       "Owner" = "owner")

var_left_list_4_afford <-
  list("Total" = "total",
       "Family with 3+ children" = "kids_3_plus",
       "Unsuitable housing" = "unsuitable",
       "Major repairs needed" = "repairs",
       "Low income" = "low_inc")


# If individuals is selected
var_left_list_5_afford <-
  list("Total" = "total",
       "Female" = "female",
       "Male" = "male")

var_left_list_6_afford <-
  list("Total" = "total", 
       "Immigrants" = "immigrants",
       "Non-immigrants" = "non_immigrants")

var_left_list_7_afford <-
  list("Total" = list("Total" = "total"),
       "Immigration characteristic" = list(
         "Immigrated before 2001" = "before_2001",
         "Immigrated between 2001 and 2010" = "2001_to_2010",
         "Immigrated between 2011 and 2016" = "2011_to_2016",
         "Economic immigrants" = "eco_imm",
         "Immigrants sponsored by family" = "sponsored_imm",
         "Refugees" = "refugees_imm",
         "Other immigrants" = "other_imm"),
       "Visible minority / Indigenous" =   list(
         "Visible minority" = "visible_min",
         "Does not belong to a visible minority group" = "not_visible_min",
         "Aboriginal" = "aboriginal"),
       "Family characteristic" = list(
         "Lone parent (lone-parent family)" = "lone_parents",
         "Individual living alone" = "living_alone",
         "Low income after tax" = "low_inc"))

# Disabled options - immigrants not selected
vars_afford_add_dis_nimm <- 
  unlist(var_left_list_7_afford) %in% 
  c(var_left_list_7_afford[["Immigration characteristic"]])

# Disabled options - immigrants selected
vars_afford_add_dis_imm <- 
  unlist(var_left_list_7_afford) %in% "aboriginal"