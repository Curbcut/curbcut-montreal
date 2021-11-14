### GREEN ALLEY MODULE GLOBALS #################################################

# Map token
token_alley <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                      "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
width_alley_higher_zoom <- 75
rv_alley <- reactiveValues(poly_selected = NA, zoom = width_alley_higher_zoom)

# Dropdown menu
var_list_alley <- 
  list("----" = " ", 
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_prop_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop"),
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
       "Immigration" = list(
         "Immigrants (%)" =  "imm_prop",
         "New immigrants (%)" = "imm_new_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"))


# Fill color --------------------------------------------------------------

alleys <- 
alleys %>% 
  mutate(fill = case_when(type == "green" ~ "#008100EE",
                         type == "community" ~ "#F6BE00EE",
                         type == "mixed" ~ "#B37400EE",
                         type == "none" ~ "#262626EE",
                         TRUE ~ NA_character_))

# Legend ------------------------------------------------------------------

alley_legend_en <- 
  mapdeck_legend(
    legend_element(
      variables = c("Green", "Community", "Mixed", "None"),
      colours = c("#008100EE","#F6BE00EE", "#B37400EE", "#262626EE"),
      colour_type = "fill",
      variable_type = "category",
      title = "Green alley type")
  )
