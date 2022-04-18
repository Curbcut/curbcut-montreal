
## NATURAL INFRASTRUCTURE GLOBALS ##############################################


# Dropdown list -----------------------------------------------------------

vars_natural_inf_left <- 
  list("Conservation prioritization" = "conservation_prioritization",
       "Contribution to flood prevention" = "ni_contribution_flood_prevention", 
       "Contribution to biodiversity conservation" = "ni_contribution_biodiversity_conservation",
       "Contribution to heat island reduction" = "ni_contribution_heat_island_reduction")

vars_natural_inf_left_ni_contribution_biodiversity_conservation <- 
  list("----" = " ",
       "Habitat quality" = "habitat_quality", 
       "Habitat connectivity" = "habitat_connectivity", 
       "Favorable climatic conditions" = "favorable_climatic_conditions")

vars_natural_inf_left_ni_contribution_flood_prevention <- 
  list("----" = " ",
       "Flood risk areas" = "flood_risks")

vars_natural_inf_left_ni_contribution_heat_island_reduction <- 
  list("----" = " ",
       "Heat islands" = "heat_islands",
       "Cool islands" = "cool_islands")

natural_inf_tiles <- 
  list("habitat_quality" = 1,
       "habitat_connectivity" = 2,
       "favorable_climatic_conditions" = 3,
       "ni_contribution_flood_prevention" = 4,
       "ni_contribution_biodiversity_conservation" = 5,
       "ni_contribution_heat_island_reduction" = 6,
       "conservation_prioritization" = 7,
       "flood_risks" = 8,
       "heat_islands" = 9,
       "cool_islands" = 10)


# Variable explanation ----------------------------------------------------

# natural_inf_explanations <-
#   list("habitat_quality" =
#          paste0("Habitat quality was mapped based on the <a href = 'https://mf",
#                 "fp.gouv.qc.ca/documents/forets/inventaire/norme-cartographie-",
#                 "ecoforestiere.pdf' target = '_blank'>3rd Quebec ecoforestry ",
#                 "inventory</a> and was complemented by data from the ",
#                 "<a href = 'https://www.donneesquebec.ca/recherche/dataset/car",
#                 "tes-topographiques-a-l-echelle-de-1-20-000' target = '_blank'",
#                 ">provincial topographic database</a>."),
#        "habitat_connectivity" = 
#          paste0("Connectivity was assessed for the year 2000 based on analysis ",
#                 "of habitat quality and various <a href = 'https://besjournals.onlinel",
#                 "ibrary.wiley.com/doi/10.1111/2041-210X.12470' target = '_blank'>meas",
#                 "ures of short and long connectivity</a>. The highest values are non-",
#                 "habitat locations and represent areas of resistance to species movem",
#                 "ent where movement is difficult or threatens their survival, such as",
#                 " roads or built-up areas. The lowest values are areas known as speci",
#                 "es habitat, which are areas where species movement is facilitated."),
#        "favorable_climatic_conditions" = 
#          paste0("The climatic conditions are a synthesis of different climate ",
#                 "scenarios that were modelled under the 1971-2000 climate ",
#                 "conditions in eastern America based on mean annual ",
#                 "temperature and mean annual precipitation. The model was then ",
#                 "used to predict the future climate conditions for the <a href ",
#                 "= 'https://fr.davidsuzuki.org/publication-scientifique/le-role",
#                 "-des-infrastructures-naturelles-dans-la-prevention-des-inonda",
#                 "tions-dans-la-communaute-metropolitaine-de-montreal/' target ",
#                 "= '_blank'>studied species</a> in the years 2015 and 2050 under ",
#                 "different scenarios representative of climate change."))