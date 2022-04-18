
## NATURAL INFRASTRUCTURE GLOBALS ##############################################


# Dropdown list -----------------------------------------------------------

vars_natural_inf_left <- 
  list("Conservation prioritization" = "c_priority",
       "Contribution to flood prevention" = "c_flood", 
       "Contribution to biodiversity conservation" = "c_bio",
       "Contribution to heat island reduction" = "c_heat")

vars_natural_inf_left_c_bio <- 
  list("----" = " ",
       "Habitat quality" = "habitat_qual", 
       "Habitat connectivity" = "habitat_con", 
       "Favorable climatic conditions" = "favorable_cc")

vars_natural_inf_left_c_flood <- 
  list("----" = " ",
       "Flood risk areas" = "flood")

vars_natural_inf_left_c_heat <- 
  list("----" = " ",
       "Heat islands" = "heat",
       "Cool islands" = "cool")


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