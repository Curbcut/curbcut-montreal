# ### PERMITS MODULE GLOBALS #####################################################
# 
# # Time slider values
# permits_slider <- list(
#   min = 1990,
#   max = 2021,
#   interval = 1,
#   init = 2021)
# 
# # Dropdown menu
# var_left_list_1_permits <- 
#   list("Count" = "count",
#        "Per sq km" = "sqkm",
#        "Per 1,000 residents" = "per1k")
# 
# var_left_list_2_permits <- 
#   list("Total" = "total",
#        "Dwellings combination" = "combination",
#        "Condo conversion" = "conversion",
#        "Demolition" = "demolition",
#        "New construction" = "new_construction",
#        "Renovation" = "renovation")
# 
# # Mapdeck point cloud legend
# permits_legend_en <- 
#   mapdeck_legend(
#     legend_element(
#       variables = c("Dwellings combination", "Condo conversion", "Demolition", 
#                     "New construction", "Renovation"),
#       colours = c("#008533EE","#F59600EE", "#7C0082EE", "#992400EE", 
#                   "#0E6399EE"),
#       colour_type = "fill",
#       variable_type = "category",
#       title = "Permits type")
#   )
# 
# # permits_legend_fr <- 
# #   mapdeck_legend(
# #     legend_element(
# #       variables = c("Piéton", "Cycliste", "Autre", "Inconnu"),
# #       colours = c("#91BD9AEE","#6C83B5EE", "#F39D60EE", "#E8E8E8EE"),
# #       colour_type = "fill",
# #       variable_type = "category",
# #       title = "Type d'accident")
# #   )
