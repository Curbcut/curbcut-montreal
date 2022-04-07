# #### PERMITS EXPLORE MODULE ###############################################
# 
# permits_explore_graph <- function(id, x, var_left, select_id, ...) {
#   
#   moduleServer(id, function(input, output, session) {
#     reactive({
#       
#       type <- unique(str_extract(var_left(), "(?<=permits_).*(?=_count)"))
#       time <- unique(str_extract(var_left(), "(?<=count_).*"))
#       
#       cat <- if (type == "total" && is.na(select_id())) {
#         "total"
#       } else if (type != "total" && is.na(select_id())) {
#         "uni_type"
#       } else if (!is.na(select_id())) {
#         "selected"
#       }
#       
#       if (cat == "total") {
#         z <- x() |> 
#           mutate(type =
#                    case_when(type == "combination" ~ "Combin.",
#                              type == "conversion" ~ "Conver.",
#                              type == "demolition" ~ "Demo.",
#                              type == "new_construction" ~ "New con.",
#                              type == "renovation" ~ "Reno.")) |> 
#           filter(!is.na(ID))
#         
#         color_val <- 
#           z |> 
#           group_by(type) |> 
#           slice(1)
#         col <- color_val$fill
#         names(col) <- color_val$type
#         
#         ggplot(z, aes(type)) +
#           geom_bar(aes(fill = type)) +
#           scale_fill_manual(values = col) +
#           scale_y_log10() +
#           theme_minimal() +
#           theme(legend.position = "none", panel.grid.minor.x = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.minor.y = element_blank(),
#                 axis.title = element_text(size = 8))
#         
#       } else if (cat == "uni_type" && length(time) == 2) {
#         z <- x() |> 
#           count(year)
#         
#         ggplot(z, aes(year, n)) +
#           geom_line(colour = colour_bivar$fill[5]) +
#           stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
#                       formula = y ~ x, colour = colour_bivar$fill[9]) +
#           labs(y = "Count", x = NULL) +
#           theme_minimal() +
#           theme(legend.position = "none", panel.grid.minor.x = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.minor.y = element_blank(),
#                 axis.title = element_text(size = 8))
#       }
#       
#     })
#   })
# }
