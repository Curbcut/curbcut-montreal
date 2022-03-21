# #### MARKET SUSTAINABILITY EXPLORE MODULE ######################################
# 
# marketed_sustainability_explore_graph <- function(id, x, select_id, ...) {
#   
#   moduleServer(id, function(input, output, session) {
#     reactive({
# 
#       col_fill <- x |> arrange(sustainability_prop) |> pull(fill)
#       
#       if (is.na(select_id)) {
#         ggplot(x, aes(sustainability_prop)) +
#           geom_histogram(aes(fill = fill), alpha = 0.5, bins = 25) +
#           scale_fill_manual(values = rev(col_fill), na.translate = FALSE) +
#           labs(x = "Prop. of sustainability-related words", y = NULL) + 
#           theme_minimal() +
#           theme(legend.position = "none", panel.grid.minor.x = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.minor.y = element_blank(),
#                 axis.title = element_text(size = 8))
#       } else {
# 
#         ggplot(x, aes(sustainability_prop)) +
#           geom_histogram(aes(fill = round(sustainability_prop, digits = 2) == 
#                                round(sustainability_prop[ID == select_id],
#                                      digits = 2)),
#                          bins = 25) +
#           scale_fill_manual(values = col_fill[c(1, length(col_fill))], 
#                             na.translate = FALSE) +
#           labs(x = "Prop. of sustainability-related words", y = NULL) + 
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
