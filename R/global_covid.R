### COVID MODULE GLOBALS #################################################

# Map token
token_covid <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                      "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initial zoom
map_zoom_covid <- 11

# Initialize reactive values
rv_covid <- reactiveValues(path_selected = NA, point_selected = NA, 
                           zoom = "borough")

# Dropdown menu
var_list_covid <- 
  list("May 2020" = "may_2020",
       "July 2020" = "july_2020",
       "October 2020" = "oct_2020")

covid_legend_plot <- 
  covid |> 
  distinct(type, fill) |> 
  mutate(x = 1:9,
         y = 1:9) |> 
  rbind(covid |> 
          distinct(type, fill) |> 
          mutate(x = 1:9,
                 y = 1:9)) |> 
  ggplot() +
  geom_line(aes(x,y, color = type), size = 2) +
  scale_color_manual(name = NULL,
                     values = c("Circuit des voies actives \net sécuritaires" = "#FF5733FF",
                                "Corridor piéton élargi" = "#5F940EFF",
                                "Corridor projeté" = "#10A9A7FF",
                                "File d'attente encadrée" = "#2D80CAFF",
                                "Rue partiellement fermée" = "#75BB79FF",
                                "Rue familiale et active" = "#FF7C2DFF",
                                "Rue fermée" = "#6F2094FF",
                                "Circulation locale" = "#FFD733FF",
                                "Rue partagée" = "#75A7BAFF")) +
  theme_void()

covid_legend <- cowplot::get_legend(covid_legend_plot)
