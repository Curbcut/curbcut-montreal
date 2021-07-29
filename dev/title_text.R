#### title_text data setup #####################################################

library(tidyverse)
library(qs)

title_text <- 
  read_csv("dev/data/title_text.csv")

title_text <-
  title_text %>%
  add_row(tab = "ped", type = "title",
          text = "Pedestrian capacity for social distancing", .before = 6) %>% 
  add_row(tab = "housing", type = "title",
          text = "The housing realm", .before = 6) %>% 
  add_row(tab = "housing", type = "main",
          text = "Housing is important", .before = 6) %>% 
  add_row(tab = "housing", type = "extra",
          text = "VERY important", .before = 6) %>% 
  mutate(tab = if_else(tab == "pedestrian_ct", "ped", tab)) %>% 
  add_row(tab = "alley", type = "title",
          text = "Green alleys") %>% 
  add_row(tab = "alley", type = "main",
          text = paste0("Green alleys consist in the transformation of back ",
                        "alleys into spaces that improve people’s living ",
                        "environments, on an environmental and social level. ",
                        "They are presented as safer and healthier spaces. ",
                        "When adequately designed, these public spaces can ", 
                        "help reduce heat island effects, noise and air pollution, and improve water management. They can also enhance social interactions and become recreational spaces for children where car circulation is reduced.")) %>% 
  add_row(tab = "alley", type = "extra",
          text = "<p>The Green Alley Program currently in place in Montreal was implemented in 1995. In August 2020, there were a total of 454 green alleys in the city, and others will be created in 2021. First put in place in Plateau-Mont-Royal and Rosemont-La-Petite-Patrie, the program was then extended to most of Montreal’s boroughs. The program promotes public participation and citizen governance: the initiative to create a green alley comes from the residents themselves. The Ecoquartiers (most boroughs have at least one of them) are key actors in green alley development: they receive, select the applications, and follow the evolution of the projects. <p>The Green Alley dataset is a mix of data from the City of Montreal Open Data Portal, from boroughs’ Eco-quartiers, newspaper articles and personal observations (56 alleys were visited and classified). No complete list of green alleys in Montreal exists. <p>Green alleys are diverse in activities, design, dimension, and dwellings density in the surrounding area. Within that diversity, we classify four types of alleys: <ul> <li>Green alleys: most of the area includes greenery such as permeable surfaces, gardens, and green walls that provide environmental benefits like cooling the street temperature, filtrating rainwater, and contributing to enriching the biodiversity of the alleys. They are generally closed to transit. <li>Community-oriented alleys: their main characteristic is the existence of gathering spaces that propel community life. This alley type presents activities organized by the residents, the appropriation of the alley with furniture and art, and play areas for children. <li>Mixed alleys: they include green elements and are also spaces that allow diverse community activities, especially interventions that create safe areas for children, such as measures that reduce car traffic and, in some cases, parking. <li>Neither green nor community-oriented alleys: they are abandoned or used as parking spaces or as ways to access private garages, without any environmental or social-community benefit.") %>% 
  add_row(tab = "covid", type = "title",
          text = "2020 and 2021 Covid interventions") %>% 
  add_row(tab = "covid", type = "main",
          text = "Text TKTK") %>% 
  add_row(tab = "covid", type = "extra",
          text = "Text TKTK")
  
qsave(title_text, "data/title_text.qs")
