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
                        "help reduce heat island effects, noise and air ",
                        "pollution, and improve water management. They can ",
                        "also enhance social interactions and become ",
                        "recreational spaces for children where car ",
                        "circulation is reduced.")) %>% 
  add_row(tab = "alley", type = "extra",
          text = paste0("<p>The Green Alley Program currently in place in ", 
                        "Montreal was implemented in 1995. In August 2020, ",
                        "there were a total of 454 green alleys in the city, ",
                        "and others will be created in 2021. First put in place in Plateau-Mont-Royal and Rosemont-La-Petite-Patrie, the program was then extended to most of Montreal’s boroughs. The program promotes public participation and citizen governance: the initiative to create a green alley comes from the residents themselves. The Ecoquartiers (most boroughs have at least one of them) are key actors in green alley development: they receive, select the applications, and follow the evolution of the projects. <p>The Green Alley dataset is a mix of data from the City of Montreal Open Data Portal, from boroughs’ Eco-quartiers, newspaper articles and personal observations (56 alleys were visited and classified). No complete list of green alleys in Montreal exists. <p>Green alleys are diverse in activities, design, dimension, and dwellings density in the surrounding area. Within that diversity, we classify four types of alleys: <ul> <li>Green alleys: most of the area includes greenery such as permeable surfaces, gardens, and green walls that provide environmental benefits like cooling the street temperature, filtrating rainwater, and contributing to enriching the biodiversity of the alleys. They are generally closed to transit. <li>Community-oriented alleys: their main characteristic is the existence of gathering spaces that propel community life. This alley type presents activities organized by the residents, the appropriation of the alley with furniture and art, and play areas for children. <li>Mixed alleys: they include green elements and are also spaces that allow diverse community activities, especially interventions that create safe areas for children, such as measures that reduce car traffic and, in some cases, parking. <li>Neither green nor community-oriented alleys: they are abandoned or used as parking spaces or as ways to access private garages, without any environmental or social-community benefit.")) %>% 
  add_row(tab = "covid", type = "title",
          text = "2020 and 2021 Covid interventions") %>% 
  add_row(tab = "covid", type = "main",
          text = paste0("The onset of the COVID-19 pandemic prompted municipalities ",
                        "across the world to restrict public transit systems and advise citizens ",
                        "to make only necessary trips. For many without access to motorized vehicles, ",
                        "active transportation, walking and cycling, became the primary mode of transport. ",
                        "To accommodate the increased demand for these modes of safe travel, and to ensure ",
                        "the possibility to practice physical distancing while travelling, many cities ",
                        "made rapid changes to cycling and pedestrian networks, including constructing ",
                        "new bike lanes, creating temporary infrastructure, and shutting down streets ",
                        "to motor vehicle traffic. These changes served to minimize viral transmission ",
                        "during travel and to more safely connect people to essential services, health ",
                        "care, and greenspace.")) %>% 
  add_row(tab = "covid", type = "extra",
          text = paste0("In May 2020, the City of Montréal announced plans to establish ",
                        "over 300 kilometers of active transport infrastructure by the ",
                        "end of the summer. These plans included six distinct types of ",
                        "street changes: active transportation circuits, family and active ",
                        "streets, partially closed streets, closed streets, expanded ",
                        "pedestrian corridors, and planned corridors. The active ",
                        "transportation circuits are intended to link green spaces, ",
                        "provide increased access to parks for densely populated areas, ",
                        "and increase access to local commerce, while promoting physical ",
                        "distancing during travel. Family and active streets define ",
                        "portions of the street which have been closed to cars entirely ",
                        "to encourage physical distancing while residents spend time outdoors. ",
                        "These streets were chosen based on proximity to green space and ",
                        "population density. While some streets have partially closed to ",
                        "traffic, pedestrianizing a portion of the available car lanes, ",
                        "other streets have been closed entirely to motor vehicles to maximize ",
                        "pedestrian space. Expanded pedestrian corridors are locations where ",
                        "sidewalks have been widened in order to allow greater capacity for ",
                        "physical distancing while walking. The last group of street changes ",
                        "are labelled as planned corridors and  encompass all types of street ",
                        "changes, not specifically defined by the city.<p> The City of Montréal ",
                        "made multiple revisions to the plans over the course of the summer ",
                        "of 2020. The initial plans released in May were ambitious, providing ",
                        "cycling and pedestrian infrastructure to nearly 30% of the city ",
                        "population and envisioning a cohesive city-wide network of health ",
                        "corridors supplemented by local, small-scale interventions to support ",
                        "physical distancing. By July the plans were reduced from 311 kilometers ",
                        "to 104 kilometers and by fall just 80 kilometers, primarily due to the ",
                        "removal of nearly all the active transportation circuits. These circuits ",
                        "were intended to provide an alternative to public transit, creating a ",
                        "comprehensive system connecting Montreal's various boroughs.<p> The City ",
                        "of Montreal has announced more public space interventions for summer ",
                        "2021 in response to the lasting effects of COVID-19 and ongoing demands ",
                        "for greater physical distancing capacity in dense urban areas. However, ",
                        "rather than prioritizing mobility and access to essential services as in ",
                        "the previous year, the rollout of vaccines and the gradual easing of ",
                        "restrictions has allowed for the city's scope to narrow.  The 2021 plans ",
                        "consist of pedestrianization interventions on 13 commercial streets around ",
                        "Montréal, an effort to revitalize the local economy and provide safer public ",
                        "spaces for comfortable shopping. The implementation of pedestrian-only zones ",
                        "on streets with high concentrations of restaurants, bars, and other retail ",
                        "shops creates appealing destinations for Montréal residents and promotes ",
                        "recovery for the businesses which have been affected by COVID-19."))
  
qsave(title_text, "data/title_text.qs")
