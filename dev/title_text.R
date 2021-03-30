#### title_text data setup #####################################################

library(tidyverse)
library(qs)

title_text <- 
  read_csv("dev/data/title_text.csv")

title_text <-
  title_text %>%
  add_row(tab = "ped", type = "title",
          text = "Pedestrian capacity for social distancing", .before = 6) %>% 
  mutate(tab = if_else(tab == "pedestrian_ct", "ped", tab))

qsave(title_text, "data/title_text.qs")
