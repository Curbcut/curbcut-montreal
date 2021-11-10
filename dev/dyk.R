#### Did-you-know data setup ###################################################

library(tidyverse)
library(qs)

dyk <-
  read_csv("dev/data/dyk.csv") %>%
  mutate(across(c(left_variable, right_variable), tolower)) %>% 
  mutate(left_variable = str_replace(left_variable, "transit", "transport")) %>% 
  mutate(left_variable = str_replace(left_variable, "ale_index", "canale_ind"))

dyk <- 
  dyk %>% 
  mutate(
    cat_1 = case_when(
      left_variable == "canale_ind" ~ "canale",
      left_variable == "ct_ped" ~ "ped",
      left_variable == "da_ped" ~ "ped",
      left_variable == "sidewalk_ped" ~ "ped",
      TRUE ~ left_variable),
    cat_2 = case_when(
      str_detect(right_variable, "housing") ~ "housing",
      str_detect(right_variable, "income") ~ "income",
      str_detect(right_variable, "immigrant") ~ "immigration",
      str_detect(right_variable, "transport") ~ "transport",
      TRUE ~ NA_character_))

category_table <- 
  dyk %>% 
  select(category = cat_1, variable = left_variable) %>% 
  bind_rows(dyk %>% select(category = cat_2, variable = right_variable)) %>% 
  distinct() %>% 
  na.omit()

dyk <- 
  dyk %>% 
  mutate(
    category = map2(cat_1, cat_2, c),
    category = map(category, na.omit),
    category = map(category, sort),
    .before = left_variable) %>% 
  filter(category != "ped") %>% 
  select(-cat_1, -cat_2)

dyk <- 
  dyk %>% 
  mutate(left_variable = if_else(left_variable == "canale_ind", left_variable,
                                 NA_character_))

dyk <- 
  dyk %>% 
  mutate(variable = map2(left_variable, right_variable, c),
         variable = map(variable, na.omit),
         variable = map(variable, sort),
         .before = left_variable) %>% 
  select(-left_variable, -right_variable)

dyk <- 
  dyk %>% 
  add_row(category = list("covid"), variable = list("covid_may_2020"),
          text = paste0("The city's initial plans were released in May 2020 and ", 
                        "included over 200 kilometers of active transportation ", 
                        "infrastructure.")) %>%
  add_row(category = list("covid"), variable = list("covid_may_2020"),
          text = paste0("The circuits present in the May plans would have ",
                        "connected Montreal-Nord and Ahuntsic-Cartierville, ",
                        "two of the city's most socioeconomically disadvantaged ",
                        "boroughs.")) %>%
  add_row(category = list("covid"), variable = list("covid_may_2020"),
          text = paste0("Visible minority, Black, immigrant, and elderly ",
                        "populations were consistently underrepresented in those ",
                        "with direct access to the infrastructure.")) %>%
  add_row(category = list("covid"), variable = list("covid_july_2020"),
          text = paste0("The city's revised plans, released in July 2020, were ",
                        "reduced by nearly 90 kilometers, bringing the total ",
                        "intervention length to 122 kilometers.")) %>%  
  add_row(category = list("covid"), variable = list("covid_july_2020"),
          text = paste0("With reductions made in the intervention plans over the ",
                        "course of summer 2020, a disproportionate decline in access  ",
                        "was observed for racialized, immigrant, and elderly ",
                        "populations.")) %>% 
  add_row(category = list("covid"), variable = list("covid_july_2020"),
          text = paste0("White and low income folkss were overrepresented in the ",
                        "populations with access to the infrastructure and also the ",
                        "least affected by the city's decision to reduce the ",
                        "scope of the plans.")) %>% 
  add_row(category = list("covid"), variable = list("covid_oct_2020"),
          text = paste0("By October 2020, the city had reduced the plans to ",
                        "just 80 kilometers of active transportation ",
                        "interventions.")) %>% 
  add_row(category = list("covid"), variable = list("covid_oct_2020"),
          text = paste0("The City of Montreal did not make any statements ",
                        "concerning these dramatic reductions.")) %>% 
  add_row(category = list("covid"), variable = list("covid_2021"),
          text = paste0("The City of Montreal released plans in 2021 to ",
                        "pedestrianize 13 major commercial streets.")) %>%
  add_row(category = list("covid"), variable = list("covid_2021"),
          text = paste0("The dominant retail on the pedestrianized streets is ",
                        "restaurants and bars, making up just over 40%.")) %>%
  add_row(category = list("covid"), variable = list("covid_2021"),
          text = paste0("The city received backlash from local merchants ",
                        "concerning the 2020 pedestrian interventions, prompting ",
                        "the prioritization of their interests in the 2021 plans.")) %>%
  add_row(category = list("crash"), variable = list("crash"),
          text = paste0("Since 2012, total collisions on the Island of Montreal ",
                        "have been decreasing.")) %>% 
  add_row(category = list("crash"), variable = list("crash"),
          text = paste0("The majority of crashes in Montreal involve a collision ",
                        "between 2 or more motor vehicles.")) %>% 
  add_row(category = list("crash"), variable = list("crash_total_2019"),
          text = paste0("In 2019, there were 19,296 total collisions on the ",
                        "Island of Montreal. That's on average 52 crashes per ",
                        "day.")) %>% 
  add_row(category = list("crash"), variable = list("crash_total_2018"),
          text = paste0("In 2018, there were 21,379 total collisions on the ",
                        "Island of Montreal. That's on average 58 crashes per ",
                        "day.")) %>% 
  add_row(category = list("crash"), variable = list("crash_total_2017"),
          text = paste0("In 2017, there were 21,974 total collisions on the ",
                        "Island of Montreal. That's on average 60 crashes per ",
                        "day.")) %>% 
  add_row(category = list("crash"), variable = list("crash_total_2016"),
          text = paste0("In 2016, there were 21,668 total collisions on the ", 
                        "Island of Montreal. That's on average 59 crashes per ",
                        "day.")) %>%
  add_row(category = list("crash"), variable = list("crash_total_2015"),
          text = paste0("In 2015, there were 21,574 total collisions on the ",
                        "Island of Montreal. That's on average 59 crashes per ",
                        "day.")) %>% 
  add_row(category = list("crash"), variable = list("crash_total_2014"),
          text = paste0("In 2014, there were 21,402 total collisions on the ",
                        "Island of Montreal. That's on average 58 crashes per ",
                        "day.")) %>%
  add_row(category = list("crash"), variable = list("crash_total_2013"),
          text = paste0("In 2013, there were 31,607 total collisions on the ",
                        "Island of Montreal. That's on average 86 crashes per ",
                        "day.")) %>%
  add_row(category = list("crash"), variable = list("crash_total_2012"),
          text = paste0("In 2012, there were 31,652 total collisions on the ",
                        "Island of Montreal. That's on average 86 crashes per ",
                        "day."))

qsavem(dyk, category_table, file = "data/dyk.qsm")
