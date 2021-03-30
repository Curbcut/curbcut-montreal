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

qsavem(dyk, category_table, file = "data/dyk.qsm")
