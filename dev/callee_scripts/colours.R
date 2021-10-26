#### Colours data setup ########################################################
# Independent script 

colour_bivar <- tibble(
  "3 - 3" = "#2A5A5B", "2 - 3" = "#567994", "1 - 3" = "#6C83B5", 
  "3 - 2" = "#5A9178", "2 - 2" = "#90B2B3", "1 - 2" = "#B5C0DA",
  "3 - 1" = "#73AE80", "2 - 1" = "#B8D6BE", "1 - 1" = "#E8E8E8") %>%
  tidyr::pivot_longer(everything(), "group",values_to = "fill") %>% 
  add_row(group = c("NA - 1", "NA - 2", "NA - 3", "1 - NA", "2 - NA", "3 - NA",
                    "NA - NA"),
          fill = rep("#B3B3BB", 7))

colour_borough <- 
  colour_bivar %>% 
  mutate(fill = paste0(fill, "EE"))

colour_CT <- 
  colour_bivar %>% 
  mutate(fill = paste0(fill, "CC"))

colour_DA <- 
  colour_bivar %>% 
  mutate(fill = paste0(fill, "AA"))

colour_street <- 
  colour_bivar %>% 
  mutate(fill = paste0(fill, "AA"))

colour_DA_2 <- 
  colour_bivar %>% 
  mutate(fill = paste0(fill, "80"))

colour_scale <- 
  c("#E8E8E8", "#B8D6BE", "#73AE80", "#E8E8E8", "#B5C0DA", "#6C83B5")

qsavem(colour_bivar, colour_borough, colour_CT, colour_DA, colour_street, colour_DA_2, 
       colour_scale, file = "data/colours.qsm")
