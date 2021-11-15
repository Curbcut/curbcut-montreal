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

colour_building <- 
  colour_bivar %>%
  mutate(fill = paste0(fill, "AA"))
  
colour_scale <- 
  c("#E8E8E8", "#B8D6BE", "#73AE80", "#E8E8E8", "#B5C0DA", "#6C83B5")


# For univariate percent change -------------------------------------------

colour_delta <- tibble("1 - 1" = "#CA0020", "2 - 1" = "#F4A582", 
                       "3 - 1" = "#E8E8E8", "4 - 1" = "#92C5DE",
                       "5 - 1" = "#0571B0", "NA - 1" = "#B3B3BB") %>%
  tidyr::pivot_longer(everything(), "group", values_to = "fill") 

colour_delta_borough <- 
  colour_delta %>% 
  mutate(fill = paste0(fill, "EE"))

colour_delta_CT <- 
  colour_delta %>% 
  mutate(fill = paste0(fill, "CC"))

colour_delta_DA <- 
  colour_delta %>% 
  mutate(fill = paste0(fill, "AA"))

colour_delta_street <- 
  colour_delta %>% 
  mutate(fill = paste0(fill, "AA"))

colour_delta_building <- 
  colour_delta %>%
  mutate(fill = paste0(fill, "AA"))

colour_isopleth <- 
  tibble(group = 1:3, fill = c("#D0DFD3", colour_scale[2:3]))

qsavem(colour_bivar, colour_borough, colour_CT, colour_DA, colour_street, 
       colour_building, colour_scale, colour_delta, colour_delta_borough, 
       colour_delta_CT, colour_delta_DA, colour_delta_street, 
       colour_delta_building, colour_isopleth, file = "data/colours.qsm")
