#### Add metadata to street ############################################
# Dependent script: needs 'borough', 'CT', 'DA' and 'street' objects

borough_index <- 
  street %>% 
  st_transform(32618) %>% 
  st_centroid() %>% 
  st_nearest_feature(st_transform(borough, 32618))

street <- 
  street %>% 
  mutate(CSDUID = map_chr(borough_index, ~borough$ID[.x]), .after = name_2) %>% 
  st_set_agr("constant")

CT_index <- 
  street %>% 
  st_transform(32618) %>% 
  st_centroid() %>% 
  st_nearest_feature(st_transform(CT, 32618))

street <- 
  street %>% 
  mutate(CTUID = map_chr(CT_index, ~CT$ID[.x]), .after = name_2) %>% 
  st_set_agr("constant")

DA_index <- 
  street %>% 
  st_transform(32618) %>% 
  st_centroid() %>% 
  st_nearest_feature(st_transform(DA, 32618))

street <- 
  street %>% 
  mutate(DAUID = map_chr(DA_index, ~DA$ID[.x]), .after = name_2) %>% 
  st_set_agr("constant")

rm(borough_index, CT_index, DA_index)
