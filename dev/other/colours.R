#### Colours data setup ########################################################

# Basic colour palettes ---------------------------------------------------

col_left_5 <- c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")
col_left_3 <- c("#E8E8E8", "#B8D6BE", "#73AE80")
col_right_3 <- c("#E8E8E8", "#B5C0DA", "#6C83B5")
col_delta_5 <- c("#CA0020", "#F4A582", "#E8E8E8", "#92C5DE", "#0571B0")
col_bivar <- c(col_left_3, "#B5C0DA", "#90B2B3", "#5A9178", "#6C83B5",
               "#567994", "#2A5A5B")
col_iso <- col_bivar[c(3, 6, 9)]
col_NA <- "#B3B3BB"


# Opacity levels ----------------------------------------------------------

colour_alpha <- tibble(
    zoom = c("borough", "CT", "DA", "building", "street"),
    alpha = c("EE", "CC", "AA", "CC", "CC"))


# Univariate 5-level colour tables ----------------------------------------

colour_left_5 <-
  tibble(group = c("1", "2", "3", "4", "5", "NA"),
         fill = c(col_left_5, col_NA))

colour_left_5_borough <- 
  colour_left_5 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_left_5_CT <- 
  colour_left_5 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "CT")$alpha))

colour_left_5_DA <- 
  colour_left_5 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "DA")$alpha))

colour_left_5_building <- 
  colour_left_5 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "building")$alpha))

colour_left_5_street <- 
  colour_left_5 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "street")$alpha))


# Univariate 3-level colour tables ----------------------------------------

colour_left_3 <-
  tibble(group = c("1", "2", "3", "NA"),
         fill = c(col_left_3, col_NA))

colour_left_3_borough <- 
  colour_left_3 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_left_3_CT <- 
  colour_left_3 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "CT")$alpha))

colour_left_3_DA <- 
  colour_left_3 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "DA")$alpha))

colour_left_3_building <- 
  colour_left_3 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "building")$alpha))

colour_left_3_street <- 
  colour_left_3 |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "street")$alpha))


# Bivariate colour tables -------------------------------------------------

colour_bivar <-
  tibble(group = c("1 - 1", "2 - 1", "3 - 1",
                   "1 - 2", "2 - 2", "3 - 2",
                   "1 - 3", "2 - 3", "3 - 3",
                   "NA - 1", "NA - 2", "NA - 3", 
                   "1 - NA", "2 - NA", "3 - NA",
                   "NA - NA"),
    fill = c(col_bivar, rep(col_NA, 7)))

colour_bivar_borough <- 
  colour_bivar |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_bivar_CT <- 
  colour_bivar |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "CT")$alpha))

colour_bivar_DA <- 
  colour_bivar |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "DA")$alpha))

colour_bivar_building <- 
  colour_bivar |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "building")$alpha))
  
colour_bivar_street <- 
  colour_bivar |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "street")$alpha))


# Delta colour tables -----------------------------------------------------

colour_delta <- tibble(group = c("1 - 1", "2 - 1", "3 - 1", 
                                 "4 - 1", "5 - 1", "NA - 1"),
                       fill = c(col_delta_5, col_NA))

colour_delta_borough <- 
  colour_delta |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_delta_CT <- 
  colour_delta |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_delta_DA <- 
  colour_delta |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_delta_building <- 
  colour_delta |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_delta_street <- 
  colour_delta |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))


# Isopleth colour tables --------------------------------------------------

colour_iso <- tibble(group = c("1", "2", "3"), fill = col_iso)

colour_iso_borough <- 
  colour_iso |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_iso_CT <- 
  colour_iso |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_iso_DA <- 
  colour_iso |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_iso_building <- 
  colour_iso |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))

colour_iso_street <- 
  colour_iso |> 
  mutate(fill = paste0(fill, filter(colour_alpha, zoom == "borough")$alpha))


# Objects for legends -----------------------------------------------------

legend_left_3 <- tibble(
  x = 1:3, y = 1, fill = col_left_3, 
  label = c("Bottom\nthird", "Middle\nthird", "Top\nthird"))
legend_left_5 <- tibble(x = 1:5, y = 1, fill = col_left_5)
legend_delta_5 <- tibble(x = 1:5, y = 1, fill = col_delta_5)
legend_bivar <- colour_bivar |> 
  slice(1:9) |> 
  tidyr::separate(group, into = c("x", "y"), sep = " - ")
legend_iso <- tibble(x = 1:3, y = 1, fill = col_iso)


# Save output -------------------------------------------------------------

qsavem(colour_alpha, colour_bivar, colour_bivar_borough, colour_bivar_building,
       colour_bivar_CT, colour_bivar_DA, colour_bivar_street, colour_delta,
       colour_delta_borough, colour_delta_building, colour_delta_CT,
       colour_delta_DA, colour_delta_street, colour_iso, colour_iso_borough,
       colour_iso_building, colour_iso_CT, colour_iso_DA, colour_iso_street,
       colour_left_3, colour_left_3_borough, colour_left_3_building,
       colour_left_3_CT, colour_left_3_DA, colour_left_3_street, colour_left_5, 
       colour_left_5_borough, colour_left_5_building, colour_left_5_CT, 
       colour_left_5_DA, colour_left_5_street, legend_bivar, legend_delta_5, 
       legend_iso, legend_left_3, legend_left_5, col_bivar, col_delta_5, 
       col_iso, col_left_3, col_left_5, col_NA, col_right_3, 
       file = "data/colours.qsm")
