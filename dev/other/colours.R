#### Colours data setup ########################################################

# Basic colour palettes ---------------------------------------------------

# col_left_5 <- c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")
col_left_5 <- c("#D7E8DB", "#B0D1B7", "#88B993", "#60A26F", "#388B4B")
col_left_3 <- c("#E8E8E8", "#B8D6BE", "#73AE80")
col_right_3 <- c("#E8E8E8", "#B5C0DA", "#6C83B5")
col_delta_5 <- c("#CA0020", "#F4A582", "#E8E8E8", "#92C5DE", "#0571B0")
col_bivar <- c(col_left_3, "#B5C0DA", "#90B2B3", "#5A9178", "#6C83B5",
               "#567994", "#2A5A5B")
col_iso <- col_bivar[c(3, 6, 9)]
col_NA <- "#B3B3BB"


# Opacity level -----------------------------------------------------------

colour_alpha <- c("borough" = "EE", "CT" = "CC", "DA" = "AA", "building" = "99", 
                  "street" = "CC")


# Univariate 5-level colour table -----------------------------------------

colour_left_5 <-
  tibble(group = c(0:5, "NA"),
         fill = c(col_NA, col_left_5, col_NA))


# Univariate 3-level colour table -----------------------------------------

colour_left_3 <-
  tibble(group = c(1:3, "NA"),
         fill = c(col_left_3, col_NA))


# Bivariate colour table --------------------------------------------------

colour_bivar <-
  tibble(group = c("1 - 1", "2 - 1", "3 - 1",
                   "1 - 2", "2 - 2", "3 - 2",
                   "1 - 3", "2 - 3", "3 - 3",
                   "NA - 1", "NA - 2", "NA - 3", 
                   "1 - NA", "2 - NA", "3 - NA",
                   "NA - NA"),
    fill = c(col_bivar, rep(col_NA, 7)))


# Delta colour table ------------------------------------------------------

colour_delta <- tibble(group = c("1 - 1", "2 - 1", "3 - 1", 
                                 "4 - 1", "5 - 1", "NA - 1"),
                       fill = c(col_delta_5, col_NA))


# Isopleth colour tables --------------------------------------------------

colour_iso <- tibble(group = c("1", "2", "3"), fill = col_iso)


# Objects for legends -----------------------------------------------------

legend_left_3 <- tibble(x = 1:3, y = 1, fill = col_left_3)
legend_left_5 <- tibble(x = 1:5, y = 1, fill = col_left_5)
legend_delta_5 <- tibble(x = 1:5, y = 1, fill = col_delta_5)
legend_bivar <- colour_bivar |> 
  slice(1:9) |> 
  tidyr::separate(group, into = c("x", "y"), sep = " - ")
legend_iso <- tibble(x = 1:3, y = 1, fill = col_iso)


# Save output -------------------------------------------------------------

qsavem(colour_alpha, colour_bivar, colour_delta, colour_iso, colour_left_3, 
       colour_left_5, legend_bivar, legend_delta_5, legend_iso, legend_left_3, 
       legend_left_5, col_bivar, col_delta_5, col_iso, col_left_3, col_left_5, 
       col_NA, col_right_3, file = "data/colours.qsm")
