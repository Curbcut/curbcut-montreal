#### Colours data setup ########################################################

library(tibble)


# Basic colour palettes ---------------------------------------------------

# col_left_5 <- c("#D7E8DB", "#B0D1B7", "#88B993", "#60A26F", "#388B4B")
col_left_5 <- c("#C7DFCC", "#9DC6A6", "#73AE80", "#517A5A", "#2E4633")
col_left_3 <- c("#E8E8E8", "#B8D6BE", "#73AE80")
col_right_5 <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
col_right_3 <- c("#E8E8E8", "#B5C0DA", "#6C83B5")
col_delta_5 <- c("#CA0020", "#F4A582", "#E8E8E8", "#92C5DE", "#0571B0")
col_bivar <- c(col_left_3, "#B5C0DA", "#90B2B3", "#5A9178", "#6C83B5",
               "#567994", "#2A5A5B")
col_qual <- c(col_left_3[3], col_right_3[3], "#5B362A", "#B58A6C", col_bivar[9], 	
              "#AE7673")
col_pe <- c("#CA0020", "#F4A582", "#A9A9A9", "#BAE4B3", "#31A354")
col_NA <- "#B3B3BB"

col_viridis <- scales::viridis_pal()(25)



# Rdeck colours -----------------------------------------------------------

c_NA <- tibble(
  palette = "NA",
  group = "0",
  value = col_NA)

c_q5 <- tibble(
  palette = "q5",
  group = as.character(1:5),
  value = col_left_5)

c_bivar <- tibble(
  palette = "bivar",
  group = as.character(6:14),
  value = col_bivar)

c_delta <- tibble(
  palette = "delta",
  group = as.character(15:19),
  value = col_delta_5)

c_qual <- tibble(
  palette = "qual",
  group = as.character(20:25),
  value = col_qual)

c_viridis <- tibble(
  palette = "viridis",
  group = as.character(26:50),
  value = col_viridis)

colour_table <- dplyr::bind_rows(c_NA, c_q5, c_bivar, c_delta, c_qual, 
                                 c_viridis)
  

# Univariate 5-level colour table -----------------------------------------

colour_left_5 <-
  tibble(group = c(0:5, "NA"),
         fill = c(col_NA, col_left_5, col_NA))


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

colour_delta <- tibble(group = c(1:5, "NA"),
                       fill = c(col_delta_5, col_NA))


# Isopleth colour tables --------------------------------------------------

colour_iso <- tibble(group = 1:5, fill = col_right_5)


# Objects for legends -----------------------------------------------------

legend_left_5 <- tibble(x = 0:5, y = 1, fill = c(col_NA, col_left_5))
legend_delta_5 <- tibble(x = 1:5, y = 1, fill = col_delta_5)
legend_bivar <- colour_bivar |> 
  dplyr::slice(1:9) |> 
  tidyr::separate(group, into = c("x", "y"), sep = " - ")
legend_qual <- tibble(x = 0:6, y = 1, fill = c(col_NA, col_qual))
legend_iso <- tibble(x = 1:5, y = 1, fill = col_right_5)


# Save output -------------------------------------------------------------

qs::qsavem(colour_bivar, colour_delta, colour_iso, colour_left_5, 
           colour_table, legend_bivar, legend_delta_5, legend_qual, legend_iso, 
           legend_left_5, col_pe, file = "data/colours.qsm")

rm(c_bivar, c_delta, c_NA, c_qual, c_q5, c_viridis, col_bivar, col_delta_5, 
   col_right_5, col_left_3, col_left_5, col_NA, col_qual, col_right_3,
   col_viridis)
