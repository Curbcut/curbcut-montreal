#### PLACE EXPLORER TESTS ######################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(ggplot2)
  library(stringr)
  library(qs)
  library(shiny)
  library(glue)
  
  variables <- qread("data/variables.qs")
  
  qload("data/census.qsm")
  qload("data/colours.qsm")

  qload("data/place_explorer.qsm")
  postal_codes <- qread("data/postal_codes.qs")
  
  source("R/functions/_get_pe_data_order.R")
  source("R/functions/_get_pe_block_plot.R")
  source("R/functions/_get_pe_block_sentence.R")
  source("R/functions/_get_pe_block_text.R")
  source("R/functions/_get_title_card.R")
  source("R/functions/_prep_title_card.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})

df <- sample(c("borough", "CT", "DA"), 1)
select_id <- sample(get(df)$ID, 1)

df <- "DA"
select_id <- "24663334"
island_or_region = "island"
# select_id <- "24663329"
# select_id <- "24661269"

test <- get_title_card(df, select_id, island_or_region)

themes <- pe_theme_order[[df]][[island_or_region]]
themes <- themes[themes$ID == select_id, ]
themes <- themes$theme

x <- 1

data_order <- get_pe_data_order(
  df = df, 
  theme = themes[[x]],
  select_id = select_id, 
  island_or_region = island_or_region)

to_grid <- get_pe_block_text(
  df = df,
  theme = themes[[x]],
  select_id = select_id,
  island_or_region = island_or_region,
  data_order = data_order)

plots <- get_pe_block_plot(
  df = df,
  theme = themes[[x]],
  select_id = select_id,
  island_or_region = island_or_region,
  data_order = data_order)

sentence <- get_pe_block_sentence(
  df = df,
  theme = themes[[x]],
  select_id = select_id,
  island_or_region = island_or_region,
  data_order = data_order)






island_or_region <- "island"
theme <- "Housing"

test <- place_explorer_block_plot(df, theme, select_id, island_or_region)

bench::mark(x = place_explorer_block_plot(df, theme, select_id, island_or_region))
