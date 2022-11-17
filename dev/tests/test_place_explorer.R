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
  source("R/functions/_get_pe_themes.R")
  source("R/functions/_get_pe_block.R")
  source("R/functions/_get_CT_access_vars.R")
  source("R/functions/_get_title_card.R")
  source("R/functions/_prep_title_card.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})

df <- sample(c("borough", "CT", "DA"), 1)
select_id <- sample(get(df)$ID, 1)

df <- "DA"
select_id <- "24650676"
island_or_region = "region"
# select_id <- "24663329"
# select_id <- "24661269"

df <- "CT"
select_id <- "4620145.00"
island_or_region <- "island"

title_card <- get_title_card(df, select_id, island_or_region)
themes <- get_pe_themes(df, select_id, island_or_region)

x <- themes[[1]]
theme <- x[1]

block <- get_pe_block(
  df = df, 
  theme = x[1],
  select_id = select_id, 
  island_or_region = island_or_region)

block[[3]]
block









island_or_region <- "island"
theme <- "Housing"

test <- place_explorer_block_plot(df, theme, select_id, island_or_region)

bench::mark(x = place_explorer_block_plot(df, theme, select_id, island_or_region))
