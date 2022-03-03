#### PLACE EXPLORER TESTS ######################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(sf)
  library(qs)
  library(shiny)
  library(glue)
  
  variables <- qread("data/variables.qs")
  
  qload("data/census.qsm")
  grid <- qread("data/grid.qs")
  
  qload("data/place_explorer.qsm")
  postal_codes <- qread("data/postal_codes.qs")
  
  source("R/functions/_place_explorer_funs.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})

df <- sample(c("borough", "CT", "DA"), 1)
select_id <- sample(get(df)$ID, 1)

df <- "DA"
select_id <- "24663329"

select_id <- sample(get(df)$ID, 1)
test <- get_title_card(df, select_id, island_only_comparison = TRUE)
test[[5]]$graph


bench::mark(
  title_card = {test <- get_title_card(df, select_id, island_only_comparison = TRUE)}
)