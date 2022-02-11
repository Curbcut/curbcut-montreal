#### DYK TESTS #################################################################

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
  qload("data/colours.qsm")
  
  qload("data/census.qsm")
  grid <- qread("data/grid.qs")
  
  source("R/functions/_get_data_table.R")
  source("R/functions/_get_data_type.R")
  source("R/functions/_get_data.R")
  source("R/functions/_get_dyk_table.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})

get_dyk_table("canale_ind", " ")
get_dyk_table("housing_tenant_pct", " ")
get_dyk_table("canale_ind", "housing_tenant_pct")
