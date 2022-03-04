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
  
  qload("data/census.qsm")
  grid <- qread("data/grid.qs")
  
  source("R/functions/_get_dyk_table.R")
  source("R/functions/_get_dyk_link_vars.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})

var_left <- "canale_ind"
var_right <- " "
var_right <- "iden_imm_pct"
poi <- "little_burgundy"
bench::mark(x = get_dyk_table("canale", "canale_ind", " "))
bench::mark(x = get_dyk_table("canale", "canale_ind", " ", poi))
get_dyk_table("canale", "housing_tenant_pct", " ")
get_dyk_table("canale", "canale_ind", "housing_tenant_pct")
