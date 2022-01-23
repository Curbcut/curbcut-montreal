#### LEGEND TESTS ##############################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(sf)
  library(qs)
  library(shiny)
  
  variables <- qread("data/variables.qs")
  qload("data/colours.qsm")
  
  qload("data/census.qsm")
  grid <- qread("data/grid.qs")

  source("R/get_data_table.R")
  source("R/get_data_type.R")
  source("R/get_data.R")
  source("R/get_legend_breaks.R")
  source("R/get_legend_labels.R")
  source("R/render_legend.R")
  source("R/translation.R")
  source("R/utils.R")
  
})


# Univariate, quantitative ------------------------------------------------

{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(var_left, var_right, df, data_type)}


# Univariate, qualitative -------------------------------------------------

{df <- "grid"
var_left <- "climate_destructive_storms_ind"
# var_left <- "climate_flood_ind"
var_right <- " "
data <- get_data(df, var_left, var_right, island = TRUE)
data_type <- get_data_type(df, var_left, var_right)
render_legend(var_left, var_right, df, data_type)}


# Bivariate, single date --------------------------------------------------

{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- "housing_tenant_pct_2016"
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(var_left, var_right, df, data_type)}


# Delta -------------------------------------------------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- " "
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(var_left, var_right, df, data_type)}


# Bivariate, multi-date ---------------------------------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- c("housing_value_avg_dollar_2001", "housing_value_avg_dollar_2016")
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(var_left, var_right, df, data_type)}
