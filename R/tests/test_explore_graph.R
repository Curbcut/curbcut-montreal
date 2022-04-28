#### EXPLORE GRAPH TESTS #######################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(purrr)
  library(qs)
  library(sf)
  library(shiny)
  library(stringr)
  
  variables <- qread("data/variables.qs")
  qload("data/colours.qsm")
  
  qload("data/census.qsm")
  grid <- qread("data/grid.qs")
  building <- qread("data/building.qs")

  source("R/functions/_explore_graph.R")
  source("R/functions/_get_axis_labels.R")
  source("R/functions/_get_data_table.R")
  source("R/functions/_get_data_type.R")
  source("R/functions/_get_data.R")
  source("R/functions/_get_plot_type.R")
  source("R/functions/_get_var_type.R")
  source("R/functions/_get_x_scale.R")
  source("R/functions/_get_y_scale.R")
  source("R/functions/_render_explore_graph.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})


# Histogram, no selection -------------------------------------------------

{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Histogram, NA selection -------------------------------------------------

{select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Histogram, active selection ---------------------------------------------

{select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Bar, no selection -------------------------------------------------------

{df <- "grid"
var_left <- "climate_destructive_storms_ind"
# var_left <- "climate_heat_wave_ind"
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Bar, NA selection -------------------------------------------------------

{select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Bar, active selection ---------------------------------------------------

{select_id <- 392
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Scatterplot, no selection -----------------------------------------------

{df <- "borough"
# var_left <- "canale_ind_2016"
# var_right <- "housing_tenant_pct_2016"
var_left <- "housing_tenant_pct_2016"
var_right <- "inc_median_dollar_2016"
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Scatterplot, NA selection -----------------------------------------------

{select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Scatterplot, active selection -------------------------------------------

{select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Boxplot, no selection ---------------------------------------------------

{df <- "grid"
var_left <- "climate_destructive_storms_ind"
var_right <- "housing_tenant_pct_2016"
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Boxplot, NA selection ---------------------------------------------------

{select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Boxplot, active selection -----------------------------------------------

{select_id <- 392
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date univariate scatterplot, no selection -------------------------

{df <- "borough"
var_left <- c("housing_value_avg_dollar_2001", "housing_value_avg_dollar_2016")
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date univariate scatterplot, NA selection -------------------------

{select_id <- "2471025"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date univariate scatterplot, active selection ---------------------

{select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date univariate scatterplot, same dates ---------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2016", "housing_tenant_pct_2016")
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date bivariate scatterplot, no selection --------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- c("inc_median_dollar_2001", "inc_median_dollar_2016")
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date bivariate scatterplot, NA selection --------------------------

{select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date bivariate scatterplot, active selection ----------------------

{select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Multi-date bivariate scatterplot, same dates ----------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2016", "housing_tenant_pct_2016")
var_right <- c("inc_median_dollar_2016", "inc_median_dollar_2016")
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, df, select_id)}


# Date line graph ---------------------------------------------------------

# TKTK
df <- "date"
