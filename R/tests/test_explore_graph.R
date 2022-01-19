#### EXPLORE GRAPH TESTS #######################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(sf)
  library(qs)
  library(shiny)
})

variables <- qread("data/variables.qs")
qload("data/colours.qsm")

qload("data/census.qsm")
grid <- qread("data/grid.qs")
street <- qread("data/street.qs")
building <- qread("data/building.qs")
crash <- qread("data/crash.qs")

source("R/explore_graph.R")
source("R/get_axis_labels.R")
source("R/get_data_table.R")
source("R/get_data_type.R")
source("R/get_data.R")
source("R/get_plot_type.R")
source("R/get_var_type.R")
source("R/get_x_scale.R")
source("R/get_y_scale.R")
source("R/render_explore_graph.R")
source("R/translation.R")


# Histogram, no selection -------------------------------------------------

df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Histogram, NA selection -------------------------------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Histogram, active selection ---------------------------------------------

select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Bar, no selection -------------------------------------------------------

df <- "grid"
var_left <- "climate_destructive_storms_ind"
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Bar, NA selection -------------------------------------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Bar, active selection ---------------------------------------------------

select_id <- 392
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Scatterplot, no selection -----------------------------------------------

df <- "borough"
var_left <- "canale_ind_2016"
var_right <- "housing_tenant_pct_2016"
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Scatterplot, NA selection -----------------------------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Scatterplot, active selection -------------------------------------------

select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Boxplot, no selection ---------------------------------------------------

df <- "grid"
var_left <- "climate_destructive_storms_ind"
var_right <- "housing_tenant_pct_2016"
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Boxplot, NA selection ---------------------------------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Boxplot, active selection -----------------------------------------------

select_id <- 392
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Multi-date univariate scatterplot, no selection -------------------------

df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Multi-date univariate scatterplot, NA selection -------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Multi-date univariate scatterplot, active selection ---------------------

select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Multi-date bivariate scatterplot, no selection --------------------------

df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- c("inc_median_dollar_2001", "inc_median_dollar_2016")
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Multi-date bivariate scatterplot, NA selection --------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Multi-date bivariate scatterplot, active selection ----------------------

select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
explore_graph(data, var_type, var_left, var_right, select_id, df, 
              build_str_as_DA = TRUE, plot_type = "auto")


# Date line graph ---------------------------------------------------------

# TKTK
df <- "date"
