#### EXPLORE INFO TABLE TESTS ##################################################

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
  street <- qread("data/street.qs")
  building <- qread("data/building.qs")
  crash <- qread("data/crash.qs")
  
  source("R/get_data_table.R")
  source("R/get_data_type.R")
  source("R/get_data.R")
  source("R/get_info_table_data.R")
  source("R/get_var_type.R")
  source("R/info_table.R")
  source("R/translation.R")
  source("R/utils.R")
  
})


# Histogram, no selection -------------------------------------------------

df <- "DA"
var_left <- "canale_ind_2016"
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, select_id, df)


# Histogram, reserve selection --------------------------------------------

select_id <- "24720187"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, select_id, df)


# Histogram, NA selection -------------------------------------------------

var_left <- "housing_tenant_pct_1996"
data <- get_data(df, var_left, var_right)
select_id <- "24740053"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, select_id, df)


# Histogram, active selection ---------------------------------------------

df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, select_id, df)


# Bar, no selection -------------------------------------------------------

df <- "grid"
var_left <- "climate_flood_ind"
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, select_id, df)


# Bar, NA selection -------------------------------------------------------

select_id <- "A"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, select_id, df)


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
