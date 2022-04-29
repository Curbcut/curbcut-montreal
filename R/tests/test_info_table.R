#### EXPLORE INFO TABLE TESTS ##################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(ggplot2)
  library(stringr)
  library(sf)
  library(qs)
  library(shiny)
  library(glue)
  
  variables <- qread("data/variables.qs")
  qload("data/colours.qsm")
  
  qload("data/census.qsm")
  # building <- qread("data/building.qs")
  # grid <- qread("data/grid.qs")

  source("R/functions/_get_data_table.R")
  source("R/functions/_get_data_type.R")
  source("R/functions/_get_data.R")
  source("R/functions/_get_info_table_data.R")
  source("R/functions/_get_var_type.R")
  source("R/functions/_info_table.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
})


# Handle NAs --------------------------------------------------------------

# Special case for Kahnawake
{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- "2467802"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Special case for Kanestake
{select_id <- "2472802"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Univariate, NA selection
{df <- "DA"
var_left <- "housing_tenant_pct_1996"
data <- get_data(df, var_left, var_right)
select_id <- "24740053"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, NA selection
{var_right <- "canale_ind_2016"
data <- get_data(df, var_left, var_right)
select_id <- "24740053"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# NA delta data
{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2001")
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}


# Univariate single-date cases --------------------------------------------

# Univariate, quantitative, no selection
{df <- "DA"
var_left <- "canale_ind_2016"
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Univariate, quantitative, valid selection
{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Univariate, qualitative, no selection
{df <- "grid"
var_left <- "climate_flood_ind"
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Univariate, qualitative, valid selection
{select_id <- 392
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}


# Univariate multi-date cases ---------------------------------------------

# Univariate, quantitative, no selection
{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Univariate, quantitative, valid selection
{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Univariate, quantitative, NA selection
{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- " "
data <- get_data(df, var_left, var_right)
select_id <- "2471025"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}


# Univariate, qualitative, no selection
# Univariate, qualitative, valid selection


# Bivariate cases ---------------------------------------------------------

# Bivariate, quantitative, no selection
{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- "housing_tenant_pct_2016"
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, quantitative, valid selection
{select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, qualitative x, quantitative y, no selection
{df <- "grid"
var_left <- "climate_flood_ind"
var_right <- "housing_tenant_pct_2016"
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, qualitative x, quantitative y, valid selection
{df <- "grid"
var_left <- "climate_flood_ind"
var_right <- "housing_tenant_pct_2016"
data <- get_data(df, var_left, var_right)
select_id <- 398
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, quantitative x, qualitative y, no selection
{df <- "grid"
var_left <- "housing_tenant_pct_2016"
var_right <- "climate_flood_ind"
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, quantitative x, qualitative y, valid selection
{df <- "grid"
var_left <- "housing_tenant_pct_2016"
var_right <- "climate_flood_ind"
data <- get_data(df, var_left, var_right)
select_id <- 398
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}


# Bivariate multi-date cases ----------------------------------------------

# Bivariate, quantitative, no selection
{df <- "borough"
var_left <- c("inc_median_dollar_2006", "inc_median_dollar_2016")
var_right <- c("housing_tenant_pct_2006", "housing_tenant_pct_2016")
island <- FALSE
data <- get_data(df, var_left, var_right)
select_id <- NA
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, quantitative, valid selection
{select_id <- "2458012"
var_type <- get_var_type(data, var_left, var_right, df, select_id)
info_table(data, var_type, var_left, var_right, df, select_id)}

# Bivariate, qualitative x, quantitative y, no selection
# Bivariate, qualitative x, quantitative y, valid selection
# Bivariate, quantitative x, qualitative y, no selection
# Bivariate, quantitative x, qualitative y, valid selection


# Special cases -----------------------------------------------------------

# Date graph
