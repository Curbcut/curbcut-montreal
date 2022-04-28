#### LEGEND TESTS ##############################################################

# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(ggplot2)
  library(stringr)
  library(qs)
  library(shiny)
  library(glue)

  variables <- qread("data/variables.qs")
  qload("data/colours.qsm")
  
  qload("data/census.qsm")
  grid <- qread("data/grid.qs")
  qload("data/alley.qsm")

  source("R/functions/_get_data_table.R")
  source("R/functions/_get_data_type.R")
  source("R/functions/_get_data.R")
  source("R/functions/_get_legend_breaks.R")
  source("R/functions/_get_legend_labels.R")
  source("R/functions/_render_legend.R")
  source("R/functions/_translation.R")
  source("R/functions/_utils.R")
  
  island_CSDUID <- 
    c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
      "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
      "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
      "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
      "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
      "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
      "2466142", "2466072", "2466023")
  
})


# Univariate, quantitative ------------------------------------------------

{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- " "
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(data, var_left, var_right, df, data_type)}


# Univariate, ordinal -----------------------------------------------------

{df <- "grid"
var_left <- "climate_destructive_storms_ind"
# var_left <- "climate_flood_ind"
var_right <- " "
data <- get_data(df, var_left, var_right, island = TRUE)
data_type <- get_data_type(df, var_left, var_right)
render_legend(data, var_left, var_right, df, data_type)}


# Univariate, manual breaks -----------------------------------------------

{df <- "CT"
var_left <- "access_jobs_total_pwd_count"
var_right <- " "
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
breaks <- 6 * 5:0
names(breaks) <- "Minutes to reach census tract"
render_legend(data, var_left, var_right, df, data_type, breaks = breaks)}


# Univariate, qualitative -------------------------------------------------

{df <- "alley"
var_left <- "alley_qual"
var_right <- " "
data <- get_data(df, var_left, var_right, island = TRUE)
data_type <- get_data_type(df, var_left, var_right)
render_legend(data, var_left, var_right, df, data_type)}


# Bivariate, single date --------------------------------------------------

{df <- "borough"
var_left <- "canale_ind_2016"
var_right <- "housing_tenant_pct_2016"
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(data, var_left, var_right, df, data_type)}


# Delta -------------------------------------------------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- " "
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(data, var_left, var_right, df, data_type)}


# Bivariate, multi-date ---------------------------------------------------

{df <- "borough"
var_left <- c("housing_tenant_pct_2001", "housing_tenant_pct_2016")
var_right <- c("inc_50_pct_2001", "inc_50_pct_2016")
data <- get_data(df, var_left, var_right)
data_type <- get_data_type(df, var_left, var_right)
render_legend(data, var_left, var_right, df, data_type)}
