## PREDRAW GGPLOT FOR PLACE EXPLORER ###########################################

library(tidyverse)
library(qs)
qload("data/place_explorer.qsm")
source("R/functions/_utils.R")
library(future)
library(furrr)

## Get data ------------------------------------------------------------------

all_tables <- 
  list("CMA" = c("CSD", "CT", "DA", "grid", "building"),
       "island" = c("CSD", "CT", "DA", "grid", "building"),
       "city" = c("CSD", "CT", "DA", "grid", "building"),
       "centraide" = c("CSD", "CT", "DA", "grid", "building"))

all_tables <- map(all_tables, ~.x[seq_len(which(.x == "DA"))])

all_tables <- 
  imap(all_tables, function(scales, geo) {
    map_chr(set_names(scales), function(scale) {
      paste(geo, scale, sep = "_")
    })
  }) |> unlist() |> unname()


borough_imgs <- 
  list.files("www/place_explorer/", full.names = TRUE) |> 
  str_subset("_borough_")

boroughs_imgs_renamed <- 
  str_replace(borough_imgs, "_borough_", "_CSD_")

walk2(borough_imgs, boroughs_imgs_renamed, file.rename)
file.rename()

# Do this operation in parallel
old_plan <- plan()
plan(multisession)

future_walk(all_tables, function(df) {
  vars <- unique(pe_variable_order[[df]] |> 
                   reduce(rbind) |> 
                   pull(var_code))
  walk(vars, function(var) {
    
    data <- 
      pe_var_hierarchy[[df]][[var]][, c("var", "percentile")]
    
    data <- data[!is.na(data$percentile),]
    data <- data[!is.na(data$var),]
    outliers <- find_outliers(data$var)
    if (length(outliers) > 0) data <- data[-outliers, ]
    
    quantiles <- 
      quantile(data$var, probs = seq(0, 1, 0.05))
    
    walk(names(quantiles), function(quantile) {
      
      quant_val <- quantiles[[quantile]]
      
      out <- 
        ggplot(data) +
        geom_density(aes(x = var), size = 0.25, color = "#A9A9A9") +
        geom_vline(aes(xintercept = quant_val), color = "#000000", size = 0.25,
                   alpha = 1) +
        theme_void(base_size = 100)
      
      name <- paste0(paste0("www/place_explorer/"),
                     paste(df, var, str_remove(quantile, "%$"), 
                           sep = "_"),
                     ".png")
      
      ggsave(plot = out, filename = name, width = 150, height = 30,
             units = "px")
    })
  })
})

plan(old_plan)
