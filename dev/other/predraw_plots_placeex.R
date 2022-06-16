## PREDRAW GGPLOT FOR PLACE EXPLORER ###########################################

library(tidyverse)
library(qs)
qload("data/place_explorer.qsm")
source("R/functions/_utils.R")
library(future)
library(furrr)

## Get data ------------------------------------------------------------------

dfs <- c("borough", "CT", "DA")
island_or_regions <- c("island", "regions")

# Create a multi-distributed plan for nested parallel operations
old_plan <- plan()

third_op <- 
  floor(availableCores() / length(dfs) / length(island_or_regions))

plan(list(tweak(multisession, workers = length(dfs)), 
          tweak(multisession, workers = length(island_or_regions)),
          tweak(multisession, workers = third_op)))

future_walk(dfs, function(df) {
  future_walk(island_or_regions, function(island_or_region) {
    vars <- unique(pe_variable_order[[df]][[island_or_region]]$var_code)
    future_walk(vars, function(var) {
      
      data <- 
        pe_var_hierarchy[[df]][[var]][
          , c("var", paste0(island_or_region, "_percentile"))]
      
      data <- data[!is.na(data[[paste0(island_or_region, "_percentile")]]),]
      data <- data[!is.na(data$var),]
      outliers <- find_outliers(data$var)
      if (length(outliers) > 0) data <- data[-outliers, ]
      
      quantiles <- 
        quantile(data$var, probs = seq(0, 1, 0.05))
      
      map(names(quantiles), function(quantile) {
        
        quant_val <- quantiles[[quantile]]
        
        out <- 
          ggplot(data) +
          geom_density(aes(x = var), size = 0.25, color = "#A9A9A9") +
          geom_vline(aes(xintercept = quant_val), color = "#000000", size = 0.25,
                     alpha = 1) +
          theme_void(base_size = 100)
        
        name <- paste0(paste0("www/place_explorer/", df, "_"),
                       paste(island_or_region, var, str_remove(quantile, "%$"), 
                             sep = "_"),
                       ".png")
        
        ggsave(plot = out, filename = name, width = 150, height = 30,
               units = "px")
      })
    })
  })
})


plan(old_plan)
