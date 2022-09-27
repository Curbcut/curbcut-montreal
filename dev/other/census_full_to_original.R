library(tidyverse)
library(qs)
library(qs)

all_tables <- 
  list("CMA" = c("borough", "CT", "DA", "grid", "building"),
       "island" = c("borough", "CT", "DA", "grid", "building"),
       "city" = c("borough", "CT", "DA", "grid", "building"),
       "centraide" = c("centraide", "CT", "DA", "grid", "building"))

walk(names(all_tables), ~{
  dat <- paste0(.x, "_full")
  qload(paste0("data2/", dat, ".qsm"), env = .GlobalEnv)
})

iwalk(all_tables, function(scales, geo) {
  walk(scales, function(scale) {
    now <- paste(geo, scale, "full", sep = "_")
    wanted <- paste(geo, scale, sep = "_")
    
    assign(wanted, get(now), envir = .GlobalEnv)
    rm(list = now, envir = .GlobalEnv)
  })
})
