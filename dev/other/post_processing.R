#### Data post-processing ######################################################

# This script should be run after all content has been added to tables


# Post-processing script --------------------------------------------------

post_process <- function(x) {
  x |> 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA)), 
         across(where(is.numeric), ~replace(., is.infinite(.), NA))) 
}


# Apply script to all tables ----------------------------------------------

borough <- post_process(borough)
building <- post_process(building)
crash <- post_process(crash)
CT <- post_process(CT)
DA <- post_process(DA)
grid <- post_process(grid)
street <- post_process(street)
