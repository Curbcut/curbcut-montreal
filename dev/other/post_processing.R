#### Data post-processing ######################################################

# This script should be run after all content has been added to tables


# Post-processing script --------------------------------------------------

post_process <- function(x) {
  x |> 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA)), 
         across(where(is.numeric), ~replace(., is.infinite(.), NA))) 
}


# Apply script to all tables ----------------------------------------------

map(all_tables, ~assign(.x, post_process(get(.x)), envir = .GlobalEnv))


# Clean up ----------------------------------------------------------------

rm(post_process)
