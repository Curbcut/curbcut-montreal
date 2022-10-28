#### Data post-processing ######################################################

# This script should be run after all content has been added to tables


# Post-processing script --------------------------------------------------

post_process <- function(x) {
  x |> 
    mutate(across(where(is.numeric), ~replace(., is.nan(.), NA)), 
           across(where(is.numeric), ~replace(., is.infinite(.), NA))) |> 
    mutate(ID = as.character(ID)) |> 
    st_transform(4326) |> 
    arrange(ID)
}


# Apply script to all tables ----------------------------------------------

iwalk(all_tables, function(scales, geo) {
  walk(scales, function(scale) {
    geo_scale <- paste(geo, scale, sep = "_")
    processed <- post_process(get(paste(geo, scale, sep = "_")))
    
    assign(geo_scale, processed, envir = .GlobalEnv)
  })
})


# Clean up ----------------------------------------------------------------

rm(post_process)
