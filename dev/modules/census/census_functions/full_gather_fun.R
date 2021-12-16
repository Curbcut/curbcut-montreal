#### FULL GATHER FUNCTION #########################################################

# Full census data gather function ----------------------------------------

add_census_data <- function(census_vec, scales, years, parent_vectors = NULL,
                            CMA = "24462", crs = 32618) {
  
  # Get empty geometries
  message("Getting empty geometries...")
  geoms <- get_empty_geometries(scales, years, CMA, crs)
  
  # Download data
  message("Downloading census data...")
  data_raw <- get_census_vectors(census_vec, geoms, scales, years,
                                 parent_vectors, CMA)
  
  # Get aggregation type
  message("Interpolating...")
  data_agg <- get_agg_type(census_vec, scales, years)
  
  # Interpolate
  data_inter <- interpolate(data_raw, scales, years, data_agg = data_agg,
                            census_vec = census_vec)
  ## Swap CSD to borough
  message("Swapping CSD to borough...")
  data_swaped <- swap_csd_to_borough(data_inter, years, var_add, var_avg)
  # From here, no CSD, but borough
  scales[scales == "CSD"] <- "borough"
  ## Interpolate to building, grid & street
  message("Interpolating other geometries (grid, ... ) ...")
  data_other_inter <- interpolate_other_geoms(c("grid"),
                                              data_swaped, years,
                                              var_add, var_avg)
  ## Get units type
  message("Normalizing all data ...")
  data_unit <- get_unit_type(census_vec, scales, years)
  ## Normalize pct variables
  data_norm <- normalize(data_other_inter, census_vec, data_unit)
  ## Drop variables which aren't included in final tables
  message("Other manipulations (dropping variables, q3, q5, ...) ...")
  data_final <- drop_vars(data_norm, census_vec)
  ## Add q3 and q5 versions
  cat_q5 <- get_categories_q5(data_final, census_vec)
  data_q3 <- add_q3(data_final)
  breaks_q3 <- get_breaks_q3(data_q3, census_vec)
  breaks_q5 <- get_breaks_q5(data_final, cat_q5)
  data_q5 <- add_q5(data_final, breaks_q5)
  data_breaks <- merge_breaks(data_final, data_q3, data_q5)
  ## Add years
  data_years <- add_years(data_breaks, years)
  ## Finalize output
  message("Reducing ...")
  reduce_years(data_years)
}
