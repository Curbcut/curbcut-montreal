#### FULL GATHER FUNCTION #########################################################

# Full census data gather function ----------------------------------------

add_census_data <- function(census_vec, scales, years, parent_vectors = NULL,
                            CMA = "24462", crs = 32618) {
  
  # Get empty geometries
  geoms <- get_empty_geometries(scales, years, CMA, crs)
  
  # Download data
  data_raw <- get_census_vectors(census_vec, geoms, scales, years,
                                 parent_vectors, CMA)
  
  # Get aggregation type
  data_agg <- get_agg_type(data_raw, census_vec, scales, years)
  
  # Interpolate
  data_inter <- interpolate(data_raw, scales, years, data_agg)
  
  # Swap CSD to borough
  if (CMA == "24462") {
    data_inter <- swap_csd_to_borough(data_inter, years, crs, data_agg)
    scales[scales == "CSD"] <- "borough"
  }
  
  # Interpolate to building, grid & street
  data_other_inter <- interpolate_other(data_inter, "grid", years, crs, 
                                        data_agg)
  
  # Get units type
  message("Normalizing all data ...")
  data_unit <- get_unit_type(census_vec, scales, years)
  
  # Normalize pct variables
  data_norm <- normalize(data_other_inter, census_vec, data_unit)
  
  # Drop variables which aren't included in final tables
  message("Other manipulations (dropping variables, q3, q5, ...) ...")
  data_final <- drop_vars(data_norm, census_vec)
  
  # Add q3 and q5 versions
  cat_q5 <- get_categories_q5(data_final, census_vec)
  data_q3 <- add_q3(data_final)
  breaks_q3 <- get_breaks_q3(data_q3, census_vec)
  breaks_q5 <- get_breaks_q5(data_final, cat_q5)
  data_q5 <- add_q5(data_final, breaks_q5)
  data_breaks <- merge_breaks(data_final, data_q3, data_q5)
  
  # Add years
  data_years <- add_years(data_breaks, years)
  
  # Finalize output
  message("Reducing ...")
  reduce_years(data_years)
}
