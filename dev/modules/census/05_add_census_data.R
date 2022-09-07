#### FULL GATHER FUNCTION #########################################################

# Full census data gather function ----------------------------------------

add_census_data <- function(census_vec, scales, years, parent_vectors = NULL,
                            region = "24", crs = 32618) {
  
  #Overwrite the first few scales to only get the normal census data
  census_scales <- str_subset(scales, "CSD|CT|DA")

  # Get empty geometries
  geoms <- get_empty_geometries(census_scales, years, region, crs)
  
  # Download data
  data_raw <- get_census_vectors(census_vec, geoms, census_scales, years,
                                 parent_vectors)
  
  # Get aggregation type
  data_agg <- get_agg_type(data_raw, census_vec, census_scales, years)
  
  # Interpolate
  data_inter <- interpolate(data_raw, census_scales, years, data_agg)
  
  # Swap CSD to borough if it's Montreal
  if (region == "24" && crs == 32618) {
    data_inter <- swap_csd_to_borough(data_inter, years, crs, data_agg,
                                      scales = census_scales)
    scales[scales == "CSD"] <- "borough"
  }
  
  # Interpolate to building, grid & street
  other_scales <- str_subset(scales, "CSD|borough|CT|DA", negate = TRUE)
  data_other_inter <-
        interpolate_other(data_inter, other_scales, years, crs, data_agg)
  
  data_all_inter <- c(data_inter, data_other_inter)

  # Get units type
  data_unit <- get_unit_type(census_vec, scales, years)
  
  # Normalize pct variables
  data_norm <- normalize(data_all_inter, census_vec, data_unit)
  
  # Drop variables which aren't included in final tables
  data_final <- drop_vars(data_norm, census_vec)
  
  # Add q3 and q5 versions
  data_for_q5 <- map(data_final, bind_rows)
  cat_q5 <- get_categories_q5_list(data_for_q5, census_vec)
  data_q3 <- add_q3_list(data_final)
  breaks_q3 <- get_breaks_q3_list(data_q3, census_vec)
  breaks_q5 <- get_breaks_q5_list(data_for_q5, cat_q5)
  data_q5 <- add_q5_list(data_final, breaks_q5)
  data_breaks <- merge_breaks(data_final, data_q3, data_q5)
  
  # Add years
  data_years <- add_years(data_breaks, years)
  
  # Finalize output
  data_out <- reduce_years(data_years)
  
  # Add to variable table
  new_vars <- add_vars(data_out, census_vec, breaks_q3, breaks_q5, scales,
                       years)
  
  # Return output
  return(list(data_out, new_vars))
}
