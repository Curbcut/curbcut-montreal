ba_ndvi <- function(scales_variables_modules,
                    data_output_path = "dev/data/ndvi/",
                    years = cc.data::ndvi_years(),
                    skip_scales = NULL, scales_sequences, crs) {

  # Scales to go over
  skip_scales <- c(skip_scales, "building", "street")
  scales <- scales_variables_modules$scales[!names(scales_variables_modules$scales) %in% skip_scales]
  scales <- lapply(scales, `[`, "ID")
  
  
  # Add it to all the scales ------------------------------------------------
  
  scales <- lapply(scales, sf::st_transform, crs)
  all_files <- list.files(data_output_path, recursive = TRUE, full.names = TRUE)
  
  grd <- qs::qread(sprintf("%sgrd30.qs", data_output_path))
  grd <- sf::st_centroid(grd)
  grd <- sf::st_transform(grd, crs)
  
  scales_to_add_vals <- scales
  
  # Check if 'data.qs' file exists
  data_file_path <- sprintf("%sdata.qs", data_output_path)
  if (file.exists(data_file_path)) {
    # Read the existing data
    existing_data <- qs::qread(data_file_path)
    
    # Extract names of scales already processed
    existing_scales <- names(existing_data)
    
    # Filter out scales that are already processed
    scales <- scales[!names(scales) %in% existing_scales]
  }

  # Applying a function to each element in 'scales'
  scales_ndvi_dat <- sapply(scales, function(scale) {
    # Extracting the 'ID' column from 'scaled' dataframe
    scale <- scale["ID"]
    
    # Calculating intersections between 'scale' and 'grd' using sf package
    intersections <- sf::st_intersects(scale, grd)
    
    # Processing each intersection
    processed_intersections <- lapply(intersections, function(intersection) {
      # Return NA if no intersection
      if (length(intersection) == 0) return(NA)
      
      # Subsetting 'grd' for the intersected indices
      fits <- grd[intersection, ]
      
      # Finding NDVI column names matching the specific pattern and years
      ndvi_cols <- grep("ndvi_\\d{4}", names(fits), value = TRUE)
      ndvi_cols <- grep(paste0(years, collapse = "|"), ndvi_cols, value = TRUE)
      
      # Initializing an empty list to store results
      results <- list()
      
      # Calculating mean for each NDVI column, ignoring NA values
      for (n in ndvi_cols) {
        results[[n]] <- mean(fits[[n]], na.rm = TRUE)
        # Assigning NA_real_ for NA results for consistency
        if (is.na(results[[n]])) results[[n]] <- NA_real_
      }
      
      # Converting the results list to a tibble
      tibble::as_tibble(results)
    })
    
    # Combining the processed data with the original 'scale' data (minus geometry)
    # and converting the result to a tibble
    final_output <- cbind(sf::st_drop_geometry(scale), Reduce(rbind, processed_intersections))
    tibble::as_tibble(final_output)
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  # Combine new data with existing data
  if (exists("existing_data")) {
    scales_ndvi_dat <- c(existing_data, scales_ndvi_dat)
  }
  
  # Save the updated data
  qs::qsave(scales_ndvi_dat, data_file_path)
  
  # Add the grids to scales_ndvi_dat
  grd_dat <- sapply(c("grd30", "grd60", "grd120", "grd300"), \(x) {
    qs::qread(sprintf("%s%s.qs", data_output_path, x))
  }, simplify = FALSE, USE.NAMES = TRUE)
  scales_ndvi_dat <- c(scales_ndvi_dat, grd_dat)
  
  # Apply to our scales
  interpolated <- mapply(\(scale_name, scale_df) {
    if (!scale_name %in% names(scales_ndvi_dat)) return(scale_df)
    merge(scale_df, scales_ndvi_dat[[scale_name]], by = "ID", all.x = TRUE)
  }, names(scales_variables_modules$scales), scales_variables_modules$scales)
  
  # Data tibble -------------------------------------------------------------
  
  time_regex <- "_\\d{4}$"
  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = interpolated,
                         unique_var = "ndvi",
                         time_regex = time_regex)
  
  
  # Variables table ---------------------------------------------------------
  
  avail_scale <- names(scales_ndvi_dat)

  interpolated_ref <- tibble::tibble(
    scale = avail_scale,
    interpolated_from = rep("rasters (30m*30m)", length(avail_scale))
  )
  
  
  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "ndvi",
      type = "ind",
      var_title = "Vegetation",
      var_short = "Vegetation",
      explanation = "the presence and intensity of vegetation",
      exp_q5 = "are living in areas with _X_ level of presence and intensity of vegetation",
      parent_vec = "households",
      theme = "Ecology",
      private = FALSE,
      pe_include = TRUE,
      dates = years,
      avail_scale = avail_scale,
      source = "Curbcut",
      interpolated = interpolated_ref
    )
  
  
  # Possible sequences ------------------------------------------------------
  
  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = avail_scale)
  
  
  # Modules table -----------------------------------------------------------
  
  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "ndvi",
      theme = "Ecology",
      nav_title = "Vegetation",
      title_text_title = "Vegetation",
      title_text_main = paste0(
        "<p>The Normalized Difference Vegetation Index (NDVI) measures the ",
        "average amount of vegetation present in an area during the growing ",
        "season. It is used for environmental conservation, urban planning, ",
        "and climate change mitigation."
      ),
      title_text_extra = paste0(
        "<p>NDVI plays a significant role in various applications, including ",
        "analyzing urban greenness, monitoring agricultural growth, and ",
        "assessing wildfire risks. Calculated from Harmonized Landsat ",
        "Sentinel-2 (HLS) data, NDVI represents average vegetation during ",
        "the growing season (May 1st through August 31st)."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>The NDVI data on this page is derived from the HLSS30.v2.0 and HLSL30.v2.0 satellites, ",
        "spanning from 2013 to the present. The process includes the following detailed steps:</p>",
        "<ul>",
        "<li><strong>Data Retrieval:</strong> Specific NDVI bands are extracted based on the satellite collection. ",
        "These bands are used to analyze the vegetation intensity and are organized into a data frame.</li>",
        "<li><strong>Raster Stacking:</strong> Three bands (Red, Near-infrared (NIR), and Fmask) are extracted, cropped, and masked according ",
        "to the area of interest. These bands are essential for analyzing vegetation patterns and filtering out cloud contamination.</li>",
        "<li><strong>NDVI Calculation:</strong> NDVI is computed using the formula (NIR-Red)/(NIR+Red), quantifying the vegetation's health. ",
        "This index measures the difference between near-infrared (which vegetation strongly reflects) and red light (which vegetation absorbs).</li>",
        "<li><strong>Quality Filtering with Fmask:</strong> The Fmask (Function of Mask) band is utilized to filter out poor quality pixels. ",
        "Fmask is an algorithm that identifies and masks unwanted features like clouds, cloud shadows, water, or snow/ice. In the HLS data, ",
        "values of 0 and 64 in the Fmask layer indicate clean and useful pixels, ensuring that the resulting NDVI is free from these artifacts. ",
        "This filtering is vital for obtaining a true state of the vegetation and underlying surface.</li>",
        "<li><strong>Resampling:</strong> All NDVI rasters are resampled to a specific extent to align perfectly, allowing for accurate aggregation.</li>",
        "</ul>",
        "<p>This process, with its meticulous handling of NDVI bands and quality filtering using Fmask, provides a scientifically robust view of vegetation ",
        "trends, essential for urban sustainability studies and environmental justice analyses.</p>"
      ),
      var_left = c("ndvi"),
      dates = years,
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &
          !is.na(scales_variables_modules$variables$parent_vec)
      ],
      default_var = "ndvi",
      avail_scale_combinations = avail_scale_combinations
    )
  
  
  # Return ------------------------------------------------------------------
  
  return(list(
    scales = interpolated,
    variables = variables,
    modules = modules,
    data = data
  ))
}
