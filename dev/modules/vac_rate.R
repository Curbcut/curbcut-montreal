#### Vacancy rate data setup ###################################################


# Download from the CMHC package ------------------------------------------

# Relevant dimensions
dimensions <-
  c("Bedroom Type", "Year of Construction", "Rent Ranges")
dimensions_short <-
  c("bed", "year", "rent_range")

# Retrieval
cmhc <-
  sapply(2010:2021, \(yr) {
    over_year <-
      mapply(\(x, y) {
        # Get data
        out <- cmhc::get_cmhc(survey = "Rms",
                              series = "Vacancy Rate",
                              dimension = x,
                              breakdown = "Survey Zones",
                              geo_uid = 24462,
                              year = yr)[, 1:3]
        # Rename column and update for real percentage
        names(out)[2] <- y
        out[3] <- out[3] / 100
        
        # Pivot and rename
        out <- tidyr::pivot_wider(out,
                                  names_from = tidyr::all_of(y),
                                  values_from = "Value")
        names(out) <- gsub("Non-Market/Unknown", "non_market", names(out))
        names(out) <- gsub(" |-", "_", tolower(names(out)))
        names(out) <- gsub("___", "_", names(out))
        names(out) <- gsub("\\+", "plus", names(out))
        names(out) <- gsub("_units", "", names(out))
        names(out) <- gsub("bedroom", "bed", names(out))
        names(out) <- gsub("\\$", "", names(out))
        names(out) <- gsub("less_than", "less", names(out))
        names(out) <- gsub(",", "", names(out))
        names(out) <- paste("vac_rate", y, names(out), "pct", yr, sep = "_")
        names(out)[1] <- "name"
        
        # Change the name to the closest string in the CMHC zone scale
        out <- out[!is.na(out$name), ]
        out$name <-
          sapply(out$name,
                 agrep, x = cmhc_cmhczone$name,
                 value = TRUE, USE.NAMES = FALSE)
        
        # Return
        out
      }, dimensions, dimensions_short, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    cmhc <- Reduce(merge, over_year)
    tibble::as_tibble(cmhc)
  }, simplify = FALSE, USE.NAMES = TRUE)

merged <-
  Reduce(\(x, y) merge(x, y, by = "name", all.x = TRUE),
         cmhc, init = sf::st_drop_geometry(cmhc_cmhczone)[, "name"])



# Calculate breaks --------------------------------------------------------

breaks <- calculate_breaks(tables_list = list(cmhc_cmhczone = merged))



# Merge data --------------------------------------------------------------

cmhc_cmhczone <- 
  left_join(cmhc_cmhczone, breaks$tables_list$cmhc_cmhczone, by = "name") |> 
  relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

vars <- names(merged)[!grepl("name|ID|geometry", names(merged))]
unique_vars <- unique(gsub("_\\d{4}$", "", vars))

breaks_q3_active <-
  map(set_names(unique_vars), \(y) {
    imap_dfr(breaks$tables_q3, function(x, scale) {
      vars_of_group <- vars[str_starts(vars, y)]
      map(vars_of_group, \(z) {
        date <- str_extract(z, "\\d{4}$")
        tibble(scale = scale, date = date, rank = 0:3, var = x[[z]])
      }) |> reduce(bind_rows)
    })
  })

# Get breaks_q5
breaks_q5_active <-
  map(set_names(unique_vars), \(y) {
    imap_dfr(breaks$tables_q5, function(x, scale) {
      vars_of_group <- vars[str_starts(vars, y)]
      map(vars_of_group, \(z) {
        date <- str_extract(z, "\\d{4}$")
        tibble(scale = scale, rank = 0:5, var = x[[z]])
      }) |> reduce(bind_rows) |> unique()
    })
  })

interpolation_keys <- 
  map_chr(set_names(names(breaks$tables_list)), ~FALSE)

# Add to the variables table
variables <-
  lapply(unique_vars, \(var) {
    
    # Create title and explanation
    cat_title <- (\(x) {
      # Bedroom types
      if (grepl("_bed_", var)) {
        if (grepl("bachelor", var)) return("studio apartments")
        suff <- "housing units"
        if (grepl("1_bed", var)) return(paste("one-bedroom", suff))
        if (grepl("2_bed", var)) return(paste("two-bedroom", suff))
        if (grepl("3_bed_plus", var)) return(paste("three-bedroom and larger", suff))
        if (grepl("bed_total", var)) return(paste("all", suff))
      }
      # Year of construction
      if (grepl("_year_", var)) {
        pre <- "housing units built"
        if (grepl("before_1960", var)) return(paste(pre, "before 1960"))
        if (grepl("1960_1979", var)) return(paste(pre, "between 1960 and 1979"))
        if (grepl("1980_1999", var)) return(paste(pre, "between 1980 and 1999"))
        if (grepl("2000_or_later", var)) return(paste(pre, "after 2000"))
        if (grepl("year_total", var)) return(paste("all housing units"))
      }
      # Rent ranges
      if (grepl("_rent_range_", var)) {
        pre <- "housing units with a rent"
        if (grepl("less_750", var)) return(paste(pre, "below $750"))
        if (grepl("750_999", var)) return(paste(pre, "between $750 and $999"))
        if (grepl("1000_1249", var)) return(paste(pre, "between $1,000 and $1,249"))
        if (grepl("1250_1499", var)) return(paste(pre, "between $1,250 and $1,499"))
        if (grepl("1500_plus", var)) return(paste(pre, "higher than $1,500"))
        if (grepl("non_market", var)) return(paste("units with an unknown rent"))
        if (grepl("rent_range_total", var)) return(paste("all housing units"))
      }
    })(var)
    title <- paste("Vacancy rate in", cat_title)
    explanation <- paste("the percentage of all available",
                         gsub("^all ", "", cat_title),
                         "in a rental property that are vacant or unoccupied")
    
    # Create short title
    cat_short <- (\(x) {
      # Bedroom types
      if (grepl("_bed_", var)) {
        if (grepl("bachelor", var)) return("studio")
        if (grepl("1_bed", var)) return("1bed")
        if (grepl("2_bed", var)) return("2bed")
        if (grepl("3_bed_plus", var)) return("3+bed")
        if (grepl("bed_total", var)) return("total")
      }
      # Year of construction
      if (grepl("_year_", var)) {
        if (grepl("before_1960", var)) return("<1960")
        if (grepl("1960_1979", var)) return(">1960<1979")
        if (grepl("1980_1999", var)) return(">1980<1999")
        if (grepl("2000_or_later", var)) return(">2000")
        if (grepl("year_total", var)) return("total")
      }
      # Rent ranges
      if (grepl("_rent_range_", var)) {
        if (grepl("less_750", var)) return("<$750")
        if (grepl("750_999", var)) return(">$750<$999")
        if (grepl("1000_1249", var)) return(">$1k<$1.25k")
        if (grepl("1250_1499", var)) return(">$1.25k<$1.5k")
        if (grepl("1500_plus", var)) return(">$1.5k")
        if (grepl("non_market", var)) return("?")
        if (grepl("rent_range_total", var)) return("total")
      }
    })(var)
    short <- paste("Vac. rate", cat_short)
    
    # Create group_name
    cat_group_name <- (\(x) {
      # Bedroom types
      if (grepl("_bed_", var)) return("Bedroom type")
      # Year of construction
      if (grepl("_year_", var)) return("Year of construction")
      # Rent ranges
      if (grepl("_rent_range_", var)) return("Rent range")
    })(var)
    group_name <- paste("Vacancy rate by", tolower(cat_group_name))
    
    # Create group_diff
    group_diff <- list(paste("For", cat_title))
    names(group_diff) <- cat_group_name

    out <-
      add_variables(
        data = variables,
        var_code = var,
        var_title = title,
        var_short = short,
        explanation = explanation,
        category = NA,
        theme = "Vacancy rate",
        private = FALSE,
        dates = c(2010:2021),
        scales = names(breaks$tables_list),
        breaks_q3 = breaks_q3_active[[var]],
        breaks_q5 = breaks_q5_active[[var]],
        source = "Canada Mortgage and Housing Corporation",
        interpolated = interpolation_keys,
        grouping = group_name,
        group_diff = group_diff)
    
    out[out$var_code == var, ]
  }) |> (\(x) Reduce(rbind, x, init = variables))()


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(id = "vac_rate",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>The vacancy rate data in this module comes from ",
                       "the Canada Mortgage and Housing Corporation.</p>"))


# Clean up ----------------------------------------------------------------

rm(dimensions, cmhc, breaks, merged, vars, breaks_q3_active, breaks_q5_active, 
   unique_vars, interpolation_keys)
