## BUILD AND APPEND TENURE DATA ################################################

build_and_append_tenure <- function(scales_variables_modules, crs) {
  
  # Read and prepare data ---------------------------------------------------
  
  # Read the data placed in the centraide folder
  csvs <- list.files("dev/data/centraide/tenure_status", full.names = TRUE)
  csvs_2016 <- stringr::str_subset(csvs, "2016.csv$")
  csvs_2021 <- stringr::str_subset(csvs, "2021.csv$")
  
  
  # 2016 data --------------------------------------------------------------
  
  data <- lapply(csvs_2016, read.csv, header = FALSE, fileEncoding = "ISO-8859-1")
  data <- lapply(data, tibble::as_tibble)
  # Check if header are all the same
  data_check <- lapply(data, \(x) x[1:4, 2:ncol(x)])
  data_check <- sapply(seq_along(data_check), \(x) {
    if (x == 1) return(TRUE)
    identical(data_check[[x - 1]], data_check[[x]])
  })
  if (!all(data_check)) {
    stop("The data you are going to rbind is not arranged the same way.")
  }
  
  data_init <- data[[1]]
  data_rest <- lapply(data[2:length(data)], \(x) x[5:nrow(x), ])
  data_2016 <- Reduce(rbind, data_rest, init = data_init)
  
  # data_2016 <- data_2016[c(1:4, 6:nrow(data_2016)), ]
  
  # Clean up the table
  data_2016[1,] <- lapply(data_2016[1,], \(x) {
    if (x == "") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total", x)) return("total")
    if (grepl("^Propriétaire", x)) return("owner")
    if (grepl("^Locataire", x)) return("tenant")
    "FILTEROUT"   
  })
  
  data_2016[2,] <- lapply(data_2016[2,], \(x) {
    if (x == "") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total", x)) return("total")
    if (grepl("^30 % ou plus du revenu est consacré aux frais de logement", x)) return("sc30")
    if (grepl("^50 % au plus du revenu est consacré aux frais de logement", x)) return("sc50")
    if (grepl("^80 % ou plus du revenu est consacré aux frais de logement", x)) return("sc80")
    "FILTEROUT"    
  })
  
  data_2016[3,] <- lapply(data_2016[3,], \(x) {
    if (x == "") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total - Taille convenable du logement", x)) return("total")
    if (grepl("^De taille convenable", x)) return("suit")
    if (grepl("^De taille insuffisante", x)) return("unsuit")
    if (grepl("^Seulement entretien régulier ou réparations mineures requises", x)) return("minorrep")
    if (grepl("^Réparations majeures requises", x)) return("majorrep")
    
    if (grepl("^Total - État du logement", x)) return("FILTEROUT")
    "FILTEROUT"    
  })
  
  data_2016[4,] <- lapply(data_2016[4,], \(x) {
    if (x == "Géographie") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total \u0096 Genre de ménage incluant la structure de la famille de recensement", x)) return("total")
    # Census families
    if (grepl("^Ménages comptant une famille de recensement", x)) return("family")
    if (grepl("^Une famille de recensement monoparentale", x)) return("monop")
    if (grepl("^Avec un parent seul de sexe masculin", x)) return("monopmen")
    if (grepl("^Avec un parent seul de sexe féminin", x)) return("monopwomen")
    
    if (grepl("^Un couple, avec ou sans enfants dans leur famille de recensement", x)) return("couple")
    if (grepl("^Couples sans enfants", x)) return("nochildren")
    if (grepl("^Avec enfants", x)) return("withchildren")
    if (grepl("^1 ou 2 enfants", x)) return("12children")
    if (grepl("^3 enfants ou plus", x)) return("3pluschildren")
    
    # No census families
    if (grepl("^Total \u0096 Ménages composés d\u0092une personne seule (18 ans et plus)", x)) return("livingalone")
    if (grepl("^Personnes vivant seules de 18 à 64 ans", x)) return("la1864")
    if (grepl("^65 ans et plus", x)) return("la65")
    
    "FILTEROUT"
  })
  
  # Change the name
  names(data_2016) <-
    sapply(seq_len(ncol(data_2016)), \(x) {
      # Grab the right column
      dt <- data_2016[1:4, x]
      # Glue together spaced by an underscore
      paste0(unlist(dt), collapse = "_")
    })
  
  # Filter out 
  data_2016 <- data_2016[!grepl("FILTEROUT", names(data_2016))]
  
  # Clean up
  data_2016 <- data_2016[5:nrow(data_2016), ]
  names(data_2016)[1] <- "ID"
  data_2016$ID <- gsub(" (.*)$", "", data_2016$ID)
  data_2016[, -1] <- suppressWarnings(sapply(data_2016[, -1], as.numeric))
  
  # Add the Laval secteur number
  data_2016$ID[data_2016$ID == "Secteur"] <- 
    paste0("Secteur ", 1:6)
  
  # Les deux centre-ville
  data_2016$ID[data_2016$ID == "2031"] <- 
    c("2031 | Centre-ville : Faubourg Saint- Laurent",
      "2031 | Centre-ville : Peter-McGill")
  
  # Aggregate territories
  agg <- data_2016[data_2016$ID %in% c(710, 708), 2:ncol(data_2016)] |> 
    colSums()
  agg <- mapply(\(n, v) {
    out <- tibble::tibble(v)
    names(out) <- n
    out
  }, names(agg), agg, SIMPLIFY = FALSE) |> (\(x) Reduce(cbind, x))() |> 
    tibble::as_tibble()
  new_1 <- cbind(tibble::tibble(ID = as.character(716)), agg) |> tibble::as_tibble()
  
  agg <- data_2016[data_2016$ID %in% c(706, 707), 2:ncol(data_2016)] |> 
    colSums()
  agg <- mapply(\(n, v) {
    out <- tibble::tibble(v)
    names(out) <- n
    out
  }, names(agg), agg, SIMPLIFY = FALSE) |> (\(x) Reduce(cbind, x))() |> 
    tibble::as_tibble()
  new_2 <- cbind(tibble::tibble(ID = as.character(715)), agg) |> tibble::as_tibble()
  
  data_2016 <- rbind(data_2016[!data_2016$ID %in% c(710, 708, 706, 707), ],
                     new_1, new_2) |> tibble::as_tibble()
  
  # Add geometries so we can interpolate data to 2021 geometries
  census_2016 <- cancensus::get_census("CA16", regions = list(CMA = 24462), 
                                       level = "CT", geo_format = "sf")
  census_2021 <- cancensus::get_census("CA21", regions = list(CMA = 24462), 
                                       level = "CT", geo_format = "sf")
  
  census_2016 <- census_2016["GeoUID"]
  names(census_2016)[1] <- "ID"
  census_2021 <- census_2021["GeoUID"]
  names(census_2021)[1] <- "ID"
  
  census_2016 <- merge(census_2016, data_2016, by = "ID")
  
  ct_data_2016 <- cc.buildr::interpolate_from_area(to = census_2021, 
                                                   from = census_2016, 
                                                   round_additive = TRUE, 
                                                   additive_vars = names(data_2016)[2:ncol(data_2016)], 
                                                   crs = crs)
  ct_data_2016 <- dplyr::mutate(ct_data_2016, 
                                dplyr::across(dplyr::where(is.numeric), ~round(.x/5)*5))
  
  centraide_data_2016 <- data_2016[!data_2016$ID %in% census_2016$ID, ]
  
  data_2016 <- rbind(sf::st_drop_geometry(ct_data_2016), centraide_data_2016)
  
  # 2021 data ---------------------------------------------------------------
  
  data <- lapply(csvs_2021, read.csv, header = FALSE, fileEncoding = "ISO-8859-1")
  data <- lapply(data, tibble::as_tibble)
  # Check if header are all the same
  data_check <- lapply(data, \(x) x[1:4, 2:ncol(x)])
  data_check <- sapply(seq_along(data_check), \(x) {
    if (x == 1) return(TRUE)
    identical(data_check[[x - 1]], data_check[[x]])
  })
  if (!all(data_check)) {
    stop("The data you are going to rbind is not arranged the same way.")
  }
  
  data_init <- data[[1]]
  data_rest <- lapply(data[2:length(data)], \(x) x[5:nrow(x), ])
  data_2021 <- Reduce(rbind, data_rest, init = data_init)
  
  # Clean up the table
  data_2021[1,] <- lapply(data_2021[1,], \(x) {
    if (x == "") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total", x)) return("total")
    if (grepl("^Propriétaire", x)) return("owner")
    if (grepl("^Locataire", x)) return("tenant")
    "FILTEROUT"   
  })
  
  data_2021[2,] <- lapply(data_2021[2,], \(x) {
    if (x == "") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total", x)) return("total")
    if (grepl("^30 % ou plus du revenu est consacré aux frais de logement", x)) return("sc30")
    if (grepl("^50 % ou plus du revenu est consacré aux frais de logement", x)) return("sc50")
    if (grepl("^80 % ou plus du revenu est consacré aux frais de logement", x)) return("sc80")
    "FILTEROUT"    
  })
  
  data_2021[3,] <- lapply(data_2021[3,], \(x) {
    if (x == "") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total - Taille convenable du logement", x)) return("total")
    if (grepl("^Logement de taille convenable", x)) return("suit")
    if (grepl("^Logement de taille non convenable", x)) return("unsuit")
    if (grepl("^Seulement entretien régulier ou réparations mineures requises", x)) return("minorrep")
    if (grepl("^Réparations majeures requises", x)) return("majorrep")
    
    if (grepl("^Total - État du logement", x)) return("FILTEROUT")
    "FILTEROUT"    
  })
  
  data_2021[4,] <- lapply(data_2021[4,], \(x) {
    if (x == "Géographie") return("ID")
    x <- stringr::str_trim(x)
    if (grepl("^Total \u0096 Genre de ménage incluant la structure de la famille de recensement", x)) return("total")
    # Census families
    if (grepl("^Ménages comptant une famille de recensement", x)) return("family")
    if (grepl("^Une famille de recensement monoparentale", x)) return("monop")
    if (grepl("^Avec un parent qui est un homme+", x)) return("monopmen")
    if (grepl("^Avec un parent qui est une femme+", x)) return("monopwomen")
    
    if (grepl("^Un couple, avec ou sans enfants dans leur famille de recensement", x)) return("couple")
    if (grepl("^Sans enfants", x)) return("nochildren")
    if (grepl("^Avec enfants", x)) return("withchildren")
    if (grepl("^1 ou 2 enfants", x)) return("12children")
    if (grepl("^3 enfants ou plus", x)) return("3pluschildren")
    
    # No census families
    if (grepl("^Total \u0096 Ménages composés d\u0092une personne seule (18 ans et plus)", x)) return("livingalone")
    if (grepl("^Personnes vivant seules de 18 à 64 ans", x)) return("la1864")
    if (grepl("^65 ans et plus", x)) return("la65")
    
    "FILTEROUT"
  })
  
  # Change the name
  names(data_2021) <-
    sapply(seq_len(ncol(data_2021)), \(x) {
      # Grab the right column
      dt <- data_2021[1:4, x]
      # Glue together spaced by an underscore
      paste0(unlist(dt), collapse = "_")
    })
  
  # Filter out 
  data_2021 <- data_2021[!grepl("FILTEROUT", names(data_2021))]
  
  # Clean up
  data_2021 <- data_2021[5:nrow(data_2021), ]
  names(data_2021)[1] <- "ID"
  data_2021$ID <- gsub(" (.*)$", "", data_2021$ID)
  data_2021[, -1] <- suppressWarnings(sapply(data_2021[, -1], as.numeric))
  
  # Add the Laval secteur number
  data_2021$ID[data_2021$ID == "Secteur"] <- 
    paste0("Secteur ", 1:6)
  
  data_2021$ID[data_2021$ID == "2031"] <- 
    c("2031 | Centre-ville : Faubourg Saint- Laurent",
      "2031 | Centre-ville : Peter-McGill")
  
  # Aggregate territories
  agg <- data_2021[data_2021$ID %in% c(710, 708), 2:ncol(data_2021)] |> 
    colSums()
  agg <- mapply(\(n, v) {
    out <- tibble::tibble(v)
    names(out) <- n
    out
  }, names(agg), agg, SIMPLIFY = FALSE) |> (\(x) Reduce(cbind, x))() |> 
    tibble::as_tibble()
  new_1 <- cbind(tibble::tibble(ID = as.character(716)), agg) |> tibble::as_tibble()
  
  agg <- data_2021[data_2021$ID %in% c(706, 707), 2:ncol(data_2021)] |> 
    colSums()
  agg <- mapply(\(n, v) {
    out <- tibble::tibble(v)
    names(out) <- n
    out
  }, names(agg), agg, SIMPLIFY = FALSE) |> (\(x) Reduce(cbind, x))() |> 
    tibble::as_tibble()
  new_2 <- cbind(tibble::tibble(ID = as.character(715)), agg) |> tibble::as_tibble()
  
  data_2021 <- rbind(data_2021[!data_2021$ID %in% c(710, 708, 706, 707), ],
                     new_1, new_2) |> tibble::as_tibble()
  
  
  # Add normalization -------------------------------------------------------
  
  names(data_2016)[2:ncol(data_2016)] <- 
    paste0(names(data_2016)[2:ncol(data_2016)], "_count")
  result <- lapply(data_2016[, 2:ncol(data_2016)], \(x) {
    x / data_2016$total_total_total_total_count
  })
  names(result) <- gsub("_count$", "_pct", names(result))
  result <- tibble::as_tibble(as.data.frame(result))
  data_2016 <- cbind(tibble::tibble(ID = data_2016$ID), result) |> 
    merge(data_2016, by = "ID")
  
  names(data_2021)[2:ncol(data_2021)] <- 
    paste0(names(data_2021)[2:ncol(data_2021)], "_count")
  result <- lapply(data_2021[, 2:ncol(data_2021)], \(x) {
    x / data_2021$total_total_total_total_count
  })
  names(result) <- gsub("_count$", "_pct", names(result))
  result <- tibble::as_tibble(as.data.frame(result))
  data_2021 <- cbind(tibble::tibble(ID = data_2021$ID), result) |> 
    merge(data_2021, by = "ID")
  
  
  # Merge the two years -----------------------------------------------------
  
  names(data_2016)[2:ncol(data_2016)] <- 
    paste0(names(data_2016)[2:ncol(data_2016)], "_2016")
  
  names(data_2021)[2:ncol(data_2021)] <- 
    paste0(names(data_2021)[2:ncol(data_2021)], "_2021")
  
  data <- merge(data_2016, data_2021, by = "ID")
  names(data)[2:ncol(data)] <- paste0("tenure_", names(data)[2:ncol(data)])
  
  data[is.na(data)] <- NA
  
  
  # Get list of data variables ----------------------------------------------
  
  average_vars <- names(data)[grepl("_pct_\\d{4}$", names(data))]
  additive_vars <- names(data)[grepl("_count_\\d{4}$", names(data))]
  vars <- c(average_vars, additive_vars)
  
  
  # Interpolate data to all possible scales ---------------------------------
  
  names(data)[1] <- "CT_ID"
  
  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = "CT",
      all_scales = scales_variables_modules$scales,
      weight_by = "households",
      average_vars = average_vars,
      additive_vars = additive_vars, 
      # All region except Centraide
      only_regions = names(scales_variables_modules$scales)[
        names(scales_variables_modules$scales) != "centraide"
      ],
      crs = crs
    )
  
  # Construct manually the Centraide data
  centraide_raw <- sf::st_read("dev/data/geometry/centraide_2023_raw.shp")
  # centraide_raw$UID[centraide_raw$Quartier == "Pierrefonds-Roxboro"] <- "707"
  # centraide_raw$UID[centraide_raw$Quartier == "L'Île-Bizard–Sainte-Geneviève"] <- "710"
  
  centraide_auto <- merge(centraide_raw, data, by.x = "UID", by.y = "CT_ID")
  fb_stl <- cbind(centraide_raw[centraide_raw$Quartier == "Faubourg Saint-Laurent", ],
                  data[data$CT_ID == "2031 | Centre-ville : Faubourg Saint- Laurent", ]) |> 
    tibble::as_tibble()
  fb_stl <- fb_stl[names(fb_stl) != "CT_ID"]
  petr_mc <- cbind(centraide_raw[centraide_raw$Quartier == "Peter McGill", ],
                   data[data$CT_ID == "2031 | Centre-ville : Peter-McGill", ]) |> 
    tibble::as_tibble()
  petr_mc <- petr_mc[names(petr_mc) != "CT_ID"]
  
  # Laval secteur
  lvl <- data[grepl("Secteur", data$CT_ID), ]
  lvl$CT_ID <- c("101", "102", "103", "104", "105", "106")
  lvl <- merge(centraide_raw[centraide_raw$UID %in% c("101", "102", "103", "104", "105", "106"), ],
               lvl, by.x = "UID", by.y = "CT_ID") |> 
    tibble::as_tibble()
  
  # Bind all
  centraide_dat <- rbind(centraide_auto, sf::st_as_sf(fb_stl), sf::st_as_sf(petr_mc),
                         sf::st_as_sf(lvl))
  centraide_dat <- dplyr::bind_rows(centraide_raw[centraide_raw$Quartier == "Kahnawake", ],
                                    centraide_dat) |> 
    tibble::as_tibble()
  centraide_dat <- centraide_dat[2:ncol(centraide_dat)]
  names(centraide_dat)[1] <- "name"
  
  # Merge to the data interpolated
  data_interpolated$scales$centraide$centraide$name[
    data_interpolated$scales$centraide$centraide$name == "L'Île-Bizard?Sainte-Geneviève"
  ] <- "L'Île-Bizard–Sainte-Geneviève"
  data_interpolated$scales$centraide$centraide <- 
    dplyr::left_join(data_interpolated$scales$centraide$centraide,
                     centraide_dat[names(centraide_dat) != "geometry"], by = "name")
  
  # Merge the data to the CT centraide scale
  names(data)[1] <- "ID"
  data_interpolated$scales$centraide$CT <- 
    dplyr::left_join(data_interpolated$scales$centraide$CT,
                     data, by = "ID")
  
  # Rewrite NO interpolation for Centraide region 
  data_interpolated$interpolated_ref <- 
    rbind(data_interpolated$interpolated_ref,
          tibble::tibble(df = c("centraide_centraide", "centraide_CT"),
                         interpolated_from = c("FALSE", "FALSE")))
  
  
  # Make a types named list -------------------------------------------------
  
  # This will be used to inform which methods to use to calculate breaks and
  # the region values. Percentages, dollars, index, ... get treated differently.
  # See the `add_variable`'s documentation to see possible types.
  unique_vars <- unique(gsub("_\\d{4}", "", vars))
  
  average_vars <- unique(gsub("_\\d{4}$", "", average_vars))
  additive_vars <- unique(gsub("_\\d{4}$", "", additive_vars))
  
  types_avg <- lapply(rep("pct", length(average_vars)), c)
  names(types_avg) <- average_vars
  types_count <- lapply(rep("count", length(additive_vars)), c)
  names(types_count) <- additive_vars
  types <- c(types_avg, types_count)
  
  
  # Calculate breaks --------------------------------------------------------
  
  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars,
      types = types,
      use_quintiles = TRUE
    )
  
  
  # Get the variables values per regions ------------------------------------
  
  # Make a parent string the same way as the types
  parent_strings <- lapply(unique_vars, \(x) "tenure_total_total_total_total_count")
  names(parent_strings) <- unique_vars
  parent_strings[grepl("_count$", names(parent_strings))] <- "population"
  
  region_vals <- variables_get_region_vals(
    scales = data_interpolated$scales,
    vars = unique_vars,
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table)
  
  
  # Variables table ---------------------------------------------------------
  
  # For more information on how to append the information, read the
  # documentation of `add_variable`. Every variable needs to have its own entry
  # in the variables table. The following is an example.
  
  variables <- lapply(unique_vars, \(var) {
    
    # All characteristics
    characteristics <- (\(x) {
      out <- list()
      
      if (grepl("_owner_", var)) out <- c(out, "are owners")
      if (grepl("_tenant_", var)) out <- c(out, "are tenants")
      
      if (grepl("_sc30_", var)) out <- c(out, "are spending more than 30% of their income on shelter")
      if (grepl("_sc50_", var)) out <- c(out, "are spending more than 50% of their income on shelter")
      if (grepl("_sc80_", var)) out <- c(out, "are spending more than 80% of their income on shelter")
      
      if (grepl("_suit_", var)) out <- c(out, "live in suitable accommodations (enough bedrooms for the size and composition of the households)")
      if (grepl("_unsuit_", var)) out <- c(out, "live in unsuitable accommodations (not enough bedrooms for the size and composition of the households)")
      if (grepl("_minorrep_", var)) out <- c(out, "live in dwellings needing minor repairs (loose floor tiles, bricks or shingles; or defective steps, railing or siding)")
      if (grepl("_majorrep_", var)) out <- c(out, "live in dwellings needing major repairs (defective plumbing or electrical wiring; needing structural repairs to walls, floors or ceilings)")
      
      if (grepl("_family_", var)) out <- c(out, "are census families in private households")
      if (grepl("_monop_", var)) out <- c(out, "are one-parent families")
      if (grepl("_monopmen_", var)) out <- c(out, "are one-parent families in which the parent is a man+")
      if (grepl("_monopwomen_", var)) out <- c(out, "are one-parent families in which the parent is a woman+")
      if (grepl("_couple_", var)) out <- c(out, "are couple families")
      if (grepl("_nochildren_", var)) out <- c(out, "are families without children")
      if (grepl("_withchildren_", var)) out <- c(out, "are families with children")
      if (grepl("_12children_", var)) out <- c(out, "are families with 1 or 2 children")
      if (grepl("_3pluschildren_", var)) out <- c(out, "are families with 3 or more children")
      if (grepl("_livingalone_", var)) out <- c(out, "live alone")
      if (grepl("_la1864_", var)) out <- c(out, "live alone and are aged between 18 and 64 years old")
      if (grepl("_la65_", var)) out <- c(out, "live alone and are aged 65 years old and over")

      return(out)
    })()

    # Titles
    title <- (\(x) {
      out <- list()
      
      if (grepl("_owner_", var)) out <- c(out, "Owners")
      if (grepl("_tenant_", var)) out <- c(out, "Tenants")
      
      if (grepl("_sc30_", var)) out <- c(out, "Spending > 30% of income on shelter")
      if (grepl("_sc50_", var)) out <- c(out, "Spending > 50% of income on shelter")
      if (grepl("_sc80_", var)) out <- c(out, "Spending > 80% of income on shelter")
      
      if (grepl("_suit_", var)) out <- c(out, "Suitable accommodations")
      if (grepl("_unsuit_", var)) out <- c(out, "Unsuitable accommodations")
      if (grepl("_minorrep_", var)) out <- c(out, "Dwellings needing only minor repairs")
      if (grepl("_majorrep_", var)) out <- c(out, "Dwellings needing major repairs")
      
      if (grepl("_family_", var)) out <- c(out, "Census families in private households")
      if (grepl("_monop_", var)) out <- c(out, "One-parent families")
      if (grepl("_monopmen_", var)) out <- c(out, "One-parent families in which the parent is a man+")
      if (grepl("_monopwomen_", var)) out <- c(out, "One-parent families in which the parent is a woman+")
      if (grepl("_couple_", var)) out <- c(out, "Couple families")
      if (grepl("_nochildren_", var)) out <- c(out, "Families without children")
      if (grepl("_withchildren_", var)) out <- c(out, "Families with children")
      if (grepl("_12children_", var)) out <- c(out, "Families with 1 or 2 children")
      if (grepl("_3pluschildren_", var)) out <- c(out, "Families with 3 or more children")
      if (grepl("_livingalone_", var)) out <- c(out, "Individuals living alone")
      if (grepl("_la1864_", var)) out <- c(out, "Individuals living alone aged between 18 and 64 years old")
      if (grepl("_la65_", var)) out <- c(out, "Individuals living alone aged 65 years old and over")
      
      if (length(out) == 0) return({
        if (grepl("_pct", var)) return("All households")
        return("Households")
      })
      stringr::str_to_sentence(paste0(out, collapse = ", "))
    })()
    
    short_title <- (\(x) {
      out <- list()
      
      if (grepl("_owner_", var)) out <- c(out, "Own.")
      if (grepl("_tenant_", var)) out <- c(out, "Ten.")
      
      if (grepl("_sc30_", var)) out <- c(out, ">30%.")
      if (grepl("_sc50_", var)) out <- c(out, ">50%.")
      if (grepl("_sc80_", var)) out <- c(out, ">80%.")
      
      if (grepl("_suit_", var)) out <- c(out, "Suit.")
      if (grepl("_unsuit_", var)) out <- c(out, "Unsuit.")
      if (grepl("_minorrep_", var)) out <- c(out, "Min.")
      if (grepl("_majorrep_", var)) out <- c(out, "Maj.")
      
      
      if (grepl("_family_", var)) out <- c(out, "Fam.")
      if (grepl("_monop_", var)) out <- c(out, "1-p fam.")
      if (grepl("_monopmen_", var)) out <- c(out, "1-p fam. M+.")
      if (grepl("_monopwomen_", var)) out <- c(out, "1-p fam. W+.")
      if (grepl("_couple_", var)) out <- c(out, "Couple.")
      if (grepl("_nochildren_", var)) out <- c(out, "No child.")
      if (grepl("_withchildren_", var)) out <- c(out, "With child.")
      if (grepl("_12children_", var)) out <- c(out, "1-2 child.")
      if (grepl("_3pluschildren_", var)) out <- c(out, "3+ child.")
      if (grepl("_livingalone_", var)) out <- c(out, "Alone.")
      if (grepl("_la1864_", var)) out <- c(out, "Alone 18-64.")
      if (grepl("_la65_", var)) out <- c(out, "Alone 65+.")
      
      if (length(out) == 0) return({
        if (grepl("_pct", var)) return("All hou.")
        return("Hou.")
      })
      paste0(out, collapse = " ")
    })()
    
    # Explanation
    explanation <- if (length(characteristics) > 1) {
      exp <- "the percentage of households that have all of the following characteristics:<ul><li>"
      exp_c <- paste0(characteristics, collapse = "<li>")
      paste0(exp, exp_c, "</ul>") 
    } else if (length(characteristics) == 1) {
      paste0("the percentage of households that ", characteristics[[1]])
    } else {
      "the percentage of all households"
    }
    if (grepl("_count$", var)) explanation <- gsub(" percentage ", " number ", explanation)
    
    # Explanation (for q5)
    exp_q5 <- if (length(characteristics) > 1) {
      exp <- "have all of the following characteristics:<ul><li>"
      exp_c <- paste0(characteristics, collapse = "<li>")
      paste0(exp, exp_c, "</ul>") 
    } else if (length(characteristics) == 1) {
      characteristics[[1]]
    } else {
      "live in the area"
    }
    if (grepl("_count$", var)) exp_q5 <- gsub(" percentage ", " number ", exp_q5)
    
    # Auto variables
    group_name <- 
      if (grepl("_count$", var)) "Number of households" else "Percent of all households"
    group_diff <- list(
      "Tenure status" = (\(x) {
        if (grepl("_owner_", var)) return("Owners")
        if (grepl("_tenant_", var)) return("Tenants")
        return("Total")
      })(),
      
      "Shelter cost to income ratio" = structure(
        factor(
          (\(x) {
            if (grepl("_sc30_", var)) return(">30%")
            if (grepl("_sc50_", var)) return(">50%")
            if (grepl("_sc80_", var)) return(">80%")
            return(">0%")
          })(), 
          levels = c(">0%", ">30%", ">50%", ">80%")), 
        class = "slider"),
      
      "Suitability and condition" = (\(x) {
        if (grepl("_suit_", var)) return("Suitable accommodations")
        if (grepl("_unsuit_", var)) return("Unsuitable accommodations")
        if (grepl("_minorrep_", var)) return("Dwellings needing minor repairs")
        if (grepl("_majorrep_", var)) return("Dwellings needing major repairs")
        return("Total")
      })(),
      
      "Family composition" = (\(x) {
        if (grepl("_family_", var)) return("Census families in private households")
        if (grepl("_monop_", var)) return("One-parent")
        if (grepl("_monopmen_", var)) return("One-parent families in which the parent is a man+")
        if (grepl("_monopwomen_", var)) return("One-parent families in which the parent is a woman+")
        if (grepl("_couple_", var)) return("Couple families")
        if (grepl("_nochildren_", var)) return("Families without children")
        if (grepl("_withchildren_", var)) return("Families with children")
        if (grepl("_12children_", var)) return("Families with 1 or 2 children")
        if (grepl("_3pluschildren_", var)) return("Families with 3 or more children")
        if (grepl("_livingalone_", var)) return("Living alone")
        if (grepl("_la1864_", var)) return("Living alone and aged between 18 and 64 years old")
        if (grepl("_la65_", var)) return("Living alone and aged 65 years old and over")
        return("Total")
      })()

    )
    
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = types[[var]],
      var_title = title,
      var_short = short_title,
      explanation = explanation,
      exp_q5 = exp_q5,
      parent_vec = parent_strings[[var]],
      theme = "Housing",
      private = FALSE,
      group_name = group_name,
      group_diff = group_diff,
      dates = with_breaks$avail_dates[[var]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      region_values = region_vals[[var]],
      source = "Centraide of Greater Montreal",
      interpolated = data_interpolated$interpolated_ref,
      # Allow title duplicate so that the parent vector is always 'households'
      # which is an already existing variable title for other parent vectors.
      allow_title_duplicate = TRUE
    ) |> (\(x) x[nrow(x), ])()
  }) |> (\(x) Reduce(rbind, x))()
  
  variables <- rbind(scales_variables_modules$variables, variables)
  
  # Modules table -----------------------------------------------------------
  
  modules <- scales_variables_modules$modules
  
  var_right <- variables$var_code[
    grepl("^climate|^alp", variables$var_code)
  ]
  var_right <- c(var_right, 
                 variables$var_code[grepl("^access_", variables$var_code) & 
                                      variables$pe_include])
  
  modules <-
    modules |>
    add_module(
      id = "tenure",
      theme = "Housing",
      nav_title = "Tenure status",
      title_text_title = "Tenure status",
      title_text_main = paste0(
        "<p><img src='centraide_logo/centraide_logo_en.png' ",
        "style='width:60%;margin-left:50px;'><p>Tenure status measures whether ",
        "a household owns or rents its home. Housing needs can vary ",
        "dramatically by tenure status."
      ),
      title_text_extra = paste0(
        "<p>The datasets visualized on this page come from the 2016 and 2021 Canad",
        "ian Censuses."),
      regions = c("CMA", "island", "city", "centraide"),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>The census data (2016-2021) on this page comes from custom tabulations ",
        "from Statistics Canada ordered by Centraide of Greater Montreal</p>"
      ),
      var_left = variables[grepl("^tenure_", variables$var_code), 
                           c("var_code", "group_name", "group_diff")],
      main_dropdown_title = "Data representation",
      dates = with_breaks$avail_dates[["tenure_total_total_total_total_count"]],
      var_right = var_right,
      suffix_zoom_levels = "max_CT",
      add_advanced_controls = c("mnd", "Shelter cost to income ratio", "Suitability and condition",
                                "Family composition"),
      default_var = "tenure_tenant_total_total_total_pct"
    )
  
  
  # Return ------------------------------------------------------------------
  
  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))
  
}
