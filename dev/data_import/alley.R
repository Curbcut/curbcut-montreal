## BUILD AND APPEND ALLEY DATA #################################################

build_and_append_alley <- function(scales_variables_modules, crs) {

  # # Individual borough info -------------------------------------------------
  # 
  # alley_boroughs <-
  #   read.csv("dev/data/alley/info_borough.csv", sep = ";") |>
  #   tibble::as_tibble()
  # 
  # boroughs <- scales_variables_modules$scales$city$CSD[c("ID", "name")] |>
  #   sf::st_drop_geometry()
  # 
  # alley_boroughs <-
  #   merge(alley_boroughs[names(alley_boroughs) != "ID"], boroughs, by = "name") |>
  #   tibble::as_tibble()
  # 
  # alley_boroughs$text_en <- sapply(seq_along(alley_boroughs$name), \(x) {
  #   row <- alley_boroughs[x, ]
  # 
  #   start <- sprintf("<p>The first green alley inauguration whas in %s.", row$first_alley)
  # 
  #   other <- row[c("app_process", "management", "budget")]
  #   other <- sprintf("%s<p>%s", start, paste0(other[!is.na(other)], collapse = " "))
  # 
  #   guide <- sprintf("%s<p><a href = '%s' target = '_blank'>The green alley guide of %s.</a>",
  #                    other, if (is.na(row$guide)) "" else row$guide, row$name)
  # 
  #   out <- if (!is.na(row$contact)) {
  #     sprintf("%s<p>Contact: <a href = 'mailto:%s'>%s</a>", guide, row$contact, row$contact)
  #   } else guide
  # 
  #   return(out)
  # })
  # 
  # alley_boroughs$text_fr <- sapply(seq_along(alley_boroughs$name), \(x) {
  #   row <- alley_boroughs[x, ]
  # 
  #   start <- sprintf(paste0("<p>La première inauguration d'une allée verte dans ",
  #                           "l'arrondissement a eu lieu en %s."), row$first_alley)
  # 
  #   other <- row[c("app_process_fr", "management_fr", "budget_fr")]
  #   other <- sprintf("%s<p>%s", start, paste0(other[!is.na(other)], collapse = " "))
  # 
  #   guide <- sprintf("%s<p><a href = '%s' target = '_blank'>Guide d'aménagement d'une ruelle verte (%s).</a>",
  #                    other, if (is.na(row$guide)) "" else row$guide, row$name)
  # 
  #   out <- if (!is.na(row$contact)) {
  #     sprintf("%s<p>Contact: <a href = 'mailto:%s'>%s</a>", guide, row$contact, row$contact)
  #   } else guide
  # 
  #   return(out)
  # })
  # 
  # alley_boroughs <- alley_boroughs[c("ID", "name", "text_en", "text_fr")]
  # 
  # 
  # # Individual alleys -------------------------------------------------------
  # 
  # ga_shp <- list.files("dev/data/alley/shp") |>
  #   stringr::str_subset(".shp$")
  # 
  # ga_shp <- sapply(ga_shp, \(x) {
  #   sf::st_read(sprintf("dev/data/alley/shp/%s", x)) |>
  #     tibble::as_tibble() |>
  #     sf::st_as_sf() |>
  #     sf::st_transform(crs) |>
  #     sf::st_zm()
  # }, simplify = FALSE, USE.NAMES = TRUE)
  # 
  # unvisited <-
  #   ga_shp$`ruelles-vertes.shp` |>
  #   dplyr::count(RUELLE_ID,  DATE_AMENA) |>
  #   dplyr::rename(name = RUELLE_ID, date = DATE_AMENA) |>
  #   dplyr::group_by(name) |>
  #   dplyr::summarize(date = min(date[n == max(n)])) |>
  #   # dplyr::mutate(date = as.Date(date, "%Y%m%d")) |>
  #   dplyr::select(name) |>
  #   sf::st_cast("MULTIPOLYGON")
  # 
  # # File from Google Earth
  # unvisited_2 <-
  #   ga_shp$`RV_OnlyGM.shp` |>
  #   # dplyr::mutate(date = as.Date(NA)) |>
  #   dplyr::select(name = Name) |> #, date, geometry) |>
  #   sf::st_buffer(2) |>
  #   sf::st_cast("MULTIPOLYGON")
  # 
  # # Additional unvisited alley of Montreal Nord
  # alleys_mn <-
  #   ga_shp$`mt_nordrv.shp` |>
  #   # dplyr::mutate(date = as.Date(NA)) |>
  #   dplyr::select(name = Name) |> #, date) |>
  #   (\(x) sf::st_buffer(x, 2))() |>
  #   sf::st_cast("MULTIPOLYGON")
  # 
  # unvisited <- rbind(unvisited, unvisited_2, alleys_mn)
  # 
  # visited <-
  #   ga_shp$`rv_visited.shp` |>
  #   dplyr::transmute(name = stringr::str_remove(Name, "^\\d*\\."),
  #                    name = stringr::str_trim(name)) |>
  #   sf::st_buffer(2)
  # 
  # visited_21 <-
  #   ga_shp$`alleys_21.shp` |>
  #   dplyr::transmute(name = gsub("\\\n", "", Name)) |>
  #   sf::st_buffer(2)
  # 
  # visited_22 <-
  #   ga_shp$`alleys_22.shp` |>
  #   dplyr::transmute(name = gsub("\\\n", "", Name)) |>
  #   sf::st_buffer(2)
  # 
  # visited <- rbind(visited, visited_21, visited_22)
  # 
  # # Merge the alley info
  # alley_info <- readxl::read_xlsx("dev/data/alley/alley_info.xlsx")
  # names(alley_info)[1:2] <- c("ID", "name")
  # 
  # # Try and fix the ID column
  # alley_info$ID <- tolower(alley_info$ID)
  # alley_info$ID <- gsub("not in sus \\\r\\\\", "", alley_info$ID)
  # alley_info$ID <- gsub("not on sus map \\\r\\\n", "", alley_info$ID)
  # alley_info$ID <- gsub("\\(not in sus\\)", "", alley_info$ID)
  # alley_info$ID <- gsub("\\(2022\\) not in sus", "", alley_info$ID)
  # alley_info$ID <- gsub("not in sus!", "", alley_info$ID)
  # alley_info$ID <- gsub("not in sus - ", "", alley_info$ID)
  # alley_info$ID <- gsub("not in sus\\. \\d*\\.", "", alley_info$ID)
  # alley_info$ID <- gsub("not in sus\\. ", "", alley_info$ID)
  # alley_info$ID <- gsub("\\r\\n.*", "", alley_info$ID)
  # alley_info$ID <- gsub("not in sus \\(", "", alley_info$ID)
  # alley_info$ID <- gsub("\\(.*\\)", "", alley_info$ID)
  # alley_info$ID <- gsub(")", "", alley_info$ID)
  # alley_info$ID <- stringr::str_trim(alley_info$ID)
  # 
  # # Merge the alley info with the geometries
  # visited$name <- gsub(" \\?$", "", visited$name)
  # visited$name <- gsub("\\(.*\\)", "", visited$name)
  # visited$name <- gsub(" entre ", " ", visited$name)
  # visited$name <- stringr::str_trim(visited$name)
  # unvisited$name <- stringr::str_trim(unvisited$name)
  # 
  # all_alleys <- rbind(visited, unvisited)
  # all_alleys$name <- tolower(all_alleys$name)
  # 
  # direct_match <- merge(all_alleys, alley_info, by.x = "name", by.y = "ID") |>
  #   tibble::as_tibble() |>
  #   sf::st_as_sf()
  # 
  # # Do some partial matching on the missings alleys
  # raw_alleys_no_match <- all_alleys[!all_alleys$name %in% direct_match$name, ]
  # alleys_info_no_match <- alley_info[!alley_info$ID %in% direct_match$name, ]
  # alleys_info_no_match <- alleys_info_no_match[!is.na(alleys_info_no_match$ID), ]
  # 
  # switch_id <- sapply(alleys_info_no_match$ID, agrep, x = raw_alleys_no_match$name,
  #                     value = TRUE, max.distance = 0.01,
  #                     simplify = FALSE, USE.NAMES = FALSE)
  # 
  # alleys_info_no_match$ID <- sapply(seq_along(switch_id), \(x) {
  #   new_val <- switch_id[[x]]
  #   old_val <- alleys_info_no_match$ID[[x]]
  # 
  #   if (length(new_val) == 0) return(old_val)
  #   return(new_val)
  # })
  # 
  # partial_match <- merge(raw_alleys_no_match, alleys_info_no_match, by.x = "name", by.y = "ID") |>
  #   tibble::as_tibble() |>
  #   sf::st_as_sf()
  # 
  # alleys <- rbind(direct_match, partial_match)
  # raw_alleys_no_match <- all_alleys[!all_alleys$name %in% alleys$name, ]
  # 
  # # Merge the visited alleys with the non-visited
  # raw_alleys_no_match$name <- stringr::str_to_title(raw_alleys_no_match$name)
  # alleys <- alleys[2:ncol(alleys)]
  # names(alleys)[1] <- "name"
  # alleys$created[alleys$created == "NA"] <- NA_character_
  # alleys$circulation[alleys$circulation == "Open"] <- "Opened"
  # 
  # touches <- sf::st_intersects(raw_alleys_no_match, alleys)
  # raw_alleys_no_match <- raw_alleys_no_match[sapply(touches, \(x) length(x) == 0), ]
  # 
  # # Manual match file coming from Emma
  # fix <- readxl::read_xlsx("dev/data/alley/alley_info_June2023.xlsx",skip = 1)
  # fix <- fix[2:ncol(fix)]
  # fix <- fix[1:(nrow(fix)-1), ]
  # fix$ID <- stringr::str_trim(fix$ID)
  # fix$ID <- tolower(fix$ID)
  # nomatch_sf <- raw_alleys_no_match
  # nomatch_sf$name <- tolower(nomatch_sf$name)
  # fix$name <- tolower(fix$name)
  # manual_match <- merge(fix, nomatch_sf, by.x = "ID", by.y = "name")
  # manual_match$created[manual_match$created == "NA"] <- NA
  # manual_match$Group <- manual_match$Group |> as.numeric()
  # 
  # # Bind the matched alleys with the non-matched
  # alleys <- dplyr::bind_rows(alleys, manual_match, raw_alleys_no_match)
  # 
  # # Get the text all writtened up
  # alleys$text_en <- sapply(seq_along(alleys$name), \(x) {
  #   row <- alleys[x, ]
  #   if (is.na(row$description)) return("<p>We do not have information on this green alley.")
  # 
  #   text <- list()
  # 
  #   if (!is.na(row$created)) text$inaug <- sprintf("Inauguration date: %s.",
  #                                                  row$created)
  #   text$desc <- row$description
  #   text$circulation <- sprintf("The green alley is %s to circulation.",
  #                               tolower(row$circulation))
  # 
  #   paste0(unlist(text), collapse = "<p>")
  # })
  # alleys$text_fr <- sapply(seq_along(alleys$name), \(x) {
  #   row <- alleys[x, ]
  #   if (is.na(row$description_fr)) return("<p>Nous ne disposons pas d'informations sur cette ruelle verte.")
  # 
  #   text <- list()
  # 
  #   if (!is.na(row$created)) text$inaug <- sprintf("Date d'inauguration: %s.",
  #                                                  row$created)
  #   text$desc <- row$description_fr
  #   text$circulation <- sprintf("La ruelle verte est %s à la circulation.",
  #                               tolower(row$circulation_fr))
  # 
  #   paste0(unlist(text), collapse = "<p>")
  # })
  # 
  # alleys$ID <- seq_along(alleys$name)
  # alleys <- alleys[c("ID", "name", "borough", "type", "text_en", "text_fr", "created",
  #                    "photo_ID", "geometry")]
  # alleys$type <- tolower(alleys$type)
  # 
  # # # Rename the photos in the folder
  # # before <- list.files("dev/data/alley/raw_images", full.names = TRUE)
  # # after <- tolower(before)
  # # after <- gsub("[^a-z|0-9|\\.|\\/|\\_]*", "", after)
  # # mapply(file.rename, before, after)
  # 
  # # Same treatment to the alleys$photo_ID vector
  # alleys$photo_ID <- tolower(alleys$photo_ID)
  # alleys$photo_ID <- gsub("[^a-z|0-9|\\.|\\/|\\_]*", "", alleys$photo_ID)
  # photo_available <- alleys$photo_ID %in% list.files("dev/data/alley/raw_images")
  # alleys$photo_ID <- sapply(seq_along(photo_available), \(x) {
  #   if (photo_available[[x]]) alleys$photo_ID[[x]] else NA
  # })
  # 
  # # Save --------------------------------------------------------------------
  # 
  # alleys_sf <- alleys
  # qs::qsave(alleys_sf, "dev/data/alley/alley_sf.qs")
  # 
  # alleys <- sf::st_drop_geometry(alleys)
  # qs::qsavem(alleys, alley_boroughs, file = "data/alleys.qsm")
  # 
  # 
  # # Resize images -----------------------------------------------------------
  # 
  # photos <- list.files("dev/data/alley/raw_images", full.names = TRUE)
  # 
  # zzz <- sapply(photos, function(photo) {
  #   img <- magick::image_read(photo)
  #   img_info <- magick::image_info(img)
  #   img <- magick::image_resize(img,
  #                               paste0(1000, "x",
  #                                      1000/img_info$width*img_info$height,"!"))
  #   path <- photo |> stringr::str_replace("dev/data/alley/raw_images/", "www/alleys/")
  #   magick::image_write(img, path)
  # })


  # Meters square of alleys per DAs -----------------------------------------
  
  # Meters square of alleys per kilometers square
  alleys <- qs::qread("dev/data/alley/alley_sf.qs")
  
  city_data <- 
    sapply(c("CSD", "CT", "DA"), \(scale) {
      
      data <- scales_variables_modules$scales$city[[scale]]["ID"]
      data <- sf::st_transform(data, crs)
      alleys_sf <- alleys["geometry"]
      intersected <- sf::st_intersection(alleys_sf, data)
      intersected$alley_area <- cc.buildr::get_area(intersected)
      
      # Drop the alley geometry and calculate per DAs
      intersected <- sf::st_drop_geometry(intersected)
      intersected <- stats::aggregate(intersected[, "alley_area"], 
                                      by = list(ID = intersected$ID), 
                                      FUN = sum) |> tibble::as_tibble()
      
      intersected <- merge(intersected, data, by = "ID")
      data_alley <- sf::st_as_sf(intersected)
      data_alley$da_area <- cc.buildr::get_area(data_alley)
      # 1 square kilometer = 1,000,000 square meters
      data_alley$alley_sqkm <- data_alley$alley_area / data_alley$da_area * 1000000
      data_alley <- data_alley[c("ID", "alley_sqkm")]
      data_final <- merge(sf::st_drop_geometry(data_alley), data, all = TRUE)
      data_final$alley_sqkm[is.na(data_final$alley_sqkm)] <- 0
      
      # Meters square of alleys per 1k residents
      data <- scales_variables_modules$scales$city[[scale]][c("ID", "population")]
      data <- sf::st_transform(data, crs)
      data_alley <- merge(intersected, sf::st_drop_geometry(data))
      
      data_alley$alley_per1k <- data_alley$alley_area / data_alley$population * 1000
      
      data <- merge(data_final, sf::st_drop_geometry(data_alley[c("ID", "alley_per1k")]), by = "ID",
                    all = TRUE)
      data$alley_per1k[is.na(data$alley_per1k)] <- 0
      
      data <- sf::st_drop_geometry(data)
      names(data)[2:3] <- paste0(names(data)[2:3], "_2023")
      data
      
    }, simplify = FALSE, USE.NAMES = TRUE)
  
  # Add to the scales
  data_interpolated <- list()
  data_interpolated$scales <- scales_variables_modules$scales
  data_interpolated$scales$city$CSD <- 
    merge(data_interpolated$scales$city$CSD, city_data$CSD, by = "ID")
  data_interpolated$scales$city$CT <- 
    merge(data_interpolated$scales$city$CT, city_data$CT, by = "ID")
  data_interpolated$scales$city$DA <- 
    merge(data_interpolated$scales$city$DA, city_data$DA, by = "ID")

  # Make a types named list -------------------------------------------------

  vars <- c("alley_sqkm_2023", "alley_per1k_2023")
  types <- list(`alley_sqkm` = "sqkm",
                `alley_per1k` = "per1k")


  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars,
      types = types
    )


  # Get the variables values per regions ------------------------------------

  # Make a parent string the same way as the types
  parent_strings <- list(`alley_sqkm` = NA,
                         `alley_per1k` = "population")

  region_vals <- variables_get_region_vals(
    scales = data_interpolated$scales,
    vars = c("alley_sqkm", "alley_per1k"),
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table)


  # Variables table ---------------------------------------------------------

  # For more information on how to append the information, read the
  # documentation of `add_variable`. Every variable needs to have its own entry
  # in the variables table. The following is an example.
  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "alley_sqkm",
      type = "sqkm",
      var_title = "Green alleys (m2) per square kilometre",
      var_short = "Green alleys",
      explanation = "the density of green alleys measured by square metres per square kilometres",
      exp_q5 = "the density of green alleys is _X_ square metres per square kilometres",
      parent_vec = NA,
      pe_include = TRUE,
      theme = "Ecology",
      private = FALSE,
      dates = "2023",
      avail_df = c("city_CSD", "city_CT", "city_DA"),
      breaks_q3 = with_breaks$q3_breaks_table$alley_sqkm,
      breaks_q5 = with_breaks$q5_breaks_table$alley_sqkm,
      region_values = region_vals$alley_sqkm,
      source = "Curbcut",
      interpolated = tibble::tibble(df = c("city_CSD", "city_CT", "city_DA"),
                                    interpolated_from = c("FALSE", "FALSE", "FALSE")),
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense")
    )
  
  variables <-
    add_variable(
      variables = variables,
      var_code = "alley_per1k",
      type = "per1k",
      var_title = "Green alleys (m2) per 1,000 residents",
      var_short = "Green alleys",
      explanation = "the density of green alleys measured by square metres per 1000 residents",
      exp_q5 = "the density of green alleys is _X_ square metres per 1,000 residents",
      parent_vec = NA,
      pe_include = TRUE,
      theme = "Ecology",
      private = FALSE,
      dates = "2023",
      avail_df = c("city_CSD", "city_CT", "city_DA"),
      breaks_q3 = with_breaks$q3_breaks_table$alley_per1k,
      breaks_q5 = with_breaks$q5_breaks_table$alley_per1k,
      region_values = region_vals$alley_sqkm,
      source = "Curbcut",
      interpolated = tibble::tibble(df = c("city_CSD", "city_CT", "city_DA"),
                                    interpolated_from = c("FALSE", "FALSE", "FALSE")),
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense")
    )


  # Modules table -----------------------------------------------------------

  # Facultative. If a page is to be added accompanying this data, add modules
  # description. Read the documentation of `add_module`. If no module is to be
  # created, assign `scales_variables_modules$modules` to modules.
  modules <- scales_variables_modules$modules
  
  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "alley",
      theme = "Ecology",
      nav_title = "Green alleys",
      title_text_title = "Green alleys",
      title_text_main = paste0(
        "<p>Green alleys are spaces that have been transformed by residents for th",
        "eir own activities. When adequately designed, these public spaces can ",
        "help reduce heat island effects, noise, and air pollution. These alley",
        "s have been classified into four types: green alleys, community-orient",
        "ed alleys, mixed alleys, and unmaintained alleys."
      ),
      title_text_extra = paste0(
        "<p>The datasets visualized on this page come from the City of Montreal Op",
        "en Data Portal, and Curbcut. To learn more about the Green Alley Progr",
        "am in Montreal, visit <a href = ‘https://montreal.ca/en/topics/green-a",
        "lleyways’ target = ‘_blank’>the city’s green alleyways page</a>. "
      ),
      regions = c("city"),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>We built an extensive set of green alleys across Montreal, which we",
        "re categorized as either green or community alleys. Our data was caref",
        "ully collected and validated, using various resources both public and ",
        "private. Here are our key data sources:</p><ul><li><a href=’https://do",
        "nnees.montreal.ca/dataset/ruelles-vertes’ target=’_blank’>Official Mon",
        "treal Open Data Portal - Green Alleys Dataset</a></li><li><a href=’htt",
        "ps://www.google.com/maps/d/viewer?mid=143hjP-d1kJ9dlifQF_2jtys85B4&ll=",
        "45.52200058770156%2C-73.47754611620758&z=11’ target=’_blank’>Google Ma",
        "ps Green Alleys - 2022 Version</a></li><li>Specific Maps Sent by the b",
        "orough of Mercier-Hochelaga-Maisonneuve (These are not publicly access",
        "ible) </li><li><a href=’https://docs.google.com/spreadsheets/d/1gbNQnE",
        "ErVOQdfN95Fg0uPHJitKC-JAEZ/edit#gid=486678456’ target=’_blank’>Montrea",
        "l-Nord Green Alleys List</a></li><li> Montréal-Nord Green Alleys Map s",
        "ent by the borough</li><li> NDG Green Alleys Map sent by the borough</",
        "a></li></ul><p>In addition to the above-mentioned sources, our team al",
        "so incorporated green alleys that we discovered during our site visits",
        ". This combination of digital data and on-ground exploration allows us",
        " to create a comprehensive and authentic catalogue of Montreal's green",
        " alleys. </p>"
      ),
      main_dropdown_title = NA_character_
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}
