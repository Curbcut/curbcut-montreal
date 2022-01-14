#### SUS DATA TESTING ########################################################
#' @data is a named list of all the dataframes to evaluate. They must be the
#' usual normal geometries after the data joining.


meta_testing <- function(data = list("borough" = borough, "CT" = CT, "DA" = DA, 
                                     "grid" = grid, "street" = street, 
                                     "building" = building)) {
  
  # List to store all the possible warnings
  warn_vec <- c()
  
  # Borough naming is untouched
  if ("borough" %in% names(data)) {
    borough_naming <- 
      utf8::as_utf8(c("Lavaltrie", "Richelieu", "Saint-Mathias-sur-Richelieu",
        "Saint-Jean-sur-Richelieu", "Chambly", "Carignan",
        "Saint-Basile-le-Grand", "McMasterville", "Otterburn Park",
        "Mont-Saint-Hilaire", "Beloeil", "Saint-Mathieu-de-Beloeil",
        "Brossard", "Saint-Lambert", "Boucherville", "Saint-Bruno-de-Montarville",
        "Longueuil", "Sainte-Julie", "Saint-Amable", "Varennes", "Verchères",
        "Charlemagne", "Repentigny", "Saint-Sulpice --parish municipality",
        "L'Assomption", "L'Épiphanie", "L'Épiphanie --parish municipality",
        "Saint-Lin--Laurentides", "Terrebonne", "Mascouche", "Laval",
        "Montréal-Est", "Ahuntsic-Cartierville", "Outremont", "Saint-Léonard",
        "Mercier-Hochelaga-Maisonneuve", "Côte-des-Neiges-Notre-Dame-de-Grâce",
        "Rosemont-La Petite-Patrie", "Saint-Laurent",
        "Villeray-Saint-Michel-Parc-Extension", "Anjou", "Pierrefonds-Roxboro",
        "Verdun", "LaSalle", "Ville-Marie", "Le Plateau-Mont-Royal", "Le Sud-Ouest",
        "Rivière-des-Prairies-Pointe-aux-Trembles", "Lachine", "Montréal-Nord", 
        "L'Île-Bizard-Sainte-Geneviève", "Westmount", "Montréal-Ouest", 
        "Côte-Saint-Luc", "Hampstead", "Mont-Royal", "Dorval", "L'Île-Dorval", 
        "Pointe-Claire", "Kirkland", "Beaconsfield", "Baie-d'Urfé", 
        "Sainte-Anne-de-Bellevue", "Senneville", "Dollard-des-Ormeaux", 
        "Saint-Mathieu", "Saint-Philippe", "La Prairie", "Candiac", "Delson", 
        "Sainte-Catherine", "Saint-Constant", "Saint-Isidore --parish municipality", 
        "Mercier", "Châteauguay", "Léry", "Kahnawake", "Beauharnois", 
        "Saint-Zotique", "Les Coteaux", "Coteau-du-Lac", "Les Cèdres", 
        "Pointe-des-Cascades", "L'Île-Perrot", "Notre-Dame-de-l'Île-Perrot", 
        "Pincourt", "Terrasse-Vaudreuil", "Vaudreuil-Dorion", 
        "Vaudreuil-sur-le-Lac", "L'Île-Cadieux", "Hudson", "Saint-Lazare", 
        "Saint-Eustache", "Deux-Montagnes", "Sainte-Marthe-sur-le-Lac", 
        "Pointe-Calumet", "Saint-Joseph-du-Lac", "Oka", "Saint-Placide", 
        "Kanesatake", "Boisbriand", "Sainte-Thérèse", "Blainville", "Rosemère", 
        "Lorraine", "Bois-des-Filion", "Sainte-Anne-des-Plaines", "Mirabel", 
        "Saint-Colomban", "Saint-Jérôme", "Gore"))
    data_borough_names <- utf8::as_utf8(data$borough$name)
    if (!all(str_detect(borough_naming,
                        paste0(data_borough_names, collapse = "|")))) {
      forgotten_name <- 
        borough_naming[!str_detect(borough_naming, 
                                      paste0(data_borough_names, collapse = "|"))] |> 
        na.omit()
      
      warn_vec <- 
        c(warn_vec, 
          paste0("Borough `", forgotten_name, "` is missing from borough df."))
    }
  }
  
  
  # SF and geometry column
  # First, are they sf?
  all_sf <- map_lgl(data, ~{"sf" %in% class(.x)})
  if (all(all_sf)) {
  
  # Expected sf?
  expected_sf <- 
    c("borough" = "MULTIPOLYGON", "CT" = "MULTIPOLYGON", "DA" = "MULTIPOLYGON",
      "grid" = "MULTIPOLYGON", "street" = "LINESTRING", 
      "building" = "MULTIPOLYGON")
  
  expected_sf <- 
  (map(data, ~{as.character(unique(st_geometry_type(.x)))}) == 
      expected_sf)
  
  if (!all(expected_sf)) {
    wrong_sf <- names(expected_sf[!expected_sf])
    warn_vec <- 
      c(warn_vec, 
        paste0("`", wrong_sf, "` is of unexpected sf."))
  }
  
  # geometry column last
  geometry_last <- map_lgl(data, ~{
    names(.x)[length(names(.x))] == "geometry"
  })
  
  if (!all(geometry_last)) {
    wrong_placement <- names(geometry_last[!geometry_last])
    warn_vec <- 
      c(warn_vec, 
        paste0("`", wrong_placement, "` should have `geometry` as its last ",
               "column."))
  }
  } else {
    not_sf <- names(all_sf[!all_sf])
    warn_vec <- 
      c(warn_vec, 
        paste0("`", not_sf, "` is not an sf dataframe, as it should."))
  }
  
  # Check first fields name
  field_names <- 
  list("borough" = c("ID", "name", "name_2", "CSDUID", "population", 
                     "households"),
       "building" = c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", 
                      "osm_ID", "grid_ID"),
       "CT" = c("ID", "name", "name_2", "CTUID", "CSDUID", "population", 
                "households"),
       "DA" = c("ID", "name", "name_2", "DAUID", "CTUID", "CSDUID", "population", 
                "households"),
       "grid" = c("ID", "name", "name_2", "CSDUID", "population", "households"),
       "street" = c("ID", "name", "name_2", "street_type", "DAUID", "CTUID", 
                    "CSDUID", "osm_ID", "grid_ID"))
  
  expected_first_fields <- map_lgl(set_names(names(field_names)), ~{
    first_fields <- unlist(field_names[names(field_names) == .x])
    all((data[[.x]][, 1:length(first_fields)] |>
      names() |>
        str_remove("geometry")) |>
        str_subset(".+") == first_fields)
  })
  
  if (!all(expected_first_fields)) {
    wrong_first_fields <- names(expected_first_fields[!expected_first_fields])
    
    warnings_vec <- 
      map_chr(wrong_first_fields, ~{
      paste0("`", .x, "` has its first columns corrupted. ",
             "They should be in order: ", paste(field_names[[.x]], 
                                                collapse = ", "))
    })
    
    warn_vec <- 
      c(warn_vec, warnings_vec)
  }
  
  
  # Number of rows
  # Check first fields name
  number_rows <- 
    list("borough" = 111,
         "building" = 66884,
         "CT" = 970,
         "DA" = 6469,
         "grid" = 9923,
         "street" = 68938)
  
  expected_nrow <- map_lgl(set_names(names(data)), ~{
    nrow(data[[.x]]) == number_rows[names(number_rows) == .x]
    })
  
  if (!all(expected_first_fields)) {
    wrong_nrow <- names(expected_nrow[!expected_nrow])
    
    warnings_vec <- 
      map_chr(wrong_nrow, ~{
        paste0("`", .x, "` has its number of rows corrupted. ",
               "The number of rows should be: ", number_rows[[.x]])
      })
    
    warn_vec <- 
      c(warn_vec, warnings_vec)
  }
  
  
  # Unique IDs
  unique_ids <- map_lgl(data, ~{
    length(unique(.x[["ID"]])) == length(.x[["ID"]])
  })
  
  if (!all(unique_ids)) {
    not_unique_ids <- names(unique_ids[!unique_ids])
    warn_vec <- 
      c(warn_vec, 
        paste0("`", not_unique_ids, "` doesn't have unique IDs."))
  }
  
  
  if (length(warn_vec) > 0 ) {
    # Display all warning/error messages, be sure that they can be fixed all
    # at once. 8 lines fit before truncated.
    if (length(warn_vec) > 8) {
      lines_8 <- str_which(1:length(warn_vec) %% 8, "0")
      all_errors <- map(lines_8, ~{
        warn_vec[eval(parse(text = paste0(c(.x-7, .x), collapse = ":")))]
      })
      if (!length(warn_vec) %in% lines_8) {
        all_errors <- 
          c(all_errors, 
            list(warn_vec[(lines_8[length(lines_8)] + 1):length(warn_vec)]))
      }
    } else {
      all_errors <- list(warn_vec)
    }
    map(all_errors, ~{
      warning(paste(.x, collapse = "\n  "))
    })
    stop("These warnings relate to problems in the data.")
    
  }
}

