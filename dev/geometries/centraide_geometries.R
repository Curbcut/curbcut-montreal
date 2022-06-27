# Import Centraide geometries --------------------------------------------

# Load necessary data to get population and households number for centraide
# zones.
qload("dev/data/centraide/StatCan_Recensement2016/Fichiers_Sources/tables.qsm")

source("dev/other/crosstabs_fun.R")


# Import centraide geometries ---------------------------------------------

centraide <- 
  rbind(
    {read_sf(paste0("dev/data/centraide/StatCan_Recensement2016/_Geographie/",
                    "Centraide_Quartiers_Laval_Temporaire.shp")) |> 
        rename(name = Quartier)},
    {read_sf(paste0("dev/data/centraide/StatCan_Recensement2016/_Geographie/",
                    "Centraide_Sous_Territoires_Montreal_RiveSud.shp")) |> 
        transmute(name = SouTerr)}
  )

# Add population and households using the two tables
centraide <- 
  centraide |> 
  mutate(ID = paste0("centraide_", row_number())) |> 
  mutate(name_2 = name) |> 
  left_join(rename(get_housing_characteristics()$centraide, 
                   name = ID, households = var), by = "name") |> 
  left_join(rename(get_vulnerable_pop()$centraide, 
                   name = ID, population = var), by = "name") |> 
  relocate(ID, name, name_2, population, households, .before = geometry)

