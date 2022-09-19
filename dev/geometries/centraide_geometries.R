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
  ) |> 
  st_transform(4326)

# Add population and households using the two tables
centraide <- 
  centraide |> 
  mutate(ID = paste0("centraide_", row_number())) |> 
  mutate(name_2 = name) |> 
  left_join(rename(get_housing_char()$centraide, 
                   name = ID, households = var), by = "name") |> 
  left_join(rename(get_vulnerable_pop()$centraide, 
                   name = ID, population = var), by = "name") |> 
  relocate(ID, name, name_2, population, households, .before = geometry)



# Add name_2 to centraide DAs and CTs -------------------------------------

add_name_2 <- function(df, geo) {
  df_filtered <- 
    df |> 
    st_filter(geo) |> 
    filter(is.na(name_2)) |> 
    select(ID)
  
  df_filtered <- 
    df_filtered |>
    mutate(previous_area = units::drop_units(st_area(geometry))) |>
    st_intersection(select(geo, name_2)) |>
    st_set_agr("constant") |> 
    st_make_valid() |> 
    mutate(new_area = units::drop_units(st_area(geometry))) |>
    filter({new_area / previous_area} > 0.33) |> 
    transmute(ID, name_2_centraide = name_2) |> 
    st_drop_geometry()
  
  df |> 
    left_join(df_filtered, by = "ID") |> 
    mutate(name_2 = if_else(is.na(name_2), name_2_centraide, name_2)) |> 
    select(-name_2_centraide)
}

CT <- add_name_2(CT, centraide)
DA <- add_name_2(DA, centraide)


# Cleanup -----------------------------------------------------------------

rm(table1, table2)
