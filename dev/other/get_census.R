# Get census function

get_census <- function(region = list(PR = "24"), scale, format = TRUE, 
                       crs = 32618) {
  out <- cancensus::get_census(
    dataset = "CA16",
    regions = region,
    level = scale,
    geo_format = "sf",
    quiet = TRUE)  |> 
    as_tibble() |>
    st_as_sf() |>
    select(ID = GeoUID, any_of(var_select), geometry) |>
    arrange(ID) |>
    mutate(name = ID, .after = ID) |> 
    st_set_agr("constant")
  
  if (!format) return(out)
  
  keep_ids <- 
    out |>
    st_transform(crs) |> 
    st_point_on_surface() |> 
    st_filter(st_transform(master_polygon, crs)) |> 
    pull(ID)
  
  out |> 
    filter(ID %in% keep_ids)
}
