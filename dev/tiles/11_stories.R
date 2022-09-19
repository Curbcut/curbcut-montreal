#### STORIES TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/stories.qsm")
source("dev/tiles/_tile_functions.R")


# Create and upload main map tileset --------------------------------------

# Table rase first
delete_tileset_source("stories-stories")
delete_tileset("stories-stories")

stories |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  select(ID, name, geometry) |> 
  upload_tile_source("stories-stories")

stories_recipe <- 
  create_recipe(
    layer_names = "stories-stories",
    source = "mapbox://tileset-source/sus-mcgill/stories-stories",
    minzoom = 3,
    maxzoom = 13, 
    recipe_name = "stories-stories")

create_tileset("stories-stories", stories_recipe)
publish_tileset("stories-stories")


# Create and upload metro_evolution maps ----------------------------------

metro_evolution <- 
  list.files("dev/data/stories/shp/metro_evolution/", full.names = TRUE) |> 
  str_subset("\\.shp$")

metro_evolution <-
  map(set_names(metro_evolution), read_sf) |> 
  (\(x) map2(x, names(x), ~{mutate(.x, date = .y)}))() |> 
  reduce(rbind) |> 
  mutate(date = as.character(str_extract(date, "\\d{4}"))) |> 
  mutate(fill = case_when(Type == "Proposed extension" ~ "#1263A6",
                          Type == "Proposed extensions" ~ "#1263A6",
                          Type == "Proposed line" ~ "#000000",
                          Type == "Proposed lines" ~ "#000000",
                          Type == "Proposed orange and green lines" ~ "#000000", 
                          Type == "Proposed red line" ~ "#8B0000",
                          Type == "Green line" ~ "#00A650", 
                          Type == "Orange line" ~ "#F47216", 
                          Type == "Yellow line" ~ "#FCD300",
                          Type == "Proposed orange line extension" ~ "#f5D7A4",
                          Type == "Proposed green line extension" ~ "#76C274",
                          Type == "Proposed blue line" ~ "#9CC0F0",
                          Type == "Blue line" ~ "#1082CD",
                          Type == "Proposed blue line extension" ~ "#9CC0F0",
                          Type == "Proposed line 10" ~ "#000000",
                          Type == "Proposed line 11" ~ "#454545",
                          Type == "Proposed line 6" ~ "#696868",
                          Type == "Proposed line 7" ~ "#949292",
                          Type == "Proposed line 8" ~ "#B5B1B1",
                          Type == "Blue line extension" ~ "#9CC0F0",
                          Type == "Proposed yellow line extension" ~ "#FAF093",
                          Type == "Proposed pink line" ~ "#E60E70")) |> 
  select(date, fill)

metro_evolution <- 
  map_dfr(unique(metro_evolution$date), ~{
    out <- 
      metro_evolution |> 
      filter(date == .x) |> 
      mutate(new_fill = fill)
    
    names(out)[names(out) == "new_fill"] <- paste0("fill_", .x)
    
    out
  }) |> 
  relocate(geometry, .after = last_col()) |> 
  mutate(across(fill_1910:fill_2000, ~{ifelse(is.na(.x), "#FFFFFF00", .x)})) |> 
  select(-date, -fill)

metro_evolution |> 
  upload_tile_source("stories-metro_evolution")

stories_recipe <- 
  create_recipe(
    layer_names = "stories-metro_evolution",
    source = "mapbox://tileset-source/sus-mcgill/stories-metro_evolution",
    minzoom = 3,
    maxzoom = 13, 
    recipe_name = "stories-metro_evolution")

create_tileset("stories-metro_evolution", stories_recipe)
publish_tileset("stories-metro_evolution")


# Create and upload cycling_infrastructure maps -------------------

# Import paths
lines <- 
  st_read("dev/data/stories/shp/cycling_infrastructure/Velo.gdb", 
          layer = "RESEAU_1991_2016")  %>% 
  st_transform(4326) |>
  as_tibble() |> 
  st_as_sf() |> 
  select(IdRte, starts_with("An")) |> 
  mutate(An2016 = if_else(is.na(An2016), as.numeric(0), as.numeric(An2016))) |> 
  st_zm()

# Don't keep geometries without bike lanes at any year
lines <- 
lines[!!rowSums(st_drop_geometry(lines)[
  str_starts(names(st_drop_geometry(lines)), "An")]), ]
lines <- lines[, sort(names(lines))]
lines <- relocate(lines, IdRte) |> rename(geometry = Shape)

# We have paths for all these years
all_years <- 
  names(lines) |> 
  str_subset("\\d{4}$") |> 
  str_extract("\\d{4}$")

# Decide colors we want as fill, and create the source with fill_year
new_color <- "#73AE80"
removed_color <- "#CA0020"
remained_color <- "#2E4633"

lines_fill <- 
map_dfc(all_years, function(year) {
  
  actual_year <- as.name(paste0("An", year))
  previous_year <- as.name(paste0("An", all_years[which(all_years == year) - 1]))
  
  out <-
    lines |> 
    st_drop_geometry() |> 
    (\(x) if (year == all_years[1]) {
      mutate(x, new_fill = case_when(!!actual_year == 1 ~ new_color,
                                     TRUE ~ "#FFFFFF00")) 
    } else {
      mutate(x, new_fill = case_when(!!actual_year == 1 & !!previous_year == 0 ~ 
                                       new_color,
                                     !!actual_year == 0 & !!previous_year == 1 ~ 
                                       removed_color,
                                     !!actual_year == 1 & !!previous_year == 1 ~ 
                                       remained_color,
                                     TRUE ~ "#FFFFFF00"))
    })() |> 
    # filter(new_fill != "#FFFFFF00") |> 
    select(new_fill)
  
  
  names(out) <- paste0("fill_", year)
  
  out
  
})

lines <- 
  bind_cols(lines_fill, lines[, c("geometry")]) |> 
  st_as_sf()

# Cut that invalid geometry
lines <- lines[-81965, ]

# Add 2022 lines 
lines_2022 <- 
  st_read("dev/data/stories/shp/cycling_infrastructure/Cycling_Lane.shp") |>  
  st_transform(4326) |> 
  as_tibble() |> 
  transmute(ID = row_number(), fill_2022 = "#73AE80", geometry) |> 
  st_as_sf() |> 
  filter(!st_is_empty(geometry))

lines <- 
  bind_rows(lines, lines_2022)  |>  
  st_transform(4326) |> 
  select(-ID) |> 
  relocate(geometry, .after = last_col()) |> 
  mutate(across(where(is.character), ~if_else(is.na(.x), "#FFFFFF00", .x)))


# Join all lines that share the same combinations of colors
nested <- 
  lines |> 
  group_nest(across(c(-geometry)))

joined_geometries <- 
  map_dfr(nested$data, ~{tibble(geometry = st_combine(.x))})

lines <- 
  bind_cols(select(nested, -data), joined_geometries) |> 
  st_as_sf() |> 
  filter(!st_is_empty(geometry))

# Separate the last lines of geometries, as it's too heavy for mapbox
# lines[nrow(lines), ] |> 
#   st_intersection(st_make_grid(borough)) |> 
#   bind_rows(lines[1:(nrow(lines) - 1), ]) |> 
lines |> 
  # Upload
  upload_tile_source("stories-cycling_infrastructure-l")


# Import bixi points
bixi_2016 <- 
  read_csv("dev/data/stories/shp/cycling_infrastructure/Stations_2016.csv") |> 
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) |> 
  transmute(year2016 = TRUE)

bixi_2022 <- read_csv("dev/data/stories/shp/cycling_infrastructure/20220105_stations.csv") |> 
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) |> 
  filter(!pk %in% c(856)) |> 
  transmute(year2022 = TRUE)

# Create dataframe with both 2016 and 2022 data points
both_years <- 
st_join(bixi_2016, bixi_2022) |> 
  filter(year2016 & year2022)

only_2016 <- 
  st_join(bixi_2016, bixi_2022) |> 
  filter(year2016 & is.na(year2022))

only_2022 <- 
  st_join(bixi_2022, bixi_2016) |> 
  filter(year2022 & is.na(year2016))

bixi <- 
bind_rows(only_2016, only_2022, both_years) |> 
  mutate(across(all_of(c("year2016", "year2022")), ~if_else(is.na(.x), FALSE, TRUE))) |> 
  mutate(point_2016 = if_else(year2016 == TRUE, as.character(3), as.character(0))) |> 
  mutate(point_2022 = if_else(year2022 == TRUE, as.character(3), as.character(0))) |> 
  mutate(fill_2016 = if_else(year2016 == TRUE, "#000000", "#FFFFFF00")) |> 
  mutate(fill_2022 = if_else(year2022 == TRUE, "#000000", "#FFFFFF00")) |> 
  select(fill_2016, fill_2022, point_2016, point_2022)

# Make sure points are transparent for all the years where we do not have bixi
# info
bixi <- 
  bind_cols(map_dfc(all_years[1:(length(all_years) - 1)], ~{
    out <- tibble(col1 = rep("0", nrow(bixi)),
                  col2 = rep("#FFFFFF00", nrow(bixi)))
    names(out) <- c(paste0("point_", .x), paste0("fill_", .x))
    out
  }), bixi) |> 
  st_as_sf()

# Join all points that share the same combinations of colors
nested_bixi <- 
  bixi |> 
  group_nest(across(c(-geometry)))

joined_geometries <- 
  map_dfr(nested_bixi$data, ~{tibble(geometry = st_combine(.x))})

bixi <- 
  bind_cols(select(nested_bixi, -data), joined_geometries) |> 
  st_as_sf() |> 
  filter(!st_is_empty(geometry))

# Upload
bixi |> 
  upload_tile_source("stories-cycling_infrastructure-b")

# Recipe to send both lines and points
stories_recipe <- 
  create_recipe(
    layer_names = c("lines", "bixi"),
    source = c(
      lines = "mapbox://tileset-source/sus-mcgill/stories-cycling_infrastructure-l",
      bixi = "mapbox://tileset-source/sus-mcgill/stories-cycling_infrastructure-b"),
    minzoom = c(lines = 3, bixi = 12),
    maxzoom = c(lines = 16, bixi = 16), 
    layer_size = c(lines = 2500, bixi = 2500),
    simp_zoom = c(lines = 1, bixi = 1),
    fallback_simp_zoom = c(lines = 3, bixi = 3),
    recipe_name = "stories-cycling_infrastructure")

# Create and publish tileset
create_tileset("stories-cycling_infrastructure", stories_recipe)
publish_tileset("stories-cycling_infrastructure")


