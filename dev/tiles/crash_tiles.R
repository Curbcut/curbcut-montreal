# Read the data placed in a folder in `dev/data/`
data <- read.csv("dev/data/crash/collisions_routieres.csv") |> 
  tibble::as_tibble()

cols <- c("AN", "GRAVITE",
          "NB_VICTIMES_PIETON", "NB_VICTIMES_VELO", "CD_GENRE_ACCDN",
          "NB_VICTIMES_TOTAL", "LOC_LONG", "LOC_LAT")
data <- data[cols]

# As sf
data <- data[!is.na(data$LOC_LAT), ]
data <- data[!is.na(data$LOC_LONG), ]
data <- sf::st_as_sf(data, coords = c("LOC_LONG", "LOC_LAT"), crs = 4326)
years <- unique(data$AN)

# Add ID, ped, cyc columns
data$ID <- paste0("crash_", seq_along(data$geometry))
data$ped <- data$NB_VICTIMES_PIETON > 0
data$cyc <- data$NB_VICTIMES_VELO > 0
data <- data[c("ID", "ped", "cyc", "AN")]

# Save to keep track of the ID of every point
qs::qsave(data, file = "dev/data/crash/crash_ID.qs")

# Publish every year of crash point
lapply(years, \(year) {
  df <- data[data$AN == year, ]
  df <- df[c("ID", "ped", "cyc")]
  
  # Upload tile source
  id <- sprintf("mtl_crash_%s", year)
  cc.buildr::tileset_delete_tileset_source(id, "curbcut", .cc_mb_token)
  cc.buildr::tileset_upload_tile_source(df, id, "curbcut", .cc_mb_token)

  recipe <- cc.buildr::tileset_create_recipe(
    layer_names = id,
    source = sprintf("mapbox://tileset-source/curbcut/%s", id),
    minzoom = 3,
    maxzoom = 15,
    simp_zoom = 15,
    simp_value = 1,
    layer_size = 2500,
    recipe_name = id)
  
  cc.buildr::tileset_delete_tileset(id, "curbcut", .cc_mb_token)
  cc.buildr::tileset_create_tileset(id, recipe, "curbcut", .cc_mb_token)
  cc.buildr::tileset_publish_tileset(id, "curbcut", .cc_mb_token)
})
