#### ALLEY TILE PROCESSING ###################################################

alley_sf <- qs::qread("dev/data/alley/alley_sf.qs")

# Transform to the right CRS for mapbox
alley_sf <- sf::st_transform(alley_sf, crs = 4326)
alley_sf <- alley_sf |> sf::st_cast("MULTIPOLYGON")
alley_sf <- alley_sf[c("ID", "type")]
alley_sf$ID <- alley_sf$ID |> as.character()

# Reset
cc.buildr::tileset_delete_tileset_source("mtl_alleys",
                                         username = "sus-mcgill", 
                                         access_token = .cc_mb_token)
cc.buildr::tileset_delete_tileset("mtl_alleys",
                                  username = "sus-mcgill", 
                                  access_token = .cc_mb_token)

# Upload tile source
cc.buildr::tileset_upload_tile_source(df = alley_sf,
                                      id = "mtl_alleys",
                                      username = "sus-mcgill", 
                                      access_token = .cc_mb_token)

# Create recipe
recipe <- cc.buildr::tileset_create_recipe(
  layer_names = "alley", 
  source = "mapbox://tileset-source/sus-mcgill/mtl_alleys", 
  minzoom = 0, 
  maxzoom = 14, 
  layer_size = 2500, 
  recipe_name = "mtl_alleys")

# Create tileset and publish
cc.buildr::tileset_create_tileset("mtl_alleys", 
                                  recipe, 
                                  username = "sus-mcgill", 
                                  access_token = .cc_mb_token)
cc.buildr::tileset_publish_tileset("mtl_alleys", 
                                  username = "sus-mcgill", 
                                  access_token = .cc_mb_token)