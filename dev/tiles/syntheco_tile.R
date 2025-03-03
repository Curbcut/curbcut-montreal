# Read the data placed in a folder in `dev/data/`
data <- qs::qread("dev/data/syntheco/hou.qs")["HH_ID"]
names(data)[1] <- "ID"

# Publish
id <- "mtl_syntheco6"
# cc.buildr::tileset_delete_tileset_source(id, "curbcut", .cc_mb_token)
# cc.buildr::tileset_upload_tile_source_large(df = data, id = id, 
#                                             username = "curbcut", 
#                                             access_token = .cc_mb_token)

recipe <- '{
  "recipe": {
    "version": 1,
    "layers": {
      "mtl_syntheco": {
        "source": "mapbox://tileset-source/curbcut/mtl_syntheco",
        "minzoom": 3,
        "maxzoom": 15,
        "tiles": {
          "layer_size": 2500
        },
        "features": {
          "filter": [
            "all",
            [
              "case",
              ["==", ["zoom"], 16], true,
              ["==", ["zoom"], 15], true,
              ["==", ["zoom"], 14], true,
              ["==", ["zoom"], 13], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 5], 0],
              ["==", ["zoom"], 12], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 10], 0],
              ["==", ["zoom"], 11], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 20], 0],
              ["==", ["zoom"], 10], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 40], 0],
              ["==", ["zoom"], 9], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 80], 0],
              ["==", ["zoom"], 8], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 160], 0],
              ["==", ["zoom"], 7], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 320], 0],
              ["==", ["zoom"], 6], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 640], 0],
              ["==", ["zoom"], 5], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 1280], 0],
              ["==", ["zoom"], 4], ["==", ["%", ["to-number", ["slice", ["get", "ID"], 12]], 2560], 0],
              false
            ]
          ]
        }
      }
    }
  },
  "name": "mtl_syntheco"
}'




cc.buildr::tileset_delete_tileset(id, "curbcut", .cc_mb_token)
cc.buildr::tileset_create_tileset(id, recipe, "curbcut", .cc_mb_token)
cc.buildr::tileset_publish_tileset(id, "curbcut", .cc_mb_token)
