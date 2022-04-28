## RESIZE MCP PHOTOS ########################################################

photos <- list.files("dev/Rmd/mcp/visuals", full.names = TRUE)
photos <- map(photos, list.files, full.names = TRUE) |> unlist()

walk(photos, function(photo) {
  img <- magick::image_read(photo)
  img_info <- magick::image_info(img)
  img <- magick::image_resize(img, 
                              paste0(1000, "x", 
                                     1000/img_info$width*img_info$height,"!"))
  path <- photo |> str_replace("dev/data/green_alleys/", "www/")
  magick::image_write(img, path)
})
