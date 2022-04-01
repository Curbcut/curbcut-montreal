## RESIZE ALLEY PHOTOS ########################################################

photos <- list.files("dev/data/green_alleys/alleys", full.names = TRUE)

walk(photos, function(photo) {
  img <- magick::image_read(photo)
  img_info <- magick::image_info(img)
  img <- magick::image_resize(img, 
                              paste0(1000, "x", 
                                     1000/img_info$width*img_info$height,"!"))
  path <- photo |> str_replace("dev/data/green_alleys/", "www/")
  magick::image_write(img, path)
})
