## RESIZE NEWS PHOTOS #######################################################

photos <- 
  map(list.files("dev/data/news/visuals/", full.names = TRUE),
      ~list.files(.x, full.names = TRUE)) |> unlist()


walk(photos, function(photo) {
  img <- magick::image_read(photo)
  final_path <- photo |> str_replace("dev/data/news/visuals/", 
                                     "www/news/visuals/")
  dir <- str_extract(final_path, "(?<=www/news/visuals/).*(?=/)")
  
  if (!dir %in% list.files("www/news/visuals")) {
    dir.create(paste0("www/news/visuals/", dir))
  }
  
  img_info <- magick::image_info(img)
  
  if (img_info$filesize < 1000000) return(magick::image_write(img, final_path))
  
  img <- magick::image_resize(img, 
                              paste0(1000, "x", 
                                     1000/img_info$width*img_info$height,"!"))
  magick::image_write(img, final_path)
})

