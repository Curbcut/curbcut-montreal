## RESIZE STORIES PHOTOS #######################################################

photos <- 
  map(list.files("dev/data/stories/raw_images", full.names = TRUE), 
      list.files, full.names = TRUE) |> unlist()

photos <- photos[!str_detect(photos, "banner_bubble_raw_img|.gif$")]

walk(photos, function(photo) {
  img <- magick::image_read(photo)
  final_path <- photo |> str_replace("dev/data/stories/raw_images/", 
                                     "www/stories/visuals/")
  
  dir <- str_extract(final_path, "(?<=www/stories/visuals/).*(?=/)")
  
  if (!dir %in% list.files("www/stories/visuals")) {
    dir.create(paste0("www/stories/visuals/", dir))
  }
  
  img_info <- magick::image_info(img)
  
  if (img_info$filesize < 1000000) return(magick::image_write(img, final_path))
  
  img <- magick::image_resize(img, 
                              paste0(1000, "x", 
                                     1000/img_info$width*img_info$height,"!"))
  magick::image_write(img, final_path)
})
