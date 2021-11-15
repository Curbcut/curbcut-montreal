#### Stories setup ############################################################

library(magick)

# Round the raw images ----------------------------------------------------

# IMAGES MUST BE TRANSFORMED TO PNG FIRST

round_img_shadow <- function(img_name) {

  path <- paste0("www/stories/raw_img/", img_name)
  img <- magick::image_read(path)
  shadow_right <- magick::image_read("www/dropshadow_right.png")
  
  # get height, width and crop longer side to match shorter side
  img_width <- magick::image_info(img)$width
  img1 <- magick::image_crop(img, geometry=paste0(img_width, "x", img_width, "+0+0"), repage=TRUE)
  
  # resize shadow_right to fit with image size
  shadow_info <- magick::image_info(shadow_right)
  
  shadow_right <- 
    image_resize(shadow_right, 
                 paste0(img_width*1.2, "x", 
                        img_width/shadow_info$width*shadow_info$height*1.2,"!"))
  
  
  # create an image composite using both images
  round_img_shadow <- 
    magick::image_composite(img1, shadow_right, operator='copyopacity')
  
  image_write(round_img_shadow, paste0("www/stories/round_img/", img_name))
  
}

stories_img <- list.files("www/stories/raw_img")
purrr::walk(stories_img, round_img_shadow)