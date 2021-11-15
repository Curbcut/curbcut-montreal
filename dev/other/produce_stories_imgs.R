#### Stories setup ############################################################

library(magick)

# Round the raw images ----------------------------------------------------

# IMAGES MUST BE TRANSFORMED TO PNG FIRST

img_name <- "little_burgundy.png"

round_img_shadow <- function(img_name) {

  path <- paste0("www/stories/raw_img/", img_name)
  img <- magick::image_read(path)
  shadow_right <- magick::image_read("www/dropshadow_right.png")
  
  # get height, width and crop longer side to match shorter side
  img_info <- magick::image_info(img)
  smaller_side <- min(img_info$height, img_info$width)
  img1 <- magick::image_crop(img, 
                     geometry = paste0(smaller_side, "x", 
                                       smaller_side, "!"))
  
  # resize shadow_right to fit with image size
  shadow_info <- magick::image_info(shadow_right)
  
  shadow_right <-
    image_crop(shadow_right, paste0({shadow_info$width-334}, "x", 
                                           shadow_info$height, "+167"))
  shadow_right <- 
    image_resize(shadow_right, 
                 paste0(smaller_side, "x", 
                        smaller_side,"!"))
  
  
  # create an image composite using both images
  round_img_shadow <- 
    magick::image_composite(img1, shadow_right, operator='copyopacity')
  
  image_write(round_img_shadow, paste0("www/stories/round_img/", img_name))
  
}

stories_img <- list.files("www/stories/raw_img")
purrr::walk(stories_img, round_img_shadow)
