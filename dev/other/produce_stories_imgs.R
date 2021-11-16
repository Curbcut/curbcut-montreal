#### Stories setup ############################################################

library(magick)

# Round the raw images ----------------------------------------------------

# IMAGES MUST BE TRANSFORMED TO PNG FIRST

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
  
  # Bandeau
  bandeau <- image_resize(img, 
                          paste0(1000, "x", 
                                 1000/img_info$width*img_info$height,"!"))
  
  bandeau <- image_crop(img, paste0(1000, "x",
                                    200, "+0+100"))
  
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
  image_write(bandeau, paste0("www/stories/bandeau_img/", img_name))
  
}

stories_img <- str_subset(list.files("www/stories/raw_img"), ".png$")

if (length(stories_img) != length(list.files("www/stories/raw_img"))){
  not_pngs <- 
  list.files("www/stories/raw_img")[!list.files("www/stories/raw_img") %in% 
                                      stories_img]
  
  warning(paste0("Raw images must be PNGs. It is not the case for ", 
                 not_pngs, "\n"))
  
  # Later we will transform them in codes here rather then send a message
}

purrr::walk(stories_img, round_img_shadow)
