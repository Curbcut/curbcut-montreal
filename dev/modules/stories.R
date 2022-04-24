#### Stories data setup ########################################################

# This script is totally independent

# Create key tibble -------------------------------------------------------

stories <- tibble(
  name = character(),
  title = character(),
  img = character(),
  preview = character(),
  lon = numeric(),
  lat = numeric()) 


# Adding stories ----------------------------------------------------------

stories <- 
  stories |> 
  
  add_row(
    name = "little_burgundy",
    title = paste0("Environmental racism and green gentrification in ",
                   "Montreal’s Little Burgundy"),
    img = "little_burgundy.png",
    preview = paste0("Little Burgundy has been the site of nearly 100 years ",
                     "of social and environmental transformation, and is ",
                     "currently undergoing a wave of green gentrification."),
    lon = -73.574962, 
    lat = 45.479311) |> 
  
  add_row(
    name = "mirron_quarry",
    title = paste0("The Miron Quarry: The transformation of an industrial ",
                   "limestone quarry into a flourishing environmental complex"),
    img = "mirron_quarry.png",
    preview = paste0("Residents and government are coming together to shape ",
                     "the future of the Saint-Michel Environmental Complex, ",
                     "located on the site of the former Miron Quarry."),
    lon = -73.61389,
    lat = 45.57222) |>
  
  add_row(
    name = "parc_ouest",
    title = "Understanding what lies beneath Montreal’s Grand Parc de L’Ouest",
    img = "parc_ouest.png",
    preview = paste0("In the Grand Parc de L’Ouest, a renewed drive to ",
                     "protect biodiversity and create greenspace illustrates ",
                     "the changing politics of development on the island."),
    lon = -73.93723394534209,
    lat = 45.45384833740566) |> 
  
  add_row(
    name = "turcot_interchange",
    title = paste0("The reconstruction of the Turcot Interchange: The ",
                   "intersection of community desires and government ",
                   "priorities"),
    img = "turcot_interchange.png",
    preview = paste0("The City of Montreal claims it is looking to support ",
                     "pedestrian-oriented projects as a pathway towards ",
                     "sustainable mobility, but contradictions in the ",
                     "implementation stage of the Turcot Interchange suggest ",
                     "otherwise."),
    lon = -73.600007,
    lat = 45.468355) |>
  
  add_row(
    name = "griffintown",
    title = paste0("Griffintown, From a gift of the machine age to a water ",
                   "wheel in the growth machine"),
    img = "griffintown.png",
    preview = paste0("Griffintown’s social and governance history can be ",
                     "defined through the politics of water management."),
    lon = -73.560739,
    lat = 45.496812)


# Finish table ------------------------------------------------------------
  
stories <- 
  stories |>
  mutate(ID = seq_len(n()), .before = name)


# Produce images ----------------------------------------------------------

# IMAGES MUST BE TRANSFORMED TO PNG FIRST
round_img_shadow <- function(img_name) {
  
  path <- paste0("dev/data/stories_raw_images/banner_bubble_raw_img/", 
                 img_name, ".png")
  img <- magick::image_read(path)
  shadow_right <- magick::image_read("dev/data/dropshadow_right.png")
  
  # Get height, width and crop longer side to match shorter side
  img_info <- magick::image_info(img)
  smaller_side <- min(img_info$height, img_info$width)
  img1 <- magick::image_crop(img, geometry = paste0(smaller_side, "x", 
                                                    smaller_side, "!"))
  
  # Resize to 300px
  img2 <- magick::image_resize(img1, "300x300!")
  
  # Resize shadow_right to fit with image size
  shadow_info <- magick::image_info(shadow_right)
  
  shadow_right <- magick::image_crop(shadow_right, paste0(
    {shadow_info$width - 334}, "x", shadow_info$height, "+167"))
  shadow_right <- magick::image_resize(shadow_right, "300x300!")
  
  # Create an image composite using both images
  round_img_shadow <- 
    magick::image_composite(img2, shadow_right, operator = 'copyopacity')
  
  # Bandeau
  bandeau <- magick::image_resize(img, paste0(
    1000, "x", 1000/img_info$width*img_info$height,"!"))
  bandeau <- magick::image_crop(img, paste0(1000, "x", 200, "+0+100"))
  magick::image_write(bandeau, paste0("www/stories/bandeau_img/", img_name,
                                      ".png"))
  
  # Return image for atlas
  round_img_shadow
  
}


# Create image atlas ------------------------------------------------------

story_images <- 
  stories$name |> 
  purrr::map(round_img_shadow) |> 
  magick::image_join() |> 
  magick::image_append()

magick::image_write(story_images, "www/stories/image_atlas.png")


# Create image mapping ----------------------------------------------------

stories_mapping <- 
  stories$name |> 
  seq_along() |> 
  map(~list(
    x = (.x - 1) * 300,
    y = 0,
    width = 300,
    height = 300
  )) |> 
  set_names(stories$name)


# Clean up ----------------------------------------------------------------

rm(story_images, round_img_shadow)
