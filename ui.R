curbcut::ui(
  site_name = site_name,
  web_description = paste0(
    "Curbcut est une plateforme d'exploration approfondie, dynamique et ",
    "intuitive de la durabilité urbaine."
  ), 
  web_title = paste0(site_name, " | Vers une ville durable"), 
  placeholder_video_src = "https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_placeholder.mp4",
  video_src = list(en = "https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_en.mp4",
                   fr = "https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_fr.mp4"),
  twitter_handler = "@curbcutca", 
  google_analytics = "www/google_analytics.html", 
  website_url = "https://montreal.curbcut.ca", 
  share_jpg = "https://montreal.curbcut.ca/share.jpg",
  apple_touch_icon = "https://montreal.curbcut.ca/logo192.jpg",
  lang_init = "fr",
  show_lang_button = TRUE, 
  show_cities = TRUE, 
  h1_first_line = list(en = "MONTREAL", fr = "RÉGION DE"), 
  h1_second_line = list(en = "REGION", fr = "MONTRÉAL")
  )
