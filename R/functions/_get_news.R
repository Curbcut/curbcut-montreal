### GET NEWS INFO ##############################################################

get_news_info <- function(r) {
  
  # lan <- r$lang()
  
  all_news <- 
    list.files("www/news", full.names = TRUE) |> 
    str_subset(paste0(lan, "\\.html$")) |> 
    str_extract("(?<=www/news/).*(?=_)")
    
  out <- 
  lapply(all_news, function(news) {
    
    link <- 
      paste0("www/news/", news, "_", lan, ".html")
    
    x <- paste0(readLines(link), collapse = "")
    
    title <- 
      str_extract(x, "(?<=<h1).*?(?=</h1>)") |> 
      str_remove(".*>")
    
    preview <- 
      str_extract(x, '(?<=<p>).*?(?=</p>)')

    date <- 
      str_extract(x, '(?<=<h4 class="date">).*?(?=</h4>)')
    
    img <- 
      {str_extract(x, '(?<=<img src=").*?(?=")')}[1]
    
    list(title = title,
         preview = preview,
         date = date,
         img = img)
    
  })
  
  names(out) <- all_news
  
  # NEXT WILL BE TO ORDER THE NEWS BASED ON THE DATE
  
  return(out)
}


