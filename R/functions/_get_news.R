### GET NEWS INFO ##############################################################

# Retrieve news info
get_news_info <- function(link) {
  
  x <- paste0(readLines(link), collapse = " ")
  
  title <- 
    str_extract(x, "(?<=<h1).*?(?=</h1>)") |> 
    str_remove(".*>")
  
  preview <- 
    str_extract(x, '(?<=<p>).*?(?=</p>)')
  
  date <- 
    str_extract(x, '(?<=<h4 class="date">).*?(?=</h4>)')
  
  img <- 
    {str_extract(x, '(?<=<img src=").*?(?=")')}[1]
  
  return(list(title = title,
              preview = preview,
              date = date,
              img = img))
}

get_news <- function(r) {
  
  all_news <- 
    list.files("www/news", full.names = TRUE) |> 
    str_subset("\\.html$") |> 
    str_extract("(?<=www/news/).*(?=_)") |> 
    unique()
    
  fr_news <- 
    lapply(all_news, \(x) get_news_info(paste0("www/news/", x, "_fr.html")))
  names(fr_news) <- paste0(all_news, "_fr")
  
  en_news <- 
    lapply(all_news, \(x) get_news_info(paste0("www/news/", x, "_en.html")))
  names(en_news) <- paste0(all_news, "_en")
  
  # NEXT WILL BE TO ORDER THE NEWS BASED ON THE DATE
  
  return(list(fr = fr_news, en = en_news))
}


