### 211 DOWNLOAD SCRIPT #######################################################

library(rvest)
library(tidyverse)
library(RSelenium)
library(qs)

# 211 download functions -------------------------------------------------

scrape_211 <- function(url = "https://www.211qc.ca/rechercheframe",
                      out_folder = "dev/data/211/") {
  
  categories <- 
    read_html(url) |>
    html_elements(".recherche-filtre-list-item-value") |> 
    str_extract(paste0("(?<=<span class=\\\"recherche-filtre-list-item-",
                       "value\\\">).*?(?=</span>)")) |> 
    na.omit()
    
  subcategories <- 
    imap_dfr(categories, function(category, cat_n) {
      read_html(paste0("https://www.211qc.ca/rechercheframe?cat-id=", cat_n)) |> 
        html_elements(".recherche-filtre-sub-list-item") |> 
        (\(x) tibble(category = !!category, 
                     subcategory = html_text(x),
                     href = paste0("https://www.211qc.ca/", html_attr(x, "href"))))()
    })
  
  return(subcategories)

}

data211 <- scrape_211()

data211 <- 
  data211 |> 
  filter(subcategory != "Toutes les sous-catégories")

# Start Rselenium
shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, 
                      browserName = "firefox")
remDr$open()

get_211 <- function(data211,
                    out_file = "dev/data/211/data_211_scrape.qs") {
  
  new_out <- tibble(category = character(), 
                    subcategory = character(),  
                    organism = character(), 
                    address = character())
  
  # Create 20 groups, to save in the case it fails
  data211$group <- cut(seq_len(nrow(data211)), 20)
  
  for (i in seq_along(unique(data211$group))) {
    
    actual_group <- unique(data211$group)[[i]]
    
    actual_211 <- filter(data211, group == actual_group)
    
    out <- 
      map_dfr(seq_len(nrow(actual_211)), function(data_row) {
        remDr$navigate(actual_211[data_row,]$href)
        Sys.sleep(2)
        load_check <- FALSE
        iters <- 0
        while (!load_check && iters <= 5) {
          iters <- iters + 1
          load_check <-
            suppressMessages(
              tryCatch({
                # This element is only true once the page is loaded
                page_content <- remDr$getPageSource()
                t_f <- str_detect(page_content, "recherche-content-list-item-title")
                if (isFALSE(t_f)) stop("ERROR")
                TRUE
              }, error = function(e) {
                # If the key element isn't loaded, wait one second then try again
                Sys.sleep(1)
                FALSE
              }
              ))
        }
        page_content <- remDr$getPageSource() 
        organisms <- 
          page_content[[1]] |> 
          read_html() |> 
          html_elements(".recherche-content-list-item") |> 
          (\(x) tibble(organism = html_text(html_elements(x, ".recherche-content-list-item-title")),
                       address = html_text(html_elements(x, ".organisme-address"))))()
        
        tibble(category = rep(actual_211[data_row,]$category, nrow(organisms)),
               subcategory = rep(actual_211[data_row,]$subcategory, nrow(organisms))) |> 
          bind_cols(organisms)
      })
    
    new_out <- bind_rows(new_out, out) |> 
      filter(organism != "void")
    
    qs::qsave(new_out, out_file)
    
  }
}

# No need to assign considering it will get saved in "dev/data/211/data_211_scrape.qs"
get_211(data211)


# Save with postal codes --------------------------------------------------

data211 <- qread("dev/data/211/data_211_scrape.qs")

data211 <- 
data211 |> 
  mutate(postal_code = str_trim(address) |> 
           str_extract("\\w\\d\\w \\d\\w\\d$")) |> 
  mutate(postal_code = str_remove(postal_code, " ") |> 
           str_to_lower())

qsave(data211, "dev/data/211/data_211_scrape.qs")


# Add postal codes sf -----------------------------------------------------

library(sf)
postal_codes <- qread("data/postal_codes.qs")

data211 <- 
data211 |> 
  left_join(dplyr::select(postal_codes, postal_code), by = "postal_code") |> 
  st_as_sf()

qs::qsave(data211, "dev/data/211/data_211_scrape.qs")
