#### Marketed sustainability data setup ########################################

# This script relies on objects created in dev/build_geometries.R


# # Load data ---------------------------------------------------------------
# 
# projects <- read.csv("dev/data/marketed_sustainability/projects.csv",
#                      encoding = "UTF-8") |>
#   as_tibble() |>
#   rename(website = Website) |>
#   mutate(website = ifelse(website == "", NA, website)) |>
#   mutate(ID = row_number(), .before = "civic_start") |> 
#   mutate(street_name = str_trim(street_name))
# 
# 
# # Scrape ------------------------------------------------------------------
# 
# library(rvest)
# 
# projects_text <- map(projects$website, ~{
#   if (is.na(.x) || .x == "") return(NA)
#   tryCatch({
#     html <- read_html(str_replace_all(.x, "/fr/", "/en/"))
#     p <- html_elements(html, "p") |> html_text2()
#     li <- html_elements(html, "li") |> html_text2()
# 
#     c(p, li) |>
#       str_replace_all("\n|\r", " ") |>
#       paste(collapse = " . ")
# 
#   }, error = function(e) NA, silent = TRUE)
# })
# 
# # Save scraped websites ---------------------------------------------------
# 
# marketed_sustainability <-
#   projects |>
#   select(ID:Notes) |>
#   mutate(text = unlist(projects_text)) |>
#   mutate(text = ifelse(text == "", NA, text))
# 
# rm(project, projects_text)
# 
# qsave(marketed_sustainability,
#       file = "dev/data/marketed_sustainability/marketed_sustainability.qs")
# 
# 
# # Translate dict ----------------------------------------------------------
# 
# dict <- read_csv("dev/data/marketed_sustainability/dictionary.csv")
# 
# dict <-
#   dict |>
#   transmute(word_en = Words,
#             word_fr = deeplr::toFrench2(word_en, auth_key = .deepl_key))
# 
# qsave(dict, file = "dev/data/marketed_sustainability/dict_bilingual.qs")

# Load scraped data -------------------------------------------------------

marketed_sustainability <- 
  qread("dev/data/marketed_sustainability/marketed_sustainability.qs") |> 
  # Only keep english for now
  filter(cld2::detect_language(text = text, plain_text = FALSE) %in% c("fr", "en"))

dict <- qread("dev/data/marketed_sustainability/dict_bilingual.qs")


# Add number of susutainability words detected and its proportion ---------

marketed_sustainability <- 
  bind_cols(marketed_sustainability, 
            map_dfr(marketed_sustainability$text, ~{
              
              if (is.na(.x)) return(tibble(words_detected = NA, 
                                           sustainability_prop = NA))
              
              # Detect language to pick the right dictionary language
              lang <- cld2::detect_language(text = .x, plain_text = FALSE)
              
              # Text vector cleaned of punctuations and stopwords
              clean <- .x |>
                str_remove_all("[:punct:]") |>
                str_to_lower() |>
                str_remove_all(paste0(stopwords::stopwords(lang), 
                                      collapse = "\\b|\\b")) |>
                str_replace_all("\\s+", " ") |>
                str_trim()
              
              lang_dict <- pull(select(dict, all_of(ends_with(lang))))
              
              # Vector of words detected by the dictionary
              detected <- lang_dict[str_detect(clean, lang_dict)]
              
              # Out
              tibble(words_detected = list(detected),
                     sustainability_prop = 
                       length(detected)/str_count(clean, "\\w+"))
              
            })
  )


# Add construction year ---------------------------------------------------

load("dev/data/marketed_sustainability/construction_with_year.Rdata")

construction_with_year <- 
  construction_with_year |> 
  mutate(street_name = str_remove(street_name, "  \\(.*\\)$"))

marketed_sustainability <- 
  marketed_sustainability |> 
  left_join(select(construction_with_year, civic_start, civic_end, street_name,
                   year_construction),
            by = c("civic_start", "civic_end","street_name")) |> 
  st_as_sf() |> 
  st_transform(4326)


# Adding fill color -------------------------------------------------------

range <- 
  c(min(marketed_sustainability$sustainability_prop),
    mean(marketed_sustainability$sustainability_prop),
    max(marketed_sustainability$sustainability_prop))

# Color gradient addition
marketed_sustainability <- 
  marketed_sustainability |> 
  mutate(fill = scales::gradient_n_pal(colours = c("#fee6e4", "#800000"), 
                                       values = range)(sustainability_prop),
         fill = paste0(fill, "EE"))


# Cleanup -----------------------------------------------------------------

rm(dict, construction_with_year)
