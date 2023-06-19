library(tibble)

# Translation function
.deepl_key <- "42c0646a-5ebd-ada0-07a0-d27c8eb37613:fx"
.t <- function(x) deeplr::toFrench2(x, auth_key = .deepl_key)
translation_fun <- function(strings) {
  z <- lapply(strings, \(x) {
    fr <- .t(x)
    sprintf('add_row(en = "%s",
          fr = "%s")', x, fr)
  })
  paste0(z, collapse = " |>\n") |>
    writeLines()
}

# Load stories
qs::qload("data/stories.qsm")

# Change which column is subset
strings <- stories$title
translation_fun(strings)

strings <- stories$short_title
translation_fun(strings)

strings <- stories$preview
translation_fun(strings)

strings <- unique(unlist(stories$themes))
translation_fun(strings)

#nav_title
translation_pages <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "", 
          fr = "") |>
  add_row(en = "", 
          fr = "")
  
  