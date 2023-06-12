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

# Load modules
modules <- qs::qread("data/modules.qs")

# Change which column is subset
strings <- modules$nav_title

translation_fun(strings)


translation_pages <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "Housing system", 
          fr = "Système de logement") |>
  add_row(en = "Vacancy", 
          fr = "Poste vacant") |>
  add_row(en = "Active living potential", 
          fr = "Potentiel de vie active") |>
  add_row(en = "Access to amenities", 
          fr = "Accès aux équipements") |>
  add_row(en = "Climate risk", 
          fr = "Risque climatique") |>
  add_row(en = "Natural infrastructure", 
          fr = "Infrastructures naturelles") |>
  add_row(en = "Green alleys", 
          fr = "Allées vertes") |>
  add_row(en = "Housing affordability", 
          fr = "Logement abordable") |>
  add_row(en = "Road safety", 
          fr = "Sécurité routière") |>
  add_row(en = "Bikeway comfort and safety", 
          fr = "Confort et sécurité des pistes cyclables") |>
  add_row(en = "Place explorer", 
          fr = "Explorateur de lieux") |>
  add_row(en = "Montréal stories", 
          fr = "Montréal stories")

translation_pages |> View()
