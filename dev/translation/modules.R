to_translate <- modules$theme |> unique()
to_translate <- to_translate[!is.na(to_translate)]

# vars <- vars[!vars$source %in% unique(vars$source)[c(3:7, 10)], ]



vars <- vars[vars$source == unique(vars$source)[9], ]


strings <- lapply(c("var_title", "var_short", "explanation", "explanation_nodet", "exp_q5",
         "theme", "group_name", "group_diff", "source", "rankings_chr"),
       \(x) c(unlist(vars[[x]]), names(unlist(vars[[x]])))) |>
  unlist() |>
  unique()

strings <- strings[!is.na(strings)]

z <- lapply(to_translate, \(x) {
  fr <- .t(x)

  sprintf('add_row(en = "%s",
          fr = "%s")', x, fr)
})

paste0(z, collapse = " |>\n") |>
  writeLines()

tibble(en = character(),
       fr = charager()) |> 
  add_row(en = "Housing",
          fr = "Logement") |>
  add_row(en = "Health",
          fr = "Santé") |>
  add_row(en = "Transport",
          fr = "Mobilité") |>
  add_row(en = "Climate",
          fr = "Climat") |>
  add_row(en = "Ecology",
          fr = "Écologie")