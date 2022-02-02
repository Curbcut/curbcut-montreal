#### `Variables` preparation for translation ###################################

suppressPackageStartupMessages({
  require(tidyverse)
  require(qs)
})


# Import variables and translation ----------------------------------------
variables <- qread("data/variables.qs")

translated <- read.csv("dev/translation/csv/variables_translated.csv") |> 
  as_tibble()


# Prepare for translation -------------------------------------------------

# Retrieve text from columns to translate
cols_to_translate <- 
  variables |> 
  select(var_title, var_short, explanation, theme) |> 
  names()

text_variables <- 
  map(cols_to_translate, ~variables[[.x]]) |> reduce(c) |> unique()

# Unnest breaks and retrieve qualitative breaks
breaks_quali <- 
  variables |> 
  unnest(breaks_q5) |> 
  filter(!is.na(var_name) | !is.na(var_name_short)) |> 
  select(var_name, var_name_short) |> 
  distinct()

breaks_quali <- 
  c(breaks_quali$var_name, breaks_quali$var_name_short) |> 
  unique()

# Construct vector to translate
to_translate <- c(text_variables, breaks_quali)


# Create table to join existing french translation ------------------------
variables_translated <- 
  tibble(en = to_translate) |> 
  left_join(translated, by = "en")

# Warning if missing translation
walk(seq_along(variables_translated$fr), ~{
  
  x <- pull(variables_translated[.x, "fr"])
  
  if (is.na(x)) {
    no_translation <-
      pull(variables_translated[.x, "en"])
    warning(paste0("No translation found for `", no_translation, 
                   "` (variables table)."), 
            call. = FALSE)
  }
})

# Save --------------------------------------------------------------------

# Saving fresh version of variables table translated
Encoding(variables_translated$fr) <- "UTF-8"
write_csv(variables_translated, 
          file = "dev/translation/csv/variables_translated.csv")


# Clean-up ----------------------------------------------------------------
rm("breaks_quali", "cols_to_translate", "con", "prev_translation", 
  "text_variables", "to_translate", "translated", "variables", 
  "variables_translated")