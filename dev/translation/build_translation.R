#### Build translation #########################################################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(qs)

# From a CSV to code to create tibbles ------------------------------------

# form_translation_tibble <- function(df) {
#   
#   df <- df[!is.na(df$fr), ]
# 
#   en <- gsub("(.{55})", '\\1",\n"', df$en)
#   fr <- gsub("(.{55})", '\\1",\n"', df$fr)
# 
#   writeLines(paste0('tibble(en = character(), fr = character()) |>\n',
# 
#   paste0('add_row(en = paste0("', en, '"), \nfr = paste0("', fr, '"))',
#                     collapse = ' |> \n'))
#   )
# }


# Run all the translation preparation -------------------------------------
source("dev/translation/variables.R", encoding = "utf-8")
source("dev/translation/info_table.R", encoding = "utf-8")
source("dev/translation/ui_and_misc.R", encoding = "utf-8")
source("dev/translation/home_and_about.R", encoding = "utf-8")
source("dev/translation/title_text.R", encoding = "utf-8")
source("dev/translation/dyk.R", encoding = "utf-8")
source("dev/translation/green_alleys.R", encoding = "utf-8")

# Retrieve and bind translated csvs ---------------------------------------
# translation_fr <- 
#   map(list.files("dev/translation/csv"), ~{
#     read.csv(paste0("dev/translation/csv/", .x)) |> 
#       as_tibble()
#   }) |> 
#   reduce(bind_rows) |> 
#   distinct(en, .keep_all = TRUE)

translation_fr <- 
  bind_rows(home_and_about_translated,
            info_table_translated,
            ui_and_misc_translated,
            variables_translated,
            title_text_translation,
            dyk_translated,
            green_alleys_translated) |> 
  distinct(en, .keep_all = TRUE)


# Save to the translation files -------------------------------------------

qsave(translation_fr, "data/translation_fr.qs")
