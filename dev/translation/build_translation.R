#### Build translation #########################################################

# Load libraries ----------------------------------------------------------

library(dplyr)
library(qs)

# From a CSV to code to create tibbles ------------------------------------
# form_translation_tibble <- function(df) {
# 
#   df <- df[!is.na(df$fr), ]
# 
#   en <- gsub("(.{55})", '\\1",\n"', df$en)
#   fr <- gsub("(.{55})", '\\1",\n"', df$fr)
# 
#   # If it's too long, not all lines are printed. So save in a file!
#   if (nrow(df) < 200) {
#     writeLines(paste0('tibble(en = character(), fr = character()) |>\n',
# 
#                       paste0('add_row(en = paste0("', en, '"), \nfr = paste0("', fr, '"))',
#                              collapse = ' |> \n'))
#     )
#   } else {
# 
#     iter_size <- ceiling(length(en) / ceiling(length(en)/250))
#     translation_tibbles_list <-
#       lapply(seq_len(round(length(en)/250)), \(x) {
#         en1 <- en[((x - 1) * iter_size + 1):(x * iter_size)]
#         fr1 <- fr[((x - 1) * iter_size + 1):(x * iter_size)]
#         en1 <- en1[!is.na(en1) & !is.na(fr1)]
#         fr1 <- fr1[!is.na(en1) & !is.na(fr1)]
# 
#         paste0('RENAME_TKTK <- \n',
#                if (x != 1) 'RENAME_TKTK |>\n',
#                if (x == 1) 'tibble(en = character(), fr = character()) |>\n',
#                paste0('add_row(en = paste0("', en1, '"), \nfr = paste0("', fr1, '"))',
#                       collapse = ' |> \n')) |> paste0("\n\n")
#       })
# 
#     reduce(translation_tibbles_list, paste0) |>
#       writeLines("translated_tb.txt")
#     message(
#       paste0("File `translated_tb.txt` created in the root of the directory. ",
#              "Rename RENAME_TKTK"))
#   }
# 
# }


# Run all the translation preparation -------------------------------------
source("dev/translation/variables.R", encoding = "utf-8")
source("dev/translation/info_table.R", encoding = "utf-8")
source("dev/translation/ui_and_misc.R", encoding = "utf-8")
source("dev/translation/home_and_about.R", encoding = "utf-8")
source("dev/translation/title_text.R", encoding = "utf-8")
source("dev/translation/dyk.R", encoding = "utf-8")
source("dev/translation/green_alleys.R", encoding = "utf-8")
source("dev/translation/place_explorer.R", encoding = "utf-8")
source("dev/translation/authors.R", encoding = "utf-8")
source("dev/translation/centraide_vars.R", encoding = "utf-8")
source("dev/translation/data_export.R", encoding = "utf-8")
source("dev/translation/city_amenities.R", encoding = "utf-8")

# Retrieve and bind translated csvs ---------------------------------------

translation_fr <- 
  bind_rows(home_and_about_translated,
            info_table_translated,
            ui_and_misc_translated,
            variables_translated,
            title_text_translation,
            dyk_translated,
            green_alleys_translated,
            place_explorer_translated,
            authors_translation,
            cent_variables_translated,
            data_export_translated,
            city_amenities_translation) |> 
  distinct(en, .keep_all = TRUE)



# Test --------------------------------------------------------------------

# if (!exists("title_text")) title_text <- qs::qread("data/title_text.qs")
# missing_title_text <- 
#   title_text$text[
#     !sapply(title_text$text, \(x) x %in% translation_fr$en, USE.NAMES = FALSE)]
# if (length(missing_title_text) > 0) warning("Missing title text translations")
# 
# if (!exists("variables")) title_text <- qs::qread("data/variables.qs")
# missing_variables <- 
#   c(variables$var_title[
#     !sapply(variables$var_title, \(x) x %in% translation_fr$en, USE.NAMES = FALSE)],
#     variables$var_short[
#       !sapply(variables$var_short, \(x) x %in% translation_fr$en, USE.NAMES = FALSE)],
#     variables$explanation[
#       !sapply(variables$explanation, \(x) x %in% translation_fr$en, USE.NAMES = FALSE)],
#     variables$source[
#       !sapply(variables$source, \(x) x %in% translation_fr$en, USE.NAMES = FALSE)]) |> 
#   unique()
# if (length(missing_variables) > 0) warning("Missing variables translations")
# 
# if (!exists("modules")) title_text <- qs::qread("data/modules.qs")
# missing_modules <- 
#   modules$dataset_info[
#     !sapply(modules$dataset_info, \(x) x %in% translation_fr$en, USE.NAMES = FALSE)]
# if (length(missing_modules) > 0) warning("Missing modules translations")

# # .t <- function (x) deeplr::toFrench2(x, auth_key = .deepl_key)
# purrr::map_dfr(missing_variables, \(x) {
#     tibble(en = x,
#            fr = .t(x))
# }) |> form_translation_tibble()





# Save to the translation files -------------------------------------------

qsave(translation_fr, "data/translation_fr.qs")
