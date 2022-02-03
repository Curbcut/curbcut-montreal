#### Build translation #########################################################



# Run all the translation preparation -------------------------------------
source("dev/translation/prep_variables.R")


# Retrieve and bind translated csvs ---------------------------------------
map(list.files("dev/translation/csv"), ~{
  read.csv(paste0("dev/translation/csv/", .x)) |> 
    as_tibble()
}) |> 
  reduce(bind_rows) |> 
  distinct(en, .keep_all = TRUE)


# Save to the translation files -------------------------------------------

qsave(translation_fr, "data/translation_fr.qs")
write_csv(translation_fr, "translations/translation_fr.csv")
