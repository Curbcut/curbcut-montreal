#### Build translation #########################################################



# Run all the translation preparation -------------------------------------
source("dev/translation/variables.R", encoding = "utf-8")
source("dev/translation/info_table.R", encoding = "utf-8")
source("dev/translation/ui_and_misc.R", encoding = "utf-8")
source("dev/translation/home_and_about.R", encoding = "utf-8")


# Retrieve and bind translated csvs ---------------------------------------
translation_fr <- 
  map(list.files("dev/translation/csv"), ~{
    read.csv(paste0("dev/translation/csv/", .x)) |> 
      as_tibble()
  }) |> 
  reduce(bind_rows) |> 
  distinct(en, .keep_all = TRUE)


# Save to the translation files -------------------------------------------

qsave(translation_fr, "data/translation_fr.qs")
