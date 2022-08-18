#### Get back census dfs with geometries ######################################

qs::qload("data2/census_full.qsm")
centraide <- qs::qread("data2/centraide_full.qs")

borough <- borough_full
CT <- CT_full
DA <- DA_full


# Cleanup -----------------------------------------------------------------

rm(borough_full, CT_full, DA_full)
