#### Get back census dfs with geometries ######################################

qs::qload("data2/CMA_full.qsm")
centraide <- qs::qread("data2/centraide_full.qs")
grid <- qs::qread("data2/grid_full.qs")

borough <- borough_full
CT <- CT_full
DA <- DA_full


# Cleanup -----------------------------------------------------------------

rm(borough_full, CT_full, DA_full)
