#### Get back census dfs with geometries ######################################

qload("data2/census_full.qsm")

borough <- borough_full
CT <- CT_full
DA <- DA_full


# Cleanup -----------------------------------------------------------------

rm(borough_full, CT_full, DA_full)
