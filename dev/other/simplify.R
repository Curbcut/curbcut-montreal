#### SIMPLIFY CENSUS GEOMETRIES ################################################

suppressPackageStartupMessages(library(rmapshaper))

borough$geometry <- ms_simplify(borough$geometry, keep = 0.5, keep_shapes = TRUE)
CT$geometry <- ms_simplify(CT$geometry, keep = 0.5, keep_shapes = TRUE)
DA$geometry <- ms_simplify(DA$geometry, keep = 0.5, keep_shapes = TRUE)
