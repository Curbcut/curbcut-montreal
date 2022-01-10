#### SIMPLIFY CENSUS GEOMETRIES ################################################

suppressPackageStartupMessages(library(rmapshaper))

borough$geometry <- 
  borough$geometry |> 
  st_transform(32618) |> 
  ms_simplify(keep = 0.5, keep_shapes = TRUE) |> 
  st_transform(4326)

CT$geometry <-
  CT$geometry |> 
  st_transform(32618) |> 
  ms_simplify(keep = 0.5, keep_shapes = TRUE) |> 
  st_transform(4326)

DA$geometry <-
  DA$geometry |> 
  st_transform(32618) |> 
  ms_simplify(keep = 0.5, keep_shapes = TRUE) |> 
  st_transform(4326)

