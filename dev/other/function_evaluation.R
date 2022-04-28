#### FUNCTION EVALUATION #######################################################

library(NCmisc)

# Top level scripts -------------------------------------------------------

funcs_top <- lapply(c("global.R", "server.R", "ui.R"), list.functions.in.file)

sapply(funcs_top, names) |> 
  unlist() |> 
  unique()

i <- 1; print(c("global.R", "server.R", "ui.R")[[i]]); funcs[[i]]


# R folder ----------------------------------------------------------------

funcs_R <- lapply(list.files("R", pattern = "R$", full.names = TRUE), 
                  list.functions.in.file)

sapply(funcs_R, names) |> 
  unlist() |> 
  unique()

i <- 1; print(list.files("R", pattern = "R$")[[i]]); funcs[[i]]


# R/functions folder ------------------------------------------------------

funcs_R_functions <- 
  lapply(list.files("R/functions", pattern = "R$", full.names = TRUE), 
         list.functions.in.file)

sapply(funcs_R_functions, names) |> 
  unlist() |> 
  unique()

i <- 1; print(list.files("R/functions", pattern = "R$")[[i]]); funcs[[i]]

