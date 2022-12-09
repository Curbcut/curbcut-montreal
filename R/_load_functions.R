### LOAD FUNCTIONS #############################################################

#' This script is used to make the R folder a easier to navigate. The SUS custom
#' functions are placed in the R/functions/ folder, and this script sources 
#' them the same way Shiny would if they were directly in the R/ folder.


# Load functions in R/functions/ ------------------------------------------

# Hack here to get shiny environment, as when local is set to TRUE in a 
# purrr::walk, it the source is executed inside the walk rather than in 
# the "global" environment. We can get the current environment of the shiny
# session, and feed it to the local argument.

envir <- rlang::get_env(\(x) x)

lapply(list.files("R/functions/"), \(x) 
       source(paste0("R/functions/", x), local = envir, encoding = "utf-8"))

# # Load locally in R
# source("R/functions/__make_dropdown.R")
# source("R/functions/_get_news.R")
# envir <- rlang::get_env(\(x) x)
# lapply(list.files("R/")[
#   !list.files("R/") %in% c("functions", "tests", "_load_functions.R")], \(x)
#     source(paste0("R/", x), local = envir, encoding = "utf-8")
# ) |> invisible()
