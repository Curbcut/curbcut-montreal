#### Build Rmd documents  ######################################################

# Processing function -----------------------------------------------------

process_rmd <- function(file, path) {
  
  # Error handling
  stopifnot(sum(str_detect(file, "Rmd")) == length(file))
  
  # Prep locations
  out_file <- str_replace(file, "Rmd", "html")
  out <- paste0("www/", path, "/", out_file)
  
  # Render document
  rmarkdown::render(paste0("dev/Rmd/", path, "/", file), 
                    output_dir = paste0("www/", path),
                    quiet = TRUE)
  
  # Remove long script tag
  # x <- readLines(out)
  # long_script <- str_which(x, "<script")
  # long_script <- long_script[nchar(x[long_script]) > 100000]
  # x <- x[-long_script]
  
  # Write ouput
  # writeLines(x, out)
  
  # Take head out, which breaks Sus' CSS
  x <- readLines(out)
  
  if (file %in% list.files("dev/Rmd/stories")) {
    x <- 
      str_remove_all(x, "(?<=src=\")../../../www/(?=stories)")
  } else {
    x <-
      x[-((str_detect(x, "<head") |> which()):(str_detect(x, "</head") |> which()))]
  }
  
  writeLines(x, out)
}


# MCP ---------------------------------------------------------------------

mcp_files <- list.files("dev/Rmd/mcp") |> str_subset(".Rmd$")
purrr::walk(mcp_files, process_rmd, path = "mcp")


# Crash -------------------------------------------------------------------

process_rmd("crash.Rmd", "crash")


# Montreal stories --------------------------------------------------------

stories_files <- list.files("dev/Rmd/stories")
library(here)
map_base_style <- "mapbox://styles/sus-mcgill/cl0reqoz4000z15pekuh48ld6"
map_zoom <- 10.1
map_loc <- c(-73.58, 45.53)

purrr::walk(stories_files, process_rmd, path = "stories")

process_rmd("cycling_infrastructure_en.Rmd", path = "stories")

# Standalone --------------------------------------------------------------

standalone_files <- list.files("dev/Rmd/standalone") |> str_subset(".Rmd$")
purrr::walk(standalone_files, process_rmd, path = "standalone")

