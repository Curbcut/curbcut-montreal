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
  
  # Take head out, which breaks Curbcut' CSS
  x <- readLines(out)
  
  if (file %in% c(list.files("dev/Rmd/stories"),
                  list.files("dev/Rmd/news"))) {
    x <- 
      str_remove_all(x, "(?<=src=\")../../../www/(?=stories|news)")

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
purrr::walk(stories_files, process_rmd, path = "stories")


# News --------------------------------------------------------------------

news_files <- list.files("dev/Rmd/news")
library(here)
purrr::walk(news_files, process_rmd, path = "news")


# Standalone --------------------------------------------------------------

standalone_files <- list.files("dev/Rmd/standalone") |> str_subset(".Rmd$")
purrr::walk(standalone_files, process_rmd, path = "standalone")

