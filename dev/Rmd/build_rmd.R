#### Build Rmd documents  ######################################################

# Processing function -----------------------------------------------------

process_rmd <- function(file, path, css_path = here::here("www/sus.css")) {
  
  # Error handling
  stopifnot(sum(str_detect(file, "Rmd")) == length(file))
  
  # Prep locations
  out_file <- gsub("Rmd", "html", file)
  out <- paste0("www/", path, "/", out_file)
  
  # Custom output format with the CSS file
  custom_html_output_format <- rmarkdown::html_document(css = css_path)
  
  # Render document
  rmarkdown::render(paste0("dev/Rmd/", path, "/", file), 
                    output_format = custom_html_output_format,
                    output_dir = paste0("www/", path),
                    quiet = TRUE)
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

