#### Build Rmd documents  ######################################################

# Processing function -----------------------------------------------------
# process_stories_rmd <- function(file, css_path = here::here("www/sus.css")) {
#   
#   # Error handling
#   if (!sum(str_detect(file, "Rmd")) == length(file)) return(
#     warning(sprintf("Skipped %s as it's not an Rmd document.", file))
#   )
#   
#   # Prep locations
#   out_file <- gsub("Rmd", "html", file)
#   out <- paste0("www/stories/", out_file)
#   
#   # Custom output format with the CSS file
#   custom_html_output_format <- rmarkdown::html_document(css = css_path)
#   
#   # Extract image filename
#   image_filename <- gsub("_en|_fr", "", file)
#   image_filename <- gsub("\\.Rmd", "", image_filename)
#   image_filename <- paste0("dev/Rmd/stories/bandeau_img/", image_filename, ".png")
#   image_filename <- here::here(image_filename)
#   
#   # Read RMarkdown content
#   rmd_content <- readLines(paste0("dev/Rmd/stories/", file))
#   
#   # Add image chunk right after the title texts
#   title_chunk_index <- which(str_detect(rmd_content, "---"))
#   image_chunk <- paste0(
#     c(
#       "",
#       "```{r echo=FALSE, out.width='100%', fig.align='center'}",
#       paste0("knitr::include_graphics('", image_filename, "')"),
#       "```"
#     ),
#     collapse = "\n"
#   )
#   rmd_content <- append(rmd_content, image_chunk, after = title_chunk_index[length(title_chunk_index)])
#   
#   # Save modified RMarkdown content to a temporary file
#   temp_rmd_file <- tempfile(pattern = "temp_", fileext = ".Rmd")
#   writeLines(rmd_content, temp_rmd_file)
#   
#   # Render temporary RMarkdown document
#   rmarkdown::render(
#     temp_rmd_file, 
#     output_file = out,
#     output_format = custom_html_output_format,
#     output_dir = paste0("www/stories"),
#     quiet = TRUE
#   )
# }

# MCP ---------------------------------------------------------------------

mcp_files <- list.files("dev/Rmd/mcp") |> str_subset(".Rmd$")
purrr::walk(mcp_files, process_rmd, path = "mcp")


# Montreal stories --------------------------------------------------------

# stories_files <- list.files("dev/Rmd/stories")
# library(here)
# purrr::walk(stories_files, process_stories_rmd)

# NOW IN CC.BUILDR

# News --------------------------------------------------------------------

news_files <- list.files("dev/Rmd/news")
library(here)
purrr::walk(news_files, process_rmd, path = "news")


# Standalone --------------------------------------------------------------

standalone_files <- list.files("dev/Rmd/standalone") |> str_subset(".Rmd$")
purrr::walk(standalone_files, process_rmd, path = "standalone")

