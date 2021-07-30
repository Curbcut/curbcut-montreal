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
  
  # Remove <head> tag
  x <- readLines(out)
  head_1 <- str_which(x, "<head>")
  head_2 <- str_which(x, "</head>")
  x <- x[-c(head_1:head_2)]
  
  # Write ouput
  writeLines(x, out)
}


# MCP ---------------------------------------------------------------------

mcp_files <- list.files("dev/Rmd/mcp")
walk(mcp_files, process_rmd, path = "mcp")
