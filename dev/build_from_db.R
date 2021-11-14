# Update data folders from a new run of build_data ##############################


# Download and unzip updated data -----------------------------------------

dl_unzip <- function(dir, data_url) {
  
  # Set working directory for decompression
  # simplifies unzip directory location behavior
  wd <- getwd()
  setwd(dir)
  
  unlink(list.files(full.names = TRUE))
  
  
  # Download zip file
  download.file(data_url,
                destfile = "temp.zip",
                mode = "wb")
  
  # Run decompression
    system2("unzip",
            args = c("-o", # include override flag
                     "temp.zip"),
            stdout = TRUE)
  
  # uncomment to delete archive once decompressed
  file.remove("temp.zip")
  
  # Reset working directory
  setwd(wd); rm(wd)
  
  
}


# Run the function on all data folders ------------------------------------

dl_unzip("dev/data",
         .sus_links[1])

dl_unzip("data",
         .sus_links[2])

dl_unzip("www/maps",
         .sus_links[3])
