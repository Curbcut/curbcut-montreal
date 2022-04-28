# Update data folders from a new run of build_data #############################

# Download and unzip updated data -----------------------------------------

dl_unzip <- function(dir, data_url) {
  
  if (Sys.info()["sysname"] != "Windows") {
    withr::with_options(list(timeout = 2000), {
      download.file(data_url, destfile = "temp.zip")
      unzip("temp.zip", unzip = "unzip", exdir = dir)
      file.remove("temp.zip")
      return(TRUE)
    })
  } else {
    wd <- getwd()
    setwd(dir)
    unlink(list.files(full.names = TRUE))
    download.file(data_url, destfile = "temp.zip", mode = "wb")
    system2("unzip", args = c("-o", "temp.zip"), stdout = TRUE)
    file.remove("temp.zip")
    setwd(wd)
  }
}


# Run the function on all data folders ------------------------------------

# dev_data
dl_unzip("dev/data", .sus_links[1])

# data
dl_unzip("data", .sus_links[2])

# data2
dl_unzip("data2", .sus_links[3])
