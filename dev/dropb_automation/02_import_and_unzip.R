### Update data, preferably daily ##############################################

# Download and unzip updated data -----------------------------------------

dl_unzip_data <- function(dir = "data", data_url) {
  
  if (Sys.info()["sysname"] != "Windows") {
    withr::with_options(list(timeout = 2000), {
      download.file(data_url, destfile = "temp.zip")
      unzip("temp.zip", unzip = "unzip", exdir = dir)
      unzip(paste0(dir, "/data.zip"), unzip = "unzip")
      file.remove("temp.zip")
      file.remove(paste0(dir, "/data.zip"))
      return(TRUE)
    })
  } else {
    download.file(data_url, destfile = paste0(dir, "/temp.zip"), mode = "wb")
    unzip(paste0(dir, "/temp.zip"), unzip = "unzip", exdir = dir)
    unzip(paste0(dir, "/data.zip"), unzip = "unzip")
    unlink(paste0(dir, "/temp.zip"))
    unlink(paste0(dir, "/data.zip"))
  }
}



# Data folder -------------------------------------------------------------

dl_unzip_data(.sus_dropbox)
