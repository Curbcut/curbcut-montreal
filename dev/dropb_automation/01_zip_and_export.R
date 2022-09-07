## ZIP DATA FILES AND EXPORT TO DROPBOX ########################################

# devtools::install_github("karthik/rdrop2")
# install.packages("RDCOMClient", 
#                  repos = "http://www.omegahat.net/R", type = "win.binary")


# Initialize rdrop2 -------------------------------------------------------

# token <- rdrop2::drop_auth(new_user = TRUE, cache = TRUE,
# , query_authorize_extra = list(token_access_type = "offline")) # gets credentials
# saveRDS(token, "~/droptoken.rds") # saves credentials

# Move credentials `droptoken.rds` to another folder and set the path in
# .Rprofile to .path_rdrop2_token

tok <- rdrop2::drop_auth(new_user = FALSE,
                  cache = TRUE,
                  rdstoken = .path_rdrop2_token)
tok <- tok$refresh()
saveRDS(tok, .path_rdrop2_token)


# Change local directory --------------------------------------------------

setwd(.local_sus_dir)


# Export /data to dropbox -------------------------------------------------

tryCatch({
  # Zip, replace, unlink
  zip(zipfile = "data.zip", files = "data")
  rdrop2::drop_delete("sus_data/data.zip")
  rdrop2::drop_upload("data.zip", path = "sus_data/")
  unlink("data.zip")
}, error = function(e) {
  # Send mail with the error if failed
  library(RDCOMClient)
  app <- COMCreate("Outlook.Application")
  mail <-  app$CreateItem(0)
  mail[["To"]] <-  "maxime.belangerdeblois@mcgill.ca"
  mail[["subject"]] <-  "Failed sus /data export"
  mail[["body"]] <-  
    paste0("The data failed to be exported to dropbox.\n\n", e)
  mail$Send()})


# Close all connections and unlink created files --------------------------

if ("data.zip" %in% list.files()) unlink("data.zip")

