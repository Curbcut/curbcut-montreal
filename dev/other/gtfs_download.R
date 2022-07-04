### GTFS DOWNLOAD SCRIPT #######################################################

# GTFS download function --------------------------------------------------

scrape_gtfs <- function(gtfs_urls = 
                          c("https://transitfeeds.com/l/56-montreal-qc-canada",
                            "https://transitfeeds.com/l/57-longueuil-qc-canada",
                            "https://transitfeeds.com/l/58-laval-qc-canada"),
                        out_folder = "dev/data/gtfs/") {
  
  require(rvest)
  require(tidyverse)
  
  agencies <- 
    map_dfr(gtfs_urls, function(gtfs_url) {
      # Scrape the first page and get the providers' page link
      provider_url <- 
        read_html(gtfs_url) |> 
        html_elements("td") |> 
        str_subset("<a href") |> 
        str_remove_all("(<td><a href=\")|(\">.*)") |> 
        (\(x) paste0("https://transitfeeds.com", x))()
      
      # Get a table of agency names and download links
      download_links <- 
        map_dfr(provider_url, function(prov) {
          
          links <- 
            read_html(prov) |> 
            html_elements("a") |> 
            paste0() |> 
            str_subset('<span class=\"badge\">GTFS</span>') 
          
          dl_link <- 
            links |> 
            str_extract("(?<=ref=\").*(?=\">)") |> 
            (\(x) paste0("https://transitfeeds.com", x, "/latest/download"))()
          
          agency <- 
            links |> 
            str_extract("(?<=>\n).*(?=<span class=\"badge\">GTFS</span>)") |> 
            str_remove(" GTFS") |> 
            str_trim()
          
          tibble(agency = agency,
                 dl_link = dl_link)
          
        })
      
      # Download and rename the outputs
      walk(seq(nrow(download_links)), function(row_n) {
        
        dest <- paste0(out_folder, download_links$agency[row_n], ".zip")
        
        # Download the zip file
        download.file(download_links$dl_link[row_n],
                      dest = dest,
                      mode = "wb")
        
        # Unzip the file
        dir.create(paste0(out_folder, "temp"))
        unzip(dest, exdir = paste0(out_folder, "temp/"))
        unlink(dest)
        
        # Rename all files and put it in the root of gtfs folder
        walk(list.files(paste0(out_folder, "temp/"), full.names = TRUE),
             function(from) {
               
               to <-
                 str_remove(from, "/temp") |>
                 str_replace(".txt",
                             paste0("_", download_links$agency[row_n], ".txt"))
               
               file.copy(from, to)
               
             })
        
        # Delete the temp folder
        unlink(paste0(out_folder, "temp/"), recursive = TRUE)
        
      })
      
      return(download_links)
      
    })
  
  return(agencies$agency)
}


# GTFS read function ------------------------------------------------------

read_gtfs <-  function(agencies = NULL, out_folder = "dev/data/gtfs/") {
  
  all_files <- list.files(out_folder, full.names = TRUE)
  
  all_prefixes <- 
    str_extract(all_files, paste0("(?<=", out_folder, ").*(?=_)")) |> 
    unique() |> 
    str_subset("^_", negate = TRUE)
  
  # If `agencies` isn't supplied, try taking them from the files name
  if (is.null(agencies))
    agencies <- 
    str_extract(all_files, paste0("(?<=_).*(?=.txt)")) |> 
    unique() |> 
    str_subset("_", negate = TRUE)
  
  map(set_names(all_prefixes), function(pref) {
    
    # All files corresponding to that prefix
    files <- 
      all_files |> 
      str_subset(paste0("(?<=", out_folder, ")", pref, "(?=_)"))
    
    # All unique columns of these files
    column_names <- 
      map(files, function(file) {
        read_lines(file, n_max = 1) |> 
          str_split(",") |> 
          unlist()
      }) |> 
      unlist() |> 
      unique()
    
    out <- 
      map_dfr(files, function(file) {
        
        # Extract agency name from file name
        agen <-
          str_extract(file, ".*(?=\\.txt)") |> 
          str_remove(".*_")
        
        data <- 
          read.delim2(file, header = TRUE, sep = ",") |> 
          as_tibble() |> 
          # Convert all to character so that it can bind through map_dfr
          mutate(across(everything(), as.character)) |> 
          mutate(agency = agen)
        
        missing_cols <- which(!column_names %in% names(data))
        
        new_cols <- 
          setNames(replicate(length(missing_cols), NA), 
                   column_names[missing_cols])
        
        add_column(data, !!!new_cols)
        
      })
    
    # Convert columns to the right class
    out <- type.convert(out, as.is = TRUE)
    
    # Indications of sf?
    if (sum(str_detect(names(out), "_lat$")) > 0 &&
        sum(str_detect(names(out), "_lon$"))) {
      
      require(sf)
      
      lat <- names(out)[which(str_detect(names(out), "_lat$"))]
      lon <- names(out)[which(str_detect(names(out), "_lon$"))]
      
      out <- 
        st_as_sf(out, coords = c(lon, lat), crs = 4326)
    }
    
    return(out)
    
  })
  
}
  


# Scrape and get the agencies list ----------------------------------------

agencies <- scrape_gtfs(gtfs_urls = 
                          c("https://transitfeeds.com/l/56-montreal-qc-canada",
                            "https://transitfeeds.com/l/57-longueuil-qc-canada",
                            "https://transitfeeds.com/l/58-laval-qc-canada"),
                        out_folder = "dev/data/gtfs/")

gtfs <- read_gtfs(agencies = agencies, 
                  out_folder = "dev/data/gtfs/")



# First analysis ----------------------------------------------------------

# Between 16h and 19h
stops_16_19 <- 
  gtfs$stop_times |> 
  filter(str_starts(departure_time, as.character(16:18))) |> 
  count(stop_id, name = "n_stops") |> 
  # Stops per hour (3h)
  mutate(n_stops = n_stops/3) |> 
  left_join(select(gtfs$stops, stop_id, stop_name), by = "stop_id") |> 
  st_as_sf()

# Load DAs
CT <- cancensus::get_census(
  dataset = "CA16",
  regions = list(CMA = "24462"),
  level = "CT",
  geo_format = "sf",
  quiet = TRUE) |> 
  as_tibble() |> 
  st_as_sf()

# Attach stops to DAs
stops_16_19 <- 
  stops_16_19 |> 
  st_join(select(CT, GeoUID)) |> 
  relocate(GeoUID, .before = geometry)

# Sum per DAs
stops_16_19 <- 
  stops_16_19 |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(n_stops = sum(n_stops))

# Plot normalized by shape area
CT |> 
  left_join(stops_16_19, by = "GeoUID") |> 
  mutate(stops = n_stops/`Shape Area`) |> 
  ggplot() + 
  geom_sf(aes(fill = stops), color = "transparent") +
  scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268"),
    limits = c(0, 750),
    oob = scales::squish) +
  ggtitle("Stops per hour between 16h-19h (normalized by shape area)")

