#### CC GLOBALS ################################################################

# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(curbcut)
  library(cc.landing)
  library(cc.map)
})


# Data --------------------------------------------------------------------

curbcut::load_data(site_name = "Curbcut Montréal",
                   site_url = "https://montreal.curbcut.ca",
                   stories_page = "Montréal stories",
                   inst_prefix = "mtl",
                   mapbox_username = "curbcut",
                   default_random_address = "845 Sherbrooke Ouest, Montréal, Quebec",
                   map_zoom = 9.9,
                   map_loc = c(lat = -73.70, lon = 45.53))


# Create the UI and server functions for basic modules --------------------

curbcut::create_ui_server_mods(modules = modules)


# Set up fonts ------------------------------------------------------------

systemfonts::register_font(
  name = "acidgrotesk-book",
  plain = list("www/fonts/acidgrotesk-book.woff", 0)
)


# Source the R folder -----------------------------------------------------

# Curbcut works with global environment. Must source to the current global env
lapply(list.files("R", full.names = TRUE), source, verbose = FALSE)
