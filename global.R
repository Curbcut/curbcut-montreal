#### SUS GLOBALS ###############################################################

# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(shiny.i18n)
  library(waiter)
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  library(sf)
  library(mapdeck) 
  library(mapboxapi)
  
  library(DT)
  library(qs)
  library(glue)
})


# Shiny options -----------------------------------------------------------

options(shiny.trace = FALSE) # Set TRUE for debugging
enableBookmarking("server")


# Data --------------------------------------------------------------------

var_exp <- qread("data/var_exp.qs")
title_text <- qread("data/title_text.qs")
qload("data/colours.qsm")

qload("data/census.qsm")
grid <- qread("data/grid.qs")
street <- qread("data/street.qs")
building <- qread("data/building.qs")

qload("data/covid.qsm")
green_space <- qread("data/green_space.qs")
qload("data/alleys.qsm")
crash <- qread("data/crash.qs")
tt_matrix <- qread("data/tt_matrix.qs")
metro_lines <- qread("data/metro_lines.qs")
stories <- qread("data/stories.qs")

min_census_year <- "1996"
current_census <- "2016"

island_csduid <- c("2466007", "2466023_1",  "2466023_10", "2466023_11",
                   "2466023_12", "2466023_13", "2466023_14", "2466023_15", 
                   "2466023_16", "2466023_17", "2466023_18", "2466023_19",
                   "2466023_2", "2466023_3", "2466023_4", "2466023_5",  
                   "2466023_6", "2466023_7", "2466023_8", "2466023_9",
                   "2466032", "2466047", "2466058", "2466062", "2466087", 
                   "2466092", "2466097", "2466102", "2466107", "2466112",
                   "2466117", "2466127", "2466142", "2466072")

# Translation -------------------------------------------------------------

suppressWarnings({
  i18n <- Translator$new(translation_csvs_path = "translations/")})
i18n$set_translation_language("fr")
translation_fr <- qread("data/translation_fr.qs")
sus_reactive_variables <- reactiveValues()


# Object style ------------------------------------------------------------

map_token <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                    "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")
map_style <- "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7"
map_zoom <- 10.1
map_location <- c(-73.58, 45.53)
widget_style <- "display: inline-block; padding: 5px; vertical-align: top;"


# Functions ---------------------------------------------------------------

convert_unit <- function(x, var_name = NULL) {
  
  if (length(x) == 0) return(x)
  if (is.na(x)) return(x)
  
  if (!missing(var_name) && grepl("_prop", var_name)) {
    x <- paste0(round(x * 100, 1), "%")
  } else if (!missing(var_name) && grepl("_dollar", var_name)) {
    x <- scales::dollar(x, 1)
  } else if (abs(x) >= 100) {
    x <- scales::comma(x, 1)
  } else if (abs(x) >= 10) {
    x <- as.character(round(x, 1))
  } else x <- as.character(round(x, 2))
  
  x
}

right_panel <- function(id, ...) {
  
  absolutePanel(
    id = NS(id, "right_panel"), 
    style = paste0("z-index:500; height: calc(100vh - 120px) ;", #overflow-y: auto; ",
                   "padding: 5px; border-width: 0px; width: 20vw; ",
                   "font-size: 11px;"),
    class = "panel panel-default", top = 15, right = 15, # width = 300,
    ...
  )
}

return_closest_year <- function(var, df = "borough") {
  df <- get(df)
  if (!var %in% names(df)) {
    time <- as.numeric(str_extract(var, "\\d{4}"))
    x <- 
      df %>% 
      select(contains(str_remove(var, "_\\d{4}$"))) %>% 
      names() %>% 
      str_extract("\\d{4}$") %>% 
      as.numeric() %>% 
      na.omit()
    closest_year <- x[which.min(abs(x - time))]
    var <- paste0(str_remove(var, "_\\d{4}$"), "_", closest_year)
    var <- sub("_$", "", var)
  }
  var
}

loadingLogo <- 
  function(href, src, loadingsrc) {
    tagList(
      tags$head(
        tags$script(
          "setInterval(function() {
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }
        },100)")
      ),
      tags$a(href = href, div(class = "busy",
                              img(src = loadingsrc, height = 50, width = 50,
                                  style = "padding: 7px;")),
             div(class = 'notbusy', img(src = src, height = 50, width = 50, 
                                        style = "padding: 7px;"))
             )
    )
  }


# Load data ---------------------------------------------------------------
## THESE ALL NEED TO BE TURNED INTO QS BINARIES

# # Load data for pedestrian realm 
# load(file = "data/sidewalks_WSG.Rdata")
# load(file = "data/census_circular.Rdata")
# load(file = "data/original_VAS_plan.Rdata")
# load(file = "data/revised_VAS_plan.Rdata")
# load(file = "data/census_analysis_quantile.Rdata")
# load(file = "data/census_analysis_ct.Rdata")
# 
# census_analysis_quantile_WSG <- census_analysis_quantile %>% 
#   st_transform(4326)
# 
# #mode
# cycling1 <- loadRData("data/car_1_finals.Rdata")
# cycling2 <- loadRData("data/car_3_finals.Rdata")
# cycling_network <- loadRData("data/reseau_cyclable.Rdata")
# car_share <- loadRData("data/Car_Share.Rdata")
# cycling_access <- loadRData("data/Cycling_Access.Rdata")
# trip_distance <- loadRData("data/Trip_Distance.Rdata")
# 
# load("data/cycling_total_final.Rdata")


# Other prep --------------------------------------------------------------

# # So we can switch between tabs without namespacing issues. Examples in `m_crash.R`
# # a("NAME OF LINK", onclick = "openTab('NAME OF TAB')", href="#")
# js_links_between_modules <- "
#         var openTab = function(tabName){
#           $('a', $('.sidebar')).each(function() {
#             if(this.getAttribute('data-value') == tabName) {
#               this.click()
#             };
#           });
#         }
#       "


styler <- '
/* the big panel popup when we show an RMD in a map module */
  .main_panel_text_popup {
    max-height: 83vh;
    overflow: auto;
    background-color: #fff;
    border: 1px solid transparent;
    border-radius: 4px;
    box-shadow: 0 50px 50px rgba(0,0,0,.6);
  }
  
  #sus_page > li:first-child { 
    display:none;
  }
  
  h4 {
  font-size: 13px;
  }
    
  .mapdeck_div {
  height: calc(100vh - 85px);
  }
  
  .sus_sidebar {
    font-size: 11px;
    padding: 0px 5px 0px 0px;
    margin: 0px 5px 0px 0px;
    border-width: 0px;
    height: calc(100vh - 85px);
    display: block;
    position: relative;
  }
  
  .sidebar_extra {
    overflow-x: hidden;
    overflow-y: auto;
    max-height: 20%;
  }
  
  .bottom_sidebar {
    position: absolute;
    bottom: 0;
    width: 100%;
    display: block;
  }
  
  .small_map img {
    max-width: 100%;
    height: auto;
  }
  
  .compare_dropdown .open> .dropdown-menu {
  max-height: 250px;
  font-size: 13px;
  left: auto;
  right: 0;
  }
  
  #dropdown-menu-settings {
  max-height: 250px;
  font-size: 13px;
  left: auto;
  right: -40px;
  }
  
  .form-group {
  margin: auto;
  }

'

navbar_js <- "@media (max-width: 1050px) {
    .navbar-header {
        float: none;
    }
    .navbar-left,.navbar-right {
        float: none !important;
    }
    .navbar-toggle {
        display: block;
    }
    .navbar-collapse {
        border-top: 1px solid transparent;
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.1);
    }
    .navbar-fixed-top {
        top: 0;
        border-width: 0 0 1px;
    }
    .navbar-collapse.collapse {
        display: none!important;
    }
    .navbar-nav {
        float: none!important;
        margin-top: 7.5px;
    }
    .navbar-nav>li {
        float: none;
    }
    .navbar-nav>li>a {
        padding-top: 10px;
        padding-bottom: 10px;
    }
    .collapse.in{
        display:block !important;
    }
}"