##### DESIGN RELATED SCRIPT ####################################################

# Design functions --------------------------------------------------------

# Make a standard navbarPage with addition fixed-position controls
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  container <- tags$div(class = "navbar-fixed", inputs)
  navbar[[4]][[1]][[1]]$children[[1]] <- 
    htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]], container)
  navbar
}

# Design objects ----------------------------------------------------------

# To switch between tabs without namespacing issues. Examples in `m_crash.R`
# a("NAME OF LINK", onclick = "openTab('canale')", href = NULL, style = "cursor: pointer;")
js_links_between_modules <- "
        var openTab = function(tabName){
          $('a', $('.navbar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      "

styler <- '
/* the big panel popup when we show an RMD in a map module */

  #cc_page > li:first-child { 
    display:none;
  }
  
  .navbar-header #title { 
    color: white;
  }
  
  h4 {
  font-size: 13px;
  }
  
  #dropdown-menu-settings {
  max-height: 250px;
  font-size: 13px;
  left: auto;
  right: -40px;
  }
  
  .dropdown-menu > .inner.open {
  max-height:50vh !important;
  }

  .navbar-nav>li>.dropdown-menu {
  max-height:345px;
  overflow-y:auto;
  }
  
  #dropdown-menu-geo_change {
  max-height: 250px;
  font-size: 13px;
  left: auto;
  right: 0px;
  }
  
  .form-group {
  margin: auto;
  }
  
  .container {
  height: calc(100vh - 85px);
  overflow-y: auto;
  }
  
  .ggiraph-toolbar {
  display: none;
  }
  
  .fixed_footer {
   position: fixed;
   z-index:50000;
   left: 0;
   bottom: 0;
   width: 100%;
   background-color: #6C83B5B0;
   text-align: center;
   font-size: 1.65rem;
   padding:20px;
  }

   .row-stories-maps {
  content: "";
  display: table;
  clear: both;
}

.column-stories-maps-map {
  width: 50%;
  height: 80vh;
  float: left;
  padding: 10px;
  position: sticky;
  top:50px;
}

.column-stories-maps {
  width: 50%;
  float: left;
  padding: 10px;
}

'

