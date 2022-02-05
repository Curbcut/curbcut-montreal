##### DESIGN RELATED SCRIPT ####################################################


# Design functions --------------------------------------------------------

languageButtonLabel <- function(text) {
  as.character(tags$span(tags$span(class = "material-icons", "language"), 
                         span(text)))
}

# Make a standard navbarPage with addition fixed-position controls
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$div(class = "navbar-fixed", inputs)
  navbar[[4]][[1]][[1]]$children[[1]] <- 
    htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]], form)
  navbar
}

# Replace the inner text of a <button> tag with a Material icon span
materialIconButton <- function(tag, icon) {
  tag <- tagSetChildren(tag, .cssSelector = "button", 
                        span(class = "material-icons", icon))
  tag
}

# Map module right panel
right_panel <- function(id, ...) {
  
  absolutePanel(
    id = NS(id, "right_panel"), 
    style = paste0("z-index:500; max-height: calc(100vh - 120px) ;", 
                   #overflow-y: auto; ",
                   "padding: 5px; border-width: 0px; width: 15vw; ",
                   "font-size: 11px; max-width: 300px"),
    class = "panel panel-default", top = 15, right = 15, # width = 300,
    ...
  )
}

# # unused function at the moment
# loadingLogo <- 
#   function(href, src, loadingsrc) {
#     tagList(
#       tags$head(
#         tags$script(
#           "setInterval(function() {
#         if ($('html').attr('class')=='shiny-busy') {
#         $('div.busy').show();
#         $('div.notbusy').hide();
#         } else {
#         $('div.busy').hide();
#         $('div.notbusy').show();
#         }
#         },100)")
#       ),
#       tags$a(href = href, div(class = "busy",
#                               img(src = loadingsrc, height = 50, width = 50,
#                                   style = "padding: 7px;")),
#              div(class = 'notbusy', img(src = src, height = 50, width = 50, 
#                                         style = "padding: 7px;"))
#       )
#     )
#   }


# Design objects ----------------------------------------------------------

# # To switch between tabs without namespacing issues. Examples in `m_crash.R`
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
    max-height: calc(100vh - 165px);
    overflow: auto;
    background-color: #fff;
    border: 1px solid transparent;
    border-radius: 4px;
    box-shadow: 0 50px 50px rgba(0,0,0,.6);
  }
  
  #sus_page > li:first-child { 
    display:none;
  }
  
  .navbar-header #title { 
    color: white;
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
  
  .sidebar_content {
    overflow-x: hidden;
    overflow-y: auto;
    max-height: calc(100% - 225px);
  }
  
  .sus_sidebar .open> .dropdown-menu {
  top: auto;
  bottom: 0;
  }
  
  .sus_sidebar .shiny-input-container {
    margin-bottom: 10px;
    margin-top: 10px;
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
  
  .explore_dyk {
  max-height: calc(100vh - 250px); 
  overflow-y: auto; 
  overflow-x: hidden;  
  }
  
  .container {
  height: calc(100vh - 85px);
  overflow-y: auto;
  }
  
  .ggiraph-toolbar {
  display: none;
  }


'

set_ui_lang <- "shinyjs.setLanguage = function(language) {
    document.querySelector('body').className = `user-lang-${language}`;
  };"

lang_classes <- "
    .lang-en {
      visibility: hidden;
      display: none;
    }
    .lang-fr {
      visibility: hidden;
      display: none;
    }
    
    .user-lang-en .lang-en {
      visibility: visible !important;
      display: inline; 
    }
    .user-lang-fr .lang-fr {
      visibility: visible !important;
      display: inline; 
    }"
