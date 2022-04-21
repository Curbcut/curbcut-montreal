##### DESIGN RELATED SCRIPT ####################################################

# Design functions --------------------------------------------------------

susSidebarWidgets <- function(...) {
  return(div(class = "sus-sidebar-widgets",...))
}

nowrap <- function(...) {
  return(tags$span(style = "white-space: nowrap;", ...))
}

scrollAnchor <- function(id) {
  return(tags$span(style = "position: relative;display: inline-block;height: 100%;vertical-align: top;",
         tags$span(id = id, style = "display: block;position: absolute;top: calc(var(--h-navbar) * -2);")))
}

languageButtonLabel <- function(text) {
  as.character(tags$span(tags$span(class = "material-icons", "language"), 
                         span(text)))
}

# Make a standard navbarPage with addition fixed-position controls
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  container <- tags$div(class = "navbar-fixed", inputs)
  navbar[[4]][[1]][[1]]$children[[1]] <- 
    htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]], container)
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
    class = "panel panel-default sus-map-panel sus-scroll",
    tags$div(class = "sus-map-panel-content sus-scroll-content", ...)
  )
}

# Make a link button styled to go inside of a link list group
linkListGroupElement <- function(link) {
  return(tags$li(tags$a(class = "noselect", href = NULL, onclick = link$onclick, link$name)))
}

# Make a link list group that can have link list group elements
# passed into it via the dynamic arguments ...
linkListGroup <- function(name, ...) {
  namedArgs = list(class = "")
  args = c(namedArgs, lapply(list(...), linkListGroupElement))
  
  return(tags$div(class = "sus-link-list-group",
    tags$h3(name),
    do.call(tags$ul, args)
  ))
}

linkList <- function(...) {
  return(tags$div(class = "sus-link-list",...))
}

# Make a generic global footer (for use on text pages only)
susFooter <- function() {
  return(tags$div(class = "sus-page-footer",
    tags$div(class = "sus-page-footer-content",
      tags$div(class = "sus-page-footer-logos",
        tags$a(href = "https://www.mcgill.ca/mssi/", target = "_blank",
          tags$img(class = "sus-page-footer-logo", src = "mcgill-mssi-logo-final.png")
        )
      ),
      tags$div(class = "sus-page-footer-links",
        tags$ul(
          tags$li(tags$a(href = NULL, HTML("&nbsp;"))),#sus_translate("Terms & Conditions"))),
          tags$li(tags$a(href = NULL, style = "cursor:pointer;", 
                         onclick = "openTab('why_dash')", 
                         sus_translate("About"))),
          tags$li(tags$a(href = NULL, style = "cursor:pointer;", 
                         onclick = "document.getElementById('contact').click();",
                         sus_translate("Contact"))),
          tags$li(tags$a(href = NULL, HTML("&nbsp;")))#sus_translate("Privacy Policy"))),
        )
      )
    )
  ))
}

#Make the full-width home page "SUS" banner
susBanner <- function() {
  return(tags$div(class = "sus-banner noselect",
    tags$div(class = "sus-banner-bg sus-bg-img-map"),
    tags$div(class = "sus-banner-bg sus-bg-img-skyline"),
    tags$h1(class = "sus-brand sus-banner-text", "SUS")
  ))
}

# Make a section inside of a page
susPageSection <- function(..., class="") {
  return(tags$div(class = paste("sus-page-content-section", class),...))
}

susPageSectionFeature <- function(..., class="") {
  return(susPageSection(class = paste("sus-page-content-section-feature", class), ...))
}

# Make a text page with optional header & footer, typically,
# if you include a footer, it would simply be: susFooter()
susPage <- function(..., class="", header=NULL, footer=NULL) {
  children = list(tags$div(class = "sus-page-content",...))
  if (!is.null(header)) {
    children = c(list(header), children);
  }
  if (!is.null(footer)) {
    children = c(children, list(footer));
  }
  return(tags$div(class = paste("sus-page", class), children))
}

susPalette <- function() {
  return( tags$div(class = "palette-grid",
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch"),
    tags$div(class = "palette-swatch")
  ))
}

susLegend <- function() {
  return(tags$div(class = "legend-grid",
    tags$div(class = "legend-swatch"),
    tags$div(class = "legend-swatch"),
    tags$div(class = "legend-swatch"),
    tags$div(class = "legend-swatch"),
    tags$div(class = "legend-swatch")
  ))
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
bookmark_url <- 
  'function copyUrl(text) {
       var inputc = document.body.appendChild(document.createElement("input"));
       const p = window.location.href;
       const part_replace = "https://sus-mcgill.shinyapps.io";
       inputc.value = p.replace(part_replace, "http://www.susmontreal.ca");
       inputc.focus();
       inputc.select();
       document.execCommand("copy");
       inputc.parentNode.removeChild(inputc);
       alert("URL successfully copied.");
}'

styler <- '
/* the big panel popup when we show an RMD in a map module */
  .main_panel_text_popup {
    max-height: calc(100vh - 155px);
    overflow: auto;
    background-color: #fff;
    border: 1px solid transparent;
    border-radius: 4px;
    box-shadow: 0 50px 50px rgba(0,0,0,.6);
    margin-left: 300px;
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


'

set_ui_lang <- "shinyjs.setLanguage = function(language) {
    document.querySelector('body').className = `user-lang-${language}`;
  };"

lang_classes <- "
    .lang-en {
      visibility: hidden;
      display: none !important;
    }
    .lang-fr {
      visibility: hidden;
      display: none !important;
    }
    
    .user-lang-en .lang-en {
      visibility: visible !important;
      display: inline !important; 
    }
    .user-lang-fr .lang-fr {
      visibility: visible !important;
      display: inline !important; 
    }"
