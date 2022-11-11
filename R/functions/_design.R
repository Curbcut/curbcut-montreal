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

susNewsExploreArticle <- function(id, type, author, date, title, img, preview) {
  return (div(class='action-button shiny-bound-input', id=id,
    fluidRow(
      column(9,
        tags$div(class="news-meta-data",
                tags$span(class="news-meta-data-type", type),
                tags$span(class="news-meta-data-date", date)#,
                # tags$span(class="news-meta-data-author",
                #           tagList(cc_t(r = r, "by"), " ", author)
                #           ),
                ),
        tags$h1(title),
        tags$div(class="news-preview",
          tags$p(preview)
        )
      ),
      column(3,
          tags$img(src=img, align='right')
      )
    )
  ))
}

susAuthorLink <- function(title, href=NULL, icon=NULL) {
  if (is.null(icon)) {
    icon = materialIcon("link")
  }
  return(tags$a(class="sus-author-link", href=href, target="_blank", span(class="sus-author-link-icon", icon), title)) 
}

susAuthorBio <- function(...) {
  tags$div(class="sus-author-bio", ...)
}

susAuthor <- function(name, role, img_src, ...) {
  tags$div(class="sus-author",
    tags$img(class="sus-author-img", src=img_src),
    tags$h3(class="sus-author-name", name),
    tags$h4(class="sus-author-role", role),
    ...)
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

# Make a Google fonts Material icon
# To browse the list of available icons, go to: https://fonts.google.com/icons
# For exact usage, see the "Inserting the icon" heading on the right hand sidebar,
# where you should copy the inner text of the <span>...</span> example given.
materialIcon <- function(icon) {
  span(class = "material-icons", icon)
}

# Make a custom icon, styled in the same way as the above Material icons
# To see the list of available icons, check www/icons/custom
customIcon <- function(icon) {
  span(class = "custom-icons", data_custom_icon = icon)
}

# Replace the inner text of a <button> tag with a Material icon span
materialIconButton <- function(tag, icon) {
  tag <- tagSetChildren(tag, .cssSelector = "button", 
                        materialIcon(icon))
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
  return(tags$li(tags$a(class = "noselect", href = NULL, onclick = link$onclick, link$name,
                        img(src = link$img, 
                            style = paste0("display:inline; height:20px; float:right; ",
                                           "margin-top:auto; margin-bottom:auto;")))))
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
          tags$li(tags$a(href = NULL, HTML("&nbsp;"))),#cc_t(r = r, "Terms & Conditions"))),
          tags$li(tags$a(href = NULL, style = "cursor:pointer;", 
                         onclick = "openTab('about_sus')", 
                         cc_t(r = r, "About"), materialIcon("info"))),
          tags$li(tags$a(href = NULL, style = "cursor:pointer;", 
                         onclick = "document.getElementById('contact').click();",
                         cc_t(r = r, "Contact/feedback"), materialIcon("mail"))),
          tags$li(tags$a(href = NULL, HTML("&nbsp;")))#cc_t(r = r, "Privacy Policy"))),
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
    tags$h1(class = "sus-brand sus-banner-text", "Curbcut"),
    tags$h4(class = "sus-brand sus-banner-text-city", cc_t("Montreal"))
  ))
}

susCarousel <- function(..., id="", class="") {
  return(
    tags$div(class = paste("sus-carousel", class),
      tags$div(class="sus-carousel-bullets", style = "cursor: pointer;"),       
      tags$div(class="sus-carousel-preview sus-carousel-preview-prev",
               # Previous and next have been turned off, as the preview does 
               # not translate correctly
               # style = "cursor: pointer;",
               # cc_t(r = r, "Previous:"),
               # HTML("&nbsp;"),
               tags$span(class="sus-carousel-preview-content")),
      tags$div(class="sus-carousel-preview sus-carousel-preview-next",
               # style = "cursor: pointer;",
               # cc_t(r = r, "Next:"),
               # HTML("&nbsp;"),
               tags$span(class="sus-carousel-preview-content")),
      tags$div(class="sus-carousel-nav-bttn sus-carousel-nav-bttn-left", 
               style = "cursor: pointer;",
               materialIcon("chevron_left")),
      tags$div(class="sus-carousel-nav-bttn sus-carousel-nav-bttn-right", 
               style = "cursor: pointer;",
               materialIcon("chevron_right")),
      ...)
  )
}

susCarouselSlide <- function(..., title="", preview="", id="", class="") {
  return(
    htmltools::tagAppendAttributes(
      tags$div(class = paste("sus-carousel-slide", class),
               tags$h2(title),
               ...),
    `data-preview` = " ")#preview)
  )
}

# Make controls for a page
susPageControls <- function(..., class="") {
  return(
    tagList(
      tags$div(class="sus-page-controls-spacer"),
      tags$div(class="sus-page-controls-container",
        tags$div(class = paste("sus-page-controls", class),...))
    )
  )
}

susPageImages <- function(..., class="") {
  return(tags$div(class = paste("sus-page-images", class),...))
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
       inputc.value = window.location.href;
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
  
  #dropdown-menu-settings {
  max-height: 250px;
  font-size: 13px;
  left: auto;
  right: -40px;
  }
  
  .dropdown-menu > .inner.open {
  max-height:50vh !important;
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

  tr:nth-child(even) {
  background-color: #B5C0DA50;
  }

  tr {
  border-bottom: 1px solid #ddd;
  }

  tr:hover {
  background-color: #B5C0DA50;
  }

  td {
  padding:4px;
  }

  th {
  padding:4px;
  }

'

temp_styler <- '
  #stories-back {
    z-index: 1000 !important;
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

# # Screenshot
# screenshot_js <- "shinyjs.takeShot = function(params) {
# 
#   var defaultParams = {
#     to_sh_id : null,
#     output_id : null
#   };
#   params = shinyjs.getParams(params, defaultParams);
# 
#   let div = document.getElementById(params.to_sh_id);
# 
#   html2canvas(div).then(
#     function (canvas) {
#       document.
#       getElementById(params.output_id).
#       appendChild(canvas).
#       setAttribute('id', 'map_sh');
#       
#     })
# }"

# WILL NEED THIS https://codepen.io/nathansouza/pen/OXdJbo TO DOWNLOAD

# Severe HTML -------------------------------------------------------------

sever_subtitle_fr <- 
  HTML(paste0("Il semble que Curbcut se soit arrêté de manière inattendue! Aidez-n",
              "ous à garder l'application exempte de bogues en appuyant 'Envo",
              "yer'! ",
              "<br>L'équipe de Curbcut."))

sever_subtitle_en <- 
  HTML(paste0("It appears that Curbcut has shut down unexpectedly! Help us keep ",
              "the app bug-free by clicking on “Submit”!",
              "<br>-The Curbcut team."))

create_form <- function(lang, module_id, geo) {
  
  pre <- 
    paste0("<form id='bug_report_form' action='https://docs.google.com/forms/d/",
         "e/1FAIpQLSfuQquv73dQoXA1UneCh9zREj0NG3E-RCfRpTNyJ1dIBagIeQ/formResp",
         "onse'>")
  post <- "<input type='submit' id='bug_report_submit' style = 'display:none;' /></form>"
    
  module <- 
    paste0("<input type='text' name='entry.1645395961' value='", module_id, 
           "' style = 'display:none;' />")
  geo <- 
    paste0("<input type='text' name='entry.1343914403' value='", geo, 
           "' style = 'display:none;' />")
  lang_input <- 
    paste0("<input type='text' name='entry.1443376271' value='", lang, 
           "' style = 'display:none;' />")
  
  
  additional_style <- 
    paste0("width: 75%; height: 150px; padding: 12px 20px; ",
    "box-sizing: border-box; border: 2px solid #ccc; border-radius: 4px;",
    "background-color: #f8f8f8; resize: none; font-family: ",
    "var(--ff-body); color: var(--c-paragraph); font-size: 1.65rem;")
  
  additional_text <- 
    if (lang == "fr") {
      paste0("Le rapport contient déjà des informations pertinentes sur votre ",
             "session au moment où vous avez rencontré l'erreur. Veuillez envo",
             "yer le rapport, et n'hésitez pas à ajouter des informations supp",
             "lémentaires dans ce bloc.")
    } else {
      paste0("The report already contains relevant information about your sess",
             "ion at the time you experienced the error. Please send the repor",
             "t, and feel free to add additional information in this block.")     
    }
  
  additional <- 
    paste0("<textarea name='entry.77284970 form='bug_report_form' style ='",
           additional_style, "'>", additional_text, "</textarea>")
  
  HTML(paste0(pre, module, geo, lang_input, additional, post))
  
}

severe_html <- function(lang, module_id, geo) {
  tagList(tags$h2("Uh oh..."),
          tags$p(tags$span(class = "lang-fr", sever_subtitle_fr),
                 tags$span(class = "lang-en", sever_subtitle_en)),
          create_form(lang, module_id, geo),
          tags$div(class = "sus-button-group",
                   tags$a(class = "sus-button sus-icon-button sus-button-secondary", 
                          style = "cursor: pointer;",
                          onClick = "window.location.href='/'", 
                          tags$span(class = "lang-fr", "Accueil"),
                          tags$span(class = "lang-en", "Home"), " ",
                          span(class = "material-icons", "home")),
                   tags$a(class = "sus-button sus-icon-button sus-button-primary", 
                          style = "cursor: pointer;",
                          onClick = "document.getElementById('bug_report_submit').click()", 
                          tags$span(class = "lang-fr", "Envoyer"),
                          tags$span(class = "lang-en", "Submit"), " ",
                          span(class = "material-icons", "bug_report")),
          ))
}


