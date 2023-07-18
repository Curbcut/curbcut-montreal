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

create_form <- function(lang, module_id, region) {
  
  pre <- 
    paste0("<form id='bug_report_form' action='https://docs.google.com/forms/d/",
         "e/1FAIpQLSfuQquv73dQoXA1UneCh9zREj0NG3E-RCfRpTNyJ1dIBagIeQ/formResp",
         "onse'>")
  post <- "<input type='submit' id='bug_report_submit' style = 'display:none;' /></form>"
    
  module <- 
    paste0("<input type='text' name='entry.1645395961' value='", module_id, 
           "' style = 'display:none;' />")
  region <- 
    paste0("<input type='text' name='entry.1343914403' value='", region, 
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
  
  HTML(paste0(pre, module, region, lang_input, additional, post))
  
}

severe_html <- function(lang, module_id, region) {
  tagList(tags$h2("Uh oh..."),
          tags$p(tags$span(class = "lang-fr", sever_subtitle_fr),
                 tags$span(class = "lang-en", sever_subtitle_en)),
          create_form(lang, module_id, region),
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


