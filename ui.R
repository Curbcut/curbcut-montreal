##### SUS UI SCRIPT ############################################################

ready_modules_ui <- function(mods_rdy, stand_alone_tabs) {
  
  # Alphabetical order
  mods_rdy <- mods_rdy[names(mods_rdy)[order(names(mods_rdy))]]
  
  list_args <- 
    lapply(names(mods_rdy), function(theme) {
      c(theme,
        lapply(names(mods_rdy[[theme]]), function(module) {
          name <- curbcut::cc_t(module)
          key <- unname(mods_rdy[[theme]][module])
          tabPanel(name,
                   do.call(paste0(key, "_UI"), list(key)),
                   value = key)
        })
      )
    })
  
  # navbarMenu creation
  out <- lapply(list_args, \(x) do.call(navbarMenu, x)) 
  
  # Translate and return
  lapply(out, function(x) {
    x$title <- curbcut::cc_t(x$title)
    x$menuName <- curbcut::cc_t(x$menuName)
    x
  })
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

ui <- function(request) {
  tagList(
    
    # Import packages dependencies -----------------------------------------------
    
    useShinyjs(),
    
    
    # Remove the navbar -------------------------------------------------------
    
    tags$style(type = "text/css", ".navbar-shadow{display:none;}"),
    tags$style(type = "text/css", ".navbar{display:none;}"),
    
    tags$head(tags$script('
  $(document).ready(function() {
    $(document).on("wheel", ".scrollable-div, .dropdown", function(e) {
      e.stopPropagation();
    });
  });
')),

tags$script(HTML("
  $(document).ready(function() {
    $('div[data-theme]').each(function() {
      var themeColor = getComputedStyle(this).getPropertyValue('--theme-color').trim();
      $(this).find('.dataTable').css('--dt-row-selected', themeColor);
    });
  });
")),

# Place the label inside the dropdown
tags$head(
  tags$script(src = "widgets_update.js")
),

# For mobile, open and close the menus
tags$head(tags$script("
                      $(document).ready(function(){
  $('.mobile-sidebar-menu').on('click', function() {
    $('.sus-map-sidebar').toggleClass('open'); 
  });
});")),
tags$head(tags$script("
                      $(document).ready(function(){
  $('.mobile-panel-menu').on('click', function() {
    $('.sus-map-panel').toggleClass('open'); 
  });
});")),


tags$head(tags$script("
$(document).ready(function () {
  $('#alp-alp-alp-ccslider_slu').data('ionRangeSlider').update({
    grid_num: 2
  });
});
")),

# Styling objects ------------------------------------------------------------

tags$head(tags$link(rel = "icon", href = "favicon.ico")),

# Curbcut scripts
curbcut::use_curbcut_cookie(),
curbcut::use_curbcut_js(),
curbcut::use_curbcut_css(lang_init = TRUE),

# Google analytics
# tags$head(includeHTML("www/google_analytics.html")),

# Sharing card ---------------------------------------------------------------

meta() |>
  meta_social(
    title = paste0(site_name, " | Towards a sustainable city"),
    description = paste0(
      "Curbcut is a platform for deep, dynamic, and intuitive ",
      "exploration of urban sustainability."
    ),
    url = "https://montreal.curbcut.ca",
    image = "share.jpg",
    image_alt = paste0(
      "A photo of a winding footpath through a verdant ",
      "Montreal alley."
    ),
    twitter_creator = "@curbcutca",
    twitter_card_type = "summary",
    twitter_site = "@curbcutca"
  ),


# Navigation bar -------------------------------------------------------------

shiny::actionButton("proxy_advanced_options", "", style = "display: none;"),
do.call(
  navbarPageWithInputs,
  c(
    list(
      id = "cc_page",
      windowTitle = site_name,
      title = actionLink("title", "Curbcut"),
      tabPanel(curbcut::cc_t("Home"), home_UI("home"), value = "home")
    ),
    ready_modules_ui(mods_rdy),
    list(
      collapsible = TRUE,
      inputs = list(
        # Language toggle
        curbcut::language_UI(),
        # Actions dropdown
        curbcut::settings_UI()
      )
    )
  )
)
  )
}
