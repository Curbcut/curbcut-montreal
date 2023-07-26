##### SUS UI SCRIPT ############################################################

style_files <- gsub("www/", "", list.files("www/styles", full.names = TRUE))

style_files <- paste0(style_files, "?id=8")

style_tags <- tagList(
  lapply(style_files, function(x) {
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = x))
  })
)

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
  tags$script(src = "label_placement.js")
),

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

# Styling objects ------------------------------------------------------------

tags$head(tags$link(rel = "icon", href = "favicon.ico")),

style_tags,

# tags$head(tags$script(src = "sus.js")),
tags$head(tags$script(src = "shinybrowser.js")),
tags$head(tags$script(js_links_between_modules)),
tags$head(tags$style(HTML(styler))),

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
    twitter_creator = "@susmontreal",
    twitter_card_type = "summary",
    twitter_site = "@susmontreal"
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
