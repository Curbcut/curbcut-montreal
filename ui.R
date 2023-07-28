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
  shiny::tagList(
    
    # Import packages dependencies -----------------------------------------------
    
    useShinyjs(),
    
    
    # Remove the navbar -------------------------------------------------------
    
    tags$style(type = "text/css", ".navbar-shadow{display:none;}"),
    tags$style(type = "text/css", ".navbar{display:none;}"),
    
    # Styling objects ------------------------------------------------------------
    
    tags$head(tags$link(rel = "icon", href = "favicon.ico")),
    
    # Curbcut scripts
    curbcut::use_curbcut_js(),
    curbcut::use_curbcut_css(lang_init = TRUE),
    
    # Google analytics
    tags$head(includeHTML("www/google_analytics.html")),
    
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
