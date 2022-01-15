#### SIDEBAR MODULE ############################################################

sidebar_UI <- function(id, ...) {
  
  div(
    id = "title_bar", class = "sus_sidebar", 
    div(class = "sidebar_content",
        tagList(
          uiOutput(NS(id, "title")),
          small_map_UI(NS(id, "left")),
          uiOutput(NS(id, "title_main")),
          actionLink(NS(id, "more_info"), i18n$t("Learn more")),
          hidden(uiOutput(outputId = NS(id, "title_extra")))),
    ...
  ))
}

sidebar_server <- function(id, x, var_map = NULL, var_right = NULL) {
  stopifnot(!is.reactive(x))
  
  moduleServer(id, function(input, output, session) {
    
    # Prepare text
    title <- filter(title_text, tab == x)
    title_title <- if (nrow(title) == 0) "MISSING" else 
      (filter(title, type == "title"))$text
    title_main <- if (nrow(title) == 0) "MISSING" else 
      (filter(title, type == "main"))$text
    title_extra <- if (nrow(title) == 0) "MISSING" else 
      (filter(title, type == "extra"))$text
    
    # Small map
    if (!is.null(var_map) && !is.null(var_right)) {
      small_map_server("left", reactive(paste0(
        # var_map without date
        str_remove(var_map(), "_\\d{4}"), 
        # q5 for univariate, q3 for bivariate
        ifelse(var_right() == " ", "_q5", "_q3"), 
        # var_map date
        ifelse(is.na(str_extract(var_map(), "_\\d{4}")), "",
               str_extract(var_map(), "_\\d{4}")))))
    } else if (!is.null(var_map)) small_map_server("left", var_map)

    # More info
    observeEvent(input$more_info, {
      toggle("title_extra", condition = input$more_info %% 2 == 1)
      if (input$more_info %% 2 == 1) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Learn more")
      updateActionLink(session, "more_info", label = txt)
      })
    
    output$title <- renderUI(h3(sus_translate(title_title)))
    output$title_main <- renderUI(HTML(sus_translate(title_main)))
    output$title_extra <- renderUI(HTML(sus_translate(title_extra)))
    
  })
}
