#### DID YOU KNOW MODULE ######################################################

qload("data/dyk.qsm")
dyk_hide_status <- reactive(TRUE)

dyk_UI <- function(id) {
  tagList(uiOutput(NS(id, "dyk_box")),
          uiOutput(NS(id, "dyk_contents")))
  }

dyk_server <- function(id, var_left, var_right) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  
  moduleServer(id, function(input, output, session) {
    
    dyk_output <- reactive({
      
      vars <- c(str_remove(var_left(), "_\\d{4}$"), 
                str_remove(var_right(), "_\\d{4}$"))
      vars <- vars[vars != " "]
      categories <- 
        tibble(variable = vars) %>% 
        inner_join(category_table, by = "variable") %>% 
        pull(category)
      
      # Report variables without category matches
      no_match <- vars[!vars %in% category_table$variable] 
      if (length(no_match > 0)) {
        warning("No DYK category matches for variable(s): ", 
                paste(no_match, collapse = ", "), call. = FALSE)
        }
      
      # Find rows which match both variables
      report <- 
        dyk %>% 
        rowwise() %>% 
        filter(identical(variable, vars)) %>%
        ungroup() %>% 
        slice_sample(n = 2)
      
      # If total < 2, find rows which match one variable and both categories
      if (length(categories) > 0) {
        if (nrow(report) < 2) {
          report <- 
            dyk %>% 
            rowwise() %>% 
            filter(sum(vars == variable) == 1) %>% 
            ungroup() %>% 
            rowwise() %>% 
            filter(identical(category, categories)) %>% 
            ungroup() %>% 
            slice_sample(n = 2 - nrow(report)) %>% 
            bind_rows(report)
        }
        
        # If total < 2, find rows which match both categories
        if (nrow(report) < 2) {
          report <- 
            dyk %>% 
            rowwise() %>% 
            filter(identical(category, categories)) %>% 
            ungroup() %>% 
            slice_sample(n = 2 - nrow(report)) %>% 
            bind_rows(report)
        }
      }
      
      # If total < 2, find rows which match one variable
      if (nrow(report) < 2) {
        report <- 
          dyk %>% 
          rowwise() %>% 
          filter(sum(vars == variable) == 1) %>% 
          ungroup() %>% 
          slice_sample(n = 2 - nrow(report)) %>% 
          bind_rows(report)
      }
      
      # If total < 2, find rows which match one category
      if (nrow(report) < 2) {
        report <- 
          dyk %>% 
          rowwise() %>% 
          filter(sum(category == categories) == 1) %>% 
          ungroup() %>% 
          slice_sample(n = 2 - nrow(report)) %>% 
          bind_rows(report)
      }
      
      if (nrow(report) > 0) {
        sus_translate(report$text) %>%
        paste("<li> ", ., collapse = "") %>%
        paste0("<ul>", ., "</ul>") %>%
        HTML()
      } else NULL
    })
    
    # Only show box if dyk_output isn't empty
    output$dyk_box <- renderUI({
      
      if (!is.null(dyk_output())) {
        tagList(
          hr(),
          fluidRow(column(width = 7, h4(sus_translate("Did you know?"))),
                   column(width = 5, align = "right",
                          actionLink(inputId = session$ns("hide"), 
                                     label = sus_translate("Hide"))))
        )
      }  
    })

    # Track hide status with clicks on input$hide button
    dyk_hide_status <- reactive(as.logical(input$hide %% 2 == 0))
    
    # Hide and reveal DYK status
    observeEvent(dyk_hide_status(), {
      
      if (dyk_hide_status()) txt <- sus_translate("Hide") else 
        txt <- sus_translate("Show")
      
      updateActionButton(session, "hide", label = txt)
    }, ignoreInit = TRUE)
    
    # Only show contents if dyk_output isn't empty and !dyk_hide_status
    output$dyk_contents <- renderUI({
      if (!is.null(dyk_output()) && req(dyk_hide_status())) {
        tagList(dyk_output())
      }
    })
      
  })
}
