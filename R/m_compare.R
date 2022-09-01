#### COMPARE MODULE ############################################################

compare_UI <- function(id, var_list) {
  
  tagList(
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      fluidRow(column(width = 7, h4(sus_translate(r = r, "Compare"))),
               column(width = 5, align = "right", 
                      actionLink(inputId = NS(id, "hide_compare"), 
                                 class = "sus-small-link",
                                 label = sus_translate(r = r, "Hide"))))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      div(class = "compare_dropdown", id = NS(id, "main_drop"),
          select_var_UI(NS(id, "compare"), var_list = var_list, inline = FALSE,
                        more_style = "width:100%;"))),
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      hr())
  )
}

compare_server <- function(id, r = r, var_list, df = r[[id]]$df, 
                           disabled = reactive(NULL), 
                           time = reactive(NULL), 
                           show_panel = reactive(TRUE)) {
  
  stopifnot(!is.reactive(var_list))
  stopifnot(is.reactive(disabled))
  stopifnot(is.reactive(time))
  stopifnot(is.reactive(show_panel))

  moduleServer(id, function(input, output, session) {
    
    # Creation of a new reactiveVal per module. as the `df` argument of the
    # following select_var_server recalculates var_right_1(), this 
    # reactiveVal only gets updates if var_right_1() faces an actual change,
    # resulting in no UI (additional dropdowns) reinitialization when `df` 
    # changes. It also bypasses much of the folowing reaction chain if they
    # are just not needed.
    r[[id]]$var_compare <- reactiveVal(" ")

    var_right_1 <- select_var_server("compare", r = r, 
                                     var_list = reactive(var_list), 
                                     disabled = disabled, 
                                     time = time, 
                                     df = df)
    
    # Update the reactiveVal ONLY if var_right_1() faces an actual change.
    observeEvent(var_right_1(), {
      if (var_right_1() != r[[id]]$var_compare()) 
        r[[id]]$var_compare(var_right_1())
    })

    # Is the variable part of a larger group ?
    grouped <- eventReactive(r[[id]]$var_compare(), {
      if (var_right_1() == " ") return(NA)
      var <- sub("_\\d{4}", "", var_right_1())
      variables$grouping[variables$var_code == var]
    })
    
    # Additional dropdown values
    add_dropdowns <- eventReactive(grouped(), {
      if (is.na(grouped())) return(list(" "))

      vars_s_drop <-
        variables[!is.na(variables$grouping) &
                    variables$grouping == grouped(), ]
      
      drop_names <- names(unlist(vars_s_drop$group_diff[1]))
      
      out <- 
        sapply(drop_names, \(x) {
          all_options <- sapply(vars_s_drop$group_diff, \(y) y[[x]], 
                                USE.NAMES = TRUE)
          all_options <- unique(all_options)

          # If there is a total, put it first!
          if (sum(grepl("Total", all_options)) == 0) return(all_options)
          c("Total", all_options[!grepl("Total", all_options)])
        }, 
        USE.NAMES = TRUE)
      
      if (is.list(out)) return(out) else 
        list(as.vector(out)) |> setNames(drop_names)
    })
    
    # Create and remove the added dropdowns
    observeEvent(add_dropdowns(), {
      if (unlist(add_dropdowns())[1] == " ") 
        return(removeUI(selector = "#additional_drops"))
      
      removeUI(selector = "#additional_drops")
      
      insertUI(paste0("#", id, "-", id, "-main_drop"),
               where = "afterEnd",
               tags$div(id = "additional_drops",
                        lapply(seq_along(add_dropdowns()), \(x) {
                          vars_list <- add_dropdowns()[x]
                          lab <- names(vars_list) 
                          id_s <- gsub(" |/", "_", tolower(lab))
                          
                          select_var_UI(NS(id, NS(id, id_s)),
                                        var_list = vars_list,
                                        label = lab,
                                        inline = FALSE,
                                        more_style = "width:100%;")})))
    })
    
    # The actual var_right()
    var_right <- reactive({
      if (unlist(add_dropdowns())[1] == " ") return(var_right_1())
      
      # Result of all dropdowns in a vector
      value_keys <- 
        sapply(seq_along(add_dropdowns()), \(x) {
          vars_list <- add_dropdowns()[x]
          lab <- names(vars_list) 
          id_s <- gsub(" |/", "_", tolower(lab))
          
          input[[paste0(id_s, "-var")]]
          
        }, USE.NAMES = FALSE)
      
      # If all is NULL (at first), just throw the base variable
      if (all(is.null(unlist(value_keys)))) return(var_right_1())
      
      vars_s_drop <-
        variables[!is.na(variables$grouping) &
                               variables$grouping == grouped(), ]
      
      # Arrange the lists of dropdowns possibilities in a list
      tib <- do.call(rbind.data.frame, vars_s_drop$group_diff)
      tib$ID <- seq_len(nrow(tib))
      
      # Which variable fits with all the dropdowns
      which_in_vars_s_drop <- 
        lapply(seq_along(add_dropdowns()), \(x) {
          tib[tib[[x]] == value_keys[[x]], ]}) |> 
        (\(x) Reduce(rbind, x))() |> 
        pull(ID) |> 
        table() |> 
        (\(x) which(x == length(add_dropdowns())))()

      out <- vars_s_drop$var_code[which_in_vars_s_drop]
      if (length(out) == 0) var_right_1() else out
    })
    
    # Hide compare status
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    output$hide_status <- reactive(show_panel() && input$hide_compare %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide_compare, {
      if (input$hide_compare %% 2 == 0) {
        txt <- sus_translate(r = r, "Hide")
      } else txt <- sus_translate(r = r, "Show")
      updateActionButton(session, "hide_compare", label = txt)
    })
    
    var_right
  })
}
