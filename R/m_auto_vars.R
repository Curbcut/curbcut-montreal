#### AUTO-WIDGET CREATION MODULE ###############################################

auto_vars_UI <- function(id, var_list, label) {
  tagList(
    div(id = NS(id, "main_drop"),
        select_var_UI(NS(id, "auto_var"), var_list = var_list,
        label = label))
  )
}

auto_vars_server <- function(id, r = r, var_list, df = r[[id]]$df) {
  
  stopifnot(!is.reactive(var_list))
  
  moduleServer(id, function(input, output, session) {
    
    # Get the first level dropdown
    r[[id]]$auto_var <- reactiveVal(" ")
    
    auto_var <- select_var_server("auto_var", r = r,
                                  var_list = reactive(var_list),
                                  df = df)
    
    # Update the reactiveVal ONLY if auto_var() faces an actual change.
    observeEvent(auto_var(), {
      if (auto_var()[1] != r[[id]]$auto_var()[1]) r[[id]]$auto_var(auto_var())
    })
    
    # Widgets that are alike in ALL the variables of the module, and that 
    # shouldn't move ever.
    staying_widgets <- reactive({
      groupings <- variables$grouping[variables$var_code %in% unlist(var_list)]
      if (all(is.na(groupings))) return()
      
      all_group_diffs <- 
        variables$group_diff[variables$grouping %in% groupings]
      
      amount <- table(unlist(sapply(all_group_diffs, names)))
      staying_widgets <- names(amount)[amount == length(all_group_diffs)]
      
      widgets <- 
        sapply(staying_widgets, \(x) {
          sapply(all_group_diffs, \(y) {
            y[[x]]
          }) |> unique()
        }, simplify = FALSE)
      
      # If the choices are numeric, change to numeric
      numeric <- sapply(widgets, \(x) all(grepl("[0-9]+", x)))
      widgets <- 
        mapply(\(x, y) {
          if (!numeric[y]) return(x)
          return(as.numeric(x))
        }, widgets, seq_along(widgets), SIMPLIFY = FALSE, USE.NAMES = TRUE)
      
      lapply(seq_along(widgets), \(x) {
        vars_list <- widgets[x]
        lab <- names(vars_list)
        id_s <- gsub(" |/", "_", tolower(lab))
        vars_list <- unlist(vars_list)
        
        insertUI(paste0("#", id, "-", id, "-main_drop"),
                 where = "beforeBegin",
                 tags$div(id = "staying_widgets",
                          if (is.numeric(vars_list)) {
                            step <- {vars_list[length(vars_list)] - 
                                vars_list[length(vars_list) - 1]}
                            slider_UI(NS(id, NS(id, id_s)),
                                      label = sus_translate(r = r, lab),
                                      min = min(vars_list),
                                      max = max(vars_list),
                                      step = step,
                                      value = max(vars_list))
                          } else {
                            # Translation
                            names(vars_list) <- vars_list
                            names(vars_list) <- 
                              sapply(names(vars_list), sus_translate, r = r)
                            select_var_UI(NS(id, NS(id, id_s)),
                                          var_list = vars_list,
                                          label = sus_translate(r = r, lab),
                                          inline = FALSE,
                                          more_style = "width:100%;")
                          }
                 ))
      })
      
      return(names(widgets))
      
    })
    
    # Get the variables table that fit with the auto_var() selection
    variables_possibilities <- eventReactive(r[[id]]$auto_var(), {
      group <- variables$grouping[variables$var_code == auto_var()]
      variables[!is.na(variables$grouping) & variables$grouping == group, ]
    })
    
    # Additional dropdown values
    widget_lists <- eventReactive(r[[id]]$auto_var(), {
      drop_names <- names(unlist(variables_possibilities()$group_diff[1]))
      
      widgets <-
        sapply(drop_names, \(x) {
          all_options <- sapply(variables_possibilities()$group_diff, \(y) y[[x]],
                                USE.NAMES = TRUE) |> unique()
          # If there is a total, put it first!
          if (sum(grepl("Total", all_options)) == 0) return(all_options)
          c("Total", all_options[!grepl("Total", all_options)])
        })
      
      # If the choices are numeric, change to numeric
      numeric <- sapply(widgets, \(x) all(grepl("[0-9]+", x)))
      mapply(\(x, y) {
        if (!numeric[y]) return(x)
        return(as.numeric(x))
      }, widgets, seq_along(widgets), SIMPLIFY = FALSE, USE.NAMES = TRUE)
      
    })
    
    # Create and remove the added dropdowns
    observe({
      removeUI(selector = "#auto_additional_drops")
      
      widgets_to_add <- 
        widget_lists()[!names(widget_lists()) %in% staying_widgets()]

      # The UI
      insertUI(paste0("#", id, "-", id, "-main_drop"),
               where = "afterEnd",
               tags$div(id = "auto_additional_drops",      
                        lapply(seq_along(widgets_to_add), \(x) {
                          vars_list <- widgets_to_add[x]
                          lab <- names(vars_list)
                          id_s <- gsub(" |/", "_", tolower(lab))
                          vars_list <- unlist(vars_list)
                          
                          
                          if (is.numeric(vars_list)) {
                            step <- {vars_list[length(vars_list)] - 
                                vars_list[length(vars_list) - 1]}
                            slider_UI(NS(id, NS(id, id_s)),
                                      label = sus_translate(r = r, lab),
                                      min = min(vars_list),
                                      max = max(vars_list),
                                      step = step,
                                      value = max(vars_list))
                          } else {
                            # Translation
                            names(vars_list) <- vars_list
                            names(vars_list) <- 
                              sapply(names(vars_list), sus_translate, r = r)
                            select_var_UI(NS(id, NS(id, id_s)),
                                          var_list = vars_list,
                                          label = sus_translate(r = r, lab),
                                          inline = FALSE,
                                          more_style = "width:100%;")
                          }
                        }))
      )
    })
    
    # The actual reacreated variable
    recreated_var <- reactive({
      
      # Result of all dropdowns in a named list
      value_keys <-
        mapply(\(x, y) {
          vars_list <- widget_lists()[y]
          lab <- names(vars_list)
          id_s <- gsub(" |/", "_", tolower(lab))

          if (is.numeric(unlist(vars_list))) 
            return(as.character(input[[paste0(id_s, "-slider")]]))
          return(as.character(input[[paste0(id_s, "-var")]]))
        }, widget_lists(), seq_along(widget_lists()), SIMPLIFY = FALSE,
        USE.NAMES = TRUE)
      
      which_match <- which(sapply(variables_possibilities()$group_diff, 
                                  \(x) identical(x, value_keys)))
      
      assign("value_keys", value_keys, envir = .GlobalEnv)
      assign("x", variables_possibilities()$group_diff[[1]], envir = .GlobalEnv)
      
      # Return
      out <- variables_possibilities()$var_code[which_match]
      if (length(out) == 0) auto_var() else out
    })
    
    return(recreated_var)
    
  })
}
