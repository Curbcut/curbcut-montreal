#### AUTO-WIDGET CREATION MODULE ###############################################
#' This module creates the number of widgets necessary to create a variable, using
#' the `group_name` and `group_diff` column of the `variables` table.

# Module UI ---------------------------------------------------------------

auto_vars_UI <- function(id, var_list, label = NULL) {
  tagList(
    div(id = NS(id, "main_drop"),
        div(id = NS(id, "staying_widgets_hr"), hr()),
        select_var_UI(NS(id, "auto_var"), var_list = var_list,
                      label = label),
        hidden(div(id = NS(id, "widgets_hr"), hr())))
  )
}


# Module server -----------------------------------------------------------

auto_vars_server <- function(id, r = r, module_id = NULL, var_list, 
                             df = r[[id]]$df, time = reactive(NULL)) {
  
  stopifnot(!is.reactive(var_list))
  
  moduleServer(id, function(input, output, session) {
    
    #### Name spacing gets pretty complicated from a module or the compare #####
    input_namespace <- reactive({
      if (is.null(module_id)) return(function(id_s) NS(id, NS(id, id_s)))
      return(function(id_s) NS(module_id, NS(module_id, NS(id, id_s))))
      })
    ui_selector <- reactive({
      if (is.null(module_id)) return(paste0("#", id, "-", id, "-main_drop"))
      return(paste0("#", module_id, "-", module_id, "-compare-main_drop"))
    })
    
    #### Toggle on and off the hr ##############################################
    observe({
      toggle("staying_widgets_hr", condition = length(staying_widgets()) != 0)
      toggle("widgets_hr", condition = length(widget_lists()) != 0)
      })

    #### WIDGETS THAT STAY ON THE MODULE NO MATTER WHAT ########################
    # Widgets that are alike in ALL the variables of the module (defined through 
    # `var_list`), and that  shouldn't move ever (we don't want them to be 
    # reactive by a change in the selected variable).
    staying_widgets <- reactive({
      groupings <- variables$group_name[variables$var_code %in% unlist(var_list)]
      if (all(is.na(groupings))) return()
      get_shared_group_diffs(groupings)
    })
    
    # Insert the widgets that are supposed to stay
    observeEvent(staying_widgets(), {
      lapply(seq_along(staying_widgets()), \(x) {
        vars_list <- staying_widgets()[x]
        id_s <- create_id_s(staying_widgets()[x])
        vars_list <- unlist(vars_list)
        
        insertUI(ui_selector(),
                 where = "beforeBegin",
                 tags$div(id = "staying_widgets",
                          if (is.numeric(vars_list)) {
                            step <- {vars_list[length(vars_list)] - 
                                vars_list[length(vars_list) - 1]}
                            def <- vars_list[ceiling(length(vars_list)/2)]
                            slider_UI(input_namespace()(id_s$key),
                                      label = cc_t(r = r, id_s$lab),
                                      min = min(vars_list),
                                      max = max(vars_list),
                                      step = step,
                                      value = def)
                          } else {
                            # Translation
                            names(vars_list) <- vars_list
                            names(vars_list) <- 
                              sapply(names(vars_list), cc_t, r = r)
                            select_var_UI(input_namespace()(id_s$key),
                                          var_list = vars_list,
                                          label = cc_t(r = r, id_s$lab))
                          }
                 ))
      })
    })
    
    #### CREATE THE FIRST DROPDOWN USING VAR_LIST ##############################
    # Get the first level dropdown
    r[[id]]$auto_var <- reactiveVal(" ")
    auto_var_1 <- select_var_server("auto_var", r = r,
                                  var_list = reactive(var_list),
                                  df = df,
                                  time = time)
    
    var_time <- reactive(regmatches(auto_var_1(), regexec("_\\d{4}", auto_var_1())) |> 
                           unlist())
    auto_var <- reactive(unique(gsub("_\\d{4}$", "", auto_var_1())))
    
    # Update the reactiveVal ONLY if auto_var() faces an actual change.
    observeEvent(auto_var(), {
      if (auto_var()[1] != r[[id]]$auto_var()[1]) r[[id]]$auto_var(auto_var())
    })
    # Get the variables table that fit with the auto_var() selection
    variables_possibilities <- eventReactive(r[[id]]$auto_var(), {
      if (auto_var() == " ") return(NULL)
      group <- variables$group_name[variables$var_code == auto_var()]
      variables[!is.na(variables$group_name) & variables$group_name == group, ]
    })
    
    #### ALL OTHER WIDGETS #####################################################
    # These widgets can change depending on auto_var() or on change in the 
    # staying widgets.
    
    # Subset of the variables table that fit with the staying widgets
    new_widget_possibilities <- 
      reactive({
        if (is.null(variables_possibilities())) return(NULL)
        staying_widgets_values <- 
          mapply(\(x, y) {
            vars_list <- staying_widgets()[y]
            id_s <- create_id_s(vars_list)$key
            type <- if (is.numeric(unlist(vars_list))) "-slider" else "-var"
            key <- paste0(id_s, type)
            return(as.character(input[[key]]))
          }, staying_widgets(), seq_along(staying_widgets()), SIMPLIFY = FALSE,
          USE.NAMES = TRUE)
        
        new_group <- sapply(variables_possibilities()$group_diff, \(x) {
          identical(x[which(names(x) %in% names(staying_widgets_values))],
                    staying_widgets_values)
        })
        
        variables_possibilities()[new_group, ]
      })
    
    # All widget values
    widget_lists <- eventReactive(new_widget_possibilities(), {
      if (is.null(variables_possibilities())) return(NULL)
      get_group_diffs(new_widget_possibilities())
    })
    
    # Additional widget values
    r[[id]]$widgets_to_add <- reactiveVal(" ")
    widgets_to_add <- reactive({
      widget_lists()[!names(widget_lists()) %in% names(staying_widgets())]
    })
    
    # Update the reactiveVal ONLY if auto_var() faces an actual change.
    observeEvent(widgets_to_add(), {
      if (!identical(widgets_to_add(), r[[id]]$widgets_to_add()))
        r[[id]]$widgets_to_add(widgets_to_add())
    })
    
    # Create and remove the reactive widgets
    observeEvent(r[[id]]$widgets_to_add(), {
      removeUI(selector = "#auto_additional_drops")
      if (is.null(variables_possibilities())) return(NULL)
      
      # Reactive widgets' UI
      insertUI(ui_selector(),
               where = "afterEnd",
               tags$div(id = "auto_additional_drops",      
                        lapply(seq_along(widgets_to_add()), \(x) {
                          vars_list <- widgets_to_add()[x]
                          id_s <- create_id_s(vars_list)
                          vars_list <- unlist(vars_list)

                          if (is.numeric(vars_list)) {
                            step <- {vars_list[length(vars_list)] - 
                                vars_list[length(vars_list) - 1]}
                            def <- vars_list[ceiling(length(vars_list)/2)]
                            slider_UI(input_namespace()(id_s$key),
                                      label = cc_t(r = r, id_s$lab),
                                      min = min(vars_list),
                                      max = max(vars_list),
                                      step = step,
                                      value = def)
                          } else {
                            # Translation
                            names(vars_list) <- vars_list
                            names(vars_list) <- 
                              sapply(names(vars_list), cc_t, r = r)
                            select_var_UI(input_namespace()(id_s$key),
                                          var_list = vars_list,
                                          label = cc_t(r = r, id_s$lab))
                          }
                        }))
      )
    })
    
    
    #### OUTPUT VARIABLE #######################################################
    # The actual reacreated variable
    recreated_var <- reactive({
      if (is.null(variables_possibilities())) return(" ")
      # Result of all dropdowns in a named list
      value_keys <-
        mapply(\(x, y) {
          vars_list <- widget_lists()[y]
          id_s <- create_id_s(vars_list)$key
          type <- if (is.numeric(unlist(vars_list))) "-slider" else "-var"
          key <- paste0(id_s, type)
          return(as.character(input[[key]]))
        }, widget_lists(), seq_along(widget_lists()), SIMPLIFY = FALSE,
        USE.NAMES = TRUE)
      
      which_match <- which(sapply(variables_possibilities()$group_diff, 
                                  \(x) identical(x, value_keys)))
      
      # Return
      out <- variables_possibilities()$var_code[which_match]
      out <- if (length(out) == 0) auto_var() else out
      paste0(out, var_time())
    })
    
    return(recreated_var)
    
  })
}
