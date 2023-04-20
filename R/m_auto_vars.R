# 
# autovars_UI <- function(id) {
#   shiny::tagList(
#     shiny::div(
#       id = shiny::NS(id, "autovars"),
#       shiny::hr(id = shiny::NS(id, "common_widgets")),
#       shinyjs::hidden(shiny::hr(id = shiny::NS(id, "hr_additional_widgets")))
#     )
#   )
# }
# 
# autovars_server <- function(id, r, main_dropdown_title, default_year) {
#   shiny::moduleServer(id, function(input, output, session) {
#     # Global preparation ------------------------------------------------------
# 
#     # Selector function. Retrieve the namespace function associated with the
#     # current module's session. Add a '#' to use it as a selector
#     html_ns <- function(css_id) {
#       sprintf("#%s", session$ns(css_id))
#     }
#     widget_ns <- session$ns
#     additional_picker_count <- shiny::reactiveVal(0)
#     out_var <- shiny::reactiveVal(autovars_placeholder_var(id = id))
# 
#     # Common widgets ----------------------------------------------------------
# 
#     # If some widgets are alike in all the variables to be picked through,
#     # make these widgets in top in priority. e.g. `time`
#     observe({
#       common_widgets <- autovars_common_widgets(id = id)
#       # Time widgets
#       shiny::insertUI(
#         selector = html_ns("common_widgets"),
#         where = "beforeBegin",
#         ui = {
#           min_ <- common_widgets$time |> min()
#           max_ <- common_widgets$time |> max()
#           step_ <- unique(diff(common_widgets$time))[1]
#           double_value_ <- common_widgets$time[ceiling(length(common_widgets$time) / 2)]
#           double_value_ <- c(double_value_, max_)
#           shiny::tagList(
#             slider_UI(id = widget_ns(id), slider_id = "slu", min = min_, max = max_,
#                       step = step_, label = cc_t("Select a year")),
#             slider_UI(id = widget_ns(id), slider_id = "slb", min = min_, max = max_,
#                       step = step_, label = cc_t("Select two years"), value = double_value_),
#             checkbox_UI(id = widget_ns(id), label = cc_t("Compare dates"), value = FALSE)
#           )
#         })
#       # Other widgets
#       if (length(common_widgets$widgets) > 0)
#         lapply(widgets, \(w) {
#           shiny::insertUI(selector = html_ns("additional_widgets"),
#                           where = "afterEnd",
#                           ui = {
#                             curbcut::picker_UI(id = widget_ns(id),
#                                                ...)
#                           })
#         })
#     })
#     # Grab the time values
#     slider_uni <- slider_server(id = id, slider_id = "slu")
#     slider_bi <- slider_server(id = id, slider_id = "slb")
#     slider_switch <- checkbox_server(id = id, r = r,
#                                      label = shiny::reactive("Compare dates"))
#     # Enable or disable first and second slider
#     shiny::observeEvent(slider_switch(), {
#       shinyjs::toggle(shiny::NS(id, "ccslider_slu"), condition = !slider_switch())
#       shinyjs::toggle(shiny::NS(id, "ccslider_slb"), condition = slider_switch())
#     })
#     # Grab the right time
#     time <- shiny::reactive({
#       # In the case the UIs are not initiated.
#       if (is.null(slider_switch())) return(default_year)
#       if (slider_switch()) slider_bi() else slider_uni()
#     })
# 
#     # Main dropdown -----------------------------------------------------------
# 
#     # Draw and get value from the first dropdown
#     observe({
#       shiny::insertUI(selector = html_ns("common_widgets"),
#                       where = "afterEnd",
#                       ui = {
#                         if (is.na(main_dropdown_title)) main_dropdown_title <- NULL
#                         curbcut::picker_UI(id = widget_ns(id),
#                                            picker_id = "mnd",
#                                            var_list = autovars_groupnames(id = id),
#                                            label = main_dropdown_title)
#                       })
#     })
# 
#     # Grab the main dropdown's info
#     mnd_list <- autovars_groupnames(id = id)
#     # If it's a list already formated using `dropdown_make(), use it as is. If
#     # not, format it for the picker updates.
#     mnd_list <- if (is.list(mnd_list)) mnd_list else {
#       mnd_list <- list(mnd_list)
#       names(mnd_list) <- main_dropdown_title
#       mnd_list
#     }
#     mnd <- curbcut::picker_server(id = id, r = r, picker_id = "mnd",
#                                   var_list = mnd_list)
# 
#     # Detect the variables that are under the main dropdown value
#     widgets <- shiny::reactive(autovars_widgets(id = id, group_name = mnd()))
# 
# 
#     # Additional widgets ------------------------------------------------------
# 
#     shiny::observe({
#       # Remove the content of the previous div
#       shiny::removeUI(selector = "#additional_widgets")
# 
#       # If there are additional widgets only, show the hr
#       shinyjs::toggle("hr_additional_widgets", condition = length(widgets()) > 0)
# 
#       # Other widgets (dropdowns)
#       shiny::insertUI(
#         selector = html_ns("hr_additional_widgets"),
#         where = "afterEnd",
#         ui = {
#           shiny::tags$div(
#             id = "additional_widgets",
#             do.call(shiny::tagList, mapply(function(w, l, n) {
#               w <- list(w)
#               names(w) <- n
#               curbcut::picker_UI(id = widget_ns(id),
#                                  picker_id = sprintf("d%s", l),
#                                  var_list = w,
#                                  label = n)
#             }, widgets(), seq_along(widgets()), names(widgets()), SIMPLIFY = FALSE))
#           )
#         })
#       # Update the number of picker values to be retrieved
#       additional_picker_count(length(widgets()))
#     })
# 
# 
#     # Make the final variable -------------------------------------------------
# 
#     picker_vals <- shiny::reactive({
#       picker_vals <- list()
#       for (i in seq_along(additional_picker_count())) {
#         picker_id <- sprintf("ccpicker_d%s", i)
#         picker_vals[[i]] <- input[[shiny::NS(id, picker_id)]]
#       }
#       return(unlist(picker_vals))
#     })
# 
#     shiny::observe({
#       z <- autovars_final_value(id = id, group_name = mnd(),
#                                 picker_vals = picker_vals(),
#                                 previous_var = out_var())
#       out_var(z[[1]])
#     })
# 
#     final_var <- shiny::reactive({
#       sprintf("%s_%s", out_var(), time())
#     })
# 
#     return(shiny::reactive(list(var = final_var(), time = time())))
#   })
# }
# 
# 
