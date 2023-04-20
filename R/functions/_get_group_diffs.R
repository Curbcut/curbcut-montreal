# 
# 
# autovars_groupnames <- function(id) {
#   modules <- curbcut:::get_from_globalenv("modules")
#   
#   var_lefts <- modules$var_left[modules$id == id][[1]]
#   
#   # If it is a dataframe. Just supply the group name as a character vector.
#   if (is.data.frame(var_lefts)) {
#     return(unique(var_lefts$group_name))
#   }
#   
#   # If it's a character vector, supply a full dropdown list
#   return(dropdown_make(vars = var_lefts))
# }
# 
# autovars_common_widgets <- function(id) {
#   modules <- curbcut:::get_from_globalenv("modules")
#   variables <- curbcut:::get_from_globalenv("variables")
#   
#   # Get the list of variables
#   var_list <- modules$var_left[modules$id == id][[1]]
#   
#   # If it is a dataframe. get the variable list by subsetting the `var_code` column
#   if (is.data.frame(var_list)) {
#     var_list <- var_list$var_code
#   }
#   
#   # Time?
#   time <- unlist(variables$dates[variables$var_code %in% var_list])
#   time <- unique(time)
#   time <- as.numeric(time)
#   # What happens when there is no time?
#   
#   # Other widgets
#   tb <- modules$var_left[modules$id == id][[1]]
#   # Fish for other widgets only when `tb` is a list
#   if (is.list(tb)) {
#     groups <- variables$group_diff[variables$var_code %in% var_list]
#     groups <- unlist(groups)
#     
#     widgets <- sapply(unique(names(groups)), \(n) {
#       # All entries that need this widget
#       widg <- groups[names(groups) == n]
#       
#       # Does ALL variable need this widget?
#       if (length(widg) != length(var_list)) return(NULL)
#       
#       return(unname(groups[names(groups) == n]))
#     }, simplify = FALSE, USE.NAMES = TRUE)
#     
#     widgets <- widgets[!sapply(widgets, is.null)]
#     
#     return(list(time = time, widgets = widgets))
#   }
#   
#   # When tb is a character vector, just return `time` and an empty widgets list
#   return(list(time = time, widgets = list()))
# }
# 
# autovars_widgets <- function(id, group_name) {
#   modules <- curbcut:::get_from_globalenv("modules")
#   
#   # Grab the correct tibble
#   tb <- modules$var_left[modules$id == id][[1]]
#   
#   # If `tb` is not a dataframe, return an empty list. There are no additional 
#   # widgets to be added.
#   if (!is.data.frame(tb)) return(list())
#   
#   # Grab the difference between the variables
#   groups <- tb$group_diff[tb$group_name == group_name]
#   groups <- unlist(groups)
#   
#   widgets <- sapply(unique(names(groups)), \(n) {
#     unname(groups[names(groups) == n])
#   }, simplify = FALSE, USE.NAMES = TRUE)
#   
#   return(widgets)
# }
# 
# 
# autovars_final_value <- function(id, group_name, picker_vals, previous_var) {
#   modules <- curbcut:::get_from_globalenv("modules")
#   
#   # Grab the correct tibble
#   tb <- modules$var_left[modules$id == id][[1]]
#   
#   # If it's not a dataframe, then the output value is simply the choice of the
#   # first dropdown.
#   if (!is.data.frame(tb)) 
#     return(if (is.null(group_name)) previous_var else group_name)
#   
#   # Grab the difference between the variables
#   groups <- tb$group_diff[tb$group_name == group_name]
#   var_codes <- tb$var_code[tb$group_name == group_name]
#   
#   # Which 
#   included_vals <- lapply(groups, \(x) x %in% picker_vals)
#   sum_fits <- sapply(included_vals, sum)
#   if (length(sum_fits) == 0) return(previous_var)
#   out <- var_codes[which(sum_fits == max(sum_fits))]
#   
#   # Return()
#   return(out)
# }
# 
# autovars_placeholder_var <- function(id) {
#   modules <- curbcut:::get_from_globalenv("modules")
#   
#   var_lefts <- modules$var_left[modules$id == id][[1]]
#   
#   # If it is a dataframe. Just grab the first element of the `var_code` column
#   if (is.data.frame(var_lefts)) {
#     return(var_lefts$var_code[[1]])
#   }
#   
#   # If it's a character vector, grab the first element
#   return(var_lefts[[1]])
# 
# }