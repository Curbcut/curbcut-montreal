## HELPERS FOR THE M_AUTO_VARS #################################################

change_inlist_numeric <- function(widgets) {
  # If the choices are numeric, change to numeric
  numeric <- sapply(widgets, \(x) all(grepl("[0-9]+", x)))
  mapply(\(x, y) {
    if (!numeric[y]) return(x)
    return(as.numeric(x))
  }, widgets, seq_along(widgets), SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

get_shared_group_diffs <- function(group_name) {
  all_group_diffs <- 
    variables$group_diff[variables$group_name %in% group_name]
  
  amount <- table(unlist(sapply(all_group_diffs, names)))
  staying_widgets <- names(amount)[amount == length(all_group_diffs)]
  
  widgets <- 
    sapply(staying_widgets, \(x) {
      sapply(all_group_diffs, \(y) {
        y[[x]]
      }) |> unique()
    }, simplify = FALSE)
  
  # If the choices are numeric, change to numeric to inform a slider
  change_inlist_numeric(widgets)
}


get_group_diffs <- function(variables_table) {
  drop_names <- names(unlist(variables_table$group_diff[1]))
  
  widgets <-
    sapply(drop_names, \(x) {
      all_options <- sapply(variables_table$group_diff, \(y) y[[x]],
                            USE.NAMES = TRUE) |> unique()
      # If there is a total, put it first!
      if (sum(grepl("Total", all_options)) == 0) return(all_options)
      c("Total", all_options[!grepl("Total", all_options)])
    })
  
  # If the choices are numeric, change to numeric
  change_inlist_numeric(widgets)
}


create_id_s <- function(widget) {
  vars_list <- widget
  lab <- names(vars_list)
  return(list(key = gsub(" |/", "_", tolower(lab)),
         lab = lab))
}
