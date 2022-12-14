#### MAKE DROPDOWN LIST ########################################################
#' @param multi_year A boolean. TRUE if the function should exclude variables
#' that are not available at every census year. The function looks for the 
#' maximum number of years available in all the wanted variables (after `only` 
#' and `exclude`), and only keep the variables having this corresponding 
#' number of years available.
#' @param only_vars A vector of characters. Var codes can be supplied to form
#' fully custom dropdown. Evaluated before `only`.
#' @param only A named list taking multiple vectors. Only the elements 
#' corresponding are part of the dropdown. The list can take any column of the 
#' `variables` table, but should probably be used with 3 levels: `theme`, 
#' `source` and `var_code`.
#' @param exclude A named list taking multiple vectors. All of its element will
#' be excluded from the dropdown. The list can take any column of `variables` 
#' table, but should probably be used with 3 levels: `theme`, `source` and
#' `var_code`. The `exclude` code chunk runs AFTER `only`.
#' @return A named list that can be supplied to `select_var_UI` and 
#' `select_var_server`.


# Make dropdown helper function -------------------------------------------

# In the case variables are a bigger group, and other dropdowns must be added.
# Helper function, also used to update a module after a bookmark.
choose_first_data_compare_group <- function(cat_vecs) {
  # Arrange so that 'total' are higher in the list!
  cat_vecs$totals <- 
    sapply((gregexpr("total", cat_vecs$var_code, fixed = TRUE)),
           function(i) sum(i > 0))
  indices <- match(cat_vecs$totals, 
                   names(sort(table(cat_vecs$totals), 
                              decreasing = TRUE)))
  cat_vecs <- 
    cat_vecs[order(indices, cat_vecs$totals, decreasing = TRUE), ] 
  
  lapply(unique(cat_vecs$group_name), \(group) {
    # Take the first element of the list (the ones with the most `total`)
    cat_vecs$var_code[cat_vecs$group_name == group][1]}) |> 
    setNames(unique(cat_vecs$group_name))
}


# Make dropdown function --------------------------------------------------

make_dropdown <- function(multi_year = FALSE, only_vars = NULL, 
                          only = list(source = "Canadian census"), 
                          exclude = NULL, compare = FALSE) {
  
  vars <- variables[
    !variables$var_code %in% c("emp_professional_pct", "emp_creative_pct"), ]
  
  if (!is.null(only_vars)) {
    vars <- vars[vars$var_code %in% only_vars, ]
  }
  
  if (!is.null(only)) {
    vars <- 
      Reduce(\(df1, df2) {df1[df1$var_code %in% df2$var_code, ]}, 
             lapply(names(only), \(x) vars[vars[[x]] %in% only[[x]], ]))
  }
  
  if (!is.null(exclude)) {
    vars <- 
      Reduce(\(df1, df2) {df1[df1$var_code %in% df2$var_code, ]}, 
             lapply(names(exclude), \(x) vars[!vars[[x]] %in% exclude[[x]], ]))
  }
  
  if (multi_year) 
    vars <- vars[lengths(vars$dates) == max(lengths(vars$dates)), ]
  
  out <- 
    lapply(setNames(unique(vars$theme),
                    unique(vars$theme)), 
           \(cat) {
             
             # In the case the category has multiple dropdowns
             if (cat %in% vars$theme[!is.na(vars$group_name)]) {
               cat_vecs <- 
                 vars[vars$theme == cat, c("var_code", "group_name",
                                           "group_diff")]
               
               choose_first_data_compare_group(cat_vecs)
             } else {
               cat_vecs <- 
                 vars[vars$theme == cat, c("var_code", "var_title")]
               
               lapply(cat_vecs$var_title, \(name) {
                 cat_vecs[cat_vecs$var_title == name, ]$var_code}) |> 
                 setNames(cat_vecs$var_title)
             }
           })
  
  if (compare) out <- c("----" = " ", out)
  
  return(out)
}
              