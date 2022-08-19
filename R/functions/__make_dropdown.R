#### MAKE DROPDOWN LIST ########################################################
#' @param multi_year A boolean. TRUE if the function should exclude variables
#' that are not available at every census year. The function looks for the 
#' maximum number of years available in all the wanted variables (after `only` 
#' and `exclude`), and only keep the variables having this corresponding 
#' number of years available.
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

make_dropdown <- function(multi_year = FALSE, 
                          only = list(source = "Canadian census"), 
                          exclude = NULL, 
                          compare = FALSE) {
  
  vars <- variables
  
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
  
  out <- lapply(setNames(unique(vars$theme),
                         unique(vars$theme)), \(cat) {
                           cat_vecs <- 
                             vars[vars$theme == cat, c("var_code", "var_title")]
                           
                           lapply(cat_vecs$var_title, \(name) {
                             cat_vecs[cat_vecs$var_title == name, ]$var_code}) |> 
                             setNames(cat_vecs$var_title)
                         })
  
  # Special case for now, double dropdown in compare
  # Ultimately make_dropdown() could detect presence such as transportation modes
  # and output a list of length two, indicating which value of the list needs a second
  # dropdown
  if ("theme" %in% names(unlist(only)) &&
    unlist(only)[["theme"]] == "Accessibility to amenities") {
    out <- out[[1]][grep("_walk_", unlist(out))]
    names(out) <- gsub(" by walk$", "", names(out))
    out <- list("Accessibility to amenities" = out)
  }
  
  if (compare) out <- c("----" = " ", out)
  
  return(out)
}

# Amenities' transportation modes' list
amenities_modes <- 
  list("Mode of transportation" =
         list("By walk" = "walk",
              "By bike" = "bicycle",
              "By transit" = "transit",
              "By car" = "car"))
