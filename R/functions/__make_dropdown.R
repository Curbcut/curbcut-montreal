# Function to make the dropdown lists

make_dropdown <- 
  function(exclude = NULL, multi_year = FALSE, include_only = NULL) {
    
    census_var <- variables[(variables$source == "census" &
                              !is.na(variables$theme) &
                              variables$theme != "Employment") |
                              variables$theme %in% include_only, ]

    if (!is.null(exclude)) {
      census_var <- census_var[!census_var$theme %in% exclude, ]
    }
    
    if (multi_year) {
      census_var <- census_var[lengths(census_var$dates) == 
                                 max(lengths(census_var$dates))]
    }
    
    out <- c("----" = " ",
             lapply(setNames(unique(census_var$theme),
                             unique(census_var$theme)), \(cat) {
               cat_vecs <- 
                 census_var[census_var$theme == cat, c("var_code", "var_title")]
               
               lapply(cat_vecs$var_title, \(name) {
                 cat_vecs[cat_vecs$var_title == name, ]$var_code}) |> 
                 setNames(cat_vecs$var_title)
             })
    )
    
    if (!is.null(include_only)) {
      out <- out[names(out) %in% include_only]
    }
    
    return(out)
  }
