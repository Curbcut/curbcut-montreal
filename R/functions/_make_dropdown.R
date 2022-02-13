# Function to make the dropdown lists

make_dropdown <- 
  function(exclude = NULL, multi_year = FALSE, include_only = NULL) {
    
    census_var <- 
      variables |> 
      filter(source == "census") |> 
      filter(theme != "Employment", !is.na(theme))
    
    if (!is.null(exclude)) {
      census_var <- 
        census_var |> 
        filter(!theme %in% exclude)
    }
    
    if (multi_year) {
      census_var <- 
        census_var |> 
        filter(lengths(dates) == max(lengths(dates)))
    }
    
    out <- c("----" = " ",
             map(set_names(unique(census_var$theme)), \(cat) {
               category_vectors <- 
                 census_var |> 
                 filter(theme == cat) |> 
                 select(var_code, var_title)
               
               map(category_vectors$var_title, \(name) {
                 category_vectors[category_vectors$var_title == 
                                    name, ]$var_code}) |> 
                 set_names(category_vectors$var_title)
             })
    )
    
    if (!is.null(include_only)) {
      out <- out[names(out) %in% include_only]
    }
    
    return(out)
  }
