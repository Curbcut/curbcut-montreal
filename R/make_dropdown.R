# Function to make the dropdown lists

make_dropdown <- 
  function(exclude = NULL, multi_year = FALSE) {
    
    census_var <- 
      variables |> 
      filter(source == "census") |> 
      mutate(dropdown_category = sub("_.*", "", var_code)) |> 
      mutate(dropdown_category = case_when(
        dropdown_category == "housing" ~ "Housing",
        dropdown_category == "inc" ~ "Income",
        dropdown_category == "iden" ~ "Immigration and ethnicity",
        dropdown_category == "trans" ~ "Transportation",
        dropdown_category == "emp" ~ "Employment",
        dropdown_category == "family" ~ "Family",
        dropdown_category == "lang" ~ "Language",
        dropdown_category == "age" ~ "Age",
        dropdown_category == "edu" ~ "Education",
        TRUE ~ NA_character_))
    
    if (!all(!is.na(census_var$dropdown_category))) {
      no_list_name <- 
        census_var |>
        filter(is.na(dropdown_category)) |> 
        pull(var_code) |> 
        paste0(collapse = ", ")
      warning(paste0("There is no list name for ", no_list_name, " in ",
                     "`R/global_make_dropdown.R`. It is not displayed."))
    }
    
    census_var <- 
    census_var |> 
      filter(dropdown_category != "Employment", !is.na(dropdown_category))
    
    if (!is.null(exclude)) {
      census_var <- 
        census_var |> 
        filter(!dropdown_category %in% exclude)
    }
    
    if (multi_year) {
      census_var <- 
      census_var |> 
        filter(lengths(dates) == max(lengths(dates)))
    }
    
    c("----" = " ",
      map(set_names(unique(census_var$dropdown_category)), function(cat) {
        category_vectors <- 
          census_var |> 
          filter(dropdown_category == cat) |> 
          select(var_code, var_title)
        
        map(category_vectors$var_title, function(name) {
          category_vectors[category_vectors$var_title == name, ]$var_code}) |> 
          set_names(category_vectors$var_title)
      })
    )
  }
