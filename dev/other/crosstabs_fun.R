## Crosstabulation functions ###################################################

# Vulnerable population function ------------------------------------------

#' sex = c("total", "male", "female")
#' age = c("total", "0-64", "65+")
#' shelter_cost = c("total", "<30%", "30-50%", "50%-80%", ">80%")
#' immigrant_status = c("total", "non-immigrants", "immigrants",
#' "non-permanent")
#' characteristics = c("total", ...)
#'  ... =
#'  "Before 2001"
#'  "2001 to 2010"
#'  "2011 to 2016"
#'  "Economic immigrants"
#'  "Immigrants sponsored by family"
#'  "Refugees"
#'  "Other immigrants"
#'  "First generation"
#'  "Second generation"
#'  "Third generation or more"
#'  "Visible minority"
#'  "Does not belong to a visible minority group"
#'  "Aboriginal"
#'  "Lone parents (lone-parent families)"
#'  "Persons living alone"
#'  "low income before tax"
#'  "not low income before tax"
#'  "low income after tax"
#'  
#'  
#'  

get_vulnerable_pop <- function(sex = "total", 
                               age = "total",
                               shelter_cost = "total",
                               immigrant_status = "total",
                               characteristics = "total",
                               region_CT_IDs) {
  
  table <- table1
  
  # Sex
  sex_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][5])) |> 
        stringr::str_detect(paste0("^", sex)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, sex_cols]
  
  
  # Age
  age <- 
    dplyr::case_when(age == "0-64" ~ 
                       "0 to 64 years",
                     age == "64+" ~ 
                       "65 years and over",
                     TRUE ~ age) |> 
    stringr::str_to_lower()
  
  age_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][3])) |> 
        stringr::str_detect(paste0("^", age)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, age_cols]
  
  
  # Shelter cost
  shelter_cost <- 
    dplyr::case_when(shelter_cost == "<30%" ~ 
                       "Spending less than 30% of income on shelter costs",
                     shelter_cost == "30-50%" ~ 
                       "Spending 30% to less than 50% of income on shelter costs",
                     shelter_cost == "50%-80%" ~ 
                       "Spending 50% to less than 80% of income on shelter costs",
                     shelter_cost == ">80%" ~ 
                       "Spending 80% or more of income on shelter costs",
                     TRUE ~ shelter_cost) |> 
    stringr::str_to_lower()
  
  shelter_cost_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][4])) |> 
        stringr::str_detect(paste0("^", shelter_cost)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, shelter_cost_cols]
  
  
  # Immigrant status
  immigrant_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][1])) |> 
        stringr::str_detect(paste0("^", immigrant_status)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, immigrant_cols]
  
  
  # Characteristics
  characteristics <- 
    dplyr::case_when(characteristics == "low income before tax" ~ 
                       "In low income",
                     characteristics == "not low income before tax" ~ 
                       "Not in low income",
                     characteristics == "low income after tax" ~ 
                       "Low-income status - applicable",
                     TRUE ~ characteristics) |> 
    stringi::stri_replace_all_fixed(pattern = "(", 
                                    replacement = "\\(") |> 
    stringi::stri_replace_all_fixed(pattern = ")", 
                                    replacement = "\\)") |> 
    stringr::str_to_lower()
  
  characteristics_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][2])) |> 
        stringr::str_detect(paste0("^", characteristics)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, characteristics_cols]
  
  Encoding(table1$V1) <- "latin1"
  
  return(list(
    CT = 
      tibble::tibble(ID = table1$V1[5:length(table1)],
                     var = table[[1]][5:length(table1)]) |> 
      dplyr::mutate(var = as.numeric(var)) |> 
      suppressWarnings() |> 
      dplyr::filter(stringr::str_detect(ID, paste0(region_CT_IDs, collapse = "|"))) |> 
      dplyr::mutate(ID = stringr::str_extract(ID, "^\\d{7}\\.\\d{2}")),
    centraide = 
      tibble::tibble(ID = table1$V1[5:nrow(table1)],
                     var = table[[1]][5:nrow(table1)]) |> 
      dplyr::mutate(var = as.numeric(var)) |> 
      suppressWarnings() |> 
      # 113 Centraide zones
      (\(x) dplyr::slice(x, (nrow(x) - 112):nrow(x)))() |> 
      dplyr::mutate(ID = stringr::str_extract(ID, ".*?(?= \\d{5})"))
  ))
}



# Housing characteristics function ----------------------------------------


#' tenure = c("total", "owner", "tenant")
#' bedrooms = c("total", "0", "1", "2", "3", "4", "5+")
#' shelter_cost = c("total", "<30%", "30-50%", "50%-80%", ">80%")
#' characteristics = c("total",
#' "not low income after tax", "low income after tax",
#' "unsuitable", "in core need", "major repairs needed", or...)
#' 
#' ... characteristics by structural type of dwellings:
# "single-detached house"
# "semi-detached house"
# "row house"
# "apartment or flat in a duplex"
# "apartment in a building that has five or more storeys"
# "apartment in a building that has fewer than five storeys"
# "other single-attached house"
# "mobile homes and other movable dwellings "

get_housing_char <- function(tenure = "total", 
                             bedrooms = "total",
                             shelter_cost = "total",
                             characteristics = "total",
                             region_CT_IDs) {
  
  table <- table2
  
  # Tenure
  tenure_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][4])) |> 
        stringr::str_detect(paste0("^", tenure)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, tenure_cols]
  
  # Bedroom numbers
  bedrooms <- dplyr::case_when(bedrooms == "0" ~ "No bedroom",
                               bedrooms == "5+" ~ "5",
                               TRUE ~ bedrooms)
  
  bedrooms_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][3])) |> 
        stringr::str_detect(paste0("^", bedrooms)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, bedrooms_cols]
  
  # Shelter cost
  shelter_cost <- 
    dplyr::case_when(shelter_cost == "<30%" ~ 
                       "Spending less than 30% of income on shelter costs",
                     shelter_cost == "30-50%" ~ 
                       "Spending 30% to less than 50% of income on shelter costs",
                     shelter_cost == "50%-80%" ~ 
                       "Spending 50% to less than 80% of income on shelter costs",
                     shelter_cost == ">80%" ~ 
                       "Spending 80% or more of income on shelter costs",
                     TRUE ~ shelter_cost) |> 
    stringr::str_to_lower()
  
  shelter_cost_cols <- 
    purrr::map_lgl(names(table), function(col_name) {
      stringr::str_trim(stringr::str_to_lower(table[[col_name]][2])) |> 
        stringr::str_detect(paste0("^", shelter_cost)) |> 
        all()
    }) |> which()
  
  table <- 
    table[, shelter_cost_cols]
  
  # Characteristics
  # Low income is named the same in before and in after tax. The column number
  # needs to be specified instead.
  num_characteristics <- 
    dplyr::case_when(characteristics == "not low income after tax" ~ 
                       10,
                     characteristics == "low income after tax" ~ 
                       9,
                     characteristics == "unsuitable" ~ 
                       19,
                     characteristics == "in core need" ~ 
                       21,
                     characteristics == "major repairs needed" ~ 
                       23,
                     TRUE ~ 9999) 
  
  if (num_characteristics == 9999) 
    num_characteristics <- stringr::str_to_lower(characteristics)
  
  characteristics_cols <- 
    if (!is.numeric(num_characteristics)) {
      purrr::map_lgl(names(table), function(col_name) {
        stringr::str_trim(stringr::str_to_lower(table[[col_name]][1])) |> 
          stringr::str_detect(paste0("^", num_characteristics)) |> 
          all()
      }) |> which()      
    } else {
      names(table)[num_characteristics]
    }
  
  table <- 
    table[, characteristics_cols]
  
  Encoding(table2$V1) <- "latin1"
  
  return(list(
    CT = 
      tibble::tibble(ID = table2$V1[5:length(table2)],
                     var = table[[1]][5:length(table2)]) |> 
      dplyr::mutate(var = as.numeric(var)) |> 
      suppressWarnings() |> 
      dplyr::filter(stringr::str_detect(ID, paste0(region_CT_IDs, collapse = "|"))) |> 
      dplyr::mutate(ID = stringr::str_extract(ID, "^\\d{7}\\.\\d{2}")),
    centraide = 
      tibble::tibble(ID = table2$V1[5:nrow(table2)],
                     var = table[[1]][5:nrow(table2)]) |> 
      dplyr::mutate(var = as.numeric(var)) |> 
      suppressWarnings() |> 
      # 113 Centraide zones
      (\(x) dplyr::slice(x, (nrow(x) - 112):nrow(x)))() |> 
      dplyr::mutate(ID = stringr::str_extract(ID, ".*?(?= \\d{5})"))
  ))
  
}
