#### GET CT ACCESS VARS FOR PLACE EXPLORER #####################################

get_CT_access_vars <- function(variables) {
  
  variables[!grepl("access", variables$var_code), ] |> 
  rbind({
    
    access_vars <- variables[grepl("access", variables$var_code), ]
    
    new_var_code <- c(
      access_vars$var_code[str_starts(access_vars$var_code, "access_jobs")] |> 
        str_extract("access_jobs_[^_]*"), access_vars$var_code[
          !str_starts(access_vars$var_code, "access_jobs")] |> 
        str_extract("access_[^_]*"))
    
    access_vars$var_code <- new_var_code
    unique(access_vars, incomparables = FALSE, MARGIN = 2)
    access_vars <- access_vars[!duplicated(access_vars$var_code), ]
        
        exp_suffix <- c("at weekday peak service",
                        "at weekday off-peak service",
                        "at weekday night service",
                        "at weekend peak service",
                        "at weekend off-peak service",
                        "at weekend night service")
        
        access_vars$explanation <-
          str_replace(access_vars$explanation, 
                      paste0(exp_suffix, collapse = "|"),
                      "on average")
        
        access_vars
      }
    )
  }
