#### CENSUS DOWNLOAD AND PROCESSING FUNCTIONS ##################################

# Add to variable table ---------------------------------------------------

add_vars <- function(data_out, census_vec, breaks_q3, breaks_q5, scales,
                     years) {
  
  # Get all variables at all scales
  all_vars <- unique(unlist(sapply(data_out, names)))
  all_vars <- setdiff(all_vars, "ID")
  
  # Get variables to add as rows to variables table
  add_vars <- all_vars[!grepl("_q(3|5)", all_vars)]
  add_vars <- str_remove(add_vars, "_\\d{4}$")
  add_vars <- unique(add_vars)
  
  interpolation_keys <- 
    map(set_names(names(data_out)), ~{
      if (.x %in% c("DA", "CT")) FALSE else "dissemination area"
    })
  
  map_dfr(add_vars, ~{
    
    # Get starting var table subset
    dat <- filter(census_vec, var_code == .x)
    
    # Get unique dates
    dates_active <- str_subset(all_vars, .x)
    dates_active <- str_extract(dates_active, "\\d{4}$")
    dates_active <- unique(dates_active)
    
    # Get scales
    scales_active <- c("borough", "building", "CT", "DA", "grid", "street")
    scales_active <- scales_active[map_lgl(scales_active, function(df) {
      if (!is.null(data_out[[df]])) {
        data_out[[df]] |>
          select(starts_with(.x)) |>
          length() |>
          (\(x) x > 0)()
      } else FALSE})]
    
    # Get breaks_q3
    breaks_q3_active <- map2_dfr(breaks_q3, scales, \(x, scale) {
      map2_dfr(x, years, \(y, date) {
        if (nrow(y) > 0) y |> mutate(rank = 0:3, date = date)}) |>
        mutate(scale = scale)}) |>
      select(scale, date, rank, var = all_of(.x))
    
    # Get breaks_q5
    breaks_q5_active <- map2_dfr(breaks_q5, scales, \(x, scale) {
      if (nrow(x) > 0) x |> mutate(rank = 0:5) |>
        mutate(scale = scale)}) |>
      select(scale, rank, var = all_of(.x))
    
    out <- 
      add_variables(variables,
        var_code = .x,
        var_title = dat$var_title,
        var_short = if (is.na(dat$var_short)) var_title else dat$var_short,
        explanation = dat$explanation,
        theme = case_when(str_starts(.x, "housing") ~ "Housing",
                          str_starts(.x, "inc") ~ "Income",
                          str_starts(.x, "iden") ~ "Identity",
                          str_starts(.x, "trans") ~ "Transport",
                          str_starts(.x, "emp") ~ "Employment",
                          str_starts(.x, "family") ~ "Household",
                          str_starts(.x, "lang") ~ "Language",
                          str_starts(.x, "age") ~ "Age",
                          str_starts(.x, "edu") ~ "Education"),
        category = dat$category,
        private = dat$private,
        dates = list(dates_active),
        scales = list(scales_active),
        breaks_q3 = breaks_q3_active,
        breaks_q5 = breaks_q5_active,
        source = dat$source,
        interpolated = interpolation_keys
      )
    
    out[out$var_code == .x, ]
  })
}
