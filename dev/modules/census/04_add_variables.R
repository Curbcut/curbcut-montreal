#### CENSUS DOWNLOAD AND PROCESSING FUNCTIONS ##################################

# Add to variable table ---------------------------------------------------

add_vars <- function(data_to_add, census_vec, breaks_q3, breaks_q5, scales,
                     years) {
  
  # Get all variables at all scales
  all_vars <- unique(unlist(sapply(data_to_add, names)))
  all_vars <- setdiff(all_vars, "ID")
  
  # Get variables to add as rows to variables table
  add_vars <- all_vars[!grepl("_q(3|5)", all_vars)]
  add_vars <- str_remove(add_vars, "_\\d{4}$")
  add_vars <- unique(add_vars)
  
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
      if (!is.null(data_to_add[[df]])) {
        data_to_add[[df]] |>
          select(starts_with(.x)) |>
          length() |>
          (\(x) x > 0)()
      } else FALSE})]
    
    # Get breaks_q3
    breaks_q3_active <- map2_dfr(breaks_q3, scales, function(x, scale) {
      map2_dfr(x, years, function(y, date) {
        y |> mutate(rank = 0:3, date = date)}) |>
        mutate(scale = scale)}) |>
      select(scale, date, rank, everything())
    
    # Get breaks_q5
    breaks_q5_active <- map2_dfr(breaks_q5, scales, function(x, scale) {
      map2_dfr(x, years, function(y, date) {
        y |> mutate(rank = 0:5, date = date)}) |>
        mutate(scale = scale)}) |>
      select(scale, date, rank, everything())
    
    tibble(
      var_code = .x,
      var_title = dat$var_title,
      var_short = if (is.na(dat$var_short)) var_title else dat$var_short,
      explanation = dat$explanation,
      category = dat$category,
      private = dat$private,
      dates = list(dates_active),
      scales = list(scales_active),
      breaks_q3 = list(breaks_q3_active),
      breaks_q5 = list(breaks_q5_active)
    )
  })
}
