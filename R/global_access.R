### ACCESS MODULE GLOBALS ######################################################

# Dropdown menu
var_left_list_1_access <-
  list("All jobs" = "access_jobs_total",
       "Low-skill jobs" = "access_jobs_low",
       "High-skill jobs" = "access_jobs_high",
       "Jobs < $30,000 annually" = "access_jobs_30k",
       "Schools" = "access_schools",
       "Healthcare facilities" = "access_healthcare")

var_left_list_2_access <-
  list("Weekday peak" = "pwd",
       "Weekday off-peak" = "opwd",
       "Weekday night" = "nwd",
       "Weekend peak" = "pwe",
       "Weekend off-peak" = "opwe",
       "Weekend night" = "nwe")
# 
# # Colour
# 
# access_colour <- c("#EDF8E9CC", "#BAE4B3CC", "#74C476CC", "#31A354CC", "#006D2CCC")
# 
# # Max values for map bins
# access_categories <- 
#   c("access_jobs_total", "access_jobs_low", "access_jobs_high", 
#     "access_jobs_30k", "access_schools", "access_healthcare")
# 
# access_max <- 
#   map_dbl(access_categories, ~{
#     CT |> 
#       st_drop_geometry() |> 
#       select(starts_with(.x) & !contains("q3")) |> 
#       as.matrix() |> 
#       max(na.rm = TRUE)
#   }) |> 
#   setNames(access_categories)
# 
# get_break <- function(max_val) {
#   break_val <- max_val / 5
#   round_ten <- round(break_val / (10 ^ ceiling(log10(break_val)))) == 1
#   digits <- ceiling(log10(break_val))
#   if_else(round_ten, 10 ^ digits, 5 * 10 ^ (digits - 1))
# }
# 
# colour_access <-
#   tibble(category = access_categories,
#          max_val = access_max) |> 
#   mutate(val_1 = get_break(max_val),
#          val_2 = val_1 * 2,
#          val_3 = val_1 * 3,
#          val_4 = val_1 * 4,
#          val_5 = val_1 * 5) |> 
#   select(-max_val) |> 
#   tidyr::pivot_longer(val_1:val_5) |> 
#   left_join(tibble(name =  c("val_1", "val_2", "val_3", "val_4", "val_5"),
#                    fill = access_colour), by = "name") |> 
#   select(-name)
# 
# colour_absolute <- 
#   tibble(fill_val = 1:5,
#          fill = access_colour)
