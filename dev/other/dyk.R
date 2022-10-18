#### DYK data setup ############################################################


# Setup -------------------------------------------------------------------

library(tidyverse)
library(qs)
themes <- c("Housing", "Income", "Identity", "Transport", "Family", "Language", 
            "Age", "Education", "Employment", "Urban life", "Climate risk")


# Import CSV and check for errors -----------------------------------------

dyk <-
  read_csv("dev/data/dyk.csv", show_col_types = FALSE) |> 
  rowwise() |> 
  mutate(theme = list(theme),
         variable = str_split(variable, ", ")) |> 
  ungroup()

stopifnot(
  setdiff(unique(unlist(dyk$theme)), NA) %in% themes,
  setdiff(unique(unlist(dyk$variable)), NA) %in% variables$var_code
)


# Add themes --------------------------------------------------------------

dyk <- 
  dyk |> 
  rowwise() |> 
  mutate(theme = map(variable, ~{
    variables |> 
      filter(var_code == .x) |> 
      pull(theme)}) |> 
      c(theme) |> 
      unlist() |> 
      setdiff(NA) |> 
      unique() |> 
      sort() |> 
      list()) |> 
  ungroup()



# Short distance city addition --------------------------------------------

city_amenities_walk <- 
  read.csv("dev/data/city_accessibility/short_distance_city.csv",
                                            encoding = "latin1") |>
  mutate(var_code = paste0("city_amenities_", tolower(var_code),
                           "_walk_avg")) |> 
  rowwise() |> 
  transmute(variable = list(var_code), text = str_trim(dyk_walk)) |> 
  mutate(theme = list("Transport"),
         module = "city_amenities") |> 
  filter(text != "")

city_amenities_bike <- 
  read.csv("dev/data/city_accessibility/short_distance_city.csv",
           encoding = "latin1") |> 
  mutate(var_code = paste0("city_amenities_", tolower(var_code),
                           "_bike_avg")) |> 
  rowwise() |> 
  transmute(variable = list(var_code), text = str_trim(dyk_bike)) |> 
  mutate(theme = list("Transport"),
         module = "city_amenities") |> 
  filter(text != "")

dyk <- bind_rows(dyk, city_amenities_walk, city_amenities_bike)


# Temporarily add manual DYKs ---------------------------------------------
# 
# dyk <- 
#   dyk |> 
#   add_row(category = list("covid"), variable = list("covid_may_2020"),
#           text = paste0("The city's initial plans were released in May 2020 and ", 
#                         "included over 200 kilometers of active transportation ", 
#                         "infrastructure.")) |>
#   add_row(category = list("covid"), variable = list("covid_may_2020"),
#           text = paste0("The circuits present in the May plans would have ",
#                         "connected Montreal-Nord and Ahuntsic-Cartierville, ",
#                         "two of the city's most socioeconomically disadvantaged ",
#                         "boroughs.")) |>
#   add_row(category = list("covid"), variable = list("covid_may_2020"),
#           text = paste0("Visible minority, Black, immigrant, and elderly ",
#                         "populations were consistently underrepresented in those ",
#                         "with direct access to the infrastructure.")) |>
#   add_row(category = list("covid"), variable = list("covid_july_2020"),
#           text = paste0("The city's revised plans, released in July 2020, were ",
#                         "reduced by nearly 90 kilometers, bringing the total ",
#                         "intervention length to 122 kilometers.")) |>  
#   add_row(category = list("covid"), variable = list("covid_july_2020"),
#           text = paste0("With reductions made in the intervention plans over the ",
#                         "course of summer 2020, a disproportionate decline in access  ",
#                         "was observed for racialized, immigrant, and elderly ",
#                         "populations.")) |> 
#   add_row(category = list("covid"), variable = list("covid_july_2020"),
#           text = paste0("White and low income folkss were overrepresented in the ",
#                         "populations with access to the infrastructure and also the ",
#                         "least affected by the city's decision to reduce the ",
#                         "scope of the plans.")) |> 
#   add_row(category = list("covid"), variable = list("covid_oct_2020"),
#           text = paste0("By October 2020, the city had reduced the plans to ",
#                         "just 80 kilometers of active transportation ",
#                         "interventions.")) |> 
#   add_row(category = list("covid"), variable = list("covid_oct_2020"),
#           text = paste0("The City of Montreal did not make any statements ",
#                         "concerning these dramatic reductions.")) |> 
#   add_row(category = list("covid"), variable = list("covid_2021"),
#           text = paste0("The City of Montreal released plans in 2021 to ",
#                         "pedestrianize 13 major commercial streets.")) |>
#   add_row(category = list("covid"), variable = list("covid_2021"),
#           text = paste0("The dominant retail on the pedestrianized streets is ",
#                         "restaurants and bars, making up just over 40%.")) |>
#   add_row(category = list("covid"), variable = list("covid_2021"),
#           text = paste0("The city received backlash from local merchants ",
#                         "concerning the 2020 pedestrian interventions, prompting ",
#                         "the prioritization of their interests in the 2021 plans.")) |>
#   add_row(category = list("crash"), variable = list("crash"),
#           text = paste0("Since 2012, total collisions on the Island of Montreal ",
#                         "have been decreasing.")) |> 
#   add_row(category = list("crash"), variable = list("crash"),
#           text = paste0("The majority of crashes in Montreal involve a collision ",
#                         "between 2 or more motor vehicles.")) |> 
#   add_row(category = list("crash"), variable = list("crash_total_2019"),
#           text = paste0("In 2019, there were 19,296 total collisions on the ",
#                         "Island of Montreal. That's on average 52 crashes per ",
#                         "day.")) |> 
#   add_row(category = list("crash"), variable = list("crash_total_2018"),
#           text = paste0("In 2018, there were 21,379 total collisions on the ",
#                         "Island of Montreal. That's on average 58 crashes per ",
#                         "day.")) |> 
#   add_row(category = list("crash"), variable = list("crash_total_2017"),
#           text = paste0("In 2017, there were 21,974 total collisions on the ",
#                         "Island of Montreal. That's on average 60 crashes per ",
#                         "day.")) |> 
#   add_row(category = list("crash"), variable = list("crash_total_2016"),
#           text = paste0("In 2016, there were 21,668 total collisions on the ", 
#                         "Island of Montreal. That's on average 59 crashes per ",
#                         "day.")) |>
#   add_row(category = list("crash"), variable = list("crash_total_2015"),
#           text = paste0("In 2015, there were 21,574 total collisions on the ",
#                         "Island of Montreal. That's on average 59 crashes per ",
#                         "day.")) |> 
#   add_row(category = list("crash"), variable = list("crash_total_2014"),
#           text = paste0("In 2014, there were 21,402 total collisions on the ",
#                         "Island of Montreal. That's on average 58 crashes per ",
#                         "day.")) |>
#   add_row(category = list("crash"), variable = list("crash_total_2013"),
#           text = paste0("In 2013, there were 31,607 total collisions on the ",
#                         "Island of Montreal. That's on average 86 crashes per ",
#                         "day.")) |>
#   add_row(category = list("crash"), variable = list("crash_total_2012"),
#           text = paste0("In 2012, there were 31,652 total collisions on the ",
#                         "Island of Montreal. That's on average 86 crashes per ",
#                         "day."))


# Clean up ----------------------------------------------------------------

rm(themes)
