### TIME SLIDER GLOBALS ########################################################

# Crash HTR ---------------------------------------------------------------

crash_read_uni <- paste0(
  "<b>How to read the map</b><br>",
  "The map displays the percent variation in number of ",
  "{type_crash} crashes between {time()[1]} and {time()[2]}. ",
  "Blue means an increase in {type_crash} crashes number, and red ",
  "means a decrease.")

crash_read_bi_2 <- paste0(
  "<b>How to read the map</b><br>",
  "The map displays the comparison of two percent variations. ",
  "In green, the percent variation in number of ",
  "{type_crash} crashes between {time()[1]} and {time()[2]}. ",
  "A darker green means a relative increase in {type_crash} ",
  "crashes number. In blue, the percent variation of '{var}' between ",
  "{census_years[1]} and {census_years[2]}, the closest census years available. ",
  "A darker blue means a relative increase in '{var}'. ",
  "You can find the comparison legend at ",
  "the bottom left of the map.")


crash_read_bi_1 <- paste0(
  "<b>How to read the map</b><br>",
  "The map displays the comparison of a percent variation ",
  "with a census variable. In green, the percent variation ",
  "in number of {type_crash} crashes between {time()[1]} and {time()[2]}. ",
  "A darker green means a relative increase in {type_crash} ",
  "crashes number. Displayed in blue is '{var}' numbers in {census_years}, ",
  "the closest census year available. ",
  "A darker blue means a relatively higher number of '{var}'. ",
  "You can find the comparison legend at the bottom left of the map.")


# Housing HTR -------------------------------------------------------------
housing_read_uni <- paste0(
  "<b>How to read the map</b><br>",
  "The map displays the percent variation in ",
  "'{var_left_title}' between {time()[1]} and {time()[2]}. ",
  "Blue means an increase in '{var_left_title}', and red ",
  "means a decrease.")

housing_read_bi <- paste0(
  "<b>How to read the map</b><br>",
  "The map displays the comparison of two percent variations. ",
  "In green, the percent variation in '{var_left_title}' ",
  "between {time()[1]} and {time()[2]}. ",
  "A darker green means a relative increase in '{var_left_title}'. ",
  "In blue, the percent variation of '{var_right_title}' between ",
  "{time()[1]} and {time()[2]}. ",
  "A darker blue means a relative increase in '{var_right_title}'. ",
  "You can find the comparison legend at ",
  "the bottom left of the map.")
