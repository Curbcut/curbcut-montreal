### GREEN ALLEY MODULE GLOBALS #################################################

# Initialize reactive values
width_alley_higher_zoom <- 75

# Dropdown menu
var_list_left_alley <-
  list("Individual alleys" = " ",
       "Per sq km" = "green_alley_sqkm",
       "Per 1,000 residents" = "green_alley_per1k")

# Legend ------------------------------------------------------------------
# 
# alley_legend_en <-
#   mapdeck_legend(
#     legend_element(
#       variables = c("Green", "Community", "Mixed", "None"),
#       colours = c("#008100EE","#F6BE00EE", "#B37400EE", "#262626EE"),
#       colour_type = "fill",
#       variable_type = "category",
#       title = "Green alley type")
#   )


# Functions to treat text -------------------------------------------------

alley_borough_text <- function(text_to_display) {
  original_list <- text_to_display

  # In the meanwhile of finding a better way to do this:
  # NAME
  if (!is.null(text_to_display$name)) {
    text_to_display$name =
      sus_translate("<p><b>{original_list$name}</b></p>")
  }

  if (!is.null(text_to_display$green_alley_sqm) && !is.null(text_to_display$first_alley)) {
    # FIRST INAUGURATION
    text_to_display$first_alley =
      sus_translate("<p>The first green alley inauguration in ",
                                    "{original_list$name} was in {original_list$first_alley}, and ")
    # GREEN ALLEY LENGTH
    text_to_display$green_alley_sqm =
      sus_translate("there are {prettyNum(original_list$green_alley_sqm, ',')} meters of ",
                                    "them in that borough.</p>")
  }

  # GREEN ALLEY LENGTH
  if (!is.null(text_to_display$green_alley_sqm) && is.null(text_to_display$first_alley)) {
    text_to_display$green_alley_sqm =
      sus_translate("<p>There are {prettyNum(original_list$green_alley_sqm, ',')} meters of green alleys in ",
                                    "{original_list$name}.</p>")
  }

  # APPLICATION PROCESS
  if (!is.null(text_to_display$app_process)) {
    text_to_display$app_process =
      sus_translate("<p>The application process for green alleys asks for ",
                                    "a {str_replace(original_list$app_process, ',', ', and')}.</p>")
  }
  # MANAGEMENT
  if (!is.null(text_to_display$management)) {
    text_to_display$management =
      sus_translate("<p>In terms of management, ",
                                    "{str_to_lower(original_list$management)}.</p>")
  }
  # BUDGET
  if (!is.null(text_to_display$budget)) {
    text_to_display$budget =
      sus_translate("<p>Budget: {original_list$budget}</p>")
  }
  # GUIDE
  if (!is.null(text_to_display$guide)) {
    text_to_display$guide =
      sus_translate("<p><a href = {original_list$guide}>",
                                    "The green alley guide of {original_list$name}</a></p>")
  }
  # CONTACT
  if (!is.null(text_to_display$contact)) {
    text_to_display$contact =
      sus_translate("<p>Contact: <a href = 'mailto:{original_list$contact}'> ",
                                    "{original_list$contact}</a></p>")
  }

  text_to_display

}
