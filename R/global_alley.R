### GREEN ALLEY MODULE GLOBALS #################################################

# Map token
token_alley <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                      "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
width_alley_higher_zoom <- 75
rv_alley <- reactiveValues(poly_selected = NA, zoom = width_alley_higher_zoom)

# Dropdown menu
var_list_alley <- 
  list("----" = " ", 
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_prop_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop"),
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
       "Immigration" = list(
         "Immigrants (%)" =  "imm_prop",
         "New immigrants (%)" = "imm_new_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"))


# Fill color --------------------------------------------------------------

alleys <- 
alleys %>% 
  mutate(fill = case_when(type == "green" ~ "#008100EE",
                         type == "community" ~ "#F6BE00EE",
                         type == "mixed" ~ "#B37400EE",
                         type == "none" ~ "#262626EE",
                         TRUE ~ NA_character_))

# Legend ------------------------------------------------------------------

alley_legend_en <- 
  mapdeck_legend(
    legend_element(
      variables = c("Green", "Community", "Mixed", "None"),
      colours = c("#008100EE","#F6BE00EE", "#B37400EE", "#262626EE"),
      colour_type = "fill",
      variable_type = "category",
      title = "Green alley type")
  )


# Functions to treat text -------------------------------------------------

alley_borough_text <- function(text_to_display) {
  original_list <- text_to_display
  
  # In the meanwhile of finding a better way to do this:
  # NAME
  if (!is.null(text_to_display$name)) {
    text_to_display$name = 
      str_glue(sus_translate(paste0("<p><b>{original_list$name}</b></p>")))
  } 
  
  if (!is.null(text_to_display$ga_length) && !is.null(text_to_display$first_alley)) {
    # FIRST INAUGURATION
    text_to_display$first_alley = 
      str_glue(sus_translate(paste0("<p>The first green alley inauguration in ",
                                    "{original_list$name} was in {original_list$first_alley}, and ")))
    # GREEN ALLEY LENGTH
    text_to_display$ga_length = 
      str_glue(sus_translate(paste0("there are {prettyNum(original_list$ga_length, ',')} meters of ",
                                    "them in that borough.</p>")))
  }
  
  # GREEN ALLEY LENGTH
  if (!is.null(text_to_display$ga_length) && is.null(text_to_display$first_alley)) {
    text_to_display$ga_length = 
      str_glue(sus_translate(paste0("<p>There are {prettyNum(original_list$ga_length, ',')} meters of green alleys in ",
                                    "{original_list$name}.</p>")))
  } 

  # APPLICATION PROCESS
  if (!is.null(text_to_display$app_process)) {
    text_to_display$app_process = 
      str_glue(sus_translate(paste0("<p>The application process for green alleys asks for ", 
                                    "a {str_replace(original_list$app_process, ',', ', and')}.</p>")))
  } 
  # MANAGEMENT
  if (!is.null(text_to_display$management)) {
    text_to_display$management = 
      str_glue(sus_translate(paste0("<p>In terms of management, ",
                                    "{str_to_lower(original_list$management)}.</p>")))
  } 
  # BUDGET
  if (!is.null(text_to_display$budget)) {
    text_to_display$budget = 
      str_glue(sus_translate(paste0("<p>Budget: {original_list$budget}</p>")))
  } 
  # GUIDE
  if (!is.null(text_to_display$guide)) {
    text_to_display$guide = 
      str_glue(sus_translate(paste0("<p><a href = {original_list$guide}>",
                                    "The green alley guide of {original_list$name}</a></p>")))
  } 
  # CONTACT
  if (!is.null(text_to_display$contact)) {
    text_to_display$contact = 
      str_glue(sus_translate(paste0("<p>Contact: <a href = 'mailto:{original_list$contact}'> ",
                                    "{original_list$contact}</a></p>")))
  } 
  
  text_to_display
  
}


alley_alleys_text <- function(text_to_display) {
  
  original_list <- text_to_display
  
  # In the meanwhile of finding a better way to do this:
  # NAME
  if (!is.null(text_to_display$name)) {
    text_to_display$name = 
      str_glue(sus_translate(paste0("<p><b>{original_list$name}</b></p>")))
  } 
  # INAUGURATION
  if (!is.null(text_to_display$created)) {
    text_to_display$created = 
      str_glue(sus_translate(paste0("<p>This green alley was inaugurated in ",
                                    "{original_list$created}.</p>")))
  } 
  # ALLEY TYPE
  if (!is.null(text_to_display$type)) {
    original_list$type
    type_explain <- switch(original_list$type, 
                           green = sus_translate('It is very green'),
                           community = sus_translate('It is not that green, but have a lot of community elements (generally children-oriented)'),
                           mixed = sus_translate('It has both green and community elements'),
                           none = sus_translate('It is neither green nor community-oriented (basically grey alley)'))
    text_to_display$type = 
      str_glue(paste0("<p>{type_explain}.</p>"))
  } 
  # MANAGEMENT
  if (!is.null(text_to_display$description)) {
    text_to_display$description = 
      str_glue(sus_translate(paste0("<p>Description: ",
                                    "{str_to_sentence(original_list$description)}</p>")))
  } 
  # BUDGET
  if (!is.null(text_to_display$circulation)) {
    text_to_display$circulation = 
      str_glue(sus_translate(paste0("<p>To the circulation, it is {original_list$circulation}.</p>")))
  } 
  
  if (!is.null(text_to_display$photo_ID)) {
    text_to_display$photo_ID =
      str_glue(
        sus_translate(
          paste0('<p><img src = "alleys/{original_list$photo_ID}", ',
                 'alt = "Photo of the selected green alley", ',
                 'style = "max-width: 100%;"></p>')))
  }
  
  text_to_display
  
}