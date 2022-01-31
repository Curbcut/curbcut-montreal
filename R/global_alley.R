### GREEN ALLEY MODULE GLOBALS #################################################

# Initialize reactive values
width_alley_higher_zoom <- 75

# Dropdown menu
var_list_left_alley <- 
  list("Individual alleys" = " ",
       "Per sq km" = "green_alley_sqkm",
       "Per 1,000 residents" = "green_alley_per1k")

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
      str_glue(sus_translate("<p><b>{original_list$name}</b></p>"))
  } 
  
  if (!is.null(text_to_display$green_alley_sqm) && !is.null(text_to_display$first_alley)) {
    # FIRST INAUGURATION
    text_to_display$first_alley = 
      str_glue(sus_translate("<p>The first green alley inauguration in ",
                                    "{original_list$name} was in {original_list$first_alley}, and "))
    # GREEN ALLEY LENGTH
    text_to_display$green_alley_sqm = 
      str_glue(sus_translate("there are {prettyNum(original_list$green_alley_sqm, ',')} meters of ",
                                    "them in that borough.</p>"))
  }
  
  # GREEN ALLEY LENGTH
  if (!is.null(text_to_display$green_alley_sqm) && is.null(text_to_display$first_alley)) {
    text_to_display$green_alley_sqm = 
      str_glue(sus_translate("<p>There are {prettyNum(original_list$green_alley_sqm, ',')} meters of green alleys in ",
                                    "{original_list$name}.</p>"))
  } 

  # APPLICATION PROCESS
  if (!is.null(text_to_display$app_process)) {
    text_to_display$app_process = 
      str_glue(sus_translate("<p>The application process for green alleys asks for ", 
                                    "a {str_replace(original_list$app_process, ',', ', and')}.</p>"))
  } 
  # MANAGEMENT
  if (!is.null(text_to_display$management)) {
    text_to_display$management = 
      str_glue(sus_translate("<p>In terms of management, ",
                                    "{str_to_lower(original_list$management)}.</p>"))
  } 
  # BUDGET
  if (!is.null(text_to_display$budget)) {
    text_to_display$budget = 
      str_glue(sus_translate("<p>Budget: {original_list$budget}</p>"))
  } 
  # GUIDE
  if (!is.null(text_to_display$guide)) {
    text_to_display$guide = 
      str_glue(sus_translate("<p><a href = {original_list$guide}>",
                                    "The green alley guide of {original_list$name}</a></p>"))
  } 
  # CONTACT
  if (!is.null(text_to_display$contact)) {
    text_to_display$contact = 
      str_glue(sus_translate("<p>Contact: <a href = 'mailto:{original_list$contact}'> ",
                                    "{original_list$contact}</a></p>"))
  } 
  
  text_to_display
  
}


alley_alleys_text <- function(text_to_display) {
  
  original_list <- text_to_display
  
  # In the meanwhile of finding a better way to do this:
  # NAME
  if (!is.null(text_to_display$name)) {
    text_to_display$name = 
      str_glue(sus_translate("<p><b>{original_list$name}</b></p>"))
  } 
  # INAUGURATION
  if (!is.null(text_to_display$created)) {
    text_to_display$created = 
      str_glue(sus_translate("<p>This green alley was inaugurated in ",
                                    "{original_list$created}.</p>"))
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
      str_glue(sus_translate("<p>Description: ",
                                    "{str_to_sentence(original_list$description)}</p>"))
  } 
  # BUDGET
  if (!is.null(text_to_display$circulation)) {
    
    original_list$circulation <- str_remove(original_list$circulation, "\\.")
    
    open_close <- str_extract(original_list$circulation, '.*(?=,)|.*$')
    more_info <- str_extract(original_list$circulation, "(?<=,).*")
    
    if (str_length(open_close) > 50) {
      text_to_display$circulation = 
        str_glue(sus_translate("<p>It is {open_close}</p>"))
      
    } else if (!is.na(more_info)) {
          text_to_display$circulation = 
      str_glue(sus_translate("<p>It is {open_close} to circulation, {more_info}.</p>"))
    } else {
      text_to_display$circulation = 
        str_glue(sus_translate("<p>It is {open_close} to circulation.</p>"))
      
    }
      
  } 
  
  if (!is.null(text_to_display$photo_ID)) {
    text_to_display$photo_ID =
      str_glue(
        sus_translate(
          paste0('alleys/{original_list$photo_ID}')))
  }
  
  text_to_display
  
}
