#### `Info table` preparation for translation ##################################


# Import existing translation ---------------------------------------------

info_table_translated <- read.csv("dev/translation/csv/info_table_translated.csv") |> 
  as_tibble()


# Strings - R/functions/_get_info_table_data.R ----------------------------

# small_strings <-
# as_tibble(sifr::sif("sus_translate\\(.*\\)", markers = FALSE)) |>
#   filter(path == "./R/functions/_get_info_table_data.R") |>
#   mutate(contents = str_extract(contents, "sus_translate\\(.*")) |>
#   mutate(contents = str_remove_all(contents, "sus_translate\\(|\\).*")) |>
#   pull(contents) |>
#   unique() |>
#   str_replace_all('"', "'") |>
#   str_remove_all("\\\\\\'") |>
#   str_subset("'") |>
#   str_remove_all("'")
# 
# small_strings_translated <- 
#   tibble(en = small_strings) |> 
#   left_join(translation_fr, by = "en")
# 
# map2(small_strings_translated$en, 
#      small_strings_translated$fr, ~{cat('c("', .x, '" = "',.y,'"),\n', sep = "")}) |> 
#   unlist()

# Named vector, easier to build.
small_strings_translated <- 
  c(c("borough/city" = "de l'arrondissement/de la ville"),
    c("census tract" = "du secteur de recensement"),
    c("dissemination area" = "de l'aire de diffusion"),
    c("250-m" = "de 250-m"),
    c("building" = "du bâtiment"),
    c("street" = "rue"),
    c("boroughs or cities" = "arrondissements ou villes"),
    c("census tracts" = "secteurs de recensement"),
    c("dissemination areas" = "aires de diffusion"),
    c("areas" = "aires"),
    c("buildings" = "bâtiments"),
    c("streets" = "rues"),
    c("The dissemination area around {select_name$name}" = "L'aire de diffusion autour de {select_name$name}"),
    c("Census tract {select_name$name}" = "Secteur de recensement {select_name$name} "),
    c("Dissemination area {select_name$name}" = "Zone de diffusion {select_name$name} "),
    c("The area around {select_name$name}" = "La zone autour de {select_name$name} "),
    c("{select_name$name_2} of {out$place_name}" = "{select_name$name_2} de {out$place_name}"),
    c("much larger than<<m>>" = "beaucoup plus grand que"),
    c("larger than<<m>>" = "plus grand que"),
    c("almost the same as<<m>>" = "presque identique à"),
    c("smaller than<<m>>" = "plus petit que"),
    c("much smaller than<<m>>" = "beaucoup plus petit que"),
    c("high<<m>>" = "élevé"),
    c("low<<m>>" = "faible"),
    c("moderate<<m>>" = "modéré"),
    c("increased<<m>>" = "augmenté"),
    c("decreased<<m>>" = "diminué"),
    c("majority" = "majorité"),
    c("plurality" = "pluralité"),
    c("positive<<f>>" = "positive"),
    c("negative<<f>>" = "négative"),
    c("strong<<f>>" = "forte"),
    c("moderate<<f>>" = "modérée"),
    c("weak<<f>>" = "faible"),
    c("higher" = "plus grand/e"),
    c("lower" = "plus petit/e"),
    c("with only a few exceptions" = "à quelques exceptions près"),
    c("although with some exceptions" = "bien qu'avec des exceptions"),
    c("although with many exceptions" = "bien qu'avec beaucoup d'exceptions"),
    c("weak" = "faible"),
    c("dramatically different" = "radicalement différents"),
    c("substantially different" = "sensiblement différents"),
    c("considerably different" = "modérément différents"),
    c("similar" = "similaires"))

small_strings_translated <- 
  tibble(en = names(small_strings_translated),
         fr = small_strings_translated)

# Texts info table --------------------------------------------------------

texts_translated <- 
  tibble(en = c(
    
## Handle NAs  ------------------------------------------------------------
# Special case for Kahnawake
paste0(
  "<strong>Kahnawake Mohawk Territory</strong>",
  "<p>Statistics Canada does not gather the same ",
  "data for indigenous reserves in the Census as it does ",
  "for other jurisdictions, so we cannot display findings here."),

# Special case for Kanestake
paste0(
  "<strong>Kanehsatà:ke</strong>",
  "<p>Statistics Canada does not gather the same ",
  "data for indigenous reserves in the Census as it does ",
  "for other jurisdictions, so we cannot display findings here."),

# Univariate, NA selection
paste0(
  "{z$place_name} has no data available on {z$exp_left}."),

# Bivariate, NA selection
paste0(
  "{z$place_name} has no data available on {z$exp_left} and ",
  "{z$exp_right}."),


## Univariate single-date cases -------------------------------------------

# Univariate, quantitative, no selection
paste0(
  "At the {z$scale_sing} scale, {z$exp_left} varies from ",
  "{z$min_val} to {z$max_val}, with an average value of {z$mean_val} ",
  "and a median value of {z$median_val}. ",
  "Two thirds of {z$scale_plural} have a score between {z$quant_low} ",
  "and {z$quant_high}."),


# Univariate, quantitative, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>{z$place_name} has a population of {z$pop} and a ", 
  "'{z$title_left}' score ({z$exp_left}) of {z$val_left}, which is ", 
  "{z$larger} the region-wide median of {z$median_val}.",
  "<p>{z$place_name} has a {z$high} relative score for this ", 
  "indicator, with '{z$exp_left)}' higher than ", 
  "{z$percentile} of {z$scale_plural} in the Montreal region."),

# Univariate, qualitative, no selection
paste0(
  "At the {z$scale_sing} scale, {z$exp_left} varies from ",
  "'{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$scale_plural} ",
  "({z$mode_prop}) have a value of '{z$mode_val}', while ",
  "{z$mode_prop_2} have a value of '{z$mode_val_2}'."),

# Univariate, qualitative, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>{z$place_name} has a population of {z$pop} and a ",
  "'{z$title_left}' value of '{z$val_left}', which is shared by ",
  "{z$other_with_val} of {z$scale_plural} in the Montreal region."),


## Univariate multi-date cases -------------------------------------------

# Univariate, quantitative, no selection
paste0(
  "At the {z$scale_sing} scale, the change in {z$exp_left} ",
  "between {z$start_date_left} and {z$end_date_left} varied from ",
  "{z$min_val} to {z$max_val}, with an average change of {z$mean_val} ",
  "and a median change of {z$median_val}. ",
  "Two thirds of {z$scale_plural} saw a change between {z$quant_low} ",
  "and {z$quant_high}."),

# Univariate, quantitative, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>{sentence(z$exp_left)} in {z$place_name} ",
  "{z$increase} by {sub('-', '', z$val_left)} between ",
  "{z$start_date_left} and {z$end_date_left}, which is {z$larger} ",
  "the region-wide median change of {z$median_val}.",
  "<p>{z$place_name} had a {z$high} relative change for this ",
  "indicator, with a change in {z$exp_left} larger than ",
  "{z$percentile} of {z$scale_plural} in the Montreal region."),

# Univariate, qualitative, no selection
paste0(
  "TKTK At the {z$scale_sing} scale, {z$exp_left} varies from ",
  "'{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$scale_plural} ",
  "({z$mode_prop}) have a value of '{z$mode_val}', while ",
  "{z$mode_prop_2} have a value of '{z$mode_val_2}'."),

# Univariate, qualitative, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>TKTK {z$place_name} has a population of {z$pop} and a ",
  "'{z$title_left}' value of '{z$val_left}', which is shared by ",
  "{z$other_with_val} of {z$scale_plural} in the Montreal region."),


## Bivariate cases -------------------------------------------------------

# Bivariate, quantitative, no selection
  # If correlation is close to zero
paste0(
      "<p>'{z$title_left}' has effectively no correlation ",
      "({z$corr_disp}) with '{z$title_right}' at the {z$scale_sing} ",
      "scale.",
      "<p>This means that, at the {z$scale_sing} scale, ",
      "there is no relationship between the two variables."),
# If correlation is strong
paste0(
      "<p><b>STRONG CORRELATION</b></p>",
      "<p>'{z$title_left}' has a {z$strong} {z$pos} ",
      "correlation ({z$corr_disp}) with '{z$title_right}' at ",
      "the {z$scale_sing} scale.",
      "<p>This means that, in general, {z$scale_plural} with a higher ",
      "{sub('^the ', '', z$exp_right)} tend to have a {z$higher} ",
      "{sub('^the ', '', z$exp_left)}, {z$high_low_disclaimer}."),

# Bivariate, quantitative, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>{z$place_name} has a population of {z$pop}, ",
  "a '{z$title_left}' value of {z$val_left}, ",
  "and a '{z$title_right}' value of {z$val_right}. ",
  "<p>These two scores are {z$relative_position}, in relative ",
  "terms. {z$place_name} has {sub('^the', 'a', z$exp_left)} higher ",
  "than {z$perc_left} of {z$scale_plural} and ",
  "{sub('^the', 'a', z$exp_right)} higher than {z$perc_right} ",
  "of {z$scale_plural} in the Montreal region."),

# Bivariate, qualitative x, quantitative y, no selection
paste0(
      "<p>'{z$title_left}' has effectively no correlation ",
      "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' at the ", 
      "{z$scale_sing} scale.",
      "<p>This means that, at the {z$scale_sing} scale, ",
      "there is no relationship between the two variables."),
paste0(
      "<p><b>STRONG CORRELATION</b></p>",
      "<p>'{z$title_left}' has a {z$strong} {z$pos} correlation ",
      "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' ",
      "at the {z$scale_sing} scale.",
      "<p>This means that, in general, {z$scale_plural} with a higher ",
      "{sub('^the ', '', z$exp_right)} tend to have a {z$higher} ",
      "{sub('^the ', '', z$exp_left)}, {z$high_low_disclaimer}."),

# Bivariate, qualitative x, quantitative y, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>{z$place_name} has a population of {z$pop}, ",
  "a '{z$title_left}' value of '{z$val_left}', and a ",
  "'{z$title_right}' value of {z$val_right}. ",
  "<p>{z$place_name} has {sub('^the', 'a', z$exp_right)} ",
  "higher than {z$perc} of other {z$scale_plural} with ",
  "{sub('^the', 'a', z$exp_left)} of '{z$val_left}' in the ",
  "Montreal region."),

# Bivariate, quantitative x, qualitative y, no selection

# If correlation is close to zero
paste0(
      "<p>'{z$title_left}' has effectively no correlation ",
      "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' at the ", 
      "{z$scale_sing} scale.",
      "<p>This means that, at the {z$scale_sing} scale, ",
      "there is no relationship between the two variables."),
# If correlation is strong
paste0(
      "<p><b>STRONG CORRELATION</b></p>",
      "<p>'{z$title_left}' has a {z$strong} {z$pos} correlation ",
      "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' ",
      "at the {z$scale_sing} scale.",
      "<p>This means that, in general, {z$scale_plural} with a higher ",
      "{sub('^the ', '', z$exp_right)} tend to have a {z$higher} ",
      "{sub('^the ', '', z$exp_left)}, {z$high_low_disclaimer}."),

# Bivariate, quantitative x, qualitative y, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>{z$place_name} has a population of {z$pop}, ",
  "a {z$title_left} value of '{z$val_left}', and a ",
  "'{z$title_right}' value of '{z$val_right}'. ",
  "<p>{z$place_name} has {sub('^the', 'a', z$exp_left)} ",
  "higher than {z$perc} of other {z$scale_plural} with ",
  "{sub('^the', 'a', z$exp_right)} of '{z$val_right}' in the ",
  "Montreal region."),


## Bivariate multi-date cases ---------------------------------------------

# Bivariate, quantitative, no selection
# If correlation is close to zero
paste0(
      "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
      "'{z$title_left}' had effectively no correlation ({z$corr_disp}) ", 
      "with the change in '{z$title_right}' at the {z$scale_sing} scale.",
      "<p>This means that, at the {z$scale_sing} scale, there was no ",
      "relationship between the change in the two variables."),
# If correlation is strong
paste0(
        "<p><b>STRONG CORRELATION</b></p>",
      "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
      "'{z$title_left}' had a {z$strong} {z$pos} ",
      "correlation ({z$corr_disp}) with the change in '{z$title_right}' ", 
      "at the {z$scale_sing} scale.",
      "<p>This means that, in general, {z$scale_plural} with a higher ",
      "change in {z$exp_left} tended to have a {z$higher} change in ",
      "{z$exp_right}, {z$high_low_disclaimer}."),

# Bivariate, quantitative, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>From {z$start_date_left} to {z$end_date_left}, {z$place_name} had ",
  "a change in its '{z$title_left}' value of {z$val_left}, ",
  "and a change in its '{z$title_right}' value of {z$val_right}. ",
  "<p>These two scores are {z$relative_position}, in relative ",
  "terms. {z$place_name} had a change in {z$exp_left} higher ",
  "than {z$perc_left} of {z$scale_plural} and ",
  "a change in {z$exp_right} higher than {z$perc_right} ",
  "of {z$scale_plural} in the Montreal region."),

# Bivariate, qualitative x, quantitative y, no selection
# If correlation is close to zero
paste0(
      "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
      "'{z$title_left}' had effectively no correlation ", 
      "(Spearman's rho: {z$corr_disp}) ", 
      "with the change in '{z$title_right}' at the {z$scale_sing} scale.",
      "<p>This means that, at the {z$scale_sing} scale, there was no ",
      "relationship between the change in the two variables."),
# If correlation is strong
paste0(
      "<p><b>STRONG CORRELATION</b></p>",
      "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
      "'{z$title_left}' had a {z$strong} {z$pos} ",
      "correlation (Spearman's rho: {z$corr_disp}) with the change in ", 
      "'{z$title_right}' at the {z$scale_sing} scale.",
      "<p>This means that, in general, {z$scale_plural} with a higher ",
      "change in {z$exp_left} tended to have a {z$higher} change in ",
      "{z$exp_right}, {z$high_low_disclaimer}."),

# Bivariate, qualitative x, quantitative y, valid selection
paste0(
  "<strong>{z$place_heading}</strong>",
  "<p>TKTK {z$place_name} has a population of {z$pop}, ",
  "a '{z$title_left}' value of '{z$val_left}', and a ",
  "'{z$title_right}' value of {z$val_right}. ",
  "<p>{z$place_name} has {sub('^the', 'a', z$exp_right)} ",
  "higher than {z$perc} of other {z$scale_plural} with ",
  "{sub('^the', 'a', z$exp_left)} of '{z$val_left}' in the ",
  "Montreal region."),


## Special cases ----------------------------------------------------------

# If correlation is close to zero
paste0(
      "<p>During {z$date_left}, {z$exp_left} ",
      "averaged {z$mean_val} per day. ",
      "The maximum value was {z$max_val} on {z$max_date}, and the ",
      "minimum value was {z$min_val} on {z$min_date}. ",
      "There was no growth trend during this time period."),
# If correlation is strong
paste0(
      "<p>During {z$date_left}, {z$exp_left} ",
      "averaged {z$mean_val} per day. ",
      "The maximum value was {z$max_val} on {z$max_date}, and the ",
      "minimum value was {z$min_val} on {z$min_date}. ",
      "There was a {z$strong} {z$pos} growth trend during this time ",
      "period, with {z$exp_left} {z$coef_increasing} an average of ",
      "{z$coef} each day.")

)) |> 
  left_join(info_table_translated, by = "en")


# Same file for both smaller strings and texts ----------------------------

info_table_translated <- 
  bind_rows(small_strings_translated, texts_translated) |> 
  distinct()


# Warnings ----------------------------------------------------------------

# Do we need to update this file?
modif_date_info_table <- file.info("R/functions/_info_table.R")$mtime
modif_date_this_file <- file.info("dev/translation/prep_info_table.R")$mtime

if (modif_date_info_table > modif_date_this_file) {
  warning("Info table texts have possibly been changed since last translation.")
}

# Or missing translation would give the same result
walk(seq_along(info_table_translated$fr), ~{
  
  x <- pull(info_table_translated[.x, "fr"])
  
  if (is.na(x)) {
    no_translation <-
      pull(info_table_translated[.x, "en"])
    warning(paste0("No translation found for `", no_translation, 
                   "` (variables table)."), 
            call. = FALSE)
  }
})


# Save --------------------------------------------------------------------

# Also encode english for `Kanehsatà:ke`
Encoding(info_table_translated$en) <- "UTF-8"
Encoding(info_table_translated$fr) <- "UTF-8"
write_csv(info_table_translated, 
          file = "dev/translation/csv/info_table_translated.csv")

# Clean-up ----------------------------------------------------------------

rm(info_table_translated, small_strings_translated, 
   texts_translated)
