#### INFO TABLE MODULE #########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param select A reactive which resolves to a character string giving the ID
#' of a row in the input data frame (`x`) which has been selected.

info_table_server <- function(id, x, var_type, var_left, var_right, select, 
                              zoom, var_left_label, var_right_label, 
                              build_str_as_DA) {
  
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_type))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(zoom))

  moduleServer(id, function(input, output, session) {
    
    reactive({
      
      ## Get data list ---------------------------------------------------------
      
      print("VAR LEFT")
      print(var_left())
      print("VAR RIGHT")
      print(var_right())
      
      z <- tryCatch(
        make_info_table_data(id, x, var_type, var_left, var_right, select, 
                             zoom, var_left_label, var_right_label, 
                             build_str_as_DA), 
        error = function(e) NULL)
      
      if (is.null(z)) return(z)
      
      print("INFO TABLE")
      print(z)
      
      
      ## Handle NAs ------------------------------------------------------------
      
      # Special case for Kahnawake
      if (z$var_type == "kah_na") out <- paste0(
        "<strong>Kahnawake Mohawk Territory</strong>",
        "<p>Statistics Canada does not gather the same ",
        "data for indigenous reserves in the Census as it does ",
        "for other jurisdictions, so we cannot display findings here.")
      
      # Special case for Kanestake
      if (z$var_type == "kan_na") out <- paste0(
        "<strong>Kanehsatà:ke</strong>",
        "<p>Statistics Canada does not gather the same ",
        "data for indigenous reserves in the Census as it does ",
        "for other jurisdictions, so we cannot display findings here.")
      
      # Univariate, NA selection
      if (z$var_type == "uni_na") out <- paste0(
        "{z$place_name} has no data available on {z$exp_left}.")
      
      # Bivariate, NA selection
      if (z$var_type == "bi_na") out <- paste0(
        "{z$place_name} has no data available on {z$exp_left} and ",
        "{z$exp_right}.")
      
      
      ## Univariate single-date cases ------------------------------------------
      
      # Univariate, quantitative, no selection
      if (z$var_type == "uni_quant_all") out <- paste0(
        "At the {z$scale_sing} scale, {z$exp_left} varies from ",
        "{z$min_val} to {z$max_val}, with an average value of {z$mean_val} ",
        "and a median value of {z$median_val}. ",
        "Two thirds of {z$scale_plural} have a score between {z$quant_low} ",
        "and {z$quant_high}.")
      
      # Univariate, quantitative, valid selection
      if (z$var_type == "uni_quant_select") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>{z$place_name} has a population of {z$pop} and a ", 
        "'{z$title_left}' score ({z$exp_left}) of {z$val_left}, which is ", 
        "{z$larger} the region-wide median of {z$median_val}.",
        "<p>{z$place_name} has a {z$high} relative score for this ", 
        "indicator, with {sub('^the', 'a', z$exp_left)} higher than ", 
        "{z$percentile} of {z$scale_plural} in the Montreal region.")
      
      # Univariate, qualitative, no selection
      if (z$var_type == "uni_qual_all") out <- paste0(
        "At the {z$scale_sing} scale, {z$exp_left} varies from ",
        "'{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$scale_plural} ",
        "({z$mode_prop}) have a value of '{z$mode_val}', while ",
        "{z$mode_prop_2} have a value of '{z$mode_val_2}'.")
      
      # Univariate, qualitative, valid selection
      if (z$var_type == "uni_qual_select") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>{z$place_name} has a population of {z$pop} and a ",
        "'{z$title_left}' value of '{z$val_left}', which is shared by ",
        "{z$other_with_val} of {z$scale_plural} in the Montreal region.")

      
      ## Univariate multi-date cases -------------------------------------------
      
      # Univariate, quantitative, no selection
      if (z$var_type == "uni_quant_all_multi") out <- paste0(
        "At the {z$scale_sing} scale, the change in {z$exp_left} ",
        "between {z$start_date_left} and {z$end_date_left} varied from ",
        "{z$min_val} to {z$max_val}, with an average change of {z$mean_val} ",
        "and a median change of {z$median_val}. ",
        "Two thirds of {z$scale_plural} saw a change between {z$quant_low} ",
        "and {z$quant_high}.")
      
      # Univariate, quantitative, valid selection
      if (z$var_type == "uni_quant_select_multi") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>{sub('^t', 'T', z$exp_left)} in {z$place_name} ",
        "{z$increase} by {sub('-', '', z$val_left)} between ",
        "{z$start_date_left} and {z$end_date_left}, which is {z$larger} ",
        "the region-wide median change of {z$median_val}.",
        "<p>{z$place_name} had a {z$high} relative change for this ",
        "indicator, with a change in {z$exp_left} larger than ",
        "{z$percentile} of {z$scale_plural} in the Montreal region.")
      
      # Univariate, qualitative, no selection
      if (z$var_type == "uni_qual_all_multi") out <- paste0(
        "TKTK At the {z$scale_sing} scale, {z$exp_left} varies from ",
        "'{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$scale_plural} ",
        "({z$mode_prop}) have a value of '{z$mode_val}', while ",
        "{z$mode_prop_2} have a value of '{z$mode_val_2}'.")
      
      # Univariate, qualitative, valid selection
      if (z$var_type == "uni_qual_select_multi") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>TKTK {z$place_name} has a population of {z$pop} and a ",
        "'{z$title_left}' value of '{z$val_left}', which is shared by ",
        "{z$other_with_val} of {z$scale_plural} in the Montreal region.")
      
      
      ## Bivariate cases -------------------------------------------------------
      
      # Bivariate, quantitative, no selection
      if (z$var_type == "bi_quantxy_all") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- paste0(
            "<p>'{z$title_left}' has effectively no correlation ",
            "({z$corr_disp}) with '{z$title_right}' at the {z$scale_sing} ",
            "scale.",
            "<p>This means that, at the {z$scale_sing} scale, ",
            "there is no relationship between the two variables.")
          } else {
            out <- paste0(
              if (z$strong == sus_translate("strong")) 
                "<p><b>STRONG CORRELATION</b></p>",
              "<p>'{z$title_left}' has a {z$strong} {z$pos} ",
              "correlation ({z$corr_disp}) with '{z$title_right}' at ",
              "the {z$scale_sing} scale.",
              "<p>This means that, in general, {z$scale_plural} with a higher ",
              "{sub('^the', '', z$exp_left)} tend to have a {z$higher} ",
              "{sub('^the', '', z$exp_right)}, {z$high_low_disclaimer}.")
            }
      }
      
      # Bivariate, quantitative, valid selection
      if (z$var_type == "bi_quantxy_select") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>{z$place_name} has a population of {z$pop}, ",
        "a '{z$title_left}' value of {z$val_left}, ",
        "and a '{z$title_right}' value of {z$val_right}. ",
        "<p>These two scores are {z$relative_position}, in relative ",
        "terms. {z$place_name} has {sub('^the', 'a', z$exp_left)} higher ",
        "than {z$perc_left} of {z$scale_plural} and ",
        "{sub('^the', 'a', z$exp_right)} higher than {z$perc_right} ",
        "of {z$scale_plural} in the Montreal region.")
      
      # Bivariate, qualitative x, quantitative y, no selection
      if (z$var_type == "bi_quanty_all") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- paste0(
            "<p>'{z$title_left}' has effectively no correlation ",
            "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' at the ", 
            "{z$scale_sing} scale.",
            "<p>This means that, at the {z$scale_sing} scale, ",
            "there is no relationship between the two variables.")
        } else {
          out <- paste0(
            if (z$strong == sus_translate("strong")) 
              "<p><b>STRONG CORRELATION</b></p>",
            "<p>'{z$title_left}' has a {z$strong} {z$pos} correlation ",
            "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' ",
            "at the {z$scale_sing} scale.",
            "<p>This means that, in general, {z$scale_plural} with a higher ",
            "{sub('^the', '', z$exp_left)} tend to have a {z$higher} ",
            "{sub('^the', '', z$exp_right)}, {z$high_low_disclaimer}.")
        }
      }
      
      # Bivariate, qualitative x, quantitative y, valid selection
      if (z$var_type == "bi_quanty_select") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>{z$place_name} has a population of {z$pop}, ",
        "a '{z$title_left}' value of '{z$val_left}', and a ",
        "'{z$title_right}' value of {z$val_right}. ",
        "<p>{z$place_name} has {sub('^the', 'a', z$exp_right)} ",
        "higher than {z$perc} of other {z$scale_plural} with ",
        "{sub('^the', 'a', z$exp_left)} of '{z$val_left}' in the ",
        "Montreal region.")

            
      ## Bivariate multi-date cases --------------------------------------------
      
      # Bivariate, quantitative, no selection
      if (z$var_type == "bi_quantxy_all_multi") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- paste0(
            "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
            "'{z$title_left}' had effectively no correlation ({z$corr_disp}) ", 
            "with the change in '{z$title_right}' at the {z$scale_sing} scale.",
            "<p>This means that, at the {z$scale_sing} scale, there was no ",
            "relationship between the change in the two variables.")
        } else {
          out <- paste0(
            if (z$strong == sus_translate("strong")) 
              "<p><b>STRONG CORRELATION</b></p>",
            "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
            "'{z$title_left}' had a {z$strong} {z$pos} ",
            "correlation ({z$corr_disp}) with the change in '{z$title_right}' ", 
            "at the {z$scale_sing} scale.",
            "<p>This means that, in general, {z$scale_plural} with a higher ",
            "change in {z$exp_left} tended to have a {z$higher} change in ",
            "{z$exp_right}, {z$high_low_disclaimer}.")
        }
      }
      
      # Bivariate, quantitative, valid selection
      if (z$var_type == "bi_quantxy_select_multi") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>From {z$start_date_left} to {z$end_date_left}, {z$place_name} had",
        "a change in its '{z$title_left}' value of {z$val_left}, ",
        "and a change in its '{z$title_right}' value of {z$val_right}. ",
        "<p>These two scores are {z$relative_position}, in relative ",
        "terms. {z$place_name} had a change in {z$exp_left} higher ",
        "than {z$perc_left} of {z$scale_plural} and ",
        "a change in {z$exp_right} higher than {z$perc_right} ",
        "of {z$scale_plural} in the Montreal region.")
      
      # Bivariate, qualitative x, quantitative y, no selection
      if (z$var_type == "bi_quanty_all_multi") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- paste0(
            "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
            "'{z$title_left}' had effectively no correlation ", 
            "(Spearman's rho: {z$corr_disp}) ", 
            "with the change in '{z$title_right}' at the {z$scale_sing} scale.",
            "<p>This means that, at the {z$scale_sing} scale, there was no ",
            "relationship between the change in the two variables.")
        } else {
          out <- paste0(
            if (z$strong == sus_translate("strong")) 
              "<p><b>STRONG CORRELATION</b></p>",
            "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
            "'{z$title_left}' had a {z$strong} {z$pos} ",
            "correlation (Spearman's rho: {z$corr_disp}) with the change in ", 
            "'{z$title_right}' at the {z$scale_sing} scale.",
            "<p>This means that, in general, {z$scale_plural} with a higher ",
            "change in {z$exp_left} tended to have a {z$higher} change in ",
            "{z$exp_right}, {z$high_low_disclaimer}.")
        }
      }
      
      # Bivariate, qualitative x, quantitative y, valid selection
      if (z$var_type == "bi_quanty_select_multi") out <- paste0(
        "<strong>{z$place_heading}</strong>",
        "<p>TKTK {z$place_name} has a population of {z$pop}, ",
        "a '{z$title_left}' value of '{z$val_left}', and a ",
        "'{z$title_right}' value of {z$val_right}. ",
        "<p>{z$place_name} has {sub('^the', 'a', z$exp_right)} ",
        "higher than {z$perc} of other {z$scale_plural} with ",
        "{sub('^the', 'a', z$exp_left)} of '{z$val_left}' in the ",
        "Montreal region.")
      
      
      ## Append date information -----------------------------------------------
      
      date_left <- str_extract(var_left(), "(?<=_)\\d{4}$")
      date_right <- str_extract(var_right(), "(?<=_)_\\d{4}$")
      if (length(var_right()) == 1 && var_right() == " ") date_right <- date_left
      # TEMPORARILY EXCLUDE MULTIPLE DATES TKTK
      if (length(date_left) > 1 || is.na(date_left) || is.na(date_right)) 
        date_left <- "NA"
      if (length(date_right) > 1) date_right <- "NA"
      
      if (date_left == date_right && nchar(date_left) == 4 &&
          !grepl("_na", z$var_type)) {
        out <- paste(out, "<i>(Data from {date_left}.)</i>")
      }
      
      
      ## Return output ---------------------------------------------------------
      
      out <- HTML(glue(sus_translate(out)))
      return(out)
      
      })
  })
}
