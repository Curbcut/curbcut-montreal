#### INFO TABLE MODULE #########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param select A reactive which resolves to a character string giving the ID
#' of a row in the input data frame (`x`) which has been selected.


info_table_UI <- function(id) {
  htmlOutput(NS(id, "info_table"))
  }

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
    
    output$info_table <- renderUI({
      
      ## Get data list ---------------------------------------------------------
      
      z <- make_info_table_data(id, x, var_type, var_left, var_right, select, 
                                zoom, var_left_label, var_right_label, 
                                build_str_as_DA)

      # print("DATA_LIST")
      # print(z)
      
      
      ## Handle NAs ------------------------------------------------------------
      
      # Special case for Kahnawake
      if (z$var_type == "kah_na") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>Kahnawake Mohawk Territory</strong>",
          "<p>Statistics Canada does not gather the same ",
          "data for indigenous reserves in the Census as it does ",
          "for other jurisdictions, so we cannot display findings ",
          "here."))))
      }
      
      # Special case for Kanestake
      if (z$var_type == "kan_na") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>Kanehsatà:ke</strong>",
          "<p>Statistics Canada does not gather the same ",
          "data for indigenous reserves in the Census as it does ",
          "for other jurisdictions, so we cannot display findings ",
          "here."))))
        }
        
      # Univariate, NA selection
      if (z$var_type == "uni_na") {
        out <- HTML(glue(sus_translate(paste0(
          "{z$place_name} has no data available on {z$exp_left}."))))
      }
      
      # Bivariate, NA selection
      if (z$var_type == "bi_na") {
        out <- HTML(glue(sus_translate(paste0(
          "{z$place_name} has no data available on {z$exp_left} and ",
          "{z$exp_right}."))))
      }
      
      
      ## Univariate cases ------------------------------------------------------
      
      # Univariate, quantitative, no selection
      if (z$var_type == "uni_quant_all") {
        out <- HTML(glue(sus_translate(paste0(
          "At the {z$scale_singular} scale, {z$exp_left} varies from ",
          "{z$min_val} to {z$max_val}, with an average value of {z$mean_val} ",
          "and a median value of {z$median_val}. ",
          "Two thirds of {z$scale_plural} have a score between {z$quant_low} ",
          "and {z$quant_high}."))))
        }
      
      # Univariate, quantitative, valid selection
      if (z$var_type == "uni_quant_select") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>{z$place_heading}</strong>",
          "<p>{z$place_name} has a population of ",
          "{prettyNum(round(z$selection$population), ',')} and a ", 
          "{z$title_left} score ({z$exp_left}) of {round(z$poly_value, 2)}, which is ", 
          "{z$larger_smaller} the region-wide median of {z$median_val}.",
          "<p>{z$place_name} has a {z$poor_strong} relative score for this indicator, ",
          "with {z$exp_left} higher than {z$percentile}% ",
          "of {z$scale_plural} in the Montreal region."))))
        }

      # Univariate, qualitative, no selection
      if (z$var_type == "uni_qual_all") {
        out <- HTML(glue(sus_translate(paste0(
          "At the {z$scale_singular} scale, {z$exp_left} varies from ",
          "'{z$min_val}' to '{z$max_val}'. A plurality of {z$scale_plural} ",
          "({z$mode_prop}) have a value of '{z$mode_val}', while ",
          "{z$mode_prop_2} have a value of '{z$mode_val_2}'."))))
        }

      # Univariate, qualitative, valid selection
      if (z$var_type == "uni_qual_select") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>{z$place_heading}</strong>",
          "<p>{z$place_name} has a population of ",
          "{prettyNum(round(z$selection$population), ',')} and a ",
          "'{z$title_left}' value of '{z$poly_value}', which is shared by ",
          "{z$other_with_value}% of {z$scale_plural} in the Montreal region.")
          )))
      }
      
      
      ## Bivariate cases -------------------------------------------------------
      
      # Bivariate, quantitative, no selection
      if (z$var_type == "bi_quantxy_all") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- HTML(glue(sus_translate(paste0(
            "<p>'{z$title_right}' is {z$exp_right}.",
            "<p>The {z$title_left} has effectively no correlation ",
            "({z$correlation}) with {z$exp_right} at the ",
            "{z$scale_singular} scale.",
            "<p>This means that, at the {z$scale_singular} scale, ",
            "there is no relationship between the two variables."))))
          } else {
            out <- HTML(glue(sus_translate(paste0(
              if (z$strong_weak == "strong") "<p><b>STRONG CORRELATION</b></p>",
              "<p>'{z$title_right}' is {z$exp_right}.",
              "<p>The {z$title_left} has a {z$strong_weak} {z$pos_neg} ",
              "correlation ({z$correlation}) with {z$exp_right} at ",
              "the {z$scale_singular} scale.",
              "<p>This means that, in general, {z$scale_plural} with higher ",
              "values for '{z$title_left}' tend to have {z$higher_lower} ",
              "values for '{z$title_right}', {z$high_low_disclaimer}."))))
            }
      }
      
      # Bivariate, quantitative, valid selection
      if (z$var_type == "bi_quantxy_select") {
       
        out <- HTML(glue(sus_translate(paste0(
          "<strong>{z$place_heading}</strong>",
          "<p>{z$place_name} has a population of ",
          "{prettyNum(z$selection$population, ',')}, a '{z$title_left}' value ",
          "of {round(z$selection$left_var, 2)}, and a '{z$title_right}' ",
          "value of {round(z$selection$right_var, 2)}. ",
          "<p>These two scores are {z$relative_position}, in relative ",
          "terms. {z$place_name} has a '{z$title_left}' score higher ",
          "than {z$percentile_left}% of {z$scale_plural} and ",
          "a '{z$title_right}' score higher than ",
          "{z$percentile_right}% of {z$scale_plural} in the ",
          "Montreal region."))))
      }
      
      # Bivariate, qualitative x, quantitative y, no selection
      if (z$var_type == "bi_quanty_all") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- HTML(glue(sus_translate(paste0(
            "<p>'{z$title_right}' is {z$exp_right}.",
            "<p>The {z$title_left} has effectively no correlation ",
            "(Spearman's rho: {z$correlation}) with {z$exp_right} at the ",
            "{z$scale_singular} scale.",
            "<p>This means that, at the {z$scale_singular} scale, ",
            "there is no relationship between the two variables."))))
        } else {
          out <- HTML(glue(sus_translate(paste0(
            if (z$strong_weak == "strong") "<p><b>STRONG CORRELATION</b></p>",
            "<p>'{z$title_right}' is {z$exp_right}.",
            "<p>The {z$title_left} has a {z$strong_weak} {z$pos_neg} ",
            "correlation (Spearman's rho: {z$correlation}) with {z$exp_right} ", 
            "at the {z$scale_singular} scale.",
            "<p>This means that, in general, {z$scale_plural} with higher ",
            "values for '{z$title_left}' tend to have {z$higher_lower} ",
            "values for '{z$title_right}', {z$high_low_disclaimer}."))))
        }
      }
      
      
      ## Return output ---------------------------------------------------------
      
      return(out)
      
      })
  })
}
