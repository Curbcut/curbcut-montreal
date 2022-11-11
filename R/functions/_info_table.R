#### INFO TABLE ################################################################

info_table <- function(r, data, var_type, var_left, var_right, df, select_id,
                       geo, build_str_as_DA = TRUE) {
  
  stopifnot(!is.reactive(data))
  stopifnot(!is.reactive(var_type))
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  stopifnot(!is.reactive(df))
  stopifnot(!is.reactive(select_id))
  stopifnot(!is.reactive(build_str_as_DA))
  
  
  ## Return early for all-NA table ---------------------------------------------
  
  if (var_type %in% c("NA_delta", "NA_delta_bivar")) {
    out <- "No data available."
    return(HTML(cc_t(r = r, out)))}
  
  
  ## Get data list -------------------------------------------------------------
  
  z <- get_info_table_data(
    r = r,
    data = data, 
    var_type = var_type, 
    var_left = var_left, 
    var_right = var_right, 
    df = df,
    select_id = select_id,
    geo = geo,
    build_str_as_DA = build_str_as_DA)
  
  if (is.null(z)) return(z)
  
  ## Handle NAs ----------------------------------------------------------------
  
  # ALL NA
  if (var_right[1] != " " && z$var_type == "all_na") out <- cc_t(r = r, 
     "We have no data on {z$exp_left} at the {z$scale_sing} scale.")
  
  if (var_right[1] == " " && z$var_type == "all_na") out <- cc_t(r = r, 
      "We have no data on {z$exp_left} or on {z$exp_right} at",
      " the {z$scale_sing} scale.")

  # Special case for Kahnawake
  if (z$var_type == "kah_na") out <- cc_t(r = r, 
    "<strong>Kahnawake Mohawk Territory</strong>",
    "<p>Statistics Canada does not gather the same ",
    "data for indigenous reserves in the Census as it does ",
    "for other jurisdictions, so we cannot display findings here.")
  
  # Special case for Kanestake
  if (z$var_type == "kan_na") out <- cc_t(r = r, 
    "<strong>Kanehsatà:ke</strong>",
    "<p>Statistics Canada does not gather the same ",
    "data for indigenous reserves in the Census as it does ",
    "for other jurisdictions, so we cannot display findings here.")
  
  # Univariate, NA selection
  if (z$var_type == "uni_na") out <- cc_t(r = r, 
    "{z$place_name} has no data available on {z$exp_left}.")
  
  # Bivariate, NA selection
  if (grepl("^bi_na", z$var_type)) out <- cc_t(r = r, 
    "{z$place_name} has no data available on {z$exp_left} and ",
    "{z$exp_right}.")
  
  # Univariate multi-date, NA selection
  if (z$var_type == "uni_na_delta") out <- cc_t(r = r, 
    "{z$place_name} has no data available on the change in {z$exp_left} ",
    "between {z$start_date_left} - {z$end_date_left}.")
  
  

  ## Univariate single-date cases ----------------------------------------------
  
  # Univariate, quantitative, no selection
  if (z$var_type == "uni_quant_all") out <- cc_t(r = r, 
    "At the {z$scale_sing} scale, {z$exp_left} varies from ",
    "{z$min_val} to {z$max_val}, with an average value of {z$mean_val} ",
    "and a median value of {z$median_val}. ",
    "Two thirds of {z$scale_plural} have a score between {z$quant_low} ",
    "and {z$quant_high}.")
  
  
  # Univariate, quantitative, valid selection
  if (z$var_type == "uni_quant_select") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>{z$place_name} has a population of {z$pop} and a ", 
    "'{z$title_left}' score ({z$exp_left}) of {z$val_left}, which is ", 
    "{z$larger} the territory-wide median of {z$median_val}.",
    "<p>{z$place_name} has a {z$high} relative score for this ", 
    "indicator, with '{z$exp_left}' higher than ", 
    "{z$percentile} of {z$scale_plural} {z$geo}.")
  
  # Univariate, qualitative, no selection
  if (z$var_type == "uni_qual_all") out <- cc_t(r = r, 
    "At the {z$scale_sing} scale, {z$exp_left} varies from ",
    "'{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$scale_plural} ",
    "({z$mode_prop}) have a value of '{z$mode_val}', while ",
    "{z$mode_prop_2} have a value of '{z$mode_val_2}'.")
  
  # Univariate, qualitative, valid selection
  if (z$var_type == "uni_qual_select") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>{z$place_name} has a population of {z$pop} and a ",
    "'{z$title_left}' value of '{z$val_left}', which is shared by ",
    "{z$other_with_val} of {z$scale_plural} {z$geo}.")
  
  
  ## Univariate multi-date cases -------------------------------------------
  
  # Univariate, quantitative, no selection
  if (z$var_type == "uni_quant_all_delta") out <- cc_t(r = r, 
    "At the {z$scale_sing} scale, the change in {z$exp_left} ",
    "between {z$start_date_left} and {z$end_date_left} varied from ",
    "{z$min_val} to {z$max_val}, with an average change of {z$mean_val} ",
    "and a median change of {z$median_val}. ",
    "Two thirds of {z$scale_plural} saw a change between {z$quant_low} ",
    "and {z$quant_high}.")
  
  # Univariate, quantitative, valid selection
  sentence <- \(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  if (z$var_type == "uni_quant_select_delta") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>{sentence(z$exp_left)} in {z$place_name} ",
    "{z$increase} by {sub('-', '', z$val_left)} between ",
    "{z$start_date_left} and {z$end_date_left}, which is {z$larger} ",
    "the territory-wide median change of {z$median_val}.",
    "<p>{z$place_name} had a {z$high} relative change for this ",
    "indicator, with a change in {z$exp_left} larger than ",
    "{z$percentile} of {z$scale_plural} {z$geo}.")
  
  # Univariate, qualitative, no selection
  if (z$var_type == "uni_qual_all_delta") out <- cc_t(r = r, 
    "TKTK At the {z$scale_sing} scale, {z$exp_left} varies from ",
    "'{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$scale_plural} ",
    "({z$mode_prop}) have a value of '{z$mode_val}', while ",
    "{z$mode_prop_2} have a value of '{z$mode_val_2}'.")
  
  # Univariate, qualitative, valid selection
  if (z$var_type == "uni_qual_select_delta") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>TKTK {z$place_name} has a population of {z$pop} and a ",
    "'{z$title_left}' value of '{z$val_left}', which is shared by ",
    "{z$other_with_val} of {z$scale_plural} {z$geo}.")
  
  
  ## Bivariate cases -----------------------------------------------------------
  
  # Bivariate, quantitative, no selection
  if (z$var_type == "bi_quantxy_all") {
    # If correlation is close to zero
    if (z$correlation < 0.05 && z$correlation > -0.05) {
      out <- cc_t(r = r, 
        "<p>'{z$title_left}' has effectively no correlation ",
        "({z$corr_disp}) with '{z$title_right}' at the {z$scale_sing} ",
        "scale.",
        "<p>This means that, at the {z$scale_sing} scale, ",
        "there is no relationship between the two variables.")
    } else {
      out <- paste0(
        if (z$strong == cc_t(r = r, "strong")) 
          cc_t(r = r, "<p><b>STRONG CORRELATION</b></p>"),
        cc_t(r = r, "<p>'{z$title_left}' has a {z$strong} {z$pos} ",
        "correlation ({z$corr_disp}) with '{z$title_right}' at ",
        "the {z$scale_sing} scale.",
        "<p>This means that, in general, {z$scale_plural} with a higher ",
        "{sub('^the ', '', z$exp_right)} tend to have a {z$higher} ",
        "{sub('^the ', '', z$exp_left)}, {z$high_low_disclaimer}."))
    }
  }
  
  # Bivariate, quantitative, valid selection
  if (z$var_type == "bi_quantxy_select") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>{z$place_name} has a population of {z$pop}, ",
    "a '{z$title_left}' value of {z$val_left}, ",
    "and a '{z$title_right}' value of {z$val_right}. ",
    "<p>These two scores are {z$relative_position}, in relative ",
    "terms. {z$place_name} has {sub('^the', 'a', z$exp_left)} higher ",
    "than {z$perc_left} of {z$scale_plural} and ",
    "{sub('^the', 'a', z$exp_right)} higher than {z$perc_right} ",
    "of {z$scale_plural} {z$geo}.")
  
  # Bivariate, qualitative x, quantitative y, no selection
  if (z$var_type == "bi_quanty_all") {
    # If correlation is close to zero
    if (z$correlation < 0.05 && z$correlation > -0.05) {
      out <- cc_t(r = r, 
        "<p>'{z$title_left}' has effectively no correlation ",
        "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' at the ", 
        "{z$scale_sing} scale.",
        "<p>This means that, at the {z$scale_sing} scale, ",
        "there is no relationship between the two variables.")
    } else {
      out <- paste0(
        if (z$strong == cc_t(r = r, "strong")) 
          cc_t(r = r, "<p><b>STRONG CORRELATION</b></p>"),
        cc_t(r = r, "<p>'{z$title_left}' has a {z$strong} {z$pos} correlation ",
        "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' ",
        "at the {z$scale_sing} scale.",
        "<p>This means that, in general, {z$scale_plural} with a higher ",
        "{sub('^the ', '', z$exp_right)} tend to have a {z$higher} ",
        "{sub('^the ', '', z$exp_left)}, {z$high_low_disclaimer}."))
    }
  }
  
  # Bivariate, qualitative x, quantitative y, valid selection
  if (z$var_type == "bi_quanty_select") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>{z$place_name} has a population of {z$pop}, ",
    "a '{z$title_left}' value of '{z$val_left}', and a ",
    "'{z$title_right}' value of {z$val_right}. ",
    "<p>{z$place_name} has {sub('^the', 'a', z$exp_right)} ",
    "higher than {z$perc} of other {z$scale_plural} with ",
    "{sub('^the', 'a', z$exp_left)} of '{z$val_left}' {z$out}.")
  
  # Bivariate, quantitative x, qualitative y, no selection
  if (z$var_type == "bi_quantx_all") {
    # If correlation is close to zero
    if (z$correlation < 0.05 && z$correlation > -0.05) {
      out <- cc_t(r = r, 
        "<p>'{z$title_left}' has effectively no correlation ",
        "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' at the ", 
        "{z$scale_sing} scale.",
        "<p>This means that, at the {z$scale_sing} scale, ",
        "there is no relationship between the two variables.")
    } else {
      out <- paste0(
        if (z$strong == cc_t(r = r, "strong")) 
          cc_t(r = r, "<p><b>STRONG CORRELATION</b></p>"),
        cc_t(r = r, "<p>'{z$title_left}' has a {z$strong} {z$pos} correlation ",
        "(Spearman's rho: {z$corr_disp}) with '{z$title_right}' ",
        "at the {z$scale_sing} scale.",
        "<p>This means that, in general, {z$scale_plural} with a higher ",
        "{sub('^the ', '', z$exp_right)} tend to have a {z$higher} ",
        "{sub('^the ', '', z$exp_left)}, {z$high_low_disclaimer}."))
    }
  }
  
  # Bivariate, quantitative x, qualitative y, valid selection
  if (z$var_type == "bi_quantx_select") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>{z$place_name} has a population of {z$pop}, ",
    "a {z$title_left} value of '{z$val_left}', and a ",
    "'{z$title_right}' value of '{z$val_right}'. ",
    "<p>{z$place_name} has {sub('^the', 'a', z$exp_left)} ",
    "higher than {z$perc} of other {z$scale_plural} with ",
    "{sub('^the', 'a', z$exp_right)} of '{z$val_right}' {z$out}.")
  
  
  ## Bivariate multi-date cases ------------------------------------------------
  
  # Bivariate, quantitative, no selection
  if (z$var_type == "bi_quantxy_all_delta") {
    # If correlation is close to zero
    if (abs(z$correlation) < 0.05) {
      out <- cc_t(r = r, 
        "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
        "'{z$title_left}' had effectively no correlation ({z$corr_disp}) ", 
        "with the change in '{z$title_right}' at the {z$scale_sing} scale.",
        "<p>This means that, at the {z$scale_sing} scale, there was no ",
        "relationship between the change in the two variables.")
    } else {
      out <- paste0(
        if (z$strong == cc_t(r = r, "strong")) 
          cc_t(r = r, "<p><b>STRONG CORRELATION</b></p>"),
        cc_t(r = r, "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
        "'{z$title_left}' had a {z$strong} {z$pos} ",
        "correlation ({z$corr_disp}) with the change in '{z$title_right}' ", 
        "at the {z$scale_sing} scale.",
        "<p>This means that, in general, {z$scale_plural} with a higher ",
        "change in {z$exp_left} tended to have a {z$higher} change in ",
        "{z$exp_right}, {z$high_low_disclaimer}."))
    }
  }
  
  # Bivariate, quantitative, valid selection
  if (z$var_type == "bi_quantxy_select_delta") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>From {z$start_date_left} to {z$end_date_left}, {z$place_name} had ",
    "a change in its '{z$title_left}' value of {z$val_left}, ",
    "and a change in its '{z$title_right}' value of {z$val_right}. ",
    "<p>These two scores are {z$relative_position}, in relative ",
    "terms. {z$place_name} had a change in {z$exp_left} higher ",
    "than {z$perc_left} of {z$scale_plural} and ",
    "a change in {z$exp_right} higher than {z$perc_right} ",
    "of {z$scale_plural} {z$geo}.")
  
  # Bivariate, qualitative x, quantitative y, no selection
  if (z$var_type == "bi_quanty_all_delta") {
    # If correlation is close to zero
    if (abs(z$correlation) < 0.05) {
      out <- cc_t(r = r, 
        "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
        "'{z$title_left}' had effectively no correlation ", 
        "(Spearman's rho: {z$corr_disp}) ", 
        "with the change in '{z$title_right}' at the {z$scale_sing} scale.",
        "<p>This means that, at the {z$scale_sing} scale, there was no ",
        "relationship between the change in the two variables.")
    } else {
      out <- paste0(
        if (z$strong == cc_t(r = r, "strong")) 
          cc_t(r = r, "<p><b>STRONG CORRELATION</b></p>"),
        cc_t(r = r, "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
        "'{z$title_left}' had a {z$strong} {z$pos} ",
        "correlation (Spearman's rho: {z$corr_disp}) with the change in ", 
        "'{z$title_right}' at the {z$scale_sing} scale.",
        "<p>This means that, in general, {z$scale_plural} with a higher ",
        "change in {z$exp_left} tended to have a {z$higher} change in ",
        "{z$exp_right}, {z$high_low_disclaimer}."))
    }
  }
  
  # Bivariate, qualitative x, quantitative y, valid selection
  if (z$var_type == "bi_quanty_select_delta") out <- cc_t(r = r, 
    "<strong>{z$place_heading}</strong>",
    "<p>TKTK {z$place_name} has a population of {z$pop}, ",
    "a '{z$title_left}' value of '{z$val_left}', and a ",
    "'{z$title_right}' value of {z$val_right}. ",
    "<p>{z$place_name} has {sub('^the', 'a', z$exp_right)} ",
    "higher than {z$perc} of other {z$scale_plural} with ",
    "{sub('^the', 'a', z$exp_left)} of '{z$val_left}' {z$out}.")
  
  
  ## Bivariate non-matching multi-date cases -----------------------------------
  
  # Bivariate, quantitative, no selection
  if (z$var_type == "bi_quantxy_all_deltax") {
    # If correlation is close to zero
    if (abs(z$correlation) < 0.05) {
      out <- cc_t(r = r, 
                           "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
                           "'{z$title_left}' had effectively no correlation ({z$corr_disp}) ", 
                           "with {z$date_right}'s '{z$title_right}' at the {z$scale_sing} scale.",
                           "<p>This means that, at the {z$scale_sing} scale, there was no ",
                           "relationship between the change in {z$exp_left} with {z$exp_right}.")
    } else {
      out <- paste0(
        if (z$strong == cc_t(r = r, "strong")) 
          cc_t(r = r, "<p><b>STRONG CORRELATION</b></p>"),
        cc_t(r = r, 
                      "<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
                      "'{z$title_left}' had a {z$strong} {z$pos} ",
                      "correlation ({z$corr_disp}) with {z$date_right}'s '{z$title_right}' ", 
                      "at the {z$scale_sing} scale.",
                      "<p>This means that, in general, {z$scale_plural} with a higher ",
                      "change in {z$exp_left} tended to have a {z$higher} value in ",
                      "{z$exp_right}, {z$high_low_disclaimer}."))
    }
  }
  
  # Bivariate, quantitative, valid selection
  if (z$var_type == "bi_quantxy_select_deltax") 
    out <- cc_t(r = r, 
                         "<strong>{z$place_heading}</strong>",
                         "<p>From {z$start_date_left} to {z$end_date_left}, {z$place_name} had ",
                         "a change in its '{z$title_left}' value of {z$val_left}. ",
                         "In {z$date_right}, '{z$title_right}' had a value of {z$val_right}. ",
                         "<p>These two scores are {z$relative_position}, in relative ",
                         "terms. {z$place_name} had a change in {z$exp_left} higher ",
                         "than {z$perc_left} of {z$scale_plural}. It also had ",
                         "{sub('^the', 'a', z$exp_right)} higher than {z$perc_right} ",
                         "of {z$scale_plural} {z$geo}.")
  
  
  
  ## Special cases -------------------------------------------------------------
  
  if (z$var_type == "date_all") {
    
    # If correlation is close to zero
    if (abs(z$correlation) < 0.05) {
      out <- cc_t(r = r, 
        "<p>During {z$date_left}, {z$exp_left} ",
        "averaged {z$mean_val} per day. ",
        "The maximum value was {z$max_val} on {z$max_date}, and the ",
        "minimum value was {z$min_val} on {z$min_date}. ",
        "There was no growth trend during this time period.")
    } else {
      out <- cc_t(r = r, 
        "<p>During {z$date_left}, {z$exp_left} ",
        "averaged {z$mean_val} per day. ",
        "The maximum value was {z$max_val} on {z$max_date}, and the ",
        "minimum value was {z$min_val} on {z$min_date}. ",
        "There was a {z$strong} {z$pos} growth trend during this time ",
        "period, with {z$exp_left} {z$coef_increasing} an average of ",
        "{z$coef} each day.")
    }
  }
  
  
  ## Append date information ---------------------------------------------------
  
  date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
  date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
  if (length(var_right) == 1 && var_right == " ") date_right <- date_left
  if (length(date_left) > 1 || is.na(date_left) || is.na(date_right)) 
    date_left <- "NA"
  if (length(date_right) > 1) date_right <- "NA"
  
  if (date_left == date_right && nchar(date_left) == 4 &&
      !grepl("_na", z$var_type)) {
    out <- paste(out, cc_t(r = r, "<i>(Data from {date_left}.)</i>"))
  }
  if (date_left != date_right && nchar(date_left) == 4 &&
      !grepl("_na", z$var_type)) {
    out <- paste(out, cc_t(r = r, 
                                    "<p><i>Data from {date_left} for '{z$title_left}' and ",
                                    "{date_right} for '{z$title_right}'.</i></p>"))
  }
  
  ## Return output -------------------------------------------------------------
  
  out <- HTML(out)
  
  return(out)

}
