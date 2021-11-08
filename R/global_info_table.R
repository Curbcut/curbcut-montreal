#' @return A named list with all the data components necessary to power the
#' info_table module.

make_info_table_data <- function(id, x, var_type, var_left, var_right, select, 
                                 zoom, var_left_label, var_right_label, 
                                 build_str_as_DA) {
  
  ## Get modified df for building/street ---------------------------------------
  
  if (!zoom() %in% c("building", "street")) build_str_as_DA <- FALSE
  
  if (build_str_as_DA) {
    tb <- data_server(id = "info_table",
                      var_left = var_left,
                      var_right = var_right,
                      df = reactive("DA"))
    dat <- tb()
    select_id <- (filter(building, ID == select()))$DAUID
    if (length(select_id) == 0) select_id <- NA
    
  } else {
    dat <- x()
    select_id <- select()
  }
  
  ## Titles and explanations ---------------------------------------------------
  
  var_left <- str_remove(var_left(), "_\\d{4}$")
  var_right <- str_remove(var_right(), "_\\d{4}$")
  
  title_left <- sus_translate(var_exp[var_exp$var_code == var_left,]$var_name)
  if (var_right != " ") title_right <- 
    sus_translate(var_exp[var_exp$var_code == var_right,]$var_name)
  
  exp_left <- sus_translate(var_exp[var_exp$var_code == var_left,]$explanation)
  exp_right <- 
    sus_translate(var_exp[var_exp$var_code == var_right,]$explanation)
  if (length(exp_left) == 0) warning("No var_exp: ", var_left, call. = FALSE)
  if (var_right != " " && length(exp_right) == 0) warning(
    "No var_exp: ", var_right, call. = FALSE)
  
  
  ## Selections ----------------------------------------------------------------
  
  selection_name <- filter(x(), ID == select())
  selection <- filter(dat, ID == select_id)
  active_left <- nrow(filter(selection, !is.na(left_var_q3)))
  active_right <- active_left
  if (var_right != " ") active_right <- 
    nrow(filter(selection, !is.na(left_var_q3), !is.na(right_var)))

  
  ## Special case for Kahnawake and Kanesatake ---------------------------------
  
  if (nrow(selection) > 0 && selection$ID %in% 
      c("2467802", "4620832.00", 24670285) && 
      active_left == 0 && active_right == 0) var_type <- reactive("kah_na")
  
  if (nrow(selection) > 0 && selection$ID %in% c(
    "2472802", "4620732.03", "24720184", "24720186", "24720187", "24720188", 
    "24720190", "24720191", "24720192", "24720193", "24720194", "24720195", 
    "24720196", "24720200", "24720201") && active_left == 0 && 
    active_right == 0) var_type <- reactive("kan_na")
  

  ## Scale ---------------------------------------------------------------------
  
  scale_singular <- switch(zoom(), 
                           "borough" = sus_translate("borough/city"),
                           "CT" = sus_translate("census tract"),
                           "DA" = sus_translate("dissemination area"),
                           "grid" = "250-m",
                           "building" = if (build_str_as_DA) 
                             sus_translate("dissemination area") else
                               sus_translation("building"),
                           "street" = if (build_str_as_DA) 
                             sus_translate("dissemination area") else
                               sus_translation("street"))
  
  scale_plural <- case_when(
    scale_singular == sus_translate("borough/city") ~ 
      sus_translate("boroughs or cities"),
    scale_singular == sus_translate("census tract") ~ 
      sus_translate("census tracts"),
    scale_singular == sus_translate("dissemination area") ~ 
      sus_translate("dissemination areas"),
    scale_singular == sus_translate("250-m") ~ sus_translate("areas"),
    scale_singular == sus_translate("building") ~ sus_translate("buildings"),
    scale_singular == sus_translate("street") ~ sus_translate("streets"))

  
  ## Place names ---------------------------------------------------------------
  
  place_name <- case_when(
    zoom() %in% c("building", "street") & build_str_as_DA ~
      glue(sus_translate(paste0(
        "The dissemination area around {selection_name$name}"))),
    scale_singular == sus_translate("building") ~
      glue("{selection_name$name}"),
    scale_singular == sus_translate("street") ~
      glue("{selection_name$name}"),
    scale_singular == sus_translate("borough/city") ~
      glue("{selection_name$name}"),
    scale_singular == sus_translate("census tract") ~
      glue(sus_translate(paste0("Census tract {selection_name$name}"))),
    scale_singular == sus_translate("dissemination area") ~
      glue(sus_translate(paste0("Dissemination area {selection_name$name}"))),
    scale_singular == sus_translate("250-m") ~
      glue(sus_translate(paste0("The area around {selection_name$name}"))))
  
  if (grepl("select", var_type())) {
    if (zoom() == "borough") selection_name$name_2 <- 
        sus_translate(glue("{selection_name$name_2}"))
    
    place_heading <- case_when(
      zoom() %in% c("building", "street") & build_str_as_DA ~
        glue(sus_translate(selection_name$name)),
      scale_singular == sus_translate("borough/city") ~
        glue(sus_translate(paste0("{selection_name$name_2} of {place_name}"))), 
      scale_singular == sus_translate("250-m") ~ 
        sus_translate(selection_name$name),
      TRUE ~ glue("{place_name} ({selection_name$name_2})"))
  }
  
  
  ## Descriptive statistics for left_var_q3 ------------------------------------
  
  vec_left <-
    dat %>%
    filter(!is.na(left_var_q3), !is.na(left_var)) %>%
    pull(left_var) %>% 
    na.omit()
  
  if (grepl("quant_", var_type())) {
    min_val <- round(min(vec_left), 2)
    max_val <- round(max(vec_left), 2)
    mean_val <- round(mean(vec_left), 2)
    median_val <- round(median(vec_left), 2)
    sd_val <- sd(vec_left)
    quant_low <- round(quantile(vec_left, c(1 / 3, 2 / 3))[1], 2)
    quant_high <- round(quantile(vec_left, c(1 / 3, 2 / 3))[2], 2)
  }
  
  
  ## Descriptive statistics for univariate quant selection ---------------------
  
  if (var_type() == "uni_quant_select") {
    
    poly_value <- selection$left_var
    quintile <- quantile(vec_left, c(0.2, 0.4, 0.6, 0.8))

    larger_smaller <- case_when(
      poly_value >= quintile[4] ~ sus_translate("much larger than"),
      poly_value >= quintile[3] ~ sus_translate("larger than"),
      poly_value >= quintile[2] ~ sus_translate("almost the same as"),
      poly_value >= quintile[1] ~ sus_translate("smaller than"),
      TRUE ~ sus_translate("much smaller than"))

    poor_strong <- case_when(
      stringr::str_detect(larger_smaller, sus_translate("larger")) ~
        sus_translate("high"),
      stringr::str_detect(larger_smaller, sus_translate("smaller")) ~
        sus_translate("low"),
      TRUE ~ sus_translate("moderate"))

    percentile <-
      round(length(vec_left[vec_left <= poly_value]) / length(vec_left) * 100)
    
    }
  
  
  ## Descriptive statistics for univariate qual --------------------------------
  
  if (grepl("qual_", var_type())) {
    
    min_val <- as.character(unique(round(min(vec_left, na.rm = TRUE))))
    min_val <- tolower(var_left_label[names(var_left_label) == min_val])
    max_val <- as.character(unique(round(max(vec_left, na.rm = TRUE))))
    max_val <- tolower(var_left_label[names(var_left_label) == max_val])
    mode_val <- table(round(vec_left)) %>%
      sort(decreasing = TRUE) %>%
      `[`(1) %>%
      {var_left_label[names(var_left_label) == names(.)]} %>%
      tolower()
    mode_val_2 <- table(round(vec_left)) %>%
      sort(decreasing = TRUE) %>%
      `[`(2) %>%
      {var_left_label[names(var_left_label) == names(.)]} %>%
      tolower()
    mode_prop <- table(round(vec_left)) %>%
      sort(decreasing = TRUE) %>%
      {.[1] / sum(.)} %>%
      scales::percent(0.1)
    mode_prop_2 <- table(round(vec_left)) %>%
      sort(decreasing = TRUE) %>%
      {.[2] / sum(.)} %>%
      scales::percent(0.1)
    
  }
  
  
  ## Descriptive statistics for univariate qual selection ----------------------
  
  if (var_type() == "uni_qual_select") {
    poly_value <- tolower(var_left_label[names(var_left_label) == 
                                           round(selection$left_var)])
    other_with_value <- 
      round(mean(dat$left_var == selection$left_var, na.rm = TRUE) * 100, 1)
    
  }
  
  ## Descriptive statistics for bivariate quantxy ------------------------------
  
  if (grepl("bi_quantxy", var_type())) {
    
      correlation <- 
        round(cor(dat$left_var, dat$right_var, use = "complete.obs"), 2)
      pos_neg <- if_else(correlation > 0, sus_translate("positive"), 
                         sus_translate("negative"))
      strong_weak <- case_when(
        abs(correlation) > 0.6 ~ sus_translate("strong"),
        abs(correlation) > 0.3 ~ sus_translate("moderate"),
        TRUE ~ "weak")
      higher_lower <- if_else(pos_neg == sus_translate("positive"),
                              sus_translate("higher"),
                              sus_translate("lower"))
      high_low_disclaimer <- case_when(
        strong_weak == sus_translate("strong") ~
          sus_translate("with only a few exceptions"),
        strong_weak == sus_translate("moderate") ~
          sus_translate("although with some exceptions"),
        strong_weak == sus_translate("weak") ~
          sus_translate("although with many exceptions"))
    
  }
  
  
  ## Descriptive statistics for bivariate quant selection ----------------------
  
  if (var_type() == "bi_quantxy_select") {
   
    vec_1 <- dat$left_var
    vec_2 <- dat$right_var
    
    percentile_left <- round(length(vec_1[vec_1 <= selection$left_var]) / 
                               length(vec_1) * 100)
    percentile_right <- round(length(vec_2[vec_2 <= selection$right_var]) /
                                length(vec_2) * 100)
    
    relative_position <- case_when(
      abs(percentile_left - percentile_right) > 50 ~
        sus_translate("dramatically different"),
      abs(percentile_left - percentile_right) > 30 ~
        sus_translate("substantially different"),
      abs(percentile_left - percentile_right) > 10 ~
        sus_translate("considerably different"),
      TRUE ~ sus_translate("similar")
    )
    
  }
  
  ## Descriptive statistics for quant/qual comparison --------------------------
  
  if (grepl("bi_quanty_|bi_quantx_", var_type())) {
    
    correlation <- 
      round(cor(dat$left_var, dat$right_var, use = "complete.obs",
                method = "spearman"), 2)
    pos_neg <- if_else(correlation > 0, sus_translate("positive"), 
                       sus_translate("negative"))
    strong_weak <- case_when(
      abs(correlation) > 0.6 ~ sus_translate("strong"),
      abs(correlation) > 0.3 ~ sus_translate("moderate"),
      TRUE ~ "weak")
    higher_lower <- if_else(pos_neg == sus_translate("positive"),
                            sus_translate("higher"),
                            sus_translate("lower"))
    high_low_disclaimer <- case_when(
      strong_weak == sus_translate("strong") ~
        sus_translate("with only a few exceptions"),
      strong_weak == sus_translate("moderate") ~
        sus_translate("although with some exceptions"),
      strong_weak == sus_translate("weak") ~
        sus_translate("although with many exceptions"))
    
  }
  
  
  ## Return output -------------------------------------------------------------
  
  out <- list(
    title_left = if (exists("title_left")) title_left else NULL,
    title_right = if (exists("title_right")) title_right else NULL,
    exp_left = if (exists("exp_left")) exp_left else NULL,
    exp_right = if (exists("exp_right")) exp_right else NULL,
    selection = if (exists("selection")) selection else NULL,
    var_type = var_type(),
    scale_singular = if (exists("scale_singular")) scale_singular else NULL,
    scale_plural = if (exists("scale_plural")) scale_plural else NULL,
    place_name = if (exists("place_name")) place_name else NULL,
    place_heading = if (exists("place_heading")) place_heading else NULL,
    min_val = if (exists("min_val")) min_val else NULL,
    max_val = if (exists("max_val")) max_val else NULL,
    mean_val = if (exists("mean_val")) mean_val else NULL,
    median_val = if (exists("median_val")) median_val else NULL,
    sd_val = if (exists("sd_val")) sd_val else NULL,
    quant_low = if (exists("quant_low")) quant_low else NULL,
    quant_high = if (exists("quant_high")) quant_high else NULL,
    poly_value = if (exists("poly_value")) poly_value else NULL,
    other_with_value = if (exists("other_with_value")) 
      other_with_value else NULL,
    quintile = if (exists("quintile")) quintile else NULL,
    larger_smaller = if (exists("larger_smaller")) larger_smaller else NULL,
    poor_strong = if (exists("poor_strong")) poor_strong else NULL,
    percentile = if (exists("percentile")) percentile else NULL,
    mode_val = if (exists("mode_val")) mode_val else NULL,
    mode_val_2 = if (exists("mode_val_2")) mode_val_2 else NULL,
    mode_prop = if (exists("mode_prop")) mode_prop else NULL,
    mode_prop_2 = if (exists("mode_prop_2")) mode_prop_2 else NULL,
    correlation = if (exists("correlation")) correlation else NULL,
    pos_neg = if (exists("pos_neg")) pos_neg else NULL,
    strong_weak = if (exists("strong_weak")) strong_weak else NULL,
    higher_lower = if (exists("higher_lower")) higher_lower else NULL,
    high_low_disclaimer = if (exists("high_low_disclaimer")) 
      high_low_disclaimer else NULL,
    percentile_left = if (exists("percentile_left")) percentile_left else NULL,
    percentile_right = if (exists("percentile_right")) 
      percentile_right else NULL,
    relative_position = if (exists("relative_position")) 
      relative_position else NULL
    )
  
  out
  
}
