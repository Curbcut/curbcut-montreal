#' @return A named list with all the data components necessary to power the
#' info_table module.

make_info_table_data <- function(id, x, var_left, var_right, select, zoom, 
                                 var_left_title, var_right_title,
                                 var_left_label, var_right_label) {
  
  ## Titles and explanations ---------------------------------------------------
  
  title_left <- sus_translate(var_left_title())
  title_right <- " "
  if (!is.null(var_right_title)) title_right <- sus_translate(var_right_title())
  exp_left <- var_exp[var_exp$var_code == var_left(),]$explanation
  exp_right <- var_exp[var_exp$var_code == var_right(),]$explanation
  if (length(exp_left) == 0) warning("No var_exp: ", var_left(), call. = FALSE)
  if (var_right() != " " && length(exp_right) == 0) warning(
    "No var_exp: ", var_right(), call. = FALSE)
  
  
  ## Selections ----------------------------------------------------------------
  
  selection <- x() %>% filter(ID == select())
  active_left <- nrow(filter(selection, !is.na(left_var)))
  active_right <- active_left
  if (var_right() != " ") active_right <- 
    nrow(filter(selection, !is.na(left_var), !is.na(right_var)))
  
  
  ## Decide on table type ------------------------------------------------------
  
  comp_type <- case_when(
    var_right() == " " ~ "uni",
    TRUE ~ "bi")
  
  var_type <- case_when(
    comp_type == "uni" & is.null(var_left_label) ~ "quant",
    comp_type == "bi" & is.null(var_left_label) & is.null(var_right_label) ~ 
      "quant_xy",
    comp_type == "bi" & is.null(var_left_label) & !is.null(var_right_label) ~ 
      "quant_x",
    comp_type == "bi" & !is.null(var_left_label) & is.null(var_right_label) ~ 
      "quant_y",
    TRUE ~ "qual")
  
  select_type <- case_when(is.na(select()) ~ "all", 
                           comp_type == "uni" & active_left == 0 ~ "na",
                           active_right == 0 ~ "na",
                           TRUE ~ "select")
  
  table_type <- paste(comp_type, var_type, select_type, sep = "_")
  if (select_type == "na") table_type <- paste0(comp_type, "_na")

  # Special case for Kahnawake
  if (selection$ID %in% c("2467802", "4620832.00", 24670285) && 
      select_type == "na") table_type <- "kah_na"
  
  # Special case for Kanesatake
  if (selection$ID %in% c(
    "2472802", "4620732.03", "24720184", "24720186", "24720187", "24720188", 
    "24720190", "24720191", "24720192", "24720193", "24720194", "24720195", 
    "24720196", "24720200", "24720201") && select_type == "na") table_type <- 
    "kan_na"
  

  ## Scale ---------------------------------------------------------------------
  
  scale_singular <- switch(zoom(), 
                           "borough" = sus_translate("borough/city"),
                           "CT" = sus_translate("census tract"),
                           "DA" = sus_translate("dissemination area"),
                           "grid" = "250-m")
  
  scale_plural <- case_when(
    scale_singular == sus_translate("borough/city") ~ 
      sus_translate("boroughs or cities"),
    scale_singular == sus_translate("census tract") ~ 
      sus_translate("census tracts"),
    scale_singular == sus_translate("dissemination area") ~ 
      sus_translate("dissemination areas"),
    scale_singular == sus_translate("250-m") ~ sus_translate("areas"))

  
  ## Place names ---------------------------------------------------------------
  
  place_name <- case_when(
    scale_singular == sus_translate("borough/city") ~
      glue("{selection$name}"),
    scale_singular == sus_translate("census tract") ~
      glue(sus_translate(paste0("Census tract {selection$name}"))),
    scale_singular == sus_translate("dissemination area") ~
      glue(sus_translate(paste0("Dissemination area {selection$name}"))),
    scale_singular == sus_translate("250-m") ~
      glue(sus_translate(paste0("The area around {selection$name}"))))
  
  if (select_type == "select") {
    if (zoom() == "borough") selection$name_2 <- 
        sus_translate(glue("{selection$name_2}"))
    
    place_heading <- case_when(
      scale_singular == sus_translate("borough/city") ~
        glue(sus_translate(paste0("{selection$name_2} of {place_name}"))), 
      scale_singular == sus_translate("250-m") ~ sus_translate(selection$name),
      TRUE ~ glue("{place_name} ({selection$name_2})"))
  }
  
  
  ## Descriptive statistics for left_var ---------------------------------------
  
  vec_left <-
    x() %>%
    filter(!is.na(left_var), !is.na(left_var_full)) %>%
    pull(left_var_full) %>% 
    na.omit()
  
  if (var_type == "quant") {
    min_val <- round(min(vec_left), 2)
    max_val <- round(max(vec_left), 2)
    mean_val <- round(mean(vec_left), 2)
    median_val <- round(median(vec_left), 2)
    sd_val <- sd(vec_left)
    quant_low <- round(quantile(vec_left, c(1 / 3, 2 / 3))[1], 2)
    quant_high <- round(quantile(vec_left, c(1 / 3, 2 / 3))[2], 2)
  }
  
  
  ## Descriptive statistics for univariate quant selection ---------------------
  
  if (table_type == "uni_quant_select") {
    
    poly_value <- selection$left_var_full
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
  
  if (var_type == "qual") {
    
    min_val <- as.character(unique(min(vec_left, na.rm = TRUE)))
    min_val <- tolower(var_left_label[names(var_left_label) == min_val])
    max_val <- as.character(unique(max(vec_left, na.rm = TRUE)))
    max_val <- tolower(var_left_label[names(var_left_label) == max_val])
    mode_val <- table(vec_left) %>%
      sort(decreasing = TRUE) %>%
      `[`(1) %>%
      {var_left_label[names(var_left_label) == names(.)]} %>%
      tolower()
    mode_val_2 <- table(vec_left) %>%
      sort(decreasing = TRUE) %>%
      `[`(2) %>%
      {var_left_label[names(var_left_label) == names(.)]} %>%
      tolower()
    mode_prop <- table(vec_left) %>%
      sort(decreasing = TRUE) %>%
      {.[1] / sum(.)} %>%
      scales::percent(0.1)
    mode_prop_2 <- table(vec_left) %>%
      sort(decreasing = TRUE) %>%
      {.[2] / sum(.)} %>%
      scales::percent(0.1)
    
  }
  
  out <- list(
    title_left = if (exists("title_left")) title_left else NULL,
    title_right = if (exists("title_right")) title_right else NULL,
    exp_left = if (exists("exp_left")) exp_left else NULL,
    exp_right = if (exists("exp_right")) exp_right else NULL,
    selection = if (exists("selection")) selection else NULL,
    table_type = if (exists("table_type")) table_type else NULL,
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
    quintile = if (exists("quintile")) quintile else NULL,
    larger_smaller = if (exists("larger_smaller")) larger_smaller else NULL,
    poor_strong = if (exists("poor_strong")) poor_strong else NULL,
    percentile = if (exists("percentile")) percentile else NULL,
    mode_val = if (exists("mode_val")) mode_val else NULL,
    mode_val_2 = if (exists("mode_val_2")) mode_val_2 else NULL,
    mode_prop = if (exists("mode_prop")) mode_prop else NULL,
    mode_prop_2 = if (exists("mode_prop_2")) mode_prop_2 else NULL
    )
  
  out
  
}
