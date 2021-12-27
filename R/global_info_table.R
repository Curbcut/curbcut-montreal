#' @return A named list with all the data components necessary to power the
#' info_table module.

make_info_table_data <- function(id, x, var_type, var_left, var_right, select, 
                                 zoom, var_left_label, var_right_label, 
                                 build_str_as_DA) {
  
  ## Initialize output list ----------------------------------------------------
  
  out <- list(var_type = var_type())

  
  ## Get modified df for building/street ---------------------------------------
  
  if (!zoom() %in% c("building", "street")) build_str_as_DA <- FALSE
  
  # TKTK THIS IS BROKEN FOR TWO DATES!
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
  
  
  ## Handle dates --------------------------------------------------------------
  
  date_left <- str_extract(var_left(), "(?<=_)\\d{4}$")
  
  if (length(var_left()) == 2) {
    out$start_date_left <- date_left[1]
    out$end_date_left <- date_left[2]
  }
  
  if (length(var_right()) == 2) {
    out$start_date_right <- str_extract(var_right(), "(?<=_)\\d{4}$")[1]
    out$end_date_right <- str_extract(var_right(), "(?<=_)\\d{4}$")[2]
  }
  
  
  ## Special case for date-type data -------------------------------------------
  
  if (zoom() == "date") {
    out$var_type <- "date_all"
    dat <- 
      dat |> 
      mutate(name = NA_character_, population = NA_real_)
  }
  
  ## Titles and explanations ---------------------------------------------------
  
  var_left <- unique(str_remove(var_left(), "_\\d{4}$"))
  var_right <- unique(str_remove(var_right(), "_\\d{4}$"))
  var_left_label <- sus_translate(var_left_label)
  var_right_label <- sus_translate(var_right_label)
  
  out$title_left <- 
    sus_translate(variables[variables$var_code == var_left,]$var_title)
  if (var_right != " ") out$title_right <- 
    sus_translate(variables[variables$var_code == var_right,]$var_title)
  
  out$exp_left <- 
    sus_translate(variables[variables$var_code == var_left,]$explanation)
  out$exp_right <- 
    sus_translate(variables[variables$var_code == var_right,]$explanation)
  if (length(out$exp_left) == 0) warning("No exp: ", var_left, call. = FALSE)
  if (var_right != " " && length(out$exp_right) == 0) warning(
    "No exp: ", var_right, call. = FALSE)
  
  
  ## Selections ----------------------------------------------------------------
  
  select_name <- filter(x(), ID == select())
  selection <- filter(dat, ID == select_id)
  out$selection <- selection
  active_left <- nrow(filter(selection, !is.na(left_var_q5)))
  active_right <- active_left
  if (var_right != " ") active_right <- 
    nrow(filter(selection, !is.na(left_var_q3), !is.na(right_var)))
  out$pop <- convert_unit(selection$population)
  val_left <- selection$left_var
  out$val_left <- convert_unit(val_left, var_left)
  if (var_right != " ") {
    val_right <- selection$right_var
    out$val_right <- convert_unit(val_right, var_right)
    if (grepl("_multi", out$var_type)) out$val_right <- 
      convert_unit(val_right, "_prop")
  }
  
  print(out)
  
  
  ## Special case for Kahnawake and Kanesatake ---------------------------------
  
  if (nrow(selection) > 0 && selection$ID %in% 
      c("2467802", "4620832.00", 24670285) && 
      active_left == 0 && active_right == 0) out$var_type <- "kah_na"
  
  if (nrow(selection) > 0 && selection$ID %in% c(
    "2472802", "4620732.03", "24720184", "24720186", "24720187", "24720188", 
    "24720190", "24720191", "24720192", "24720193", "24720194", "24720195", 
    "24720196", "24720200", "24720201") && active_left == 0 && 
    active_right == 0) out$var_type <- "kan_na"
  

  ## Scale ---------------------------------------------------------------------
  
  scale_sing <- switch(
    zoom(),  
    "date" = NA_character_,
    "borough" = sus_translate("borough/city"),
    "CT" = sus_translate("census tract"),
    "DA" = sus_translate("dissemination area"),
    "grid" = "250-m",
    "building" = if (build_str_as_DA) sus_translate("dissemination area") else
      sus_translation("building"),
    "street" = if (build_str_as_DA) sus_translate("dissemination area") else
      sus_translation("street"))
  out$scale_sing <- scale_sing
  
  out$scale_plural <- case_when(
    scale_sing == sus_translate("borough/city") ~ 
      sus_translate("boroughs or cities"),
    scale_sing == sus_translate("census tract") ~ 
      sus_translate("census tracts"),
    scale_sing == sus_translate("dissemination area") ~ 
      sus_translate("dissemination areas"),
    scale_sing == sus_translate("250-m") ~ sus_translate("areas"),
    scale_sing == sus_translate("building") ~ sus_translate("buildings"),
    scale_sing == sus_translate("street") ~ sus_translate("streets"),
    TRUE ~ NA_character_)
  
  
  ## Place names ---------------------------------------------------------------
  
  out$place_name <- case_when(
    zoom() %in% c("building", "street") & build_str_as_DA ~
      glue(sus_translate(paste0(
        "The dissemination area around {select_name$name}"))),
    scale_sing == sus_translate("building") ~
      glue("{select_name$name}"),
    scale_sing == sus_translate("street") ~
      glue("{select_name$name}"),
    scale_sing == sus_translate("borough/city") ~
      glue("{select_name$name}"),
    scale_sing == sus_translate("census tract") ~
      glue(sus_translate(paste0("Census tract {select_name$name}"))),
    scale_sing == sus_translate("dissemination area") ~
      glue(sus_translate(paste0("Dissemination area {select_name$name}"))),
    scale_sing == sus_translate("250-m") ~
      glue(sus_translate(paste0("The area around {select_name$name}"))),
    TRUE ~ NA_character_)
  
  if (grepl("select", out$var_type)) {
    if (zoom() == "borough") select_name$name_2 <- 
        sus_translate(glue("{select_name$name_2}"))
    
    out$place_heading <- case_when(
      zoom() %in% c("building", "street") & build_str_as_DA ~
        glue(sus_translate(select_name$name)),
      scale_sing == sus_translate("borough/city") ~
        glue(sus_translate(paste0("{select_name$name_2} of {out$place_name}"))), 
      scale_sing == sus_translate("250-m") ~ 
        sus_translate(select_name$name),
      TRUE ~ glue("{out$place_name} ({select_name$name_2})"))
  }
  
  
  ## Descriptive statistics for left_var ---------------------------------------
  
  vec_left <-
    dat %>%
    filter(!is.na(left_var_q5), !is.na(left_var)) %>%
    pull(left_var) %>% 
    na.omit()
  
  if (grepl("quant_|date", out$var_type)) {
    out$min_val <- convert_unit(min(vec_left), var_left)
    out$max_val <- convert_unit(max(vec_left), var_left)
    out$mean_val <- convert_unit(mean(vec_left), var_left)
    out$median_val <- convert_unit(median(vec_left), var_left)
    out$sd_val <- convert_unit(sd(vec_left), var_left)
    out$quant_low <- convert_unit(quantile(vec_left, c(1 / 3, 2 / 3))[1], 
                                  var_left)
    out$quant_high <- convert_unit(quantile(vec_left, c(1 / 3, 2 / 3))[2],
                                   var_left)
  }
  

  ## Descriptive statistics for univariate quant selection ---------------------
  
  if (grepl("uni_quant_select", out$var_type)) {
    
    quintile <- quantile(vec_left, c(0.2, 0.4, 0.6, 0.8))

    out$larger <- case_when(
      val_left >= quintile[4] ~ sus_translate("much larger than"),
      val_left >= quintile[3] ~ sus_translate("larger than"),
      val_left >= quintile[2] ~ sus_translate("almost the same as"),
      val_left >= quintile[1] ~ sus_translate("smaller than"),
      TRUE ~ sus_translate("much smaller than"))

    out$high <- case_when(
      stringr::str_detect(out$larger, sus_translate("larger")) ~
        sus_translate("high"),
      stringr::str_detect(out$larger, sus_translate("smaller")) ~
        sus_translate("low"),
      TRUE ~ sus_translate("moderate"))

    out$percentile <- convert_unit(length(vec_left[vec_left <= val_left]) / 
                                     length(vec_left), "_prop")
    
    out$increase <- if (val_left >= 0) sus_translate("increased") else
      sus_translate("decreased")
    
    }
  
  
  ## Descriptive statistics for univariate qual --------------------------------
  
  if (grepl("qual_", out$var_type)) {
    
    qual_tab <- sort(table(round(vec_left)), decreasing = TRUE)
    min_val <- as.character(unique(round(min(vec_left, na.rm = TRUE))))
    out$min_val <- tolower(var_left_label[names(var_left_label) == min_val])
    max_val <- as.character(unique(round(max(vec_left, na.rm = TRUE))))
    out$max_val <- tolower(var_left_label[names(var_left_label) == max_val])
    out$mode_val <- qual_tab[1] %>%
      {var_left_label[names(var_left_label) == names(.)]} %>%
      tolower()
    out$mode_val_2 <- qual_tab[2] %>%
      {var_left_label[names(var_left_label) == names(.)]} %>%
      tolower()
    mode_prop <- qual_tab[1] / sum(qual_tab)
    out$majority <- if (mode_prop > 0.5) "majority" else "plurality"
    out$mode_prop <- convert_unit(mode_prop, "_prop")
    out$mode_prop_2 <- convert_unit(qual_tab[2] / sum(qual_tab), "_prop")
    
  }
  
  
  ## Descriptive statistics for univariate qual selection ----------------------
  
  if (grepl("uni_qual_select", out$var_type)) {
    out$val_left <- 
      tolower(var_left_label[names(var_left_label) == round(val_left)])
    out$other_with_val <- 
      mean(round(dat$left_var) == round(val_left), na.rm = TRUE) |> 
      convert_unit("_prop")
    
  }
  
  ## Descriptive statistics for bivariate quantxy ------------------------------
  
  if (grepl("bi_quantxy|date", out$var_type)) {
    
      corr <- cor(dat$left_var, as.numeric(dat$right_var), use = "complete.obs")
      out$correlation <- corr
      out$corr_disp <- convert_unit(corr)
      out$pos <- if (corr > 0) sus_translate("positive") else 
        sus_translate("negative")
      out$strong <- case_when(
        abs(corr) > 0.6 ~ sus_translate("strong"),
        abs(corr) > 0.3 ~ sus_translate("moderate"),
        TRUE ~ "weak")
      out$higher <- if_else(out$pos == sus_translate("positive"),
                            sus_translate("higher"),
                            sus_translate("lower"))
      out$high_low_disclaimer <- case_when(
        out$strong == sus_translate("strong") ~
          sus_translate("with only a few exceptions"),
        out$strong == sus_translate("moderate") ~ 
          sus_translate("although with some exceptions"),
        out$strong == sus_translate("weak") ~ 
          sus_translate("although with many exceptions"))
    
  }
  
  
  ## Descriptive statistics for bivariate quantxy selection --------------------
  
  if (grepl("bi_quantxy_select", out$var_type)) {
   
    vec_1 <- dat$left_var
    vec_2 <- dat$right_var
    
    perc_left <- length(vec_1[vec_1 <= val_left]) / length(vec_1)
    out$perc_left <- convert_unit(perc_left, "_prop")
    perc_right <- length(vec_2[vec_2 <= val_right]) / length(vec_2)
    out$perc_right <- convert_unit(perc_right, "_prop")
    
    out$relative_position <- case_when(
      abs(perc_left - perc_right) > 0.5 ~ sus_translate("dramatically different"),
      abs(perc_left - perc_right) > 0.3 ~ sus_translate("substantially different"),
      abs(perc_left - perc_right) > 0.1 ~ sus_translate("considerably different"),
      TRUE ~ sus_translate("similar")
    )
    
  }
  
  ## Descriptive statistics for quant/qual comparison --------------------------
  
  if (grepl("bi_quanty_|bi_quantx_", out$var_type)) {
    
    vec_1 <- dat$left_var
    vec_2 <- dat$right_var
    
    corr <- cor(vec_1, vec_2, use = "complete.obs", method = "spearman")
    out$correlation <- corr
    out$corr_disp <- convert_unit(corr)
    out$pos <- if (corr > 0) sus_translate("positive") else 
      sus_translate("negative")
    out$strong <- case_when(
      abs(corr) > 0.6 ~ sus_translate("strong"),
      abs(corr) > 0.3 ~ sus_translate("moderate"),
      TRUE ~ "weak")
    out$higher <- if_else(out$pos == sus_translate("positive"),
                          sus_translate("higher"),
                          sus_translate("lower"))
    out$high_low_disclaimer <- case_when(
      out$strong == sus_translate("strong") ~
        sus_translate("with only a few exceptions"),
      out$strong == sus_translate("moderate") ~ 
        sus_translate("although with some exceptions"),
      out$strong == sus_translate("weak") ~ 
        sus_translate("although with many exceptions"))

  }
  
  if (grepl("bi_quanty_", out$var_type)) {
    
    out$val_left <- 
      tolower(var_left_label[names(var_left_label) == round(val_left)])
    
    out$perc <- 
      mean(val_right >= vec_2[round(dat$left_var) == round(val_left)],
           na.rm = TRUE) |> 
      convert_unit("_prop")
    
  }
  
  if (grepl("bi_quantx_", out$var_type)) {
    
    out$val_right <- 
      tolower(var_right_label[names(var_right_label) == round(val_right)])
  }
  
  
  ## Descriptive statistics for date type --------------------------------------
  
  if (grepl("date_", out$var_type)) {
    
    coef <- 
      dat %>%
      mutate(right_var = as.numeric(right_var)) %>%
      lm(left_var ~ right_var, data = .) %>%
      `$`("coefficients") %>%
      `[`("right_var") %>%
      signif(3)
    
    max_date <- 
      dat |> 
      filter(left_var == max(left_var)) |> 
      pull(right_var)
    
    if (length(max_date) %in% 2:3) max_date <- paste(
      paste(max_date[seq_len(length(max_date) - 1)], collapse = ", "),
      max_date[length(max_date)], sep = " and ")
    if (length(max_date) > 3) out$max_date <- "several different dates"
    out$max_date <- max_date
    
    min_date <- 
      dat |> 
      filter(left_var == min(left_var)) |> 
      pull(right_var)
    
    if (length(min_date) %in% 2:3) min_date <- paste(
      paste(min_date[seq_len(length(min_date) - 1)], collapse = ", "),
      max_date[length(min_date)], sep = " and ")
    if (length(min_date) > 3) min_date <- "several different dates"
    out$min_date <- min_date
    
    out$coef <- abs(coef)
    out$coef_increasing <- if (coef >= 0) "increasing" else "decreasing"
    out$date_left <- paste(date_left, collapse = '-')
  }
  
  
  ## Return output -------------------------------------------------------------
  print(out)
  return(out)
  
}
