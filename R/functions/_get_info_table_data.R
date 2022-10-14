#### GET INFO TABLE DATA #######################################################

#' @return A named list with all the data components necessary to power the
#' info_table module.

get_info_table_data <- function(r = r, data, var_type, var_left, var_right, df, 
                                select_id, geo, build_str_as_DA = TRUE) {
  
  ## Initialize dat and output list --------------------------------------------
  
  dat <- data
  out <- list(var_type = var_type)

  
  ## Get modified df for building/street ---------------------------------------
  
  if (!is_scale_in_df("building", df)) build_str_as_DA <- FALSE
  
  if (build_str_as_DA) {
    dat_select <- if (is.na(select_id)) get(paste(geo, "DA", sep = "_")) else {
      dbGetQuery(db, paste0("SELECT * FROM ", paste(geo, "building", sep = "_"), 
                            " WHERE ID = ", select_id))
    }
    dat_select_id <- select_id
    if (!is.na(select_id)) 
      select_id <- dbGetQuery(db, paste0("SELECT DAUID FROM ", 
                                         paste(geo, "building", sep = "_"), 
                                         " WHERE ID = ", 
                            select_id))$DAUID
    if (length(select_id) == 0) select_id <- NA
    
  } else {
    dat_select <- data
    dat_select_id <- select_id
  }
  
  
  ## Handle dates --------------------------------------------------------------
  
  date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
  
  if (length(var_left) == 2) {
    out$start_date_left <- date_left[1]
    out$end_date_left <- date_left[2]
  }
  
  if (length(var_right) == 2) {
    out$start_date_right <- str_extract(var_right, "(?<=_)\\d{4}$")[1]
    out$end_date_right <- str_extract(var_right, "(?<=_)\\d{4}$")[2]
  }
  
  
  ## Special case for date-type data -------------------------------------------
  
  if (is_scale_in_df("date", df)) {
    out$var_type <- "date_all"
    dat$name <- NA_character_
    dat$population <- NA_real_
  }
  
  
  ## Titles and explanations ---------------------------------------------------
  
  var_left <- unique(str_remove(var_left, "_\\d{4}$"))
  var_right <- unique(str_remove(var_right, "_\\d{4}$"))
  
  breaks_q5_left <- variables$breaks_q5[[
    which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
  breaks_q5_left <- breaks_q5_left[breaks_q5_left$scale == df,]
  
  if (!suppressWarnings(is.null(breaks_q5_left$var_name)) && 
      !all(is.na(breaks_q5_left$var_name))) {
    
    var_left_label <- 
      breaks_q5_left$var_name |> 
      setNames(breaks_q5_left$var)
    
  } else var_left_label <- NULL
  
  if (var_right != " ") {
    
    breaks_q5_right <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_right)))]]
    breaks_q5_right <- breaks_q5_right[breaks_q5_right$scale == df,]
    
    if (!suppressWarnings(is.null(breaks_q5_right$var_name)) && 
        !all(is.na(breaks_q5_right$var_name))) {
      
      var_right_label <- 
        breaks_q5_right$var_name |> 
        setNames(breaks_q5_right$var)
    
    } else var_right_label <- NULL
    
  } else var_right_label <- NULL
    
  var_left_label <- sapply(var_left_label, sus_translate, r = r)
  var_right_label <- sapply(var_right_label, sus_translate, r = r)
  
  out$title_left <- 
    sus_translate(r = r, variables[variables$var_code == var_left,]$var_title)
  if (var_right != " ") out$title_right <- 
    sus_translate(r = r, variables[variables$var_code == var_right,]$var_title)
  
  out$exp_left <- 
    sus_translate(r = r, variables[variables$var_code == var_left,]$explanation)
  out$exp_right <- 
    sus_translate(r = r, variables[variables$var_code == var_right,]$explanation)
  if (length(out$exp_left) == 0) warning("No exp: ", var_left, call. = FALSE)
  if (var_right != " " && length(out$exp_right) == 0) warning(
    "No exp: ", var_right, call. = FALSE)
  

  ## Selections ----------------------------------------------------------------
  
  # select_name and selection need to be different because of building names
  select_name <- if (is.na(dat_select_id)) dat_select[0,] else dat_select[
    dat_select$ID == dat_select_id,]
  selection <- if (is.na(select_id)) dat[0,] else dat[dat$ID == select_id,]
  out$selection <- selection
  active_left <- length(selection$ID[!is.na(selection$var_left_q3)])
  active_right <- active_left
  if (var_right != " ") active_right <- 
    length(selection$ID[!is.na(selection$var_left_q3) & 
                          !is.na(selection$var_left_q3)])
  out$pop <- convert_unit(selection$population)
  val_left <- selection$var_left
  out$val_left <- convert_unit(val_left, var_left)
  if (var_right != " ") {
    val_right <- selection$var_right
    out$val_right <- convert_unit(val_right, var_right)
    if (grepl("_delta", out$var_type)) out$val_right <- 
      convert_unit(val_right, "_pct")
  }
  

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
    gsub(".*_", "", df),  
    "date" = NA_character_,
    "CSD" = "borough/city",
    "CT" = "census tract",
    "DA" = "dissemination area",
    "DB" = "dissemination block",
    "grid" = "250-m",
    "building" = if (build_str_as_DA) "dissemination area" else "building",
    "street" = if (build_str_as_DA) "dissemination area" else "street",
    "centraide" = "centraide zone",
    "cmhczone" = "CMHC zone",
    "zone")
  
  scale_plural <- switch(
    gsub(".*_", "", scale_sing),
    "borough/city" = "boroughs or cities",
    "census tract" = "census tracts",
    "dissemination area" = "dissemination areas",
    "dissemination block" = "dissemination blocks",
    "250-m" = "areas",
    "building" = "buildings",
    "street" = "streets",
    "centraide zone" = "centraide zones",
    "cmhczone" = "CMHC zones",
    "zones")
  
  out$scale_sing <- sus_translate(r = r, scale_sing)
  out$scale_plural <- sus_translate(r = r, scale_plural)
  

  ## Place names ---------------------------------------------------------------
  
  out$place_name <- if (is_scale_in_df(c("building", "street"), df) && build_str_as_DA) {
    sus_translate(r = r, "The dissemination area around {select_name$name}")
  } else switch(
    scale_sing,
    "building" = glue("{select_name$name}"),
    "street" = glue("{select_name$name}"),
    "borough/city" = glue("{select_name$name}"),
    "census tract" = sus_translate(r = r, "Census tract {select_name$name}"),
    "dissemination area" = 
      sus_translate(r = r, "Dissemination area {select_name$name}"),
    "dissemination block" = 
      sus_translate(r = r, "Dissemination block {select_name$name}"),
    "250-m" = sus_translate(r = r, "The area around {select_name$name}"),
    "centraide zone" = glue("{select_name$name}"),
    "CMHC zone" = glue("{select_name$name}"),
    NA_character_)
  
  if (grepl("select", out$var_type)) {
    if (is_scale_in_df(first_level_choropleth, df)) select_name$name_2 <- 
        sus_translate(r = r, glue("{select_name$name_2}"))
    
    out$place_heading <- if (is_scale_in_df(c("building", "street"), df) && 
                             build_str_as_DA) {
      select_name$name
    } else if (scale_sing %in% c("borough/city", "centraide zone", "CMHC zone")) {
      sus_translate(r = r, "{select_name$name_2} of {out$place_name}")
    } else if (scale_sing == "250-m") {
      sus_translate(r = r, select_name$name)
    } else glue("{out$place_name} ({select_name$name_2})")
  }
  
  
  ## Geo name ------------------------------------------------------------------
  
  out$geo <- 
    sus_translate(r = r, 
                  switch(
                    geo, 
                    "CMA" = "in the Montreal region",
                    "city" = "in the City of Montreal",
                    "island" = "on the island of Montreal",
                    "centraide" = "in the Centraide of Greater Montreal territory",
                    "CMHC" = "in the Montreal region"))
  
  
  ## Descriptive statistics for var_left ---------------------------------------
  
  vec_left <- na.omit(dat$var_left[!is.na(dat$var_left_q3) & 
                                     !is.na(dat$var_left)])
  
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

    out$larger <- if (val_left >= quintile[4]) {
      sus_translate(r = r, "much larger than")
    } else if (val_left >= quintile[3]) {
      sus_translate(r = r, "larger than")
    } else if (val_left >= quintile[2]) {
      sus_translate(r = r, "almost the same as")
    } else if (val_left >= quintile[1]) {
      sus_translate(r = r, "smaller than")
    } else sus_translate(r = r, "much smaller than")

    out$high <- if (str_detect(out$larger, paste(sus_translate(r = r, "much larger than"), 
                                                  sus_translate(r = r, "larger than"), 
                                                  sep = "|"))) {
      sus_translate(r = r, "high")
    } else if (str_detect(out$larger, paste(sus_translate(r = r, "smaller than"), 
                                            sus_translate(r = r, "much smaller than"), 
                                            sep = "|"))) {
      sus_translate(r = r, "low")
    } else sus_translate(r = r, "moderate")

    out$percentile <- convert_unit(length(vec_left[vec_left <= val_left]) / 
                                     length(vec_left), "_pct")
    
    # Translation note: whatever if the explanation (the subject) is masculine 
    # or feminine, on n'accordera pas increased/decreased avec son sujet s'il
    # est employé avec avoir (our case here).
    out$increase <- if (val_left >= 0) sus_translate(r = r, "increased") else
      sus_translate(r = r, "decreased")
    
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
    out$majority <- if (mode_prop > 0.5) sus_translate(r = r, "majority") else 
      sus_translate(r = r, "plurality")
    out$mode_prop <- convert_unit(mode_prop, "_pct")
    out$mode_prop_2 <- convert_unit(qual_tab[2] / sum(qual_tab), "_pct")
    
  }
  
  
  ## Descriptive statistics for univariate qual selection ----------------------
  
  if (grepl("uni_qual_select", out$var_type)) {
    out$val_left <- 
      tolower(var_left_label[names(var_left_label) == round(val_left)])
    out$other_with_val <- 
      mean(round(dat$var_left) == round(val_left), na.rm = TRUE) |> 
      convert_unit("_pct")
    
  }
  
  ## Descriptive statistics for bivariate quantxy ------------------------------
  
  if (grepl("bi_quantxy|date", out$var_type)) {
    
      corr <- cor(dat$var_left, as.numeric(dat$var_right), use = "complete.obs")
      out$correlation <- corr
      out$corr_disp <- convert_unit(corr)
      out$pos <- if (corr > 0) sus_translate(r = r, "positive") else 
        sus_translate(r = r, "negative")
      
      out$strong <- if (abs(corr) > 0.6) {
        sus_translate(r = r, "strong")
      } else if (abs(corr) > 0.3) {
        sus_translate(r = r, "moderate")
      } else sus_translate(r = r, "weak")
      
      out$higher <- ifelse(out$pos == sus_translate(r = r, "positive"),
                           sus_translate(r = r, "higher"), sus_translate(r = r, "lower"))
      
      out$high_low_disclaimer <- if (out$strong == sus_translate(r = r, "strong")) {
        sus_translate(r = r, "with only a few exceptions")
      } else if (out$strong == sus_translate(r = r, "moderate")) {
        sus_translate(r = r, "although with some exceptions")
      } else if (out$strong == sus_translate(r = r, "weak")) {
        sus_translate(r = r, "although with many exceptions")
      }
  }
  
  
  ## Descriptive statistics for bivariate quantxy selection --------------------
  
  if (grepl("bi_quantxy_select", out$var_type)) {
   
    vec_1 <- dat$var_left
    vec_2 <- dat$var_right
    
    perc_left <- length(vec_1[vec_1 <= val_left]) / length(vec_1)
    out$perc_left <- convert_unit(perc_left, "_pct")
    perc_right <- length(vec_2[vec_2 <= val_right]) / length(vec_2)
    out$perc_right <- convert_unit(perc_right, "_pct")
    
    out$relative_position <- if (abs(perc_left - perc_right) > 0.5) {
      sus_translate(r = r, "dramatically different")
    } else if (abs(perc_left - perc_right) > 0.3) {
      sus_translate(r = r, "substantially different")
    } else if (abs(perc_left - perc_right) > 0.1) {
      sus_translate(r = r, "considerably different")
    } else sus_translate(r = r, "similar")
    
  }
  
  ## Descriptive statistics for quant/qual comparison --------------------------
  
  if (grepl("bi_quanty_|bi_quantx_", out$var_type)) {
    
    vec_1 <- dat$var_left
    vec_2 <- dat$var_right
    
    corr <- cor(vec_1, vec_2, use = "complete.obs", method = "spearman")
    out$correlation <- corr
    out$corr_disp <- convert_unit(corr)
    out$pos <- if (corr > 0) sus_translate(r = r, "positive") else 
      sus_translate(r = r, "negative")
    
    out$strong <- if (abs(corr) > 0.6) {
      sus_translate(r = r, "strong")
    } else if (abs(corr) > 0.3) {
      sus_translate(r = r, "moderate")
    } else sus_translate(r = r, "weak")
    
    out$higher <- ifelse(out$pos == sus_translate(r = r, "positive"),
                         sus_translate(r = r, "higher"), sus_translate(r = r, "lower"))
    
    out$high_low_disclaimer <- if (out$strong == sus_translate(r = r, "strong")) {
      sus_translate(r = r, "with only a few exceptions")
    } else if (out$strong == sus_translate(r = r, "moderate")) {
      sus_translate(r = r, "although with some exceptions")
    } else if (out$strong == sus_translate(r = r, "weak")) {
      sus_translate(r = r, "although with many exceptions")
    }

  }
  
  if (grepl("bi_quanty_", out$var_type)) {
    
    out$val_left <- 
      tolower(var_left_label[names(var_left_label) == round(val_left)])
    
    out$perc <- 
      mean(val_right >= vec_2[round(dat$var_left) == round(val_left)],
           na.rm = TRUE) |> 
      convert_unit("_pct")
    
  }
  
  if (grepl("bi_quantx_", out$var_type)) {
    
    out$val_right <- 
      tolower(var_right_label[names(var_right_label) == round(val_right)])
    
    out$perc <- 
      mean(val_left >= vec_1[round(dat$var_right) == round(val_right)],
           na.rm = TRUE) |> 
      convert_unit("_pct")
    
  }
  
  
  ## Descriptive statistics for date type --------------------------------------
  
  if (grepl("date_", out$var_type)) {
    
    coef <- dat
    coef$var_right <- as.numeric(coef$var_right)
    coef <- lm(var_left ~ var_right, data = coef)
    coef <- coef$coefficients
    coef <- signif(coef$var_right, 3)
    
    max_date <- dat$var_right[dat$var_left == max(dat$var_left)]
    
    if (length(max_date) %in% 2:3) max_date <- paste(
      paste(max_date[seq_len(length(max_date) - 1)], collapse = ", "),
      max_date[length(max_date)], sep = sus_translate(r = r, " and "))
    if (length(max_date) > 3) out$max_date <- 
      sus_translate(r = r, "several different dates")
    out$max_date <- max_date
    
    min_date <- dat$var_right[dat$var_left == min(dat$var_left)]
    
    if (length(min_date) %in% 2:3) min_date <- paste(
      paste(min_date[seq_len(length(min_date) - 1)], collapse = ", "),
      max_date[length(min_date)], sep = sus_translate(r = r, " and "))
    if (length(min_date) > 3) min_date <- 
      sus_translate(r = r, "several different dates")
    out$min_date <- min_date
    
    out$coef <- abs(coef)
    out$coef_increasing <- if (coef >= 0) sus_translate(r = r, "increasing") else 
      sus_translate(r = r, "decreasing")
    out$date_left <- paste(date_left, collapse = '-')
  }
  
  
  ## Return output -------------------------------------------------------------
  
  return(out)
  
}
