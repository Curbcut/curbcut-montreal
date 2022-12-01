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
      dbGetQuery(building_conn, 
                 paste0("SELECT * FROM ", paste(geo, "building", sep = "_"), 
                        " WHERE ID = '", select_id, "'"))
    }
    dat_select_id <- select_id
    if (!is.na(select_id)) 
      select_id <- dbGetQuery(building_conn, 
                              paste0("SELECT DA_ID FROM ", paste(geo, "building", sep = "_"), 
                                     " WHERE ID = '", select_id, "'"))$DA_ID
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
  
  if (length(unique(var_right)) == 1) {
    out$date_right <- str_extract(var_right[1], "(?<=_)\\d{4}$")
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
    
  var_left_label <- sapply(var_left_label, cc_t, r = r)
  var_right_label <- sapply(var_right_label, cc_t, r = r)
  
  out$title_left <- 
    cc_t(r = r, variables[variables$var_code == var_left,]$var_title)
  if (var_right != " ") out$title_right <- 
    cc_t(r = r, variables[variables$var_code == var_right,]$var_title)
  
  out$exp_left <- 
    cc_t(r = r, variables[variables$var_code == var_left,]$explanation)
  out$exp_right <- 
    cc_t(r = r, variables[variables$var_code == var_right,]$explanation)
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
    if (grepl("_delta$", out$var_type)) out$val_right <- 
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
  
  scale_sing <- 
    scales_dictionary$sing[scales_dictionary$scale == gsub(".*_", "", df)]
  
  scale_plural <- 
    scales_dictionary$plur[scales_dictionary$scale == gsub(".*_", "", df)]
  
  out$scale_sing <- cc_t(r = r, scale_sing)
  out$scale_plural <- cc_t(r = r, scale_plural)
  

  ## Place names ---------------------------------------------------------------
  
  name <- select_name$name
  out$place_name <- 
    scales_dictionary$place_name[scales_dictionary$scale == gsub(".*_", "", df)]
  out$place_name <- cc_t(r = r, out$place_name)
  
  name_2 <- cc_t(r = r, select_name$name_2)
  out$place_heading <- 
    scales_dictionary$place_heading[scales_dictionary$scale == gsub(".*_", "", df)]
  out$place_heading <- cc_t(r = r, out$place_heading)
  
  
  ## Geo name ------------------------------------------------------------------
  
  out$geo <- 
    cc_t(r = r, regions_dictionary$to_compare[regions_dictionary == geo])

  
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
      cc_t(r = r, "much larger than")
    } else if (val_left >= quintile[3]) {
      cc_t(r = r, "larger than")
    } else if (val_left >= quintile[2]) {
      cc_t(r = r, "almost the same as")
    } else if (val_left >= quintile[1]) {
      cc_t(r = r, "smaller than")
    } else cc_t(r = r, "much smaller than")

    out$high <- if (str_detect(out$larger, paste(cc_t(r = r, "much larger than"), 
                                                  cc_t(r = r, "larger than"), 
                                                  sep = "|"))) {
      cc_t(r = r, "high")
    } else if (str_detect(out$larger, paste(cc_t(r = r, "smaller than"), 
                                            cc_t(r = r, "much smaller than"), 
                                            sep = "|"))) {
      cc_t(r = r, "low")
    } else cc_t(r = r, "moderate")

    out$percentile <- convert_unit(length(vec_left[vec_left <= val_left]) / 
                                     length(vec_left), "_pct")
    
    # Translation note: whatever if the explanation (the subject) is masculine 
    # or feminine, on n'accordera pas increased/decreased avec son sujet s'il
    # est employé avec avoir (our case here).
    out$increase <- if (val_left >= 0) cc_t(r = r, "increased") else
      cc_t(r = r, "decreased")
    
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
    out$majority <- if (mode_prop > 0.5) cc_t(r = r, "majority") else 
      cc_t(r = r, "plurality")
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
      out$pos <- if (corr > 0) cc_t(r = r, "positive") else 
        cc_t(r = r, "negative")
      
      out$strong <- if (abs(corr) > 0.6) {
        cc_t(r = r, "strong")
      } else if (abs(corr) > 0.3) {
        cc_t(r = r, "moderate")
      } else cc_t(r = r, "weak")
      
      out$higher <- ifelse(out$pos == cc_t(r = r, "positive"),
                           cc_t(r = r, "higher"), cc_t(r = r, "lower"))
      
      out$high_low_disclaimer <- if (out$strong == cc_t(r = r, "strong")) {
        cc_t(r = r, "with only a few exceptions")
      } else if (out$strong == cc_t(r = r, "moderate")) {
        cc_t(r = r, "although with some exceptions")
      } else if (out$strong == cc_t(r = r, "weak")) {
        cc_t(r = r, "although with many exceptions")
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
      cc_t(r = r, "dramatically different")
    } else if (abs(perc_left - perc_right) > 0.3) {
      cc_t(r = r, "substantially different")
    } else if (abs(perc_left - perc_right) > 0.1) {
      cc_t(r = r, "considerably different")
    } else cc_t(r = r, "similar")
    
  }
  
  ## Descriptive statistics for quant/qual comparison --------------------------
  
  if (grepl("bi_quanty_|bi_quantx_", out$var_type)) {
    
    vec_1 <- dat$var_left
    vec_2 <- dat$var_right
    
    corr <- cor(vec_1, vec_2, use = "complete.obs", method = "spearman")
    out$correlation <- corr
    out$corr_disp <- convert_unit(corr)
    out$pos <- if (corr > 0) cc_t(r = r, "positive") else 
      cc_t(r = r, "negative")
    
    out$strong <- if (abs(corr) > 0.6) {
      cc_t(r = r, "strong")
    } else if (abs(corr) > 0.3) {
      cc_t(r = r, "moderate")
    } else cc_t(r = r, "weak")
    
    out$higher <- ifelse(out$pos == cc_t(r = r, "positive"),
                         cc_t(r = r, "higher"), cc_t(r = r, "lower"))
    
    out$high_low_disclaimer <- if (out$strong == cc_t(r = r, "strong")) {
      cc_t(r = r, "with only a few exceptions")
    } else if (out$strong == cc_t(r = r, "moderate")) {
      cc_t(r = r, "although with some exceptions")
    } else if (out$strong == cc_t(r = r, "weak")) {
      cc_t(r = r, "although with many exceptions")
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
      max_date[length(max_date)], sep = cc_t(r = r, " and "))
    if (length(max_date) > 3) out$max_date <- 
      cc_t(r = r, "several different dates")
    out$max_date <- max_date
    
    min_date <- dat$var_right[dat$var_left == min(dat$var_left)]
    
    if (length(min_date) %in% 2:3) min_date <- paste(
      paste(min_date[seq_len(length(min_date) - 1)], collapse = ", "),
      max_date[length(min_date)], sep = cc_t(r = r, " and "))
    if (length(min_date) > 3) min_date <- 
      cc_t(r = r, "several different dates")
    out$min_date <- min_date
    
    out$coef <- abs(coef)
    out$coef_increasing <- if (coef >= 0) cc_t(r = r, "increasing") else 
      cc_t(r = r, "decreasing")
    out$date_left <- paste(date_left, collapse = '-')
  }
  
  
  ## Return output -------------------------------------------------------------
  
  return(out)
  
}
