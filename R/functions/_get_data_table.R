#### GET DATA TABLE ############################################################

# Get var_left and var_right from db --------------------------------------

# Fun to get from SQL db
get_data_sql <- function(var, df) {
  conn <- paste0(df, "_conn")
  do.call("dbGetQuery", list(rlang::sym(conn),
                     paste0("SELECT * FROM ", var, " ORDER BY ID")))
}


# Get data variables and rename -------------------------------------------

get_dt_vars <- function(df, var_left, var_right = NULL) {
  # Get var_left and rename
  vl <- get_data_sql(var_left, df)[, -1]
  names(vl) <- c("var_left", "var_left_q3", "var_left_q5")
  
  # Get var_right and rename
  if (!is.null(var_right)) {
    vr <- tryCatch(get_data_sql(var_right, df)[, -1], 
                   error = function(e) {
                     data.frame(var_right = rep(NA, nrow(vl)),
                                var_right_q3 = rep(NA, nrow(vl)),
                                var_right_q5 = rep(NA, nrow(vl)))
                   })
    names(vr) <- c("var_right", "var_right_q3", "var_right_q5")
  }
  
  # Bind IDs and names with var_left and var_right
  out <- cbind(get(df), vl)
  if (!is.null(var_right)) out <- cbind(out, vr)
  
  # Return
  return(out)
}


# Get the table -----------------------------------------------------------

get_data_table <- function(df, geo, var_left, var_right, data_type, 
                           point_df = FALSE) {
  
  if (grepl("^building_", data_type)) df <- paste(geo, "DA", sep = "_")
  
  # Univariate
  if (data_type %in% c("building_q5", "q5")) {
    data <- get_dt_vars(df, var_left)
  }
  
  # Bivariate
  if (data_type %in% c("building_bivar", "bivar")) {
    data <- get_dt_vars(df, var_left, var_right)
    data$group <- paste(data$var_left_q3, data$var_right_q3, sep = " - ")
  }

  # Delta
  if (data_type %in% c("building_delta", "delta")) {
    vl <- lapply(var_left, \(x) get_data_sql(x, df)[, -1])
    data <- Reduce(cbind, vl)
    names(data)[1] <- "var_left_1"
    names(data)[4] <- "var_left_2"
    
    data$var_left <- (data$var_left_2 - data$var_left_1) / data$var_left_1
    
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data_med <- median(abs(data$var_left[abs(data$var_left) > 0.02]), 
                       na.rm = TRUE)
    data$var_left_q3 <- "5"
    data$var_left_q3[data$var_left < data_med] <- "4"
    data$var_left_q3[data$var_left < 0.02] <- "3"
    data$var_left_q3[data$var_left < -0.02] <- "2"
    data$var_left_q3[data$var_left < -1 * data_med] <- "1"
    data$var_left_q3[is.na(data$var_left)] <- NA_character_
    data$group <- data$var_left_q3
    
    data <- data[, c("var_left_1", "var_left_2", "var_left", "var_left_q3", "group")]

    data <- cbind(get(df), data)
  }
  
  # NA_delta
  if (data_type %in% c("building_NA_delta", "NA_delta")) {
    data <- get(df)
    data$group <- "NA"
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_left_1 <- NA
    data$var_left_2 <- NA
  }
  
  # Delta x & q3 y version
  if (data_type %in% "bivar_xdelta_yq3") {
    vars <- lapply(c(var_left, var_right[1]), \(x) get_data_sql(x, df)[, -1])
    data <- Reduce(cbind, vars)
    names(data)[1] <- "var_left_1"
    names(data)[4] <- "var_left_2"
    names(data)[7] <- "var_right"
    names(data)[8] <- "var_right_q3"
    data <- data[, which(names(data) %in% c("var_left_1", "var_left_2", "var_right", 
                                    "var_right_q3"))]
    
    
    data$var_left <- (data$var_left_2 - data$var_left_1) / abs(data$var_left_1)
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data$var_left_q3 <- ntile(data$var_left, 3)
    data$group <- paste(data$var_left_q3, "-", data$var_right_q3)
    
    data <- cbind(get(df), data)
  }
  
  # Delta bivariate
  if (data_type %in% c("building_delta_bivar", "delta_bivar")) {
    vars <- lapply(c(var_left, var_right), \(x) get_data_sql(x, df)[, -1])
    data <- Reduce(cbind, vars)
    names(data)[1] <- "var_left_1"
    names(data)[4] <- "var_left_2"
    names(data)[7] <- "var_right_1"
    names(data)[10] <- "var_right_2"
    data <- data[, which(names(data) %in% c("var_left_1", "var_left_2", "var_right_1", 
                                            "var_right_2"))]
    
    data$var_left <- (data$var_left_2 - data$var_left_1) / abs(data$var_left_1)
    data$var_left <- replace(data$var_left, is.nan(data$var_left), NA)
    data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)
    data$var_left_q3 <- ntile(data$var_left, 3)
    data$var_right <- (data$var_right_2 - data$var_right_1) / abs(data$var_right_1)
    data$var_right <- replace(data$var_right, is.nan(data$var_right), NA)
    data$var_right <- replace(data$var_right, is.infinite(data$var_right), NA)
    data$var_right_q3 <- ntile(data$var_right, 3)
    data$group <- paste(data$var_left_q3, "-", data$var_right_q3)
    
    data <- cbind(get(df), data)
  }
    
  # NA_delta_bivar
  if (data_type %in% c("building_NA_delta_bivar", "NA_delta_bivar")) {
    data <- get(df)
    data$group <- "NA - NA"
    data$var_left <- NA
    data$var_left_q3 <- NA
    data$var_right <- NA
    data$var_right_q3 <- NA
  }
    
  # Point data TKTK update with base R
  # if (data_type == "point") {
  #   
  #   type_pattern <- paste(unique(get(point_df)$type), collapse = "|")
  #   selected_type <- str_extract(var_left, type_pattern)
  #   time <- str_extract(var_left, time_format)
  #   
  #   data <- 
  #     point_df |> 
  #     get() %>%
  #     { if (str_detect(var_left[1], "_total_"))
  #       . else filter(., str_detect(type, selected_type))} %>%
  #     { if (length(time) == 2) {
  #       filter(., year %in% time[1]:time[2])
  #     } else {
  #       filter(., year == time)
  #     }}
  # }
  
  # Return data
  return(tibble::as_tibble(data))
  
}
