#### HOUSING TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
building_full <- qread("data/building_full.qs", nthreads = 4)
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")
source("R/functions/_utils.R")
qload("data/colours.qsm")


# Get variables to add ----------------------------------------------------

left_vars <-
  variables |> 
  filter(theme == "Housing", source == "census") |> 
  pull(var_code)

all_years <- 
  variables |> 
  filter(theme == "Housing", source == "census") |> 
  pull(dates) |> 
  unlist() |> 
  unique() |> 
  sort()

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment", "Housing"), !is.na(theme)) |> 
  pull(var_code)


# Process borough ---------------------------------------------------------

df <- "borough"
dat <- get(df)

# Get all multi-date right vars
rv_delta_bivar <- map_dfc(right_vars, \(var) {
  
  var_year <-
    variables |> 
    filter(var_code == var) |> 
    pull(dates) |> 
    pluck(1)
  
  if (length(var_year) != length(all_years)) return(NULL)
  
  var_year_comb <-
    expand.grid(var_year, var_year) |> 
    as_tibble() |> 
    mutate(across(everything(), as.character)) |> 
    mutate(across(everything(), as.numeric)) |> 
    filter(Var1 < Var2) |> 
    as.vector() |> 
    split(1:10) |> 
    map(unlist) |> 
    map(unname)
  
  # Get delta values for all 2-year comparisons
  multi_year <-
    map_dfc(var_year_comb, ~{
      
      val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                dat[[paste(var, .x[1], sep = "_")]]) / 
        dat[[paste(var, .x[1], sep = "_")]]
      
      val <- replace(val, is.nan(val), NA)
      val <- replace(val, is.infinite(val), NA)
      q3 <- ntile(val, 3)
      tibble(q3) |> 
        set_names(paste(var, "q3", paste(.x, collapse = "_"), sep = "_"))
    })
  
})

# Get all combinations for all left vars
borough_housing <- 
  dat |> 
  select(ID, name, geometry) |>
  bind_cols(map_dfc(left_vars, \(var) {
    
    var_year <-
      variables |> 
      filter(var_code == var) |> 
      pull(dates) |> 
      pluck(1)
    
    var_year_comb <-
      expand.grid(var_year, var_year) |> 
      as_tibble() |> 
      mutate(across(everything(), as.character)) |> 
      mutate(across(everything(), as.numeric)) |> 
      filter(Var1 < Var2) |> 
      as.vector() |> 
      # Need to calculate the triangular number to see how many pairs there are
      split(1:(length(var_year) * (length(var_year) - 1) / 2)) |> 
      map(unlist) |> 
      map(unname)
    
    # Get q5 values for individual years
    single_year_q5 <- 
      map_dfc(var_year, ~dat[[paste(var, "q5", .x, sep = "_")]]) |> 
      set_names(paste(var, var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get q3 values for individual years
    single_year_q3 <- 
      map_dfc(var_year, ~dat[[paste(var, "q3", .x, sep = "_")]]) |> 
      set_names(paste(var, "q3", var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get delta values for all 2-year comparisons
    multi_year <- if (length(var_year) != length(all_years)) NULL else {
      
      map_dfc(var_year_comb, ~{
        
        val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                  dat[[paste(var, .x[1], sep = "_")]]) / 
          dat[[paste(var, .x[1], sep = "_")]]
        
        val <- replace(val, is.nan(val), NA)
        val <- replace(val, is.infinite(val), NA)
        q3 <- ntile(val, 3)
        data_med <- median(abs(val[abs(val) > 0.02]), na.rm = TRUE)
        val_delta <- "19"
        val_delta[val < data_med] <- "18"
        val_delta[val < 0.02] <- "17"
        val_delta[val < -0.02] <- "16"
        val_delta[val < -1 * data_med] <- "15"
        val_delta[is.na(val)] <- "0"
        tibble(val_delta, q3) |> 
          set_names(c(paste(var, paste(.x, collapse = "_"), sep = "_"),
                      paste(var, "q3", paste(.x, collapse = "_"), sep = "_")))
      })
    }
    
    multi_year_q3 <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(contains("q3"))
    }
      
    multi_year <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(!contains("q3"))
    }
      
    # Get bivar values for all single years
    bivar <- map_dfc(right_vars, \(rv) {
      
      map_dfc(names(single_year_q3), ~{
        year <- str_extract(.x, "\\d{4}$")
        rv_2 <- paste(rv, "q3", year, sep = "_") |> 
          return_closest_year(df)
        paste(single_year_q3[[.x]], dat[[rv_2]], sep = " - ")}) |> 
        set_names(paste(var, rv, str_extract(names(single_year_q3), "\\d{4}$"),
                        sep = "_"))
      
    })
    
    # Get delta bivar values for all year combinations
    delta_bivar <- if (is.null(multi_year_q3)) NULL else {
      
      multi_year_q3 |> 
        names() |> 
        map_dfc(\(x) {
          
          vec <- multi_year_q3[[x]]
          years <- str_extract(x, "\\d{4}_\\d{4}$")
          
          rv_names <- 
            rv_delta_bivar |> 
            select(contains(years)) |> 
            names()
          
          rv_delta_bivar[rv_names] |> 
            mutate(across(everything(), 
                          ~trans_var(paste(vec, .x, sep = " - ")))) |> 
            set_names(paste(str_remove(x, "_q3.*$"), 
                            str_remove(rv_names, "_q3"),
                            sep = "_"))
          
        })
      
    }
    
    bind_cols(single_year_q5, multi_year, bivar, delta_bivar)
    
  })) |> 
  relocate(geometry, .after = last_col())


# Construct tile lookup table ---------------------------------------------

tile_lookup <- qread("data/tile_lookup.qs")

var_groups <- 
  map(left_vars, ~{
    names(borough_housing) |> 
      str_subset(.x)
  })

var_groups <- 
  map(var_groups, ~{
  if (length(.x) < 100) return(list(.x))
  one_year <- str_subset(.x, "\\d{4}_\\d{4}$", negate = TRUE)
  two_year <- str_subset(.x, "\\d{4}_\\d{4}$")
  two_year_1 <- str_subset(two_year, "1996|2001_2006")
  two_year_2 <- str_subset(two_year, "1996|2001_2006", negate = TRUE)
  list(one_year, two_year_1, two_year_2)}) |> 
  unlist(recursive = FALSE)

var_groups_long <- var_groups[lengths(var_groups) > 100]
var_groups_short <- var_groups[lengths(var_groups) <= 100]
var_groups_short <- list(c(var_groups_short[[1]], var_groups_short[[2]]),
                         c(var_groups_short[[3]], var_groups_short[[4]]))
var_groups <- c(var_groups_long, var_groups_short)

tile_lookup <- 
  tile_lookup |> 
  bind_rows(
    map2_dfr(var_groups, seq_along(var_groups), ~tibble(
      module = "housing", tile2 = .x, suffix = paste("-", .y)))
  )

qsave(tile_lookup, "data/tile_lookup.qs")


# Process CT --------------------------------------------------------------

df <- "CT"
dat <- get(df)

# Get all multi-date right vars
rv_delta_bivar <- map_dfc(right_vars, \(var) {
  
  var_year <-
    variables |> 
    filter(var_code == var) |> 
    pull(dates) |> 
    pluck(1)
  
  if (length(var_year) != length(all_years)) return(NULL)
  
  var_year_comb <-
    expand.grid(var_year, var_year) |> 
    as_tibble() |> 
    mutate(across(everything(), as.character)) |> 
    mutate(across(everything(), as.numeric)) |> 
    filter(Var1 < Var2) |> 
    as.vector() |> 
    split(1:10) |> 
    map(unlist) |> 
    map(unname)
  
  # Get delta values for all 2-year comparisons
  multi_year <-
    map_dfc(var_year_comb, ~{
      
      val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                dat[[paste(var, .x[1], sep = "_")]]) / 
        dat[[paste(var, .x[1], sep = "_")]]
      
      val <- replace(val, is.nan(val), NA)
      val <- replace(val, is.infinite(val), NA)
      q3 <- ntile(val, 3)
      tibble(q3) |> 
        set_names(paste(var, "q3", paste(.x, collapse = "_"), sep = "_"))
    })
  
})

# Get all combinations for all left vars
CT_housing <- 
  dat |> 
  select(ID, name, geometry) |>
  bind_cols(map_dfc(left_vars, \(var) {
    
    var_year <-
      variables |> 
      filter(var_code == var) |> 
      pull(dates) |> 
      pluck(1)
    
    var_year_comb <-
      expand.grid(var_year, var_year) |> 
      as_tibble() |> 
      mutate(across(everything(), as.character)) |> 
      mutate(across(everything(), as.numeric)) |> 
      filter(Var1 < Var2) |> 
      as.vector() |> 
      # Need to calculate the triangular number to see how many pairs there are
      split(1:(length(var_year) * (length(var_year) - 1) / 2)) |> 
      map(unlist) |> 
      map(unname)
    
    # Get q5 values for individual years
    single_year_q5 <- 
      map_dfc(var_year, ~dat[[paste(var, "q5", .x, sep = "_")]]) |> 
      set_names(paste(var, var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get q3 values for individual years
    single_year_q3 <- 
      map_dfc(var_year, ~dat[[paste(var, "q3", .x, sep = "_")]]) |> 
      set_names(paste(var, "q3", var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get delta values for all 2-year comparisons
    multi_year <- if (length(var_year) != length(all_years)) NULL else {
      
      map_dfc(var_year_comb, ~{
        
        val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                  dat[[paste(var, .x[1], sep = "_")]]) / 
          dat[[paste(var, .x[1], sep = "_")]]
        
        val <- replace(val, is.nan(val), NA)
        val <- replace(val, is.infinite(val), NA)
        q3 <- ntile(val, 3)
        data_med <- median(abs(val[abs(val) > 0.02]), na.rm = TRUE)
        val_delta <- "19"
        val_delta[val < data_med] <- "18"
        val_delta[val < 0.02] <- "17"
        val_delta[val < -0.02] <- "16"
        val_delta[val < -1 * data_med] <- "15"
        val_delta[is.na(val)] <- "0"
        tibble(val_delta, q3) |> 
          set_names(c(paste(var, paste(.x, collapse = "_"), sep = "_"),
                      paste(var, "q3", paste(.x, collapse = "_"), sep = "_")))
      })
    }
    
    multi_year_q3 <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(contains("q3"))
    }
    
    multi_year <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(!contains("q3"))
    }
    
    # Get bivar values for all single years
    bivar <- map_dfc(right_vars, \(rv) {
      
      map_dfc(names(single_year_q3), ~{
        year <- str_extract(.x, "\\d{4}$")
        rv_2 <- paste(rv, "q3", year, sep = "_") |> 
          return_closest_year(df)
        paste(single_year_q3[[.x]], dat[[rv_2]], sep = " - ")}) |> 
        set_names(paste(var, rv, str_extract(names(single_year_q3), "\\d{4}$"),
                        sep = "_"))
      
    })
    
    # Get delta bivar values for all year combinations
    delta_bivar <- if (is.null(multi_year_q3)) NULL else {
      
      multi_year_q3 |> 
        names() |> 
        map_dfc(\(x) {
          
          vec <- multi_year_q3[[x]]
          years <- str_extract(x, "\\d{4}_\\d{4}$")
          
          rv_names <- 
            rv_delta_bivar |> 
            select(contains(years)) |> 
            names()
          
          rv_delta_bivar[rv_names] |> 
            mutate(across(everything(), 
                          ~trans_var(paste(vec, .x, sep = " - ")))) |> 
            set_names(paste(str_remove(x, "_q3.*$"), 
                            str_remove(rv_names, "_q3"),
                            sep = "_"))
          
        })
      
    }
    
    bind_cols(single_year_q5, multi_year, bivar, delta_bivar)
    
  })) |> 
  relocate(geometry, .after = last_col())


# Process DA --------------------------------------------------------------

df <- "DA"
dat <- get(df)

# Get all multi-date right vars
rv_delta_bivar <- map_dfc(right_vars, \(var) {
  
  var_year <-
    variables |> 
    filter(var_code == var) |> 
    pull(dates) |> 
    pluck(1)
  
  if (length(var_year) != length(all_years)) return(NULL)
  
  var_year_comb <-
    expand.grid(var_year, var_year) |> 
    as_tibble() |> 
    mutate(across(everything(), as.character)) |> 
    mutate(across(everything(), as.numeric)) |> 
    filter(Var1 < Var2) |> 
    as.vector() |> 
    split(1:10) |> 
    map(unlist) |> 
    map(unname)
  
  # Get delta values for all 2-year comparisons
  multi_year <-
    map(var_year_comb, ~{
      
      val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                dat[[paste(var, .x[1], sep = "_")]]) / 
        dat[[paste(var, .x[1], sep = "_")]]
      
      val <- replace(val, is.nan(val), NA)
      val <- replace(val, is.infinite(val), NA)
      q3 <- ntile(val, 3)
      tibble(q3) |> 
        set_names(paste(var, "q3", paste(.x, collapse = "_"), sep = "_"))
    })
  
})

# Get all combinations for all left vars
DA_housing <- 
  dat |> 
  select(ID, name, geometry) |>
  bind_cols(map_dfc(left_vars, \(var) {
    
    var_year <-
      variables |> 
      filter(var_code == var) |> 
      pull(dates) |> 
      pluck(1)
    
    var_year_comb <-
      expand.grid(var_year, var_year) |> 
      as_tibble() |> 
      mutate(across(everything(), as.character)) |> 
      mutate(across(everything(), as.numeric)) |> 
      filter(Var1 < Var2) |> 
      as.vector() |> 
      # Need to calculate the triangular number to see how many pairs there are
      split(1:(length(var_year) * (length(var_year) - 1) / 2)) |> 
      map(unlist) |> 
      map(unname)
    
    # Get q5 values for individual years
    single_year_q5 <- 
      map_dfc(var_year, ~dat[[paste(var, "q5", .x, sep = "_")]]) |> 
      set_names(paste(var, var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get q3 values for individual years
    single_year_q3 <- 
      map_dfc(var_year, ~dat[[paste(var, "q3", .x, sep = "_")]]) |> 
      set_names(paste(var, "q3", var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get delta values for all 2-year comparisons
    multi_year <- if (length(var_year) != length(all_years)) NULL else {
      
      map_dfc(var_year_comb, ~{
        
        val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                  dat[[paste(var, .x[1], sep = "_")]]) / 
          dat[[paste(var, .x[1], sep = "_")]]
        
        val <- replace(val, is.nan(val), NA)
        val <- replace(val, is.infinite(val), NA)
        q3 <- ntile(val, 3)
        data_med <- median(abs(val[abs(val) > 0.02]), na.rm = TRUE)
        val_delta <- "19"
        val_delta[val < data_med] <- "18"
        val_delta[val < 0.02] <- "17"
        val_delta[val < -0.02] <- "16"
        val_delta[val < -1 * data_med] <- "15"
        val_delta[is.na(val)] <- "0"
        tibble(val_delta, q3) |> 
          set_names(c(paste(var, paste(.x, collapse = "_"), sep = "_"),
                      paste(var, "q3", paste(.x, collapse = "_"), sep = "_")))
      })
    }
    
    multi_year_q3 <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(contains("q3"))
    }
    
    multi_year <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(!contains("q3"))
    }
    
    # Get bivar values for all single years
    bivar <- map_dfc(right_vars, \(rv) {
      
      map_dfc(names(single_year_q3), ~{
        year <- str_extract(.x, "\\d{4}$")
        rv_2 <- paste(rv, "q3", year, sep = "_") |> 
          return_closest_year(df)
        paste(single_year_q3[[.x]], dat[[rv_2]], sep = " - ")}) |> 
        set_names(paste(var, rv, str_extract(names(single_year_q3), "\\d{4}$"),
                        sep = "_"))
      
    })
    
    # Get delta bivar values for all year combinations
    delta_bivar <- if (is.null(multi_year_q3)) NULL else {
      
      multi_year_q3 |> 
        names() |> 
        map_dfc(\(x) {
          
          vec <- multi_year_q3[[x]]
          years <- str_extract(x, "\\d{4}_\\d{4}$")
          
          rv_names <- 
            rv_delta_bivar |> 
            select(contains(years)) |> 
            names()
          
          rv_delta_bivar[rv_names] |> 
            mutate(across(everything(), 
                          ~trans_var(paste(vec, .x, sep = " - ")))) |> 
            set_names(paste(str_remove(x, "_q3.*$"), 
                            str_remove(rv_names, "_q3"),
                            sep = "_"))
          
        })
      
    }
    
    bind_cols(single_year_q5, multi_year, bivar, delta_bivar)
    
  })) |> 
  relocate(geometry, .after = last_col())


# Process DA --------------------------------------------------------------

df <- "DA"
dat <- get(df)

# Get all multi-date right vars
rv_delta_bivar <- map_dfc(right_vars, \(var) {
  
  var_year <-
    variables |> 
    filter(var_code == var) |> 
    pull(dates) |> 
    pluck(1)
  
  if (length(var_year) != length(all_years)) return(NULL)
  
  var_year_comb <-
    expand.grid(var_year, var_year) |> 
    as_tibble() |> 
    mutate(across(everything(), as.character)) |> 
    mutate(across(everything(), as.numeric)) |> 
    filter(Var1 < Var2) |> 
    as.vector() |> 
    split(1:10) |> 
    map(unlist) |> 
    map(unname)
  
  # Get delta values for all 2-year comparisons
  multi_year <-
    map(var_year_comb, ~{
      
      val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                dat[[paste(var, .x[1], sep = "_")]]) / 
        dat[[paste(var, .x[1], sep = "_")]]
      
      val <- replace(val, is.nan(val), NA)
      val <- replace(val, is.infinite(val), NA)
      q3 <- ntile(val, 3)
      tibble(q3) |> 
        set_names(paste(var, "q3", paste(.x, collapse = "_"), sep = "_"))
    })
  
})

# Get all combinations for all left vars
DA_housing <- 
  dat |> 
  select(ID, name, building, geometry) |>
  bind_cols(map_dfc(left_vars, \(var) {
    
    var_year <-
      variables |> 
      filter(var_code == var) |> 
      pull(dates) |> 
      pluck(1)
    
    var_year_comb <-
      expand.grid(var_year, var_year) |> 
      as_tibble() |> 
      mutate(across(everything(), as.character)) |> 
      mutate(across(everything(), as.numeric)) |> 
      filter(Var1 < Var2) |> 
      as.vector() |> 
      # Need to calculate the triangular number to see how many pairs there are
      split(1:(length(var_year) * (length(var_year) - 1) / 2)) |> 
      map(unlist) |> 
      map(unname)
    
    # Get q5 values for individual years
    single_year_q5 <- 
      map_dfc(var_year, ~dat[[paste(var, "q5", .x, sep = "_")]]) |> 
      set_names(paste(var, var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get q3 values for individual years
    single_year_q3 <- 
      map_dfc(var_year, ~dat[[paste(var, "q3", .x, sep = "_")]]) |> 
      set_names(paste(var, "q3", var_year, sep = "_")) |> 
      mutate(across(everything(), as.character))
    
    # Get delta values for all 2-year comparisons
    multi_year <- if (length(var_year) != length(all_years)) NULL else {
      
      map_dfc(var_year_comb, ~{
        
        val <- (dat[[paste(var, .x[2], sep = "_")]] - 
                  dat[[paste(var, .x[1], sep = "_")]]) / 
          dat[[paste(var, .x[1], sep = "_")]]
        
        val <- replace(val, is.nan(val), NA)
        val <- replace(val, is.infinite(val), NA)
        q3 <- ntile(val, 3)
        data_med <- median(abs(val[abs(val) > 0.02]), na.rm = TRUE)
        val_delta <- "19"
        val_delta[val < data_med] <- "18"
        val_delta[val < 0.02] <- "17"
        val_delta[val < -0.02] <- "16"
        val_delta[val < -1 * data_med] <- "15"
        val_delta[is.na(val)] <- "0"
        tibble(val_delta, q3) |> 
          set_names(c(paste(var, paste(.x, collapse = "_"), sep = "_"),
                      paste(var, "q3", paste(.x, collapse = "_"), sep = "_")))
      })
    }
    
    multi_year_q3 <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(contains("q3"))
    }
    
    multi_year <- if (is.null(multi_year)) NULL else {
      multi_year |> 
        select(!contains("q3"))
    }
    
    # Get bivar values for all single years
    bivar <- map_dfc(right_vars, \(rv) {
      
      map_dfc(names(single_year_q3), ~{
        year <- str_extract(.x, "\\d{4}$")
        rv_2 <- paste(rv, "q3", year, sep = "_") |> 
          return_closest_year(df)
        paste(single_year_q3[[.x]], dat[[rv_2]], sep = " - ")}) |> 
        set_names(paste(var, rv, str_extract(names(single_year_q3), "\\d{4}$"),
                        sep = "_"))
      
    })
    
    # Get delta bivar values for all year combinations
    delta_bivar <- if (is.null(multi_year_q3)) NULL else {
      
      multi_year_q3 |> 
        names() |> 
        map_dfc(\(x) {
          
          vec <- multi_year_q3[[x]]
          years <- str_extract(x, "\\d{4}_\\d{4}$")
          
          rv_names <- 
            rv_delta_bivar |> 
            select(contains(years)) |> 
            names()
          
          rv_delta_bivar[rv_names] |> 
            mutate(across(everything(), 
                          ~trans_var(paste(vec, .x, sep = " - ")))) |> 
            set_names(paste(str_remove(x, "_q3.*$"), 
                            str_remove(rv_names, "_q3"),
                            sep = "_"))
          
        })
      
    }
    
    bind_cols(single_year_q5, multi_year, bivar, delta_bivar)
    
  })) |> 
  relocate(building, geometry, .after = last_col())


# Process DA_building -----------------------------------------------------

DA_building_housing <- 
  DA_housing |> 
  st_set_geometry("building") |> 
  select(everything(), -geometry, geometry = building)

DA_housing <- 
  DA_housing |> 
  select(-building)


# Process building --------------------------------------------------------

building_housing <- 
  building_full |> 
  select(ID, name, DAUID) |> 
  left_join(select(st_drop_geometry(DA_housing), -name), 
            by = c("DAUID" = "ID")) |> 
  select(-DAUID)




# Test one building chunk -------------------------------------------------

building_to_process <- 
  building_housing |> 
  select(ID, name, all_of(var_groups[[1]]), geometry)

iter_size <- ceiling(nrow(building_to_process) / 100)

building_to_process_list <- 
  map(1:100, ~{
    building_to_process |> 
      slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
      geojsonsf::sf_geojson() |> 
      paste0(collapse = " ") |> 
      geojson::featurecollection()  
  })

# Iteratively post files to tile source
tmp <- tempfile(fileext = ".json")
tmp_list <- map(1:10, ~tempfile(fileext = ".json"))

map(1:10, ~{
  
  print(.x)
  to_process <- building_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
  walk2(to_process, tmp_list, geojson::ndgeo_write)
  
  # Concatenate geoJSONs
  out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
  system(out)
  
  # Upload to MTS
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                'sus-mcgill/housing-building_test?access_token=', .sus_token, 
                '" -F file=@', tmp, 
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})


# Create and publish building_empty tileset -------------------------------

building_recipe <- 
  create_recipe(
    layer_names = "building",
    source = "mapbox://tileset-source/sus-mcgill/housing-building_test",
    minzoom = 14,
    maxzoom = 14, 
    layer_size = 2500,
    recipe_name = "housing-building_test")

create_tileset("housing-building_test", building_recipe)
publish_tileset("housing-building_test")


