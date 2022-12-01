## GLOBALS FOR m_tenure.R, m_afford.R, m_dw_types.R, m_demographics.R ##########

cent_compare <- 
  make_dropdown(only = NULL, 
                only_vars = c(variables$var_code[
                  grepl("^amenities_", variables$var_code)],
                  "canale", 
                  c(variables$var_code[
                    grepl("^climate_", variables$var_code)])), 
                compare = TRUE)
