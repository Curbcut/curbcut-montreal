# Download data -----------------------------------------------------------

# Variables to remove, from all census (if present)
vars_to_remove <- 
  c("Shape Area", "Type", "Dwellings", "CD_UID", "CMA_UID", "Region Name",
    "Area (sq km)", "Adjusted Population (previous Census)", "PR_UID",
    "Population", "Households", "CSD_UID", "CT_UID", "rguid", "key",
    "gpid", "pid", "Quality Flags", "NHS Non-Return Rate", "NHS Non Return Rate")
       
# Function to download data
census_retrieval <- function(cancensus_dataset, added_var_group = NULL, montreal_code = "24462") {

  if (year_census != newest_census_year) sf = TRUE else sf = FALSE
  
  DA_census <- 
    get_census(cancensus_dataset, list(CMA = montreal_code), "DA", 
               vectors = c(census_income, census_education, census_identity, 
                           census_housing, census_employment, census_transport,
                           added_var_group), 
               quiet = TRUE, geo_format = "sf") %>% 
    as_tibble() %>% 
    {if (!sf) select(., -geometry) else .} %>%
    select(-any_of(vars_to_remove)) %>% 
    rename(ID = GeoUID) %>% 
    arrange(ID) %>% 
    {if (sf) st_as_sf(.) else .}
  
  CT_census <-
    get_census(cancensus_dataset, list(CMA = montreal_code), "CT",
               vectors = c(census_income, census_education, census_identity, 
                           census_housing, census_employment, census_transport,
                           added_var_group),
               quiet = TRUE, geo_format = "sf") %>% 
    as_tibble() %>% 
    {if (!sf) select(., -geometry) else .} %>%
    select(-any_of(vars_to_remove)) %>% 
    rename(ID = GeoUID) %>% 
    arrange(ID) %>% 
    {if (sf) st_as_sf(.) else .}
  
  CSD_census <-
    get_census(cancensus_dataset, list(CMA = montreal_code), "CSD",
               vectors = c(census_income, census_education, census_identity, 
                           census_housing, census_employment, census_transport,
                           added_var_group),
               quiet = TRUE, geo_format = "sf") %>% 
    as_tibble() %>% 
    {if (!sf) select(., -geometry) else .} %>%
    select(-any_of(vars_to_remove)) %>% 
    rename(ID = GeoUID) %>% 
    arrange(ID) %>% 
    filter(ID != "2466023") %>% 
    {if (sf) st_as_sf(.) else .}
  
  list(DA_census = DA_census, CT_census = CT_census, CSD_census = CSD_census)
  
}
