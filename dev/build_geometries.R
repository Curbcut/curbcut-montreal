#### Build geometries ##########################################################

library(tidyverse)
library(sf)
library(qs)
library(cancensus)


# Import DA, CT and borough geometries ------------------------------------

# Variables to be selected from get_census
var_select <- c("CTUID" = "CT_UID", "CSDUID" = "CSD_UID", "name" = "name",
                "population" = "Population", "households" = "Households")

# Download DAs
DA <- 
  get_census("CA16", list(CMA = "24462"), "DA", geo_format = "sf", 
             quiet = TRUE) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  select(ID = GeoUID, any_of(var_select), geometry) %>% 
  arrange(ID) %>% 
  mutate(name = ID, .after = ID) %>% 
  st_set_agr("constant")

# Download CTs
CT <-
  get_census("CA16", list(CMA = "24462"), "CT", geo_format = "sf", 
             quiet = TRUE) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  select(ID = GeoUID, any_of(var_select), geometry) %>% 
  arrange(ID) %>% 
  mutate(name = ID, .after = ID) %>% 
  st_set_agr("constant")

# Download CSDs
CSD <-
  get_census("CA16", list(CMA = "24462"), "CSD", geo_format = "sf", 
             quiet = TRUE) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  select(ID = GeoUID, any_of(var_select), geometry) %>% 
  arrange(ID) %>% 
  filter(name != "Montréal (V)") %>%
  mutate(type = "City", .after = name) %>%
  mutate(name = str_replace_all(name, "\\(PE\\)", "--parish municipality")) %>% 
  mutate(name = str_remove(name, " \\(.*\\)")) %>% 
  st_set_agr("constant")

rm(var_select)

# Get CMA boundary for clipping boroughs
CMA <- 
  get_census("CA16", list(CMA = "24462"), geo_format = "sf", quiet = TRUE) %>% 
  st_set_agr("constant")

# Import boroughs from City and clip to CMA geometry
borough <-
  read_sf("dev/data/montreal_boroughs_2019.shp") %>%
  st_set_agr("constant") %>% 
  st_transform(32618) %>% 
  st_intersection(st_transform(CMA, 32618)) %>%
  st_transform(4326) %>% 
  select(name = NOM, type = TYPE, geometry) %>%
  mutate(type = if_else(type == "Arrondissement", "Borough", "City")) %>% 
  st_cast("MULTIPOLYGON")

# Get CSDs whose geometries will be replaced with borough geometries
replacements <- 
  CSD %>% 
  st_transform(32618) %>% 
  st_centroid() %>% 
  st_join(select(st_transform(borough, 32618), new_name = name),
          left = FALSE) %>% 
  st_drop_geometry() %>% 
  inner_join(borough, ., by = c("name" = "new_name")) %>% 
  select(-name.y, -type.y) %>% 
  relocate(geometry, .after = last_col()) %>% 
  relocate(ID, .before = name) %>% 
  rename(type = type.x)

# Replace geometries
CSD <- 
  CSD %>% 
  filter(!ID %in% replacements$ID) %>% 
  bind_rows(replacements) %>% 
  arrange(ID)

# Filter borough to remaining cases
borough <- 
  borough %>% 
  filter(!name %in% replacements$name)

rm(CMA, replacements)

# Join DAs to remaining boroughs by centroid
borough_join <-
  DA %>%
  filter(CSDUID == "2466023") %>% 
  select(ID, geometry) %>% 
  st_transform(32618) %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  st_join(st_transform(borough, 32618), left = FALSE) %>%
  select(ID, name) %>%
  st_drop_geometry()

# Find any DAs in Montréal which didn't join
leftovers <-
  DA %>% 
  filter(!ID %in% borough_join$ID) %>% 
  filter(CSDUID == "2466023")

# If there is just one leftover, manually add it to Ahuntsic-Cartierville
if (nrow(leftovers) == 1) {
  borough_join <- 
    leftovers %>% 
    st_drop_geometry() %>% 
    select(ID) %>% 
    mutate(name = "Ahuntsic-Cartierville") %>% 
    bind_rows(borough_join)
  }

# Redo data processing for DAs in borough_join
borough <- 
  DA %>% 
  select(-name) %>% 
  st_drop_geometry() %>%
  inner_join(borough_join, by = "ID") %>%
  group_by(CSDUID, name) %>% 
  summarize(across(c(population:households), sum, na.rm = TRUE), 
            .groups = "drop") %>% 
  left_join(borough, ., by = "name") %>% 
  relocate(geometry, .after = last_col()) %>% 
  mutate(ID = paste0(CSDUID, "_", seq_along(name)), .before = name) %>% 
  select(-CSDUID) %>% 
  bind_rows(CSD) %>% 
  arrange(ID) %>% 
  st_set_agr("constant")

# Update CSDUID in DA and CT
borough_join <- 
  borough %>% 
  st_drop_geometry() %>% 
  select(ID, name) %>%
  left_join(borough_join, ., by = "name") %>% 
  select(ID = ID.x, CSDUID_new = ID.y)

DA <- 
  DA %>% 
  left_join(borough_join, by = "ID") %>% 
  mutate(CSDUID = coalesce(CSDUID_new, CSDUID)) %>% 
  select(-CSDUID_new)

CT <- 
  DA %>% 
  st_drop_geometry() %>% 
  select(ID = CTUID, CSDUID_new = CSDUID) %>% 
  distinct() %>% 
  filter(str_detect(CSDUID_new, "_")) %>% 
  group_by(ID) %>% 
  # Manual fix for Pierrefonds
  slice(1) %>% 
  ungroup() %>% 
  left_join(CT, ., by = "ID") %>% 
  mutate(CSDUID = coalesce(CSDUID_new, CSDUID)) %>% 
  select(-CSDUID_new)

rm(borough_join, CSD, leftovers)


# Add borough/CSD names ---------------------------------------------------

borough <- 
  borough %>% 
  rename(name_2 = type)

CT <- 
  CT %>% 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name), 
            by = "CSDUID") %>% 
  relocate(name_2, .after = name)

DA <- 
  DA %>% 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name), 
            by = "CSDUID") %>% 
  relocate(name_2, .after = name)


# Import grid geometries --------------------------------------------------

grid <-
  read_sf("dev/data/climate_shp/VulnerabilitÇ_secheresses_2016.shp") %>% 
  st_zm() %>% 
  st_make_valid() %>% 
  st_transform(32618) %>% 
  transmute(ID = seq_along(geometry)) %>% 
  filter(units::drop_units(st_area(geometry)) > 10) %>% 
  distinct(geometry, .keep_all = TRUE) %>% 
  st_transform(4326) %>% 
  st_set_agr("constant")


# Geocode grid centroids --------------------------------------------------

source("dev/grid_geocode.R")


# Add CSDUID, population and households to grid ---------------------------

DA_data <- 
  DA %>% 
  st_transform(32618) %>% 
  select(ID, population, households) %>% 
  mutate(area = st_area(geometry), .before = geometry) %>% 
  st_set_agr("constant")

grid_census <-
  grid %>% 
  select(ID) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_data) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(across(population:households, 
                ~{.x * units::drop_units(area_prop)})) %>% 
  select(ID, population, households, geometry) %>% 
  st_drop_geometry() %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  summarize(across(population:households, sum, na.rm = TRUE))

grid <- 
  grid %>% 
  left_join(grid_census, by = "ID") %>% 
  relocate(geometry, .after = last_col()) %>% 
  st_set_agr("constant")

borough_index <- 
  grid %>% 
  st_transform(32618) %>% 
  st_centroid() %>% 
  st_nearest_feature(st_transform(borough, 32618))

grid <- 
  grid %>% 
  mutate(CSDUID = map_chr(borough_index, ~borough$ID[.x]), .after = name) %>% 
  st_set_agr("constant")

grid <- 
  grid %>% 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name), 
            by = "CSDUID") %>% 
  relocate(name_2, .after = name)

rm(borough_index, DA_data, grid_census)


# Add topic variables -----------------------------------------------------

var_exp <- tibble(var_code = character(), var_name = character(),
                                explanation = character())
source("dev/census.R")
source("dev/canale.R")
source("dev/climate_risk.R")
# source("dev/ped.R")


# Save data files ---------------------------------------------------------

qsavem(borough, CT, DA, file = "data/census.qsm")
qsave(grid, file = "data/grid.qs")
qsave(var_exp, file = "data/var_exp.qs")


# Produce left and right maps ---------------------------------------------

library(patchwork)
source("dev/colours.R")

make_circle <- function(x) {
  borough %>% 
    st_transform(32618) %>% 
    st_union() %>% 
    st_centroid() %>% 
    {. + c(0, -3500)} %>% 
    st_set_crs(32618) %>% 
    st_buffer(26000) %>% 
    st_intersection(st_transform(x, 32618), .) %>% 
    st_transform(4326)
}

circle_borough <- make_circle(borough)

theme_map <- function(...) {
  default_bg <- "transparent"
  default_fc <- "black"
  default_ff <- "Helvetica"
  
  theme_minimal() +
    theme(
      text = element_text(family = default_ff, color = default_fc),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = default_bg, color = NA),
      panel.background = element_rect(fill = default_bg, color = NA),
      legend.background = element_rect(fill = default_bg, color = NA),
      legend.position = "none",
      plot.margin = unit(c(0, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 22, hjust = 0, color = default_fc),
      plot.title = element_text(size = 15, hjust = 0.5, color = default_fc),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5, color = default_fc, 
        margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm")),
      plot.caption = element_text(size = 7, hjust = .5, 
                                  margin = margin(t = 0.2, b = 0, unit = "cm"),
                                  color = "#939184"),
      ...)
}

shadow_left <- png::readPNG("www/dropshadow_left.png", native = TRUE)
legend_left <- png::readPNG("www/univariate_left.png", native = TRUE)
shadow_right <- png::readPNG("www/dropshadow_right.png", native = TRUE)
legend_right <- png::readPNG("www/univariate_right.png", native = TRUE)

walk(c("borough", "CT", "DA", "grid"), function(scale) {
  
  data <- get(scale)
  
  data <- make_circle(data)
  var_list <- c(" ", str_subset(names(data), "q3"))
  
  walk(var_list, ~{
    
    # Left map
    if (.x == " ") {
      
      p <-
        data %>% 
        ggplot() +
        {if (scale == "grid") geom_sf(data = circle_borough, fill = "grey70", 
                                      color = "white", size = 0.01)} +
        geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
        theme_map() +
        theme(legend.position = "none")
      
      {wrap_elements(shadow_left) + 
          inset_element(p, 0.18, 0.148, 0.83, 0.85, align_to = "full")} %>% 
        ggsave("out.png", ., width = 4, height = 4)
      
    } else {
      
      p <-
        data %>% 
        select(var = all_of(.x)) %>% 
        ggplot() +
        {if (scale == "grid") geom_sf(data = circle_borough, fill = "grey70", 
                                      color = "white", size = 0.01)} +
        geom_sf(aes(fill = as.factor(var)), color = "white", size = 0.01) +
        scale_fill_manual(values = colour_scale[1:3], na.value = "grey70") +
        theme_map() +
        theme(legend.position = "none")
      
      {wrap_elements(shadow_left) + 
          inset_element(p, 0.18, 0.148, 0.83, 0.85, align_to = "full") +
          inset_element(wrap_elements(
            full = legend_left) + 
              theme(plot.background = element_rect(fill = "transparent", 
                                                   colour = "transparent")), 
            0.2, 0.25, 0.46, 0.5, align_to = "full")} %>% 
        ggsave("out.png", ., width = 4, height = 4)
      
    }
    
    img <- png::readPNG("out.png")
    img <- img[251:950, 251:950,]
    png::writePNG(img, paste0("www/maps/left_", scale, "_", 
                              sub("_q3", "", .x), ".png"))
    
    
    # Right map
    if (.x == " ") {
      
      p <-
        data %>% 
        ggplot() +
        {if (scale == "grid") geom_sf(data = circle_borough, fill = "grey70", 
                                      color = "white", size = 0.01)} +
        geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
        theme_map() +
        theme(legend.position = "none")
      
      {wrap_elements(shadow_right) + 
          inset_element(p, 0.17, 0.148 , 0.818, 0.844, align_to = "full")} %>% 
        ggsave("out.png", ., width = 4, height = 4)
      
    } else {
      
      p <-
        data %>% 
        select(var = all_of(.x)) %>% 
        ggplot() +
        {if (scale == "grid") geom_sf(data = circle_borough, fill = "grey70", 
                                      color = "white", size = 0.01)} +
        geom_sf(aes(fill = as.factor(var)), color = "white", size = 0.01) +
        scale_fill_manual(values = colour_scale[4:6], na.value = "grey70") +
        theme_map() +
        theme(legend.position = "none")
      
      {wrap_elements(shadow_right) + 
          inset_element(p, 0.17, 0.148 , 0.818, 0.844, align_to = "full") +
          inset_element(wrap_elements(
            full = legend_right) + 
              theme(plot.background = element_rect(fill = "transparent", 
                                                   colour = "transparent")), 
            0.54, 0.245, 0.8, 0.495, align_to = "full")} %>% 
        ggsave("out.png", ., width = 4, height = 4)
      
    }
    
    img <- png::readPNG("out.png")
    img <- img[251:950, 251:950,]
    png::writePNG(img, paste0("www/maps/right_", scale, "_", 
                              sub("_q3", "", .x), ".png"))
    
  })
  
})

unlink("out.png")
rm(circle_borough, make_circle, theme_map)
