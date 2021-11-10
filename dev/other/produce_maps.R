# Produce left and right maps ---------------------------------------------
# Dependent script: needs 'borough' object

make_circle <- function(x) {
  borough %>% 
    st_transform(32618) %>% 
    st_union() %>% 
    st_centroid() %>% 
    {. + c(0, -3500)} %>% 
    st_set_crs(32618) %>% 
    st_buffer(26000) %>% 
    st_intersection(st_transform(st_set_agr(x, "constant"), 32618), .) %>% 
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

walk(c("borough", "CT", "DA", "grid", "building", "street"), function(scale) {
  
  data <- get(scale)
  
  data <- make_circle(data)
  var_list <- c(" ", str_subset(names(data), "q3"))
  
  walk(var_list, ~{
    
    # Left map
    if (.x == " ") {
      
      if (scale == "grid") {
        p <-
          data %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "building") {
        p <-
          data %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(fill = "#CABED0", color = "#CABED0", size = 0.05) +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "street") {
        p <-
          data %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(colour = "#CABED0", size = 0.1) +
          theme_map() +
          theme(legend.position = "none")
        
      } else {
        p <-
          data %>% 
          ggplot() +
          geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
          theme_map() +
          theme(legend.position = "none")
        
      }
      
      {wrap_elements(shadow_left) + 
          inset_element(p, 0.18, 0.148, 0.83, 0.85, align_to = "full")} %>% 
        ggsave("out.png", ., width = 4, height = 4)
      
    } else {
      
      if (scale == "grid") {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(aes(fill = as.factor(var)), color = "white", size = 0.01) +
          scale_fill_manual(values = colour_scale[1:3], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "building") {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(aes(fill = as.factor(var), colour = as.factor(var)), 
                  size = 0.05) +
          scale_fill_manual(values = colour_scale[1:3], na.value = "grey70") +
          scale_colour_manual(values = colour_scale[1:3], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "street") {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(aes(colour = as.factor(var)), size = 0.1) +
          scale_colour_manual(values = colour_scale[1:3], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      } else {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(aes(fill = as.factor(var)), color = "white", size = 0.01) +
          scale_fill_manual(values = colour_scale[1:3], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      }
      
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
      
      if (scale == "grid") {
        p <-
          data %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "building") {
        p <-
          data %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(fill = "#CABED0", color = "#CABED0", size = 0.05) +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "street") {
        p <-
          data %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(colour = "#CABED0", size = 0.1) +
          theme_map() +
          theme(legend.position = "none")
        
      } else {
        p <-
          data %>% 
          ggplot() +
          geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
          theme_map() +
          theme(legend.position = "none")
        
      }
      
      {wrap_elements(shadow_right) + 
          inset_element(p, 0.17, 0.148 , 0.818, 0.844, align_to = "full")} %>% 
        ggsave("out.png", ., width = 4, height = 4)
      
    } else {
      
      if (scale == "grid") {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(aes(fill = as.factor(var)), color = "white", size = 0.01) +
          scale_fill_manual(values = colour_scale[4:6], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "building") {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(aes(fill = as.factor(var), colour = as.factor(var)), 
                  size = 0.05) +
          scale_fill_manual(values = colour_scale[4:6], na.value = "grey70") +
          scale_colour_manual(values = colour_scale[4:6], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      } else if (scale == "street") {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(data = circle_borough, fill = "grey70", color = "white", 
                  size = 0.01) +
          geom_sf(aes(colour = as.factor(var)), size = 0.1) +
          scale_colour_manual(values = colour_scale[4:6], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      } else {
        p <-
          data %>% 
          select(var = all_of(.x)) %>% 
          ggplot() +
          geom_sf(aes(fill = as.factor(var)), color = "white", size = 0.01) +
          scale_fill_manual(values = colour_scale[4:6], na.value = "grey70") +
          theme_map() +
          theme(legend.position = "none")
        
      }
      
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