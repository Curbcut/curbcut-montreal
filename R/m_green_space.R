### GREEN SPACE MODULE ########################################################

# UI ----------------------------------------------------------------------

green_space_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Side bar
      sidebar_UI(NS(id, "sidebar"),
                 select_var_UI(NS(id, "left_groupings"), 
                               green_space_groupings,
                               label = i18n$t("Grouping")),
                 select_var_UI(NS(id, "left_type"), green_space_type,
                               label = i18n$t("Type of green space")),
                 div(class = "bottom_sidebar",
                     tagList(legend_UI(NS(id, "legend")),
                             zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    fillCol(
      
      # Map
      div(class = "mapdeck_div", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(id, compare_UI(NS(id, "green_space"), make_dropdown()),
                  div(class = "explore_dyk",
                      explore_UI(NS(id, "explore")), 
                      dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

green_space_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    selection <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "green_space"#, 
      # Adding a small map for the green_space df is needed
      # var_map = reactive(paste0("left_", df(), "_", "green_space"))
      )
    
    # If green space isn't selected, choropleth is TRUE 
    choropleth <- reactive(var_left_groupings() != " ")
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Zoom level for data
    df <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = map_zoom_levels)
    
    # Left variable servers
    var_left_groupings <- select_var_server("left_groupings", 
                                    reactive(green_space_groupings))
    var_left_type <- select_var_server("left_type", 
                                    reactive(green_space_type))
    
    # Construct left variable string
    var_left <- reactive(str_remove(paste("green_space", var_left_type(), 
                                          var_left_groupings(), sep = "_"), 
                                    "_ "))
    
    # Compare panel
    var_right <- compare_server(
      id = "green_space", 
      var_list = make_dropdown(),
      df = df,
      time = reactive("2016"),
      show_panel = choropleth)
    
    # Data
    data <- reactive({
      if (choropleth()) {
        data_server(id = "green_space", var_left = var_left,
                    var_right = var_right, df = df, zoom = zoom)() %>% 
          {if (nrow(.) == nrow(borough))
            filter(., ID %in% island_csduid)
            else filter(., CSDUID %in% island_csduid)}
      } else {
        green_space %>%
          {if (var_left_type() != "total")
            filter(., type_1 == var_left_type()) else .}
      }
    })
    
    # Green space info table if choropleth is FALSE
    green_space_info_table <- reactive({
      
      if (!choropleth()) {
      
      pnr <- function(data) prettyNum(round(data/1e+6, digits = 2), big.mark = ",")
      
      if (is.na(selection())) {
        type <- if (nrow(data()) == nrow(green_space)) {
          str_glue(sus_translate("a total of {nrow(data())} green spaces"))
        } else {
          str_glue(sus_translate("{nrow(data())} `{unique(data()$type)}`"))
        }
        
        HTML(str_glue(
          paste0("At the scale of the City of Montreal, there are {type}, ",
                 "combining {pnr(sum(data()$area))} km^2. Their area range from ",
                 "{pnr(min(data()$area))} and {pnr(max(data()$area))} km^2, with ",
                 "an average of {pnr(mean(data()$area))} km^2.")))
      } else {
        data <-
          data() |>
          mutate(total_rank = rank(-area)) |>
          group_by(CSDUID) |>
          mutate(borough_rank = rank(-area))
        
        type <- if (nrow(data()) == nrow(green_space)) {
          str_glue(sus_translate("green space"))
        } else {
          str_glue(sus_translate("`{unique(data()$type)}`"))
        }
        
        z <- data[data$ID == selection(), ]
        total_rank <- pull(z, total_rank)
        borough_rank <- pull(z, borough_rank)
        borough <- filter(borough, ID == pull(z, CSDUID))$name
        
        ordinal_form <- function(data) {
          if (data > 20) {
            if (data %% 100 %in% c(11 , 12, 13)) {
              form <- "th "
            } else {
              form <- switch(as.character(data %% 10), "1" = "st ", "2" = "nd ",
                             "3" = "rd ", "th ")
            }
            paste0(data, form)
          } else {
            switch(as.character(data), "1" = "", "2" = "second ",
                   "3" = "third ", "4" = "fourth ", "5" = "fifth ", "6" = "sixth ",
                   "7" = "seventh ", "8" = "eighth ", "9" = "ninth ", "10" = "tenth ",
                   paste0(as.character(data), "th "))
          }
        }
        
        total_rank <- ordinal_form(total_rank)
        borough_rank <- ordinal_form(borough_rank)
        
        HTML(str_glue(
          paste0("<p><b>{z$name}</b><p>",
                 "<p>The green space {z$name} is a `{z$type}` of ",
                 "{prettyNum(z$area, big.mark = ',')} m^2. It is ",
                 "categorized as a `{z$type_2}`",
                 "and is of `{z$property}` property. Its ",
                 "management entity is `{z$management}`.</p>",
                 "<p>It is the {total_rank}biggest {type} in the ",
                 "City, and the {borough_rank} largest in {borough}.</p>")))
      }}})
    
    # Green space graph if choropleth() is FALSE
    green_space_explore_graph <- reactive({
      if (!choropleth()) {
        if (is.na(selection())) {
          ggplot(data(), aes(area)) +
            geom_histogram(aes(fill = fill), alpha = 0.5, bins = 25) +
            scale_fill_manual(values = rev(col_left_5), na.translate = FALSE) + 
            labs(data = "Green space area (log10)", y = NULL) + 
            scale_x_log10() +
            theme_minimal() +
            theme(legend.position = "none", panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.title = element_text(size = 8))
        } else {
          select_id <- selection()
          
          ggplot(data(), aes(area)) +
            geom_histogram(aes(fill = round(area) == 
                                 round(area[ID == select_id])),
                           bins = 25) +
            scale_fill_manual(values = col_left_5[c(1, 5)], 
                              na.translate = FALSE) +
            labs(data = "Green space area (log10)", y = NULL) + 
            scale_x_log10() +
            theme_minimal() +
            theme(legend.position = "none", panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.title = element_text(size = 8))
        }}})
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      x = data,
      var_left = var_left,
      var_right = var_right,
      select = selection,
      df = df,
      standard = choropleth,
      info = green_space_info_table,
      graph = green_space_explore_graph)
    
    # Legend
    legend_server(
      id = "legend",
      var_left = var_left,
      var_right = var_right,
      df = df,
      show_panel = choropleth)
    
    # Did-you-know panel
    dyk_server(
      id = "dyk", 
      var_left = var_left,
      var_right = var_right)
    
    # Update map in response to variable changes or zooming
    observeEvent(data(),
                 map_change(NS(id, "map"), df = data, zoom = df,
                            overthrow_width = !choropleth()))
    
    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (jsonlite::fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) selection(NA) else selection(lst)
    })
    
    # Clear poly_selected on data change
    observeEvent(data(), selection(NA), ignoreInit = TRUE)
    
    # Update map in response to poly change
    observeEvent(selection(), {
      if (!is.na(selection())) {
        width <- switch(df(), "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() |> 
          filter(ID == selection()) |> 
          mutate(fill = substr(fill, 1, 7))
        
        mapdeck_update(map_id = NS(id, "map")) |> 
          add_polygon(
            data = data_to_add, elevation = 5, fill_colour = "fill", 
            update_view = FALSE, layer_id = "poly_highlight", 
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      } else {
        mapdeck_update(map_id = NS(id, "map")) |> 
          clear_polygon(layer_id = "poly_highlight")
      }
    })
    
    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, selection(NA))
    
  })
}
