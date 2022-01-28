### ACCESS MODULE ##############################################################

# UI ----------------------------------------------------------------------

access_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        select_var_UI(NS(id, "left_2"), var_list_left_access_2,
                      label = i18n$t("Timing")),
        select_var_UI(NS(id, "left_1"), var_list_left_access_1,
                      label = i18n$t("Destination type")),
        sliderInput(NS(id, "slider"), i18n$t("Time threshold"),
                    min = 10, max = 60, step = 1, value = 30,
                    width = "95%"),
        div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")),
                    hidden(zoom_UI(NS(id, "zoom"), map_zoom_levels)))))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id,
        compare_UI(NS(id, "access"), make_dropdown()),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

access_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    selection <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "access", 
      var_map = reactive(paste0("left_", "CT",
                                "_", var_left())))
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Zoom level for data is always CT
    df <- reactive("CT")
    
    # Time
    time <- reactive("2016")
    
    # Enable or disable slider + type of destination
    observeEvent({selection()
      var_right()}, {
        toggle("slider", condition = !is.na(selection()) && var_right() == " ")
        toggle("left_1-var", condition = is.na(selection()) || var_right() != " ")
      })
    
    # Left variable servers
    var_left_1 <- select_var_server("left_1", reactive(var_list_left_access_1))
    var_left_2 <- select_var_server("left_2", reactive(var_list_left_access_2))
    
    # Construct left variable string
    var_left <- reactive(paste0(var_left_1(), "_", var_left_2(), "_count"))
    
    # Compare panel
    var_right <- compare_server(
      id = "access", 
      var_list = make_dropdown(),
      df = df, 
      time = time)
    
    # If there's a selection, update the compare to " "
    observeEvent(selection(), {
      updatePickerInput(
        session,
        inputId = "access-compare-var", 
        choices = sus_translate(make_dropdown()),
        selected = " ")
    }, priority = 1)

    # Data
    data <- data_server(
      id = "access", 
      var_left = var_left,
      var_right = var_right, 
      df = df)
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore", 
      x = data, 
      var_left = var_left,
      var_right = var_right, 
      select_id = selection,
      df = df, 
      build_str_as_DA = TRUE)

    # Legend
    legend_server(
      id = "legend", 
      var_left = var_left, 
      var_right = var_right, 
      df = df,
      show_panel = reactive(is.na(selection()) || var_right() != " "))
    
    # Did-you-know panel
    dyk_server(
      id = "dyk", 
      var_left = var_left,
      var_right = var_right)
    
    # Update map in response to variable changes
    observeEvent({
      selection()
      var_left()
      var_right()
      input$slider
    }, {
      if (!is.na(selection()) && var_right() == " ") {
        
        tt_thresh <- input$slider * 60
        
        CTs_to_map <- 
          tt_matrix |> 
          filter(origin == selection(), travel_time <= tt_thresh,
                 timing == var_left_2()) |> 
          mutate(group = as.character(4 - pmax(1, ceiling(travel_time / tt_thresh * 3)))) |> 
          select(destination, group) |> 
          left_join(colour_iso, by = "group")
        
        data_to_add <-
          data() |>
          select(ID) |> 
          inner_join(CTs_to_map, by = c("ID" = "destination"))
        
        poly_to_add <-
          data() |>
          filter(ID == selection()) |>
          mutate(fill = "#00000033")
        
        mapdeck_update(map_id = NS(id, "map")) |>
          clear_path() |> 
          clear_polygon() |>
          add_polygon(
            data = CT, stroke_width = 10, stroke_colour = "#FFFFFF", id = "ID",
            fill_colour = "#EDF8E9CC", update_view = FALSE,
            layer_id = "poly_bg", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90") |>
          add_polygon(
            data = data_to_add, stroke_width = 10, stroke_colour = "#FFFFFF",
            fill_colour = "fill", update_view = FALSE, id = "ID",
            layer_id = "poly_iso", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90") |>
          add_polygon(
            data = poly_to_add, fill_colour = "fill", stroke_width = 20,
            stroke_colour = "#000000", update_view = FALSE, id = "ID",
            layer_id = "poly_highlight", auto_highlight = TRUE, 
            highlight_colour = "#FFFFFF90") |> 
          add_path(data = metro_lines, stroke_colour = "fill",
                   stroke_width = 50, update_view = FALSE)
        
      } else if (is.na(selection()) && var_right() == " ") {
        mapdeck_update(map_id = NS(id, "map"))  |>
          clear_path() |> 
          clear_polygon() |>
          clear_polygon(layer_id = "poly_bg") |>
          clear_polygon(layer_id = "poly_iso") |>
          clear_polygon(layer_id = "poly_highlight") |>
          add_sf(data = 
                   {data() |> 
                       rowwise() |> 
                       mutate(fill_val = list(which.max((
                         filter(colour_access, category == var_left_1()))$value >= var_left))) |> 
                       mutate(fill_val = if (length(fill_val) == 0) NA_integer_ else fill_val) |> 
                       ungroup() |> 
                       select(-fill) |> 
                       left_join(colour_absolute, by = "fill_val") |> 
                       mutate(fill = if_else(is.na(fill), "#B3B3BBCC", fill))},
                 stroke_width = 10, stroke_colour = "#FFFFFF", 
                 fill_colour = "fill", update_view = FALSE, id = "ID", 
                 auto_highlight = TRUE, highlight_colour = "#FFFFFF90") |> 
          add_path(data = metro_lines, stroke_colour = "fill",
                   stroke_width = 50, update_view = FALSE)
      } else {
          mapdeck_update(map_id = NS(id, "map")) |> 
          clear_polygon() |>
          clear_polygon(layer_id = "poly_bg") |>
          clear_polygon(layer_id = "poly_iso") |>
          clear_polygon(layer_id = "poly_highlight") |>
            add_polygon(
              data = data(), stroke_width = 10,
              stroke_colour = "#FFFFFF", fill_colour = "fill",
              update_view = FALSE, id = "ID", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90") |> 
            add_path(data = metro_lines, stroke_colour = "fill", 
                     stroke_width = 50, update_view = FALSE)
        }
      })

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      click <- fromJSON(input$map_polygon_click)$object$properties$id
      if (is.null(click)) {
        selection(NA)
      } else if (!is.na(selection()) && 
                 click == selection()) {
        selection(NA)
      } else selection(click)
    })
    
    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, selection(NA))

  })
}
