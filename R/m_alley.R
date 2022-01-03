### GREEN ALLEY MODULE #########################################################

# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        checkboxInput(inputId = NS(id, "focus_visited"), label = i18n$t(
          "Focus on green alleys visited by our team")),
        select_var_UI(NS(id, "left"), var_list_left_alley, 
                      label = i18n$t("Grouping")), 
        div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")), 
                    zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(id, compare_UI(NS(id, "alley"), make_dropdown()),
                  div(class = "explore_dyk",
                      explore_UI(NS(id, "explore")), 
                      dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)
  )
  )
}


# Server ------------------------------------------------------------------

alley_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    selection <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "alley")
    
    # Enter in choropleth() depending on var_left selection
    choropleth <- reactive(!(var_left() == " " || input$focus_visited))
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token,
      zoom = 11, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Zoom level for data
    df <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = map_zoom_levels)
    
    # Zoom level to use when focus is on
    focus_alley_zoom <- reactive({
      case_when(input$map_view_change$zoom >= 13 ~ 15,
                TRUE ~ input$map_view_change$zoom * -15 + 220)})
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- select_var_server("left", reactive(var_list_left_alley))
    
    # Compare panel
    var_right <- compare_server(
      id = "alley", 
      var_list = var_list_right_alley, 
      df = df, 
      show_panel = choropleth,
      time = time)
    
    # Data 
    data_choropleth <- data_server(
      id = "alley", 
      var_left = var_left, 
      var_right = var_right, 
      df = df,
      zoom = zoom,
      island_only = TRUE)
    
    data <- reactive({
      if (choropleth()) {
        data_choropleth()
      } else {
        list(visited = alleys[alleys$visited,],
             non_visited = alleys[!alleys$visited,])
      }
    })
    
    # Legend
    legend_server(
      id = "legend", 
      var_left = var_left, 
      var_right = var_right, 
      df = df,
      show_panel = choropleth)
    
    # Extract alley name and photo_ID for the following click event
    alley_info <- reactive({
      if (selection() %in% alleys[alleys$visited,]$ID) {
        x <-
          alleys %>%
          st_drop_geometry() %>%
          filter(ID == selection()) %>%
          mutate(name = str_glue(sus_translate(paste0(
            "<p><b>{str_to_title(name)} in ",
            "{name_2}</b></p>")))) %>%
          select(-ID, -CSDUID, -visited, -name_2, -fill) %>%
          select_if(~sum(!is.na(.)) > 0) %>%
          {if (nrow(.) > 0) as.list(.) else NULL}
        
        out <- alley_alleys_text(x)
        
        list(name = out$name, 
             photo_ID = out$photo_ID)
      }
    })
    
    # Popup the image if it's clicked on
    onclick(
      "explore-explore-alley_img", 
      { showModal(modalDialog(
        title = HTML(alley_info()$name),
        HTML(paste0('<img src="', alley_info()$photo_ID, '", width = 100%>')),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))})
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      x = data,
      var_left = var_left,
      var_right = var_right,
      select = selection,
      df = df,
      standard = choropleth,
      custom_info = alley_info_table)
    
    # Update map in response to user input
    observe({
      if (!choropleth()) {
        if (input$focus_visited) {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon() %>%
            clear_polygon(layer_id = "borough_info") %>%
            clear_polygon(layer_id = "poly_highlight") %>%
            add_polygon(data = data()$non_visited,
                        stroke_width = 15, stroke_colour = "#CFCFCF",
                        fill_colour = "#CFCFCF", layer_id = "alleys_void",
                        update_view = FALSE, id = "ID", auto_highlight = FALSE) %>%
            add_polygon(data = data()$visited,
                        stroke_width = focus_alley_zoom(), stroke_colour = "fill",
                        layer_id = "alleys_visited",
                        update_view = FALSE, id = "ID", auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90",
                        legend = alley_legend_en)
        } else {
          # Exact same as the initial
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon() %>%
            # For some reason, legend is sticky!
            clear_legend(layer_id = "alleys_visited") %>%
            clear_polygon(layer_id = "alleys_visited") %>%
            add_polygon(data = borough[borough$ID %in% alley_text$ID,],
                        stroke_width = 10, stroke_colour = "#000000",
                        fill_colour = "#FFFFFF10", update_view = FALSE, id = "ID",
                        layer_id = "borough_info", auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90") %>%
            add_polygon(data = data()$non_visited,
                        stroke_width = 15, stroke_colour = "#007700",
                        fill_colour = "#00FF00", layer_id = "alleys_void",
                        update_view = FALSE, id = "ID", auto_highlight = FALSE) %>%
            add_polygon(data = data()$visited,
                        stroke_width = 15, stroke_colour = "#007700",
                        fill_colour = "#00FF00", layer_id = "alleys_visited",
                        update_view = FALSE, id = "ID", auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90")
          
        }
      } else {
        width <- switch(zoom(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        
        mapdeck_update(map_id = NS(id, "map")) |> 
          clear_polygon("alleys_void") |> 
          clear_polygon("alleys_visited") |> 
          clear_polygon("borough_info") |> 
          clear_polygon("poly_highlight") |> 
          add_polygon(
            data = data(), stroke_width = width,
            stroke_colour = "#FFFFFF", fill_colour = "fill",
            update_view = FALSE, id = "ID", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
      }
    })
    
    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (jsonlite::fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) selection(NA) else selection(lst)
    })
    
    # Update map in response to poly_selected change outside of choropleth()
    observeEvent(selection(), {
      if (!choropleth() && !input$focus_visited) {
        if (!is.na(selection())) {
          data_to_add <-
            borough %>%
            filter(ID == selection())
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            add_polygon(
              data = data_to_add, stroke_width = 10, stroke_colour = "#000000",
              fill_colour = "#00770030", update_view = FALSE,
              layer_id = "poly_highlight", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF02")
        } else {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_highlight")
        }
      } else if (choropleth()) {
        if (!is.na(selection())) {
          width <- switch(zoom(), "borough" = 100, "CT" = 10, 2)
          data_to_add <-
            data() %>%
            filter(ID == selection()) %>%
            mutate(fill = substr(fill, 1, 7))
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            add_polygon(
              data = data_to_add, elevation = 5, fill_colour = "fill", 
              update_view = FALSE, layer_id = "poly_highlight", 
              auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
        } else {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_highlight")
        }
      }
    })
    
    # Hide explore panel
    observeEvent(input$hide, {
      
      if (input$hide %% 2 == 0) {
        shinyjs::show(id = "alley_explore")
        txt <- sus_translate("Hide")
      } else {
        shinyjs::hide(id = "alley_explore")
        txt <- sus_translate("Show")
      }
      updateActionButton(session, "hide", label = txt)
      
    })
    
    # If we aren't in choropleth, toggle off the legend/zoom
    observeEvent({choropleth()
      input$focus_visited}, {
        shinyjs::toggle("zoom-auto", condition = choropleth() || !input$focus_visited)
        shinyjs::toggle("zoom-slider", condition = choropleth() || !input$focus_visited)
        shinyjs::toggle("legend-legend_render", condition = choropleth() || !input$focus_visited)
        # If focus is clicked, toggle off the dropdown menu
        shinyjs::toggle("left-var", condition = !input$focus_visited)
      })
    
    # Hook up "Clear selection" button and other variables that clears it
    observeEvent({input$`explore-clear_selection`
      choropleth()
      input$focus_visited}, selection(NA))

  })
}