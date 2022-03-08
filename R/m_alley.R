### GREEN ALLEY MODULE #########################################################

# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  ns_id <- "alley"
  
  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
      checkbox_UI(id = NS(id, ns_id), 
                  label = sus_translate("Focus on green alleys visited by our team")),
      select_var_UI(NS(id, ns_id), var_list = var_list_left_alley, 
                    label = sus_translate("Grouping"))
      ), 
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, ns_id)), 
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
    
    # Right panel
    right_panel(
      id = id, 
      compare_UI(NS(id, ns_id), make_dropdown()),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
    
  ))
}


# Server ------------------------------------------------------------------

alley_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "alley"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(11, map_zoom_levels))
    select_id <- reactiveVal(NA)
    select_id <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(id = ns_id, x = "alley")
    
    # Enter in choropleth() depending on var_left select_id
    choropleth <- reactive(!(var_left() == " " || focus_visited()))
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token,
      zoom = 11, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Click reactive
    observeEvent(input$map_polygon_click, {
      select_id(get_click(input$map_polygon_click))})
    
    # Zoom level for data
    df <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Zoom level to use when focus is on
    focus_alley_zoom <- reactive({
      case_when(input$map_view_change$zoom >= 13 ~ 15,
                TRUE ~ input$map_view_change$zoom * -15 + 220)})
    
    # Focus on visited alleys
    focus_visited <- checkbox_server(id = ns_id)
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- select_var_server(id = ns_id, 
                                  var_list = reactive(var_list_left_alley))
    
    # Compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = make_dropdown(), 
      df = df, 
      show_panel = choropleth,
      time = time)
  
    # Data
    data <- reactive({
      if (choropleth()) {
        get_data(df(), var_left(), var_right(), island = TRUE)
        # print(get_data(df(), var_left(), var_right(), island = TRUE))
      } else {
        list(visited = alleys[alleys$visited, ],
             non_visited = alleys[!alleys$visited, ])
      }
    })
    
    # Legend
    # legend_server(
    #   id = ns_id, 
    #   var_left = var_left, 
    #   var_right = var_right, 
    #   df = df)
    
    # Extract alley name and photo_ID for the following click event
    alley_info <- reactive({
      if (select_id() %in% alleys[alleys$visited,]$ID) {
        x <-
          alleys |>
          st_drop_geometry() |>
          filter(ID == select_id()) |>
          mutate(name = sus_translate(
            "<p><b>{str_to_title(name)} in ",
            "{name_2}</b></p>")) |>
          select(-ID, -CSDUID, -visited, -name_2, -fill) |>
          select_if(~sum(!is.na(.)) > 0) %>%
          {if (nrow(.) > 0) as.list(.) else NULL}
        
        out <- alley_alleys_text(x)
        
        list(name = out$name, 
             photo_ID = out$photo_ID)
      }
    })
    
    # Popup the image if it's clicked on
    # onclick(
    #   "alley-explore-alley_img", 
    #   { showModal(modalDialog(
    #     title = HTML(alley_info()$name),
    #     HTML(paste0('<img src="', alley_info()$photo_ID, '", width = 100%>')),
    #     easyClose = TRUE,
    #     size = "l",
    #     footer = NULL
    #   ))})
    
    # Explore panel
    # explore_content <- explore_server(
    #   id = ns_id,
    #   x = data,
    #   var_left = var_left,
    #   var_right = var_right,
    #   select_id = select_id,
    #   df = df,
    #   standard = choropleth,
    #   custom_info = alley_info_table)
    
    # Update map in response to user input
    observeEvent({
      choropleth()
      focus_visited()
      data()
      select_id()
      df()
      zoom()
      focus_alley_zoom()
      }, {
      if (!choropleth()) {
        if (focus_visited()) {
          mapdeck_update(map_id = NS(id, "map")) |>
            clear_polygon() |>
            clear_polygon(layer_id = "highlight") |>
            clear_polygon(layer_id = "borough_info") |>
            clear_polygon(layer_id = "poly_highlight") |>
            add_polygon(data = data()$non_visited,
                        stroke_width = 15, stroke_colour = "#CFCFCF",
                        fill_colour = "#CFCFCF", layer_id = "alleys_void",
                        update_view = FALSE, id = "ID",
                        auto_highlight = FALSE) |>
            add_polygon(data = data()$visited,
                        stroke_width = focus_alley_zoom(),
                        stroke_colour = "fill", layer_id = "alleys_visited",
                        update_view = FALSE, id = "ID", auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90",
                        legend = alley_legend_en)
        } else {
          # Exact same as the initial
          mapdeck_update(map_id = NS(id, "map")) |>
            clear_polygon() |>
            clear_polygon(layer_id = "highlight") |>
            # For some reason, legend is sticky!
            clear_legend(layer_id = "alleys_visited") |>
            clear_polygon(layer_id = "alleys_visited") |>
          add_polygon(data = borough[borough$ID %in% alley_text$ID,],
                      stroke_width = 10, stroke_colour = "#000000",
                      fill_colour = "#FFFFFF10", update_view = FALSE,
                      id = "ID", layer_id = "borough_info",
                      auto_highlight = TRUE,
                      highlight_colour = "#FFFFFF90") |>
          add_polygon(data = data()$non_visited,
                      stroke_width = 15, stroke_colour = "#007700",
                      fill_colour = "#00FF00", layer_id = "alleys_void",
                      update_view = FALSE, id = "ID",
                      auto_highlight = FALSE) |>
          add_polygon(data = data()$visited,
                      stroke_width = 15, stroke_colour = "#007700",
                      fill_colour = "#00FF00", layer_id = "alleys_visited",
                      update_view = FALSE, id = "ID", auto_highlight = TRUE,
                      highlight_colour = "#FFFFFF90")
        }
      } else {
        
        walk(c("alleys_void", "alleys_visited", "borough_info",
               "poly_highlight"), ~{
                 mapdeck_update(map_id = NS(id, "map")) |> clear_polygon(.x)})
        
        width <- 
          switch(zoom(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        width_2 <- 
          switch(df(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
        width <- min(width, width_2)
        
        # Set transparency based on zoom
        col_zoom <- colour_alpha[names(colour_alpha) == zoom()]
        dat <- mutate(data(), fill = paste0(fill, col_zoom))
        
        mapdeck_update(map_id = NS(id, "map")) |>
          add_polygon(
            data = dat, stroke_width = width,
            stroke_colour = "#FFFFFF", fill_colour = "fill",
            update_view = FALSE, id = "ID", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF80")
        
        if (!is.na(select_id())) {
          # SELECTION
          width <- 
            switch(zoom(), "borough" = 150, "CT" = 20, "DA" = 4, "grid" = 4, 4)
          width_2 <- 
            switch(df(), "borough" = 150, "CT" = 20, "DA" = 4, "grid" = 4, 4)
          width <- min(width, width_2)
          
          # Set transparency based on zoom
          col_zoom <- colour_alpha[names(colour_alpha) == zoom()]
          
          data_to_add <-
            data() |>
            filter(ID == select_id()) |>
            mutate(fill = paste0(fill, col_zoom))
          
          if (nrow(data_to_add) != 0) {
            mapdeck_update(map_id = NS(id, "map")) |>
              add_polygon(
                data = data_to_add, fill_colour = "fill", 
                stroke_colour = "#000000", stroke_width = width, 
                update_view = FALSE, layer_id = "highlight", 
                auto_highlight = TRUE, highlight_colour = "#FFFFFF80")
          }
        } else {
          mapdeck_update(map_id = NS(id, "map")) |>
            clear_polygon(layer_id = "highlight")
        }
      }
    })
    
    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) select_id(NA) else select_id(lst)
    })



    # Update map in response to poly_selected change outside of choropleth()
    observeEvent(select_id(), {
      if (!choropleth() && !focus_visited()) {
        if (!is.na(select_id())) {
          data_to_add <-
            borough |>
            filter(ID == select_id())

          mapdeck_update(map_id = NS(id, "map")) |>
            add_polygon(
              data = data_to_add, stroke_width = 10, stroke_colour = "#000000",
              fill_colour = "#00770030", update_view = FALSE,
              layer_id = "poly_highlight", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF02")

          select_id(select_id())
        } else {
          mapdeck_update(map_id = NS(id, "map")) |>
            clear_polygon(layer_id = "poly_highlight")
        }
      }
    })

    # If we aren't in choropleth, toggle off the legend/zoom
    observeEvent({choropleth()
      focus_visited()}, {
        toggle("alley-zoom_auto",
                        condition = choropleth() && !focus_visited())
        toggle("alley-zoom_slider",
                        condition = choropleth() && !focus_visited())
        # If focus is clicked, toggle off the dropdown menu
        toggle("alley-var", condition = !focus_visited())
      })

    # Hook up "Clear select_id" button and other variables that clears it
    observeEvent(input$`alley-clear_select_id`, select_id(NA))
    observeEvent(choropleth(), select_id(NA))
    observeEvent(focus_visited(), select_id(NA))
    observeEvent(df(), if (choropleth()) select_id(NA))

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      map_id = NS(id, "map"),
      more_args = reactive(c("c-cbox" = focus_visited()))
    )

    # Update click_id() on bookmark
    observeEvent(sus_bookmark$active, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      if (isTRUE(sus_bookmark$active)) {
        delay(2000, {
          if (!is.null(sus_bookmark$select_id)) {
            if (sus_bookmark$select_id != "NA") select_id(sus_bookmark$select_id)
          }
        })
      }
      
      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})      
    }, priority = -2)
    
    # Update click_id() on module link
    observeEvent(sus_link$activity, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      delay(2000, {
        if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
      })
    }, priority = -2)

  })
}
