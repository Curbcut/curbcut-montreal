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
      right_panel(
        id = id, 
        compare_UI(NS(id, "alley"), var_list_right_alley),
        fluidRow(column(width = 7, h4(i18n$t("Explore"))), 
                 column(width = 5, align = "right", 
                        actionLink(inputId = NS(id, "hide"), 
                                   label = i18n$t("Hide")))),
        uiOutput(NS(id, "alley_explore")),
        div(class = "bottom_sidebar", conditionalPanel(
          condition = "output.poly_selected == 1", ns = NS(id),
          actionLink(inputId = NS(id, "clear_selection"),
                     label = "Clear selection"))))),
    
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
    
    # If COUNT isn't selected, choropleth is TRUE 
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
    data_1 <- data_server(
      id = "alley", 
      var_left = var_left, 
      var_right = var_right, 
      df = df, 
      zoom = zoom)
    
    data <- reactive({
      if (choropleth()) {
        data_1() %>% 
          {if (nrow(.) == nrow(borough))
            filter(., ID %in% island_csduid)
            else filter(., CSDUID %in% island_csduid)}
      }})
    
    # Legend
    legend_server(
      id = "legend", 
      var_left = var_left, 
      var_right = var_right, 
      df = df,
      show_panel = choropleth)
    
    # Explore panel
    output$alley_explore <- renderUI({
      
      if (!choropleth()) {
        
        if (selection() %in% alley_text$ID) {
          
          text_to_display <- 
            alley_text %>%
            filter(ID == selection()) %>% 
            select(-ID) %>% 
            select_if(~sum(!is.na(.)) > 0) %>% 
            {if (nrow(.) > 0) as.list(.) else NULL}
          
          text_to_display <- alley_borough_text(text_to_display)
          
          if (exists("text_to_display") && !is.null(text_to_display)) {
            HTML(unlist(text_to_display[1:(length(text_to_display) - 1)]))
          }
          
        } else if (selection() %in% alleys[alleys$visited,]$ID) {
          
          text_to_display <- 
            alleys %>%
            st_drop_geometry() %>% 
            filter(ID == selection()) %>% 
            mutate(name = str_glue(sus_translate(paste0(
              "<p><b>{str_to_title(name)} in ",
              "{name_2}</b></p>")))) %>% 
            select(-ID, -CSDUID, -visited, -name_2, -fill) %>% 
            select_if(~sum(!is.na(.)) > 0) %>% 
            {if (nrow(.) > 0) as.list(.) else NULL}
          
          text_to_display <- alley_alleys_text(text_to_display)
          
          output$alley_img <- renderImage({
            list(src = paste0("www/", text_to_display$photo_ID),
                 alt = "Photo of the selected green alley",
                 width = "100%")},
            deleteFile = FALSE)
          
          alley_photo_id <<- text_to_display$photo_ID
          
          alley_name <<- text_to_display$name
          
          if (exists("text_to_display") && !is.null(text_to_display)) {
            list(HTML(unlist(text_to_display[1:(length(text_to_display) - 1)])),
                 if (!is.null(text_to_display$photo_ID)) {
                   div(style = "margin-bottom:20px;", 
                       imageOutput(session$ns("alley_img"), height = "100%"))
                 })}}}})
    
    onclick(
      "alley_img", 
      { showModal(modalDialog(
          title = HTML(alley_name),
          HTML(paste0('<img src="', alley_photo_id, '", width = 100%>')),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ))})
    
    # Update map in response to user input in not choropleth
    observe({
        if (choropleth()) {
          
          map_change(
            id = NS(id, "map"),
            df = data,
            zoom = df,
            polygons_to_clear = c("alleys_void", "alleys_visited", 
                                  "borough_info", "poly_highlight"))
          
        } else {
          if (input$focus_visited) {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon() %>%
            clear_polygon(layer_id = "borough_info") %>%
            clear_polygon(layer_id = "poly_highlight") %>% 
            # clear_polygon(layer_id = "alleys") %>% 
            add_polygon(data = alleys[!alleys$visited,],
                        stroke_width = 15, stroke_colour = "#CFCFCF",
                        fill_colour = "#CFCFCF", layer_id = "alleys_void",
                        update_view = FALSE, id = "ID", auto_highlight = FALSE) %>%
            add_polygon(data = alleys[alleys$visited, ][!is.na(alleys[alleys$visited, ]$fill), ],
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
            add_polygon(data = alleys[!alleys$visited,],
                        stroke_width = 15, stroke_colour = "#007700", 
                        fill_colour = "#00FF00", layer_id = "alleys_void",
                        update_view = FALSE, id = "ID", auto_highlight = FALSE) %>% 
            add_polygon(data = alleys[alleys$visited,],
                        stroke_width = 15, stroke_colour = "#007700", 
                        fill_colour = "#00FF00", layer_id = "alleys_visited",
                        update_view = FALSE, id = "ID", auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90")
          
        }
        }
        
      })
    
    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (jsonlite::fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) selection(NA) else selection(lst)
    })
    
    # # Clear poly_selected when input$focus_visited is clicked
    observeEvent(input$focus_visited, {if (!choropleth()) selection() <- NA},
                 ignoreInit = TRUE)
    
    
    # Clear selection on df change
    observeEvent(df(), {if (choropleth()) selection() <- NA},
                 ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(selection(), {
      
      if (choropleth()) {
        
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
        
        #If not lags when an alley is selected
    } else if (selection() %in% alley_text$ID || 
            is.na(selection())) {
          
          if (!is.na(selection())) {
            # width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
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
    
    # Hook up "Clear selection" button
    output$poly_selected <- reactive(!is.na(selection()))
    outputOptions(output, "poly_selected", suspendWhenHidden = FALSE)
    
    # Clear click status if prompted
    observeEvent(input$`clear_selection`, {selection() <- NA})
    
    
  })
}
