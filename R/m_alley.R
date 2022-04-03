### GREEN ALLEY MODULE #########################################################


# Dropdown menu -----------------------------------------------------------

var_list_left_alley <-
  list("Borough summary" = " ",
       "Per sq km" = "green_alley_sqkm",
       "Per 1,000 residents" = "green_alley_per1k")


# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  ns_id <- "alley"
  ns_id_map <- paste0(ns_id, "-map")

  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        checkbox_UI(id = NS(id, ns_id),
                    label = sus_translate("Green alleys visited by our team")),
        select_var_UI(NS(id, ns_id), var_list = var_list_left_alley,
                      label = sus_translate("Grouping"))),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, ns_id)),
      uiOutput(NS(id, "special_explore")),
      dyk_UI(NS(id, ns_id)))

  )
}


# Server ------------------------------------------------------------------

alley_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "alley"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    
    # Sidebar
    sidebar_server(id = ns_id, x = "alley")

    # Enter in choropleth() depending on var_left select_id
    choropleth <- reactive(!(var_left() == " " || visited()))

    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = 12)) |> 
        add_mvt_layer(id = "alley-alley",
                      data = mvt_url("sus-mcgill.alley-alley"),
                      pickable = FALSE,
                      auto_highlight = FALSE,
                      get_fill_color = paste0(colour_table$value[1], "90"),
                      get_line_color = paste0(colour_table$value[1], "90"),
                      line_width_units = "pixels",
                      get_line_width = 2)
    })

    # Zoom and POI reactives
    observeEvent(get_view_state(ns_id_map), {
      zoom(get_zoom(get_view_state(ns_id_map)$zoom))
      new_poi <- observe_map(get_view_state(ns_id_map))
      if ((is.null(new_poi) && !is.null(poi())) || 
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    })
    
    # Zoom string reactive
    observeEvent(zoom(), {
      new_zoom_string <- get_zoom_string(zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    })
    
    # Click reactive
    observeEvent(get_clicked_object(ns_id_map), {
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(select_id()) && selection == select_id()) {
        select_id(NA)
      } else select_id(selection)
    })

    # Choose tileset
    tile_choropleth <- zoom_server(
      id = ns_id, 
      zoom_string = zoom_string, 
      zoom_levels = reactive(map_zoom_levels))
    
    tile <- reactive({
      if (choropleth()) {
        tile_choropleth()
      } else if (!visited()) {
        "borough_empty"
      } else "alley"
    })

    # Additional tileset identifier
    tile2 <- reactive("")
    
    # Get df for explore/legend/etc
    df <- reactive(get_df(tile(), zoom()))

    # Focus on visited alleys
    visited <- checkbox_server(id = ns_id)

    # Time
    time <- reactive("2016")

    # Left variable
    var_left_1 <- select_var_server(id = ns_id, 
                                    var_list = reactive(var_list_left_alley))
    
    var_left <- reactive(if (visited()) "alley_qual" else var_left_1())

    # Compare panel
    var_right <- compare_server(
      id = ns_id,
      var_list = make_dropdown(compare = TRUE),
      show_panel = choropleth,
      time = time)

    # Data
    data <- reactive(get_data(df(), var_left(), var_right(), island = TRUE))
    
    # Composite variable for map
    map_var <- reactive({
      if (choropleth()) {
        str_remove(paste(var_left(), var_right(), sep = "_"), "_ $")
      } else if (!visited()) {
        ""
      } else "type"
    })

    # Legend
    legend_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right,
      df = df,
      hide = reactive(tile() == "borough_empty"))
    
    # Explore panels
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)
    
    output$special_explore <- renderUI({
      if (input$`alley-hide_explore`  %% 2 == 0)
      if (var_left() == var_list_left_alley[1] && 
          select_id() %in% alley_text$ID) {
        
        data <- alley_text[alley_text$ID == select_id(),]
        
        text_to_display <- list()
        text_to_display$title <- paste0("<p><b>", sus_translate("Borough"), " ", 
                                        data$name, "</b></p>")
        text_to_display$intro <-
          paste0("<p>",
                 sus_translate("The first green alley inauguration was in "),
                 data$first_alley, if (!is.na(data$green_alley_sqm)) 
                   sus_translate(" and there are {data$green_alley_sqm} square",
                                 " meters of green alley in the borough.") else ".",
                 "</p>")
        text_to_display$text <- 
          paste0("<p>",
                 if (!is.na(data$app_process)) sus_translate(data$app_process), " ",
                 if (!is.na(data$management)) sus_translate(data$management), " ",
                 if (!is.na(data$budget)) sus_translate(data$budget),
                 "</p>")
        
        text_to_display$guide <- 
          paste0(glue("<p><a href = {data$guide}>"),
                 sus_translate("The green alley guide of {data$name}"),
                 "</a></p>")
        
        text_to_display$contact <- 
          if (!is.na(data$contact))
          glue("<p>Contact: <a href = 'mailto:{data$contact}'>",
               "{data$contact}</a></p>")
        
        HTML(unlist(text_to_display))
        
      } else if (select_id() %in% alley$ID) {
        
        data <- alley[alley$ID == select_id(),]
        
        text_to_display <- list()
        text_to_display$title <- paste0("<p><b>", data$name, " (", data$name_2, 
                                        ")", "</b></p>")
        if (!is.na(data$created)) 
          text_to_display$inauguration <- 
          paste0("<p>",
                 sus_translate("Inauguration date: "), 
                 data$created, "</p>")
        text_to_display$text <- 
          if (is.na(data$description)) {
            paste0("<p>",
                   sus_translate("We do not have information available ",
                                 "on this green alley."),
                   "</p>")
          } else {
            paste0("<p>", sus_translate(data$description), "</p>",
                   if (!is.na(data$circulation)) {
                     paste0("<p>Circulation: ", sus_translate(data$circulation),
                            "</p>")
                   })
          }
        
        output$alley_img <- renderImage({
          if (!is.na(data$photo_ID))
            list(src = paste0("www/alleys/", data$photo_ID),
                 alt = sus_translate("Photo of the selected green alley"),
                 width = "100%")},
          deleteFile = FALSE)
        
        list(HTML(unlist(text_to_display)),
             if (!is.na(data$photo_ID)) {
               div(style = "margin-bottom:20px; cursor:pointer;",
                   imageOutput(session$ns("alley_img"), height = "100%"))
             })
      }
    })
    
    # Popup the alley image if it's clicked on
    onclick(
      "alley_img", 
      {showModal(modalDialog(
        title = alley[alley$ID == select_id(),]$name,
        HTML(paste0('<img src="alley/',
                    alley[alley$ID == select_id(),]$photo_ID,
                    '", width = 100%>')),
        easyClose = TRUE,
        size = "m",
        footer = NULL
      ))})
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id,
      map_id = "map",
      tile = tile,
      tile2 =  tile2,
      map_var = map_var,
      zoom = zoom,
      select_id = select_id,
      fill = scale_fill_alley,
      fill_args = reactive(list(map_var(), tile())),
      colour = scale_colour_alley,
      colour_args = reactive(list(map_var(), tile())),
      lwd = scale_lwd_alley,
      lwd_args = reactive(list(select_id(), tile())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id, 
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # If we aren't in choropleth, toggle off the legend/zoom
    observeEvent({choropleth()
      visited()}, {
        toggle("alley-zoom_auto",
                        condition = choropleth() && !visited())
        toggle("alley-zoom_slider",
                        condition = choropleth() && !visited())
        # If focus is clicked, toggle off the dropdown menu
        toggle("alley-var", condition = !visited())
      })

    # Hook up "Clear select_id" button and other variables that clears it
    observeEvent(input$`alley-clear_select_id`, select_id(NA))
    observeEvent(choropleth(), select_id(NA))
    observeEvent(visited(), select_id(NA))
    observeEvent(df(), if (choropleth()) select_id(NA))

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
      more_args = reactive(c("c-cbox" = visited()))
    )

    # Update select_id() on bookmark
    observeEvent(sus_bookmark$active, {
      if (isTRUE(sus_bookmark$active)) {
        if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
        delay(1000, {
          if (!is.null(sus_bookmark$select_id))
            if (sus_bookmark$select_id != "NA") 
              select_id(sus_bookmark$select_id)
        })
      }
      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})
    }, priority = -2)
    
    # Update select_id() on module link
    observeEvent(sus_link$activity, {
      if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
      delay(1000, {
        if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
      })
    }, priority = -2)

  })
}
