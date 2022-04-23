### STORIES MODULE ##############################################################

# Temporary non-translated stories:
available_stories <- list.files("www/stories", full.names = TRUE) |> 
  str_subset(".html")

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  ns_id <- "stories"
  
  tagList(

    # Sidebar
    sidebar_UI(
      NS(id, "sidebar"),
      hr(id = NS(id, "hr")),
      actionLink(NS(id, "back"), sus_translate("Back to the map"))),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, paste0(ns_id, "-map")), 
                                             height = "100%")),

    # Stories
    hidden(htmlOutput(
      NS(id, "stories"),
      style = paste0("position:absolute; margin: 40px; ",
                     "max-width: 1200px; z-index:499")))

  )
}


# Server ------------------------------------------------------------------

stories_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "stories"

    # Initial reactives
    select_id <- reactiveVal(NA)
    zoom <- reactiveVal(map_zoom * -750 + 9000)

    # Sidebar
    sidebar_server(
      id = "sidebar",
      x = "stories")

    # Map
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom))
    })
    
    # Zoom level
    observeEvent(input[[paste0(ns_id, "-map_viewstate")]], {
      zoom(max(600, input[[paste0(ns_id, "-map_viewstate")]]$viewState$zoom *
                 -750 + 9000))
      select_id(NA)
    })


    # Update buffer to change the map when zoom is different
    data <- reactive(
      transform(stories, buffer = st_buffer(stories$geometry, zoom())))

    observe({
      
      row_n <- 1:nrow(data())
      images <- paste0("https://raw.githubusercontent.com/MSSI-urban/Sus/",
                       "rdeck/www/stories/round_img/", data()$img[row_n])
      bboxes <- lapply(data()$buffer, \(x) st_bbox(x) |> round(digits = 5))
      layer_ids <- paste0("image", row_n)
      
      all_add_bitmap <- paste0(
        'add_bitmap_layer(image = "', images, '", bounds = ', bboxes, ', ', 
        'id ="', layer_ids, '", pickable = TRUE)', collapse = ' |>  ')
      
      updated_map <- paste0('rdeck_proxy(id = "stories-map") |> ',
                            paste0(text = all_add_bitmap))
      
      eval(parse(text = updated_map))
      
      }) |> bindEvent(data(), zoom())
    
    # Click reactive
    observe({

      click <- st_point(c(input[[paste0(ns_id, "-map_click")]]$coordinate[[1]],
                          input[[paste0(ns_id, "-map_click")]]$coordinate[[2]])
                        ) |>
        st_sfc(crs = 4326) |>
        as.data.frame() |>
        st_as_sf() |>
        st_set_agr("constant")

      hay <- st_intersection(click, st_set_agr(data(), "constant"))

      if (nrow(hay) == 1) select_id(hay$ID) else select_id(NA)

    }) |> bindEvent(input[[paste0(ns_id, "-map_click")]])

    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({

      if (!is.na(select_id())) {

        rmd_name <- stories[stories$ID == select_id(),]$name
        bandeau_name <- stories[stories$ID == select_id(),]$img

        story_link <- paste0("www/stories/", rmd_name, "_", sus_rv$lang(),
                             ".html")

        # Construct story link, serve en if no translation available.
        story_link <- if (story_link %in% available_stories) story_link else {
          paste0("www/stories/", rmd_name, "_", "en",
                 ".html")
        }

        HTML('<div class = "main_panel_text_popup">',
             # Adding bandeau img after the first div (title)
             str_replace(
               includeHTML(story_link),
               "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                                bandeau_name,"><br><br>")) |>
               str_replace_all('<img src="visuals/',
                               '<img src="stories/visuals/'),
             '</div>')
      }

    })

    # Anytime to "Go back to map" button is clicked, poly_selected goes
    # NA causing the div to disappear
    observe(select_id(NA)) |> bindEvent(input$back)

    observe({
      toggle("hr", condition = !is.na(select_id()))
      toggle("back", condition = !is.na(select_id()))
      toggle("stories", condition = !is.na(select_id()))
    }) |> bindEvent(select_id())

    # If there's an action with the map, the rmd goes away
    observe({
      hide(id = "stories")
      select_id(NA)
    }) |> bindEvent(input$map_view_change)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(
        input[[paste0(ns_id, "-map_viewstate")]]$viewState),
      select_id = select_id,
      map_id = "map",
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
