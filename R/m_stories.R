### STORIES MODULE ##############################################################

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  id <- "stories"
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        actionLink(NS(id, "back"), curbcut::cc_t(
                                                 "Back to the map")),
        pickerInput(
          inputId = NS(id, "themes_checkbox"),
          label = curbcut::cc_t("Choose themes:"),
          choices = unique(unlist(stories$themes)),
          selected = unique(unlist(stories$themes)),
          multiple = TRUE),
        hr(id = NS(id, "hr"))
        )
    ),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, paste0(id, "-map")), 
                                           height = "100%")),
    
    # Stories
    hidden(htmlOutput(
      NS(id, "stories")))
    
  )
}


# Server ------------------------------------------------------------------

stories_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Sidebar
    sidebar_server(id = id, r = r, x = "stories")
    
    # Map
    output[[id_map]] <- renderRdeck({
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "stories",
          data = mvt_url(paste0(mapbox_username, ".", tileset_prefix, "_", 
                                "stories")),
          point_type = "icon",
          get_icon = name,
          icon_atlas = "stories/image_atlas.png",
          icon_mapping = stories_mapping,
          icon_size_scale = 60,
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#FFFFFF50")
    })

    # Click reactive
    observe({
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) && selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else r[[id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(id_map))

    # Custom map, legend and source for cycling infrastructure and metro
    # evolution stories
    selected_story <- reactive({
      if (is.na(r[[id]]$select_id())) return(NA)
      stories$name_id[stories$ID == r[[id]]$select_id()]
    })
    output$stories_custom_map <-
      renderRdeck(rdeck(width = "100%", map_style = map_base_style,
                        initial_view_state = view_state(
                          center = map_loc, zoom = as.numeric(map_zoom - 1))))
    observeEvent(input[["stories-stories_map_slider"]], {
      if (selected_story() %in% c("cycling_infrastructure", "metro_evolution"))
      do.call(paste0(selected_story(), "_map"),
              list(input[["stories-stories_map_slider"]]))
    }, ignoreInit = TRUE)
    observeEvent(selected_story(), {
      if (selected_story() %in% c("cycling_infrastructure", "metro_evolution"))
        shinyWidgets::updateSliderTextInput(
          session = session,
          inputId = "stories-stories_map_slider",
          choices = do.call(paste0(selected_story(), "_choices"), list()))
    }, ignoreInit = TRUE)
    output$stories_custom_legend <- renderPlot({
      if (selected_story() %in% c("cycling_infrastructure", "metro_evolution"))
      do.call(paste0(selected_story(), "_legend"),
              list(input[["stories-stories_map_slider"]], r$lang()))
    })
    output$stories_custom_source <- renderText({
      if (selected_story() %in% c("cycling_infrastructure", "metro_evolution"))
      do.call(paste0(selected_story(), "_source"),
              list(input[["stories-stories_map_slider"]], r))
    })

    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({

      if (!is.na(r[[id]]$select_id())) {

        rmd_name <- stories$name_id[stories$ID == r[[id]]$select_id()]
        bandeau_name <-
          paste0(stories$name_id[stories$ID == r[[id]]$select_id()], ".png")

        story_link <- paste0("www/stories/", rmd_name, "_", r$lang(),
                             ".html")

        # Construct story link, serve en if no translation available.
        story_link <- if (story_link %in% available_stories) story_link else {
          paste0("www/stories/", rmd_name, "_", "en",
                 ".html")
        }

        # images <-
        #   list.files(paste0("www/stories/visuals/", rmd_name),
        #              full.names = TRUE) |>
        #   str_subset("png$|jpeg$|jpg$|gif$") |>
        #   str_remove("^www/")
        #
        # images_tag <-
        #   lapply(images, \(x)
        #          paste0("img(src='", x, "', style='width:45%;margin:2px;')"))

        # SPECIAL CASES FOR THE Mp
        if (rmd_name %in% c("cycling_infrastructure", "metro_evolution")) {
          div(class = "main_panel_text_popup",
              div(class = "row-stories-maps",
                  div(class = "column-stories-maps",
                      # Main story
                      HTML(str_replace(
                        includeHTML(story_link),
                        # Adding bandeau img after the first div (title)
                        "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                                         bandeau_name,"><br><br>")) |>
                          str_replace_all('<img src="visuals/',
                                          '<img src="stories/visuals/') |>
                          str_replace('max-width: 940px;', 'max-width:100%;'))),
                  div(class = "column-stories-maps-map",
                      style = "overflow-y:auto;overflow-x:hidden;",
                      tagList(
                        rdeckOutput(NS(id, "stories_custom_map"), height = "65%"),
                        slider_text_UI(id = id,
                                       slider_id = NS(id, "stories_map_slider"),
                                       label = NULL,
                                       choices = do.call(paste0(rmd_name, "_choices"), list()),
                                       width = "250"),
                        plotOutput(NS(id, "stories_custom_legend"), height = 60,
                                   width = "auto"),
                        br(),
                        div(style = "text-align: center;",
                            textOutput(NS(id, "stories_custom_source"))),
                      )
                  )
              )
          )
        } else {
          div(class = "main_panel_text_popup",

              # Main story
              div(#style = "margin-right:250px;",
                HTML(str_replace(
                  includeHTML(story_link),
                  # Adding bandeau img after the first div (title)
                  "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                                   bandeau_name,"><br><br>")) |>
                    str_replace_all('<img src="visuals/',
                                    '<img src="stories/visuals/'))),
              # # Right panel
              # absolutePanel(
              #   id = NS(id, "right_panel"),
              #   class = "panel panel-default sus-map-panel sus-scroll",
              #   style = "margin-top:50px;margin-right:20px;padding:10px;",
              #   div(class = "sus-map-panel-content sus-scroll-content",
              #       div(
              #         h4(curbcut::cc_t(lang = r$lang(), "Take a walk"))),
              #       p("To come!"),
              #       hr(),
              #       div(
              #         h4(curbcut::cc_t(lang = r$lang(), "Photos"))),
              #       lapply(images_tag, \(x) eval(parse(text = x))),
              #       hr(),
              #       div(
              #         h4(curbcut::cc_t(lang = r$lang(), "Watch the video"))),
              #       hr(),
              #       "Other Content")
              # )
          )
        }
      }

    })

    # Add stories on the left-hand panel and react on a click
    observeEvent(input$themes_checkbox, {
      in_theme <-
        stories$ID[which(
          sapply(sapply(stories$themes, `%in%`, input$themes_checkbox), sum) > 0)]

      removeUI(selector = "#bullet_points")
      insertUI(paste0("#stories-hr"),
               where = "afterEnd",
               tags$ul(
                 id = "bullet_points",
                 lapply(stories$short_title[stories$ID %in% in_theme], \(x) {
                   tags$li(
                     curbcut::cc_t(lang = r$lang(),
                                   x),
                     style = "cursor: pointer; text-decoration: none;",
                     title = curbcut::cc_t(lang = r$lang(),
                                           stories$preview[stories$short_title == x]),
                     onclick = paste0("Shiny.setInputValue(`",
                                      NS(id, "clicked_linked"),
                                      "`, '",
                                      curbcut::cc_t(lang = r$lang(),
                                                    stories$ID[stories$short_title == x]),
                                      "');"),
                     onmouseover = "$(this).css('text-decoration', 'underline');",
                     onmouseout = "$(this).css('text-decoration', 'none');"
                   )
                 })
               )
      )

    })
    observeEvent(input$clicked_linked, {
      r[[id]]$select_id(input$clicked_linked)
    })

    # Update the select_id if clicked on a story title in the top navigation panel
    observeEvent(input$select_nav, {
      r[[id]]$select_id(input$select_nav)
    })

    # Hide map when "Go back to map" button is clicked
    observe(r[[id]]$select_id(NA)) |> bindEvent(input$back)

    observe({
      toggle("back", condition = !is.na(r[[id]]$select_id()))
      toggle("stories", condition = !is.na(r[[id]]$select_id()))
    }) |> bindEvent(r[[id]]$select_id())

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      map_viewstate = reactive(
        input[[paste0(id, "-map_viewstate")]]$viewState)
    )

  })
}
