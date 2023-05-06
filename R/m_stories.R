### STORIES MODULE ##############################################################

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  themes <- unique(unlist(stories$themes))
  themes <- list(Themes = setNames(themes, themes))
  shiny::tagList(

    # Sidebar
    curbcut::sidebar_UI(
      id = NS(id, id),
      shiny::actionLink(
        shiny::NS(id, "back"),
        curbcut::cc_t("Back to the map")
      ),
      curbcut::picker_UI(
        id = NS(id, id),
        label = curbcut::cc_t("Choose themes:"),
        var_list = themes,
        selected = unlist(themes),
        multiple = TRUE),
      shiny::hr(id = shiny::NS(id, "hr"))
    ),
    
    # Map
    curbcut::map_UI(id = shiny::NS(id, id)),

    # Main panel
    shiny::htmlOutput(shiny::NS(id, "stories"))
  )
}


# Server ------------------------------------------------------------------

stories_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    themes <- unique(unlist(stories$themes))
    themes <- list(Themes = setNames(themes, themes))

    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Map
    output[[id_map]] <- rdeck::renderRdeck({
      rdeck::rdeck(map_style = map_style_building, initial_view_state = 
                     rdeck::view_state(center = map_loc, zoom = map_zoom), 
                   layer_selector = FALSE) |> 
        rdeck::add_mvt_layer(
          id = "stories",
          data = rdeck::tile_json(paste0(mapbox_username, ".", tileset_prefix, "_", 
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
    curbcut::update_select_id(id = id, r = r)

    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({
      
      if (!is.na(r[[id]]$select_id())) {
        
        rmd_name <- stories$name_id[stories$ID == r[[id]]$select_id()]
        story_link <- paste0("stories/", rmd_name, "_", r$lang(), ".html")
        
        shiny::div(
          class = "main_panel_popup",
          style = "height:100%;overflow:hidden;background-color:rgba(255, 255, 255, 0.975)",
          shiny::tags$iframe(
            style = "width:100%;height:100%;",
            title = "stories",
            src = story_link,
            frameborder = 0
          )
        )
      }
    })

    # Add stories on the left-hand panel and react on a click
    themes_c <- curbcut::picker_server(id = id,
                                       picker_id = "var",
                                       r = r,
                                       var_list = shiny::reactive(themes),
                                       selected = unlist(themes))
    
    shiny::observeEvent(themes_c(), {
      in_theme <-
        stories$ID[which(
          sapply(sapply(stories$themes, `%in%`, themes_c()), sum) > 0)]
      
      shiny::removeUI(selector = "#bullet_points")
      shiny::insertUI(paste0("#stories-hr"),
               where = "afterEnd",
               shiny::tags$ul(
                 id = "bullet_points",
                 lapply(stories$short_title[stories$ID %in% in_theme], \(x) {
                   shiny::tags$li(
                     curbcut::cc_t(lang = r$lang(), x),
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
    shiny::observeEvent(input$clicked_linked, {
      r[[id]]$select_id(input$clicked_linked)
    })

    # Update the select_id if clicked on a story title in the top navigation panel
    shiny::observeEvent(input$select_nav, {
      r[[id]]$select_id(input$select_nav)
    })

    # Hide main panel when "Go back to map" button is clicked
    shiny::observeEvent(input$back, r[[id]]$select_id(NA))
    shiny::observeEvent(r[[id]]$select_id(), {
      shinyjs::toggle("back", condition = !is.na(r[[id]]$select_id()))
      shinyjs::toggle("stories", condition = !is.na(r[[id]]$select_id()))
    })

    # Bookmarking
    curbcut::bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      exclude_input = "ccpicker_var"
    )

  })
}
