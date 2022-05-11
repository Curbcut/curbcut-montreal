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
      # hr(id = NS(id, "hr")),
      actionLink(NS(id, "back"), sus_translate(r = r, "Back to the map"))
    ),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, paste0(ns_id, "-map")), 
                                             height = "100%")),

    # Stories
    hidden(htmlOutput(
      NS(id, "stories")))

  )
}


# Server ------------------------------------------------------------------

stories_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "stories"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactive
    if (is.null(r[[ns_id]]$select_id)) r[[ns_id]]$select_id <- reactiveVal(NA)

    # Sidebar
    sidebar_server(
      id = "sidebar",
      r = r,
      x = "stories")

    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "stories",
          data = mvt_url("sus-mcgill.stories-stories"),
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
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(r[[ns_id]]$select_id()) && selection == r[[ns_id]]$select_id()) {
        r[[ns_id]]$select_id(NA)
      } else r[[ns_id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(ns_id_map))
    
    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({

      if (!is.na(r[[ns_id]]$select_id())) {

        rmd_name <- stories[stories$ID == r[[ns_id]]$select_id(),]$name
        bandeau_name <- stories[stories$ID == r[[ns_id]]$select_id(),]$img

        story_link <- paste0("www/stories/", rmd_name, "_", r$lang,
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

    # Hide map when "Go back to map" button is clicked
    observe(r[[ns_id]]$select_id(NA)) |> bindEvent(input$back)

    observe({
      toggle("hr", condition = !is.na(r[[ns_id]]$select_id()))
      toggle("back", condition = !is.na(r[[ns_id]]$select_id()))
      toggle("stories", condition = !is.na(r[[ns_id]]$select_id()))
    }) |> bindEvent(r[[ns_id]]$select_id())

    # Bookmarking
    bookmark_server(
      id = ns_id,
      r = r,
      map_viewstate = reactive(
        input[[paste0(ns_id, "-map_viewstate")]]$viewState),
      select_id = r[[ns_id]]$select_id,
      map_id = "map",
    )

  })
}
