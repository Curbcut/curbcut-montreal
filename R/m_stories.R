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
      actionLink(NS(id, "back"), sus_translate("Back to the map"))
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

stories_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "stories"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactive
    select_id <- reactiveVal(NA)

    # Sidebar
    sidebar_server(
      id = "sidebar",
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
      if (!is.na(select_id()) && selection == select_id()) {
        select_id(NA)
      } else select_id(selection)
    }) |> bindEvent(get_clicked_object(ns_id_map))
    
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

    # Hide map when "Go back to map" button is clicked
    observe(select_id(NA)) |> bindEvent(input$back)

    observe({
      toggle("hr", condition = !is.na(select_id()))
      toggle("back", condition = !is.na(select_id()))
      toggle("stories", condition = !is.na(select_id()))
    }) |> bindEvent(select_id())

    # If there's an action on the map, the story goes away
    observe({
      hide(id = "stories")
      select_id(NA)
    }) |> bindEvent(get_view_state(ns_id_map))

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
