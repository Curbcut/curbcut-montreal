### STORIES MODULE ##############################################################

# Temporary non-translated stories:
available_stories <- list.files("www/stories", full.names = TRUE) |> 
  str_subset(".html")

# cycling_infrastructure legend function
cycling_infrastructure_legend <- function(year) {
  
  types_df <- 
  data.frame(year = c(1986, 1991, 1996, 2001, 2006, 2011, 2016, 2022),
             types = I(list(c("Bicycle path"), 
                            c("New bicycle path", "Removed path", "Old path"),
                            c("New bicycle path", "Removed path", "Old path"),
                            c("New bicycle path", "Removed path", "Old path"),
                            c("New bicycle path", "Removed path", "Old path"),
                            c("New bicycle path", "Removed path", "Old path"),
                            c("New bicycle path", "Removed path", "Old path", "Bixi station"),
                            c("Bicycle path", "Bixi station"))))
  
  types <- 
    unlist(types_df$types[types_df$year == year])
  
  type_fill <- 
    data.frame(type = c("Bicycle path", "New bicycle path", "Removed path", 
                        "Old path", "Bixi station"),
               fill = c("#73AE80", "#73AE80", "#CA0020", "#2E4633", "#000000"))
  
  label <- type_fill$type[type_fill$type %in% types]
  
  legend <- 
    data.frame(x = seq_along(label),
               y = 1,
               fill = 
                 unique(type_fill[type_fill$type %in% types, ]$fill))
  
  legend |> 
    ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
               fill = fill)) +
    geom_rect() + 
    scale_x_continuous(breaks = seq_along(label) - 0.5, labels = label) +
    scale_y_continuous(labels = NULL) +
    scale_fill_manual(values = setNames(
      legend$fill, legend$fill)) +
    theme_minimal() +
    theme(text = element_text(family = "SourceSansPro", size = 11),
          legend.position = "none", 
          panel.grid = element_blank())
}

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
    
    output$stories_custom_map <-
      renderRdeck(rdeck(width = "100%", map_style = map_base_style, 
                        initial_view_state = view_state(
                          center = map_loc, zoom = as.numeric(map_zoom - 1))))
    
    observeEvent(input[["stories-stories_map_slider"]], {
      rdeck_proxy("stories_custom_map") |>
      add_mvt_layer(
        id = "cycling_infrastructure",
        data = mvt_url("sus-mcgill.stories-cycling_infrastructure"),
        visible = TRUE,
        get_line_width = 2,
        get_line_color = !!rlang::sym(paste0("fill_", input[["stories-stories_map_slider"]])),
        line_width_units = "pixels",
        get_fill_color = !!rlang::sym(paste0("fill_", input[["stories-stories_map_slider"]])),
        get_point_radius = 10)

    }, ignoreInit = TRUE)
    
    output$stories_custom_legend <- renderPlot({
      cycling_infrastructure_legend(input[["stories-stories_map_slider"]])
    })
    
    output$stories_custom_source <- renderText({
      # If cycling infrastructure
      if (r[[ns_id]]$select_id() == 9) {
        if (input[["stories-stories_map_slider"]] %in% c(1986:2016)) {
        paste0("See Note 1. Data provided by: Houde, M., Apparicio, P., & Ségu",
               "in, A.-M. (2018). A ride for whom: Has cycling network expansi",
               "on reduced inequities in accessibility in Montreal, Canada? Jo",
               "urnal of Transport Geography, 68, 9–21. https://doi.org/10.101",
               "6/j.jtrangeo.2018.02.005")
        } else {
          paste0("See Note 2. Data provided by: Winters et al. The Canadian Bi",
                 "keway Comfort and Safety Metrics (Can-BICS): Measuring the b",
                 "icycling environment in all communities in Canada – for subm",
                 "ission to Health Reports (forthcoming).")
        }
      }
    })

    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({
      
      if (!is.na(r[[ns_id]]$select_id())) {
        
        rmd_name <- stories[stories$ID == r[[ns_id]]$select_id(),]$name
        bandeau_name <- stories[stories$ID == r[[ns_id]]$select_id(),]$img
        
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
        if (rmd_name %in% c("cycling_infrastructure")) {
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
                                       choices = c(1986, 1991, 1996, 2001, 2006, 
                                                   2011, 2016, 2022),
                                       width = "250"),
                        plotOutput(NS(id, "stories_custom_legend"), height = 60),
                        br(),
                        textOutput(NS(id, "stories_custom_source"))
                  )
              )
          )
          )
          # # Right panel
          # absolutePanel(
          #   id = NS(id, "right_panel"),
          #   class = "panel panel-default sus-map-panel sus-scroll",
          #   style = "margin-top:50px;margin-right:20px;padding:10px;",
          #   div(class = "sus-map-panel-content sus-scroll-content", 
          #       div(
          #         h4(sus_translate(r = r, "Take a walk"))),
          #       p("To come!"),
          #       hr(),
          #       div(
          #         h4(sus_translate(r = r, "Photos"))),
          #       lapply(images_tag, \(x) eval(parse(text = x))),
          #       hr(),
          #       div(
          #         h4(sus_translate(r = r, "Watch the video"))),
          #       hr(),
          #       "Other Content")
          # )
          
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
              #         h4(sus_translate(r = r, "Take a walk"))),
              #       p("To come!"),
              #       hr(),
              #       div(
              #         h4(sus_translate(r = r, "Photos"))),
              #       lapply(images_tag, \(x) eval(parse(text = x))),
              #       hr(),
              #       div(
              #         h4(sus_translate(r = r, "Watch the video"))),
              #       hr(),
              #       "Other Content")
              # )
          )
        }
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
      s_id = r[[ns_id]]$select_id,
      map_viewstate = reactive(
        input[[paste0(ns_id, "-map_viewstate")]]$viewState)
    )

  })
}
