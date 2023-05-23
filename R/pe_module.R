#' Launches the place explorer module server
#'
#' This function initializes the server logic for the place explorer module,
#' which is a shiny module that allows users to explore geographical areas and
#' retrieve data about them. The module consists of a map, a sidebar, and a main
#' panel that displays detailed information about a selected area.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. These
#' scales won't be selectable by the user.
#' @param map_zoom <`numeric`> The default zoom level for the map. By default,
#' this value is retrieved from the \code{map_zoom} variable in the global environment.
#' @param map_loc <`numeric vector`> The default location for the map. By default,
#' this value is retrieved from the \code{map_loc} variable in the global environment.
#' @param mapbox_username <`character`> Mapbox account username. Defaults to
#' grabbing the `mapbox_username` object from the global environment.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`. Defaults to grabbing the
#' `tileset_prefix` object from the global environment.
#' @param map_base_style <`character`> The mapbox basemap style url.
#' See https://docs.mapbox.com/api/maps/#mapbox-styles
#' @param temp_folder <`character`> The temporary folder of the app. By default
#' will grab the `temp_folder` object as it's already supposed to have been assigned
#' in the `global.R` file
#'
#' @export
place_explorer_server <- function(id, r,
                                  scales_as_DA = shiny::reactive(
                                    c("building", "street")
                                  ),
                                  map_zoom = curbcut:::get_from_globalenv("map_zoom"),
                                  map_loc = curbcut:::get_from_globalenv("map_loc"),
                                  mapbox_username = curbcut:::get_from_globalenv("mapbox_username"),
                                  tileset_prefix = curbcut:::get_from_globalenv("tileset_prefix"),
                                  map_base_style = curbcut:::get_from_globalenv("map_base_style"),
                                  temp_folder = curbcut:::get_from_globalenv("temp_folder")) {
  shiny::moduleServer(id, function(input, output, session) {
    # Declare the map id
    id_map <- paste0(id, "-map")
    # Get default values for the place explorer
    pe_vars <- curbcut:::place_explorer_vars(scales_as_DA = scales_as_DA())
    
    # Get df ------------------------------------------------------------------
    
    # Initial reactives
    rv_zoom_string <- shiny::reactiveVal(
      zoom_get_string(
        zoom = map_zoom,
        zoom_levels = pe_vars$map_zoom_levels,
        region = pe_vars$default_region
      )
    )
    
    # Get the map view state
    map_viewstate <- shiny::reactive(rdeck::get_view_state(id_map))
    
    # Zoom reactive when the view state of the map changes.
    shiny::observeEvent(map_viewstate(), {
      r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
    })
    
    # Map zoom levels change depending on r$region()
    zoom_levels_ <-
      shiny::reactive(zoom_get_levels(id = id, region = r$region()))
    # Do not include scales as DA like buildings or streets
    zoom_levels <- shiny::reactive({
      zoom_lvls <- zoom_levels_()$zoom_levels
      zoom_levels <- zoom_lvls[!names(zoom_lvls) %in% scales_as_DA()]
      
      return(list(
        zoom_levels = zoom_levels,
        region = zoom_levels_()$region
      ))
    })
    
    # Zoom string reactive
    shiny::observe({
      rv_zoom_string({
        zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = zoom_levels()$zoom_levels,
          region = zoom_levels()$region
        )
      })
    })
    
    # Choose tileset
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels
    )
    
    # Get df
    shiny::observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        r[[id]]$df(update_df(
          tile = tile(),
          zoom_string = rv_zoom_string()
        ))
      }
    )
    
    # Misc --------------------------------------------------------------------
    
    # Sidebar
    sidebar_server(id = "place_explorer", r = r)
    
    
    # Postal code search ------------------------------------------------------
    
    shiny::observeEvent(input$search_button, {
      postal_c <- tolower(input$address_searched)
      postal_c <- s_extract_all("\\w|\\d", postal_c)
      postal_c <- paste(postal_c, collapse = "")
      
      postal_codes <- curbcut:::get_from_globalenv("postal_codes")
      DA_id <- postal_codes$DA_ID[postal_codes$postal_code == postal_c]
      
      if (length(DA_id) == 0) {
        address <- input$address_searched
        shiny::showNotification(
          cc_t(
            lang = r$lang(),
            paste0("No postal code found for `{address}`")
          ),
          type = "error"
        )
      } else {
        DA_table <- curbcut:::get_from_globalenv(paste0(r$region(), "_DA"))
        if (is.null(DA_table)) {
          return(NULL)
        }
        scale <- gsub(".*_", "", r[[id]]$df())
        right_id <- DA_table[[paste0(scale, "_ID")]][DA_table$ID == DA_id]
        r[[id]]$select_id(right_id)
      }
    })
    
    
    # Map ---------------------------------------------------------------------
    
    output[[id_map]] <- rdeck::renderRdeck(
      rdeck::rdeck(
        map_style = map_base_style, initial_view_state =
          rdeck::view_state(center = map_loc, zoom = map_zoom),
        layer_selector = FALSE
      ) |>
        rdeck::add_mvt_layer(
          id = "place_explorer",
          name = "place_explorer",
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#AAB6CF80",
          get_fill_color = "#AAB6CF20",
          get_line_color = "#FFFFFF00"
        )
    )
    
    # Update the map whenever the `df` changes
    shiny::observe({
      rdeck::rdeck_proxy(id_map) |>
        rdeck::update_mvt_layer(
          id = "place_explorer",
          data = tilejson(mapbox_username, tileset_prefix, r[[id]]$df())
        )
    })
    
    # Map click
    update_select_id(id = id, r = r)
    
    
    # Select ID behavior ------------------------------------------------------
    
    # Hide main panel when "Go back to map" button is clicked
    shiny::observeEvent(input$back, r[[id]]$select_id(NA))
    shiny::observeEvent(r[[id]]$select_id(), {
      shinyjs::toggle("back", condition = !is.na(r[[id]]$select_id()))
      shinyjs::toggle("place_exp_main_panel", condition = !is.na(r[[id]]$select_id()))
      shinyjs::toggle("zoom_slider", condition = is.na(r[[id]]$select_id()))
    })
    
    
    # Main panel --------------------------------------------------------------
    
    # Add a loader page, which is an empty document with white background that
    # appears with an animation of 2 seconds. It gives time for the place
    # explorer to be fully concatenated and shown.
    output$loader <- shiny::renderUI({
      if (!is.na(r[[id]]$select_id())) {
        shiny::div(
          class = "main_panel_popup loader-page",
          shiny::tags$iframe(
            style = "width:100%;height:100%;",
            title = "place_ex",
            srcdoc = paste0(
              "<!DOCTYPE html><html><head><title></title><style",
              ">body {background-color: white;}</style></head><",
              "body><body></html>"
            ),
            frameborder = 0
          )
        )
      }
    })
    
    main_panel <- shiny::reactive({
      if (!is.na(r[[id]]$select_id())) {
        pe_links <- curbcut:::place_explorer_html_links(
          temp_folder = temp_folder,
          df = r[[id]]$df(),
          select_id = r[[id]]$select_id(),
          lang = r$lang()
        )
        
        # Show the file
        return(list(
          div = shiny::div(
            class = "main_panel_popup",
            style = "height:100%;overflow:hidden;",
            shiny::tags$iframe(
              style = "width:100%;height:100%;",
              title = "place_ex",
              src = pe_links$src,
              frameborder = 0
            )
          ),
          # temp_folder_shortcut is tempdir()
          file = pe_links$file
        ))
      } else {
        NULL
      }
    })
    
    output$main_panel <- shiny::renderUI(main_panel()$div)
    
    
    # Download ----------------------------------------------------------------
    
    output$download_portrait <-
      shiny::downloadHandler(
        filename = shiny::reactive(paste0(
          r[[id]]$df(), "_",
          r[[id]]$select_id(), ".html"
        )),
        content = function(file) {
          html_content <- readLines(main_panel()$file)
          writeLines(html_content, file)
        }, contentType = "text/html"
      )
    
    
    # Bookmarking -------------------------------------------------------------
    
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id
    )
  })
}

#' @describeIn place_explorer_server Create the UI for the place explorer module
#' @export
place_explorer_UI <- function(id, scales_as_DA = c("building", "street")) {
  # Get default values for the place explorer
  pe_vars <- curbcut:::place_explorer_vars(scales_as_DA = scales_as_DA)
  
  shiny::tagList(
    
    # Sidebar
    sidebar_UI(
      shiny::NS(id, id),
      # Search box
      shiny::strong(curbcut::cc_t("Enter postal code or click on the map")),
      # Imitate a split layout which only works this way on iOS
      shiny::HTML(paste0(
        '<div class="shiny-split-layout">
                     <div style="width: 80%;">',
        shiny::textInput(
          inputId = shiny::NS(id, "address_searched"),
          label = NULL, placeholder = "H3A 2T5"
        ),
        '</div><div style="width: 20%;">',
        shiny::actionButton(
          inputId = shiny::NS(id, "search_button"),
          label = shiny::icon("search", verify_fa = FALSE),
          style = "margin-top: var(--padding-v-md);"
        ),
        "</div></div>"
      )),
      # Back button. The CSS file places it at the right spot
      shiny::actionLink(
        shiny::NS(id, "back"),
        curbcut::cc_t("Back to the place explorer")
      ),
      bottom =
        # Scale slider
        curbcut::zoom_UI(
          id = shiny::NS(id, id),
          zoom_levels = pe_vars$map_zoom_levels
        ),
    ),
    
    # Map
    map_UI(id = shiny::NS(id, id)),
    
    # Main panel
    shinyjs::hidden(
      shiny::div(
        id = shiny::NS(id, "place_exp_main_panel"),
        shiny::htmlOutput(shiny::NS(id, "loader")),
        shiny::htmlOutput(shiny::NS(id, "main_panel")),
        shiny::downloadButton(
          class = "download_portrait",
          outputId = shiny::NS(id, "download_portrait"),
          label = cc_t("Download regional portrait")
        )
      )
    )
  )
}