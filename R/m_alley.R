### GREEN ALLEY MODULE #########################################################

# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  tabItem(tabName = "alley",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   materialSwitch(inputId = NS(id, "focus_visited"),
                                  label = i18n$t("Focus on green alleys visited by our team"), 
                                  right = TRUE)),
          right_panel(id, 
                      # compare_UI(NS(id, "alley"), var_list_alley),
                      # hr(),
                      fluidRow(column(width = 7, h4(i18n$t("Explore"))),
                               column(width = 5, align = "right", 
                                      actionLink(inputId = NS(id, "hide"), 
                                                 label = i18n$t("Hide")))),
                      shinyjs::useShinyjs(), # Needed to hide panels
                      uiOutput(NS(id, "alley_explore")),
                      # conditionalPanel(
                      #   condition = "output.poly_selected == 1", ns = NS(id),
                      #   actionLink(inputId = NS(id, "clear_selection"),
                      #              label = "Clear selection"))),
                      # dyk_UI(NS(id, "dyk"))),
          )
          # legend_bivar_UI(NS(id, "alley"))
  )
}


# Server ------------------------------------------------------------------

alley_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "alley")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_alley,
        zoom = 11, location = map_location) %>%
        add_polygon(data = borough[borough$ID %in% alley_text$ID,], 
                    stroke_width = 10, stroke_colour = "#000000",
                    fill_colour = "#FFFFFF10", update_view = FALSE, id = "ID",
                    layer_id = "borough", auto_highlight = TRUE,
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
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_alley$zoom <- case_when(input$map_view_change$zoom >= 13 ~ 15,
                                 input$map_view_change$zoom >= 12 ~ 30,
                                 input$map_view_change$zoom >= 11.5 ~ 50,
                                 TRUE ~ width_alley_higher_zoom)
    })

    # Explore panel
    output$alley_explore <- renderUI({
      
      if (rv_alley$poly_selected %in% alley_text$ID) {
        
        text_to_display <- 
          alley_text %>%
          filter(ID == rv_alley$poly_selected) %>% 
          select(-ID) %>% 
          select_if(~sum(!is.na(.)) > 0) %>% 
          {if (nrow(.) >0) as.list(.) else NULL}
        
        text_to_display <- alley_borough_text(text_to_display)
        
      } else if (rv_alley$poly_selected %in% alleys[alleys$visited,]$ID) {
        
        text_to_display <- 
          alleys %>%
          st_drop_geometry() %>% 
          filter(ID == rv_alley$poly_selected) %>% 
          mutate(name = str_glue(sus_translate(paste0("<p><b>{str_to_title(name)} in ",
                                                      "{name_2}</b></p>")))) %>% 
          select(-ID, -CSDUID, -visited, -name_2, -fill) %>% 
          select_if(~sum(!is.na(.)) > 0) %>% 
          {if (nrow(.) >0) as.list(.) else NULL}
        
        text_to_display <- alley_alleys_text(text_to_display)
        
      }
      
      if (exists("text_to_display") && !is.null(text_to_display)) {
        HTML(unlist(text_to_display))
      }
      
    })
    
    # Update map in response to user input
    observeEvent({input$focus_visited
      rv_alley$zoom},{
        if (input$focus_visited) {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "borough") %>%
            clear_polygon(layer_id = "poly_highlight") %>% 
            # clear_polygon(layer_id = "alleys") %>% 
            add_polygon(data = alleys[!alleys$visited,],
                        stroke_width = 15, stroke_colour = "#CFCFCF",
                        fill_colour = "#CFCFCF", layer_id = "alleys_void",
                        update_view = FALSE, id = "ID", auto_highlight = FALSE) %>%
            add_polygon(data = alleys[alleys$visited,],
                        stroke_width = rv_alley$zoom, stroke_colour = "fill",
                        layer_id = "alleys_visited",
                        update_view = FALSE, id = "ID", auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90",
                        legend = alley_legend_en)
        } else {
          # Exact same as the initial
          mapdeck_update(map_id = NS(id, "map")) %>%
            # For some reason, legend is sticky!
            clear_legend(layer_id = "alleys_visited") %>% 
            clear_polygon(layer_id = "alleys_visited") %>% 
            add_polygon(data = borough[borough$ID %in% alley_text$ID,], 
                        stroke_width = 10, stroke_colour = "#000000",
                        fill_colour = "#FFFFFF10", update_view = FALSE, id = "ID",
                        layer_id = "borough", auto_highlight = TRUE,
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
        
      })
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_alley$poly_selected <- NA
      } else rv_alley$poly_selected <- lst$object$properties$id
    })
    
    # # Clear poly_selected when input$focus_visited is clicked
    observeEvent(input$focus_visited, {rv_alley$poly_selected <- NA},
                 ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_alley$poly_selected, {
        
        #If not lags when an alley is selected
        if (rv_alley$poly_selected %in% alley_text$ID || 
            is.na(rv_alley$poly_selected)) {
          
          if (!is.na(rv_alley$poly_selected)) {
            # width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
            data_to_add <-
              borough %>%
              filter(ID == rv_alley$poly_selected)
            
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
      
      if(input$hide %% 2 == 0){
        shinyjs::show(id = "alley_explore")
        txt <- sus_translate("Hide")
      }else{
        shinyjs::hide(id = "alley_explore")
        txt <- sus_translate("Show")
      }
      updateActionButton(session, "hide", label = txt)
      
    })
    
    
    # # Clear click status if prompted
    # # (Namespacing hardwired to explore module; could make it return a reactive)
    # observeEvent(input$`explore-clear_selection`, {
    #   rv_alley$poly_selected <- NA})
    
    
  })
}
