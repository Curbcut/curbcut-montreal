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
                      compare_UI(NS(id, "alley"), var_list_alley),
                      hr(),
                      fluidRow(column(width = 7, h4(i18n$t("Explore"))),
                               column(width = 5, align = "right", 
                                      actionLink(inputId = NS(id, "hide"), 
                                                 label = i18n$t("Hide")))),
                      # conditionalPanel(
                      #   condition = "output.hide_status == 1", ns = NS(id),
                        uiOutput(NS(id, "alley_explore")),
                        # conditionalPanel(
                        #   condition = "output.poly_selected == 1", ns = NS(id),
                        #   actionLink(inputId = NS(id, "clear_selection"),
                        #              label = "Clear selection")))
                      # ,
                      dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "alley")))
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
        zoom = map_zoom, location = map_location) %>%
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
    
    # # Zoom level
    # observeEvent(input$map_view_change$zoom, {
    #   rv_alley$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "DA_2",
    #                              input$map_view_change$zoom >= 12 ~ "DA",
    #                              input$map_view_change$zoom >= 10.5 ~ "CT",
    #                              TRUE ~ "borough")
    # })
    # 
    # Compare panel
    var_right <- compare_server("alley", var_list_alley,
                                       reactive(rv_alley$zoom))
    
    # Explore panel
    output$alley_explore <- renderUI({
      
      if (rv_alley$poly_selected %in% alley_text$ID) {
      
        text_to_display <- 
          alley_text %>%
          filter(ID == rv_alley$poly_selected) %>% 
          select(-ID) %>% 
          select_if(~sum(!is.na(.)) > 0) %>% 
          {if (nrow(.) >0) as.list(.) else NULL}
        
        original_list <- text_to_display
        
        # In the meanwhile of finding a better way to do this:
        # NAME
        if (!is.null(text_to_display$name)) {
          text_to_display$name = 
            str_glue(sus_translate(paste0("<p><b>{original_list$name}</b></p>")))
        } 
        # FIRST INAUGURATION
        if (!is.null(text_to_display$first_alley)) {
          text_to_display$first_alley = 
            str_glue(sus_translate(paste0("<p>The first green alley inauguration in ",
                                          "{original_list$name} was in {original_list$first_alley}.</p>")))
        } 
        # APPLICATION PROCESS
        if (!is.null(text_to_display$app_process)) {
          text_to_display$app_process = 
            str_glue(sus_translate(paste0("<p>The application process for green alleys asks for ", 
                                          "a {str_replace(original_list$app_process, ',', ', and')}.</p>")))
        } 
        # MANAGEMENT
        if (!is.null(text_to_display$management)) {
          text_to_display$management = 
            str_glue(sus_translate(paste0("<p>In terms of management, ",
                                          "{str_to_lower(original_list$management)}.</p>")))
        } 
        # BUDGET
        if (!is.null(text_to_display$budget)) {
          text_to_display$budget = 
            str_glue(sus_translate(paste0("<p>Budget: {original_list$budget}</p>")))
        } 
        # GUIDE
        if (!is.null(text_to_display$guide)) {
          text_to_display$guide = 
            str_glue(sus_translate(paste0("<p><a href = {original_list$guide}>",
                                          "The green alley guide of {original_list$name}</a></p>")))
        } 
        # CONTACT
        if (!is.null(text_to_display$contact)) {
          text_to_display$contact = 
            str_glue(sus_translate(paste0("<p>Contact: {original_list$contact}</p>")))
        } 
        
        
        if (!is.null(text_to_display)) {
          HTML(unlist(text_to_display))
        }
        
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
        
        original_list <- text_to_display
        
        # In the meanwhile of finding a better way to do this:
        # NAME
        if (!is.null(text_to_display$name)) {
          text_to_display$name = 
            str_glue(sus_translate(paste0("<p><b>{original_list$name}</b></p>")))
        } 
        # INAUGURATION
        if (!is.null(text_to_display$created)) {
          text_to_display$created = 
            str_glue(sus_translate(paste0("<p>It has been inaugurated in ",
                                          "{original_list$created}.</p>")))
        } 
        # ALLEY TYPE
        if (!is.null(text_to_display$type)) {
          original_list$type
          type_explain <- switch(original_list$type, 
                                 green = sus_translate('This green alley is very green'),
                                 community = sus_translate('This green alley is not that green, but have a lot of community elements (generally children-oriented)'),
                                 mixed = sus_translate('This green alley has both green and community elements'),
                                 none = sus_translate('This green alley is neither green nor community-oriented (basically grey alley)'))
          text_to_display$type = 
            str_glue(paste0("<p>{type_explain}.</p>"))
        } 
        # MANAGEMENT
        if (!is.null(text_to_display$description)) {
          text_to_display$description = 
            str_glue(sus_translate(paste0("<p>Description: ",
                                          "{str_to_sentence(original_list$description)}</p>")))
        } 
        # BUDGET
        if (!is.null(text_to_display$circulation)) {
          text_to_display$circulation = 
            str_glue(sus_translate(paste0("<p>To the circulation, it is {original_list$circulation}.</p>")))
        } 
        
        if (!is.null(text_to_display$photo_ID)) {
          text_to_display$photo_ID =
              str_glue(
                sus_translate(
                  paste0('<p><img src = "alleys/{original_list$photo_ID}", ',
                       'alt = "Photo of the selected green alley", ',
                       'style = "max-width: 100%;"></p>')))
        }
        
        if (!is.null(text_to_display)) {
          HTML(unlist(text_to_display))
        }
        
      }
      
    })
    
    outputOptions(output, "alley_explore", suspendWhenHidden = FALSE)
    
    # Did-you-know panel
    dyk_server("dyk", reactive("alley_ind"), var_right)
    
    # # Left map
    # small_map_server("left", reactive(paste0(
    #   "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
    
    # Bivariate legend
    legend_bivar_server("alley", var_right)
    
    # Update map in response to user input
    observeEvent(input$focus_visited, {
      if (input$focus_visited) {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "borough") %>% 
          # clear_polygon(layer_id = "alleys") %>% 
          add_polygon(data = alleys[!alleys$visited,],
                      stroke_width = 15, stroke_colour = "#CFCFCF",
                      fill_colour = "#CFCFCF", layer_id = "alleys_void",
                      update_view = FALSE, id = "ID", auto_highlight = FALSE) %>%
          add_polygon(data = alleys[alleys$visited,],
                      stroke_width = 15, stroke_colour = "fill",
                      layer_id = "alleys_visited",
                      update_view = FALSE, id = "ID", auto_highlight = TRUE,
                      highlight_colour = "#FFFFFF90",
                      legend = alley_legend_en)
        } else {
          # Exact same as the initial
          mapdeck_update(map_id = NS(id, "map")) %>%
            # For some reason, legend is sticky!
            clear_legend(layer_id = "alleys_visited") %>% 
            clear_legend(layer_id = "alleys_void") %>% 
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
    
    # # Clear poly_selected on zoom
    # observeEvent(rv_alley$zoom, {rv_alley$poly_selected <- NA},
    #              ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_alley$poly_selected, {
      
      #If not lags when an alley is selected
      if (rv_alley$poly_selected %in% alley_text$ID) {
        
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
    
    # # Clear click status if prompted
    # # (Namespacing hardwired to explore module; could make it return a reactive)
    # observeEvent(input$`explore-clear_selection`, {
    #   rv_alley$poly_selected <- NA})
    
  })
}
