### GENTRIFICATION MODULE #####################################################

# UI ----------------------------------------------------------------------

gentrification_UI <- function(id) {
  ns_id <- "gentrification"
  
  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        slider_UI(NS(id, ns_id), 
                  label = cc_t(r = r, "Select two years"),
                  value = c("2006", "2016")),
        checkbox_UI(NS(id, ns_id),
                    label = cc_t(r = r, "Review a single variable ",
                                          "part of the index")),
        select_var_UI(NS(id, ns_id), 
                      var_list = var_list_left_gentrification),
        year_disclaimer_UI(NS(id, ns_id))
      ),
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels_CMA)))),
    
    # Map
    div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
    
    # Right panel
    right_panel(
      id = id, 
      compare_UI(NS(id,ns_id), make_dropdown(multi_year = T)),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
  ))
}


# Server ------------------------------------------------------------------

gentrification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "gentrification"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels_CMA))
    click_id <- reactiveVal(NULL)

    # Map
    output$map <- renderMapdeck(mapdeck(
      style = map_style,
      token = map_token,
      zoom = map_zoom,
      location = map_loc))

    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels_CMA))})

    # Click reactive
    observeEvent(input$map_polygon_click, {
      click_id(get_click(input$map_polygon_click))})

    # Zoom level for data
    df <- zoom_server(
      id = ns_id,
      zoom = zoom,
      zoom_levels = reactive(map_zoom_levels_CMA))

    # Checkbox value
    check_single_var <- checkbox_server(id = ns_id)

    # Get time from slider
    time <- slider_server(id = ns_id)

    observe({
      if (length(unique(time())) == 1 && !check_single_var()) {
        shinyalert::shinyalert(text = paste0(
          "Gentrification is a process that ",
          "can only be quantified over time. ",
          " Please, select two different years."),
          type = "error")
      }
    })

    # Greyed out right list options, depending of the year chosen
    var_list_housing_right_disabled <- reactive({
      if (!check_single_var()) NULL else {
        unlist(make_dropdown(multi_year = T)) %in% var_list_left_gentrification
      }
    })

    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id,
      var_list = make_dropdown(multi_year = TRUE),
      df = df,
      time = time)

    # Get single_var value to use if check_single_var() is clicked
    single_var <- select_var_server(
      id = ns_id,
      var_list = reactive(var_list_left_gentrification),
      time = time,
      df = df)

    # If check_single_var() is clicked, toggle on the dropdown menu
    observeEvent(check_single_var(), {
      toggle("gentrification-var", condition = check_single_var())
    })

    # Construct left variable string
    var_left <- reactive({
      if (!check_single_var()) {
        str_remove(paste("gentrification_ind", time(), sep = "_"), "_ $")
      } else {
        single_var()
      }
    })

    # Sidebar
    sidebar_server(id = ns_id, x = "gentrification")

    # Data
    data <- reactive(get_data(
      df = df(),
      var_left = var_left(),
      var_right = var_right()))

    # Disclaimers and how to read the map
    year_disclaimer_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      # If the same time is selected twice, other disclaimer
      more = reactive({length(unique(time())) == 1 &&
          !check_single_var()}),
      more_text = paste0(
        "<p style='font-size:11px;'>",
        "Gentrification is a process that can only be quantified over time. ",
        "Please, select two different years.</p>"))

    # Update map in response to variable changes or zooming
    select_id <- map_change(
      id = ns_id,
      map_id = NS(id, "map"),
      data = data,
      df = df,
      zoom = zoom,
      click = click_id,
    )

    # Legend
    legend_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right)

    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom,
      select_id = select_id)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = NS(id, "map"),
      more_args = reactive(c("c-cbox" = str_extract(check_single_var(), "^."),
                             "s-time" = paste(time(),
                                             collapse = "-")))
    )

    # Update click_id() on bookmark
    observeEvent(r$sus_bookmark$active, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      if (isTRUE(r$sus_bookmark$active)) {
        delay(2000, {
          if (!is.null(r$sus_bookmark$select_id)) {
            if (r$sus_bookmark$select_id != "NA") click_id(r$sus_bookmark$select_id)
          }
        })
      }
      
      # So that bookmarking gets triggered only ONCE
      delay(1500, {r$sus_bookmark$active <- FALSE})      
    }, priority = -2)
    
    # Update click_id() on module link
    observeEvent(r$sus_link$activity, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      delay(2000, {
        if (!is.null(r$sus_link$select_id)) click_id(r$sus_link$select_id)
      })
    }, priority = -2)
  #   
  #   # data naming for data_export
  #   data_export <- data_export_server(id = "gentrification",
  #                                     df = data, var_left = var_left, 
  #                                     var_right = var_right)
  #   
  #   # OUT
  #   reactive({list(module_short_title = "gentrification",
  #                  module_id = "gentrification",
  #                  time = time(),
  #                  data = data_export(),
  #                  token = token_gentrification,
  #                  map_zoom = input$map_view_change$zoom,
  #                  map_loc = c(input$map_view_change$longitude, 
  #                                   input$map_view_change$latitude),
  #                  zoom = zoom(),
  #                  explore_content = explore_content(),
  #                  poly_selected = rv_gentrification$poly_selected,
  #                  legend_graph = legend_graph())})
  #   
  })
}
