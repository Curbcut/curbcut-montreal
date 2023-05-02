### NATURAL INFRASTRUCTURE MODULE #########################################

process_ni_sliders <- function(x, y, z) {
  x1 <- ni_helper(x)
  y1 <- ni_helper(y)
  z1 <- ni_helper(z)
  
  if (x1 == y1 && x1 == z1) return(c(1, 1, 1))
  if (x1 == 0 && y1 == 0)   return(c(0, 0, 1))
  if (x1 == 0 && z1 == 0)   return(c(0, 1, 0))
  if (y1 == 0 && z1 == 0)   return(c(1, 0, 0))
  if (x1 == 0 && y1 == z1)  return(c(0, 1, 1))
  if (y1 == 0 && x1 == z1)  return(c(1, 0, 1))
  if (z1 == 0 && x1 == y1)  return(c(1, 1, 0))
  if (x1 == 0 && y1 == 0.5 && z1 == 1) return(c(0, 1, 2))
  if (x1 == 0 && y1 == 1 && z1 == 0.5) return(c(0, 2, 1))
  if (x1 == 0.5 && y1 == 0 && z1 == 1) return(c(1, 0, 2))
  if (x1 == 0.5 && y1 == 1 && z1 == 0) return(c(1, 2, 0))
  if (x1 == 0.5 && y1 == 0.5 && z1 == 1) return(c(1, 1, 2))
  if (x1 == 0.5 && y1 == 1 && z1 == 0.5) return(c(1, 2, 1))
  if (x1 == 1 && y1 == 0.5 && z1 == 0.5) return(c(2, 1, 1))
  if (x1 == 1 && y1 == 0 && z1 == 0.5) return(c(2, 0, 1))
  if (x1 == 1 && y1 == 0.5 && z1 == 0) return(c(2, 1, 0))
  if (x1 == 0.5 && y1 == 1 && z1 == 1) return(c(1, 2, 2))
  if (x1 == 1 && y1 == 0.5 && z1 == 1) return(c(2, 1, 2))
  if (x1 == 1 && y1 == 1 && z1 == 0.5) return(c(2, 2, 1))
  return(c(x1, y1, z1))
  
}

ni_helper <- function(x) {
  if (x == "Not important" || x == "Pas important") return(0)
  if (x == "Somewhat important" || x == "Peu important") return(0.5)
  if (x == "Important" || x == "Important") return(1)
  if (x == "Very important" || x == "Très important") return(1.5)
  if (x == "Extremely important" || x == "Extrêmement important") return(2)
}

scale_fill_natural_inf <- function(var, natural_inf_colours) {
  
  colour_table <- if (is.null(natural_inf_colours)) {
    colours_dfs$viridis
  } else natural_inf_colours
  
  rdeck::scale_color_category(
    col = !!rlang::sym(var), 
    palette = colour_table$fill,
    unmapped_color = "#FFFFFF00", 
    levels = colour_table$group,
    legend = FALSE)
  
}

# UI ----------------------------------------------------------------------

naturalinf_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      shiny::NS(id, id),
      shiny::div(class = "sus-sidebar-widgets",
                 curbcut::picker_UI(NS(id, id),
                                    picker_id = "d1",
                                    var_list = var_left_list_1_natural_inf,
                                    label = curbcut::cc_t("Theme")),
                 curbcut::picker_UI(NS(id, id),
                                    picker_id = "d2",
                                    var_list = list("----" = " "),
                                    label = curbcut::cc_t("Indicator")),
                 curbcut::slider_UI(NS(id, id),
                                    label = curbcut::cc_t("Amount of territory to protect"),
                                    min = 0,
                                    max = 25,
                                    step = 1,
                                    value = 17,
                                    post = "%"),
                 curbcut::checkbox_UI(NS(id, id),
                                      label = curbcut::cc_t("Custom priorities")),
                 curbcut::slider_text_UI(NS(id, id),
                                         slider_text_id = "bio",
                                         label = curbcut::cc_t("Biodiversity conservation"),
                                         choices = custom_slider_choices,
                                         selected = "Important"),
                 curbcut::slider_text_UI(NS(id, id),
                                         slider_text_id = "hea",
                                         label = curbcut::cc_t("Heat island reduction"),
                                         choices = custom_slider_choices,
                                         selected = "Important"),
                 curbcut::slider_text_UI(NS(id, id),
                                         slider_text_id = "flo",
                                         label = curbcut::cc_t("Flood prevention"),
                                         choices = custom_slider_choices,
                                         selected = "Important")
      ),
      bottom = shiny::tagList(legend_UI(NS(id, id)))),
    
    # Map
    curbcut::map_UI(shiny::NS(id, id)),
    
    # Right panel
    curbcut::right_panel(
      id = id,
      curbcut::explore_UI(NS(id, id)))
  )
}


# Server ------------------------------------------------------------------

naturalinf_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Left variable (theme and indicator)
    var_left_1 <- curbcut::picker_server(
      id = id,
      r = r,
      picker_id = "d1",
      var_list = shiny::reactive(var_left_list_1_natural_inf))

    var_left_2_list <- shiny::reactive({
      if (var_left_1() == "c_priority") {
        list("----" = " ")
      } else var_left_list_2_natural_inf[[var_left_1()]]
    })

    var_left_2 <- curbcut::picker_server(
      id = id,
      r = r,
      picker_id = "d2",
      var_list = var_left_2_list)

    var_left <- shiny::reactive(if (var_left_2() != " ") var_left_2() else
      var_left_1())
    
    
    # Main slider value
    main_slider <- curbcut::slider_server(id = id)
    
    # Custom priorities
    custom_priorities <- 
      curbcut::checkbox_server(
        id = id, 
        r = r, 
        label = shiny::reactive("Custom priorities"))
    
    # Hide and show UI elements depending on the picked choices
    shiny::observeEvent(var_left_1(), {
      shinyjs::toggle(NS(id, "ccpicker_d2"), condition = var_left_1() != "c_priority")
      shinyjs::toggle(NS(id, "cccheckbox_cbx"), condition = var_left_1() == "c_priority")
    })
    shiny::observeEvent(custom_priorities(), {
      shinyjs::toggle(shiny::NS(id, "ccslidertext_bio"), condition = custom_priorities())
      shinyjs::toggle(shiny::NS(id, "ccslidertext_hea"), condition = custom_priorities())
      shinyjs::toggle(shiny::NS(id, "ccslidertext_flo"), condition = custom_priorities())
    })
    shiny::observeEvent(var_left(), {
      shinyjs::toggle(shiny::NS(id, "ccslider_sld"), condition = var_left() == "c_priority")
    })

    # Reset checkbox value on first dropdown change
    shiny::observeEvent(var_left_1(), {
      shiny::updateCheckboxInput(
        session = session, 
        inputId = NS(id, "cccheckbox_cbx"),
        value = FALSE)
    })

    # Custom priority sliders
    s_bio <- curbcut::slider_text_server(
      id = id, 
      r = r, 
      slider_text_id = "bio",
      choices = shiny::reactive(custom_slider_choices))
    s_hea <- curbcut::slider_text_server(
      id = id, 
      r = r, 
      slider_text_id = "hea",
      choices = shiny::reactive(custom_slider_choices))
    s_flo <- curbcut::slider_text_server(
      id = id, 
      r = r, 
      slider_text_id = "flo",
      choices = shiny::reactive(custom_slider_choices))
    ni_slider <- shiny::reactive(process_ni_sliders(s_bio(), s_hea(), s_flo()))

    # Sidebar
    curbcut::sidebar_server(id = id, r = r)

    # SQL retrieval
    data <- reactive({
      db_call <-
        if (var_left() == "c_priority") {
          if (!custom_priorities()) {
            paste0("SELECT * FROM natural_inf_original_priorities ",
                   "WHERE slider = ", main_slider())
          } else {
            paste0("SELECT * FROM natural_inf_custom_explore ",
                   "WHERE slider = ", main_slider(),
                   " AND biodiversity = ", ni_slider()[1],
                   " AND heat_island = ", ni_slider()[2],
                   " AND flood = ", ni_slider()[3])
          }
        } else {
          paste0("SELECT * FROM natural_inf_explore")
        }

      do.call("dbGetQuery", list(rlang::sym("naturalinf_conn"), db_call))
    })

    # Map custom colours
    natural_inf_colours <- reactive({
      if (var_left() == "c_priority") {

        if (!custom_priorities()) {

          remove <- 50 - main_slider()
          ni_colour_table <- colours_dfs$viridis
          # ni_colour_table$fill <- gsub("FF$", "", ni_colour_table$fill)
          ni_colour_table$fill[ni_colour_table$group <= remove] <-
            paste0(substr(ni_colour_table$fill[
              ni_colour_table$group <= remove], 1, 7), "00")
          ni_colour_table

        } else {

          if (main_slider() == 0) {
            data.frame(group = "ABCD", fill = "#FFFFFF00")
          } else {
            db_call <- paste0("SELECT * FROM natural_inf_custom_", main_slider(),
                              " WHERE biodiversity = ", ni_slider()[1],
                              " AND heat_island = ", ni_slider()[2],
                              " AND flood = ", ni_slider()[3])
            out <- do.call("dbGetQuery", list(rlang::sym("naturalinf_conn"), db_call))[, c("group", "value")]
            names(out)[2] <- "fill"
            out
          }
        }
      } else NULL

    })

    # Update the `r[[id]]$vars` reactive
    vars <- shiny::reactive(vars_build(var_left = var_left(), df = "raster"))
    
    # Build an empty data
    data <- shiny::reactive(data.frame())
    
    # Legend
    curbcut::legend_server(
      id = id,
      r = r,
      vars = vars,
      data = data,
      df = shiny::reactive("raster")
    )
    
    # Composite variable for map
    map_var <- shiny::reactive(if (custom_priorities()) "ID" else var_left())
    
    # Choose tileset
    tile <- shiny::reactive({
      if (var_left() == "natural_inf-c_priority" && custom_priorities()) {
        return("natural_inf-custom")
      }
      return(paste0("natural_inf-", var_left()))})
    
    observe(print(tile()))
    
    # Map
    curbcut::map_server(
      id = id,
      tile = tile,
      data_colours = data,
      zoom_levels = shiny::reactive(NULL),
      select_id = r[[id]]$select_id,
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      lwd_fun = shiny::reactive(\(...) 0),
      fill_fun = shiny::reactive(scale_fill_natural_inf),
      fill_args = shiny::reactive(list(map_var(), natural_inf_colours()))
    )
    # 
    # # Explore panel
    # curbcut::explore_server(
    #   id = id,
    #   r = r,
    #   data = data,
    #   region = shiny::reactive(zoom_levels()$region),
    #   vars = r[[id]]$vars,
    #   df = r[[id]]$df,
    #   select_id = r[[id]]$select_id
    # )
    # 
    # # Bookmarking
    # curbcut::bookmark_server(
    #   id = id,
    #   r = r,
    #   select_id = r[[id]]$select_id,
    #   map_viewstate = map_viewstate
    # )
  })
}
