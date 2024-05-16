### NATURAL INFRASTRUCTURE MODULE #########################################

var_left_list_1_natural_inf <-
  list("Conservation priority" = "c_priority",
       "Biodiversity" = "c_bio",
       "Flood" = "c_flood",
       "Heat island" = "c_heat")

vars_natural_inf_left_c_bio <-
  list("Contribution to biodiversity conservation" = " ",
       "Habitat quality" = "habitat_qual",
       "Habitat connectivity" = "habitat_con",
       "Favourable climatic conditions" = "favorable_cc")

vars_natural_inf_left_c_flood <-
  list("Contribution to flood prevention" = " ",
       "Flood risk areas" = "flood")

vars_natural_inf_left_c_heat <-
  list("Contribution to heat island reduction" = " ",
       "Heat islands" = "heat",
       "Cool islands" = "cool")

var_left_list_2_natural_inf <- c(list(c_bio = vars_natural_inf_left_c_bio),
                                 list(c_flood = vars_natural_inf_left_c_flood),
                                 list(c_heat = vars_natural_inf_left_c_heat))

custom_slider_choices <-
  c("Not important", "Somewhat important",
    "Important", "Very important",
    "Extremely important")

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

  colour_table$fill <- sapply(colour_table$fill, curbcut::hex8_to_rgba)

  cc.map::map_choropleth_fill_fun(df = colour_table[c("group", "fill")],
                                  get_col = var,
                                  fallback = curbcut::hex8_to_rgba("#FFFFFF00"))
}

ni_data_get <- function(var_left, custom_priorities, main_slider, ni_slider) {
    if (var_left == "c_priority") {
      if (!custom_priorities) {
        db_get(select = "*", from = "natural_inf_original_priorities",
               where = list(slider = main_slider))
      } else {
        db_get(select = "*", from = "natural_inf_custom_explore",
               where = list(slider = main_slider,
                            biodiversity = ni_slider[1],
                            heat_island = ni_slider[2],
                            flood = ni_slider[3]))
      }
    } else {
      db_get(select = "*", from = "natural_inf_explore")
    }
}

info_table_natural_inf <- function(data, vars, lang, ...) {

  # Helper
  title_exp <- \(var) {
    paste0(
      "<li><i>", curbcut::cc_t(lang = lang, curbcut::var_get_title(var = var)),
      ":</i> ", curbcut::cc_t(lang = lang, curbcut::var_get_info(var = var, what = "explanation")))
  }


  # Craft the right text depending on var_left
  if (vars$var_left == "c_priority") {
    conservation_pct <- data$conservation_pct
    slider <- data$slider

    flood <- data$c_flood |> scales::percent()
    biodiversity <- data$c_biodiversity |> scales::percent()
    heat_island <- data$c_heat_island |> scales::percent()

    return(HTML(curbcut::cc_t(
      lang = lang,
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Preserving {slider}% of the overall ",
      "territory as natural infrastructure means that {conservation_pct}% ",
      "of the natural infrastructure of the region would be protected.</p>",
      "<p>This level of protection allows for the ",
      "conservation of {flood} of the runoff reduction, ",
      "{biodiversity} of the biodiversity conservation, and ",
      "{heat_island} of the urban heat island reduction effects ",
      "provided by natural infrastructure in the region.</p>")))

  } else if (vars$var_left %in% c("habitat_qual", "habitat_con", "favorable_cc",
                                  "c_bio")) {

    c_bio <- title_exp("c_bio")
    habitat_qual <- title_exp("habitat_qual")
    habitat_con <- title_exp("habitat_con")
    favorable_cc <- title_exp("favorable_cc")

    return(HTML(curbcut::cc_t(
      lang = lang,
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Biodiversity-related natural ",
      "infrastructure functions include:</p><ul>{c_bio}{habitat_qual}",
      "{habitat_con}{favorable_cc}</ul>")))

  } else if (vars$var_left %in% c("c_flood", "flood")) {
    c_flood <- title_exp("c_flood")
    flood <- title_exp("flood")

    return(HTML(curbcut::cc_t(
      lang = lang,
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Flood-related natural ",
      "infrastructure functions include:</p><ul>{c_flood}{flood}</ul>")))

  } else if (vars$var_left %in% c("c_heat", "heat", "cool")) {
    c_heat <- title_exp("c_heat")
    heat <- title_exp("heat")
    cool <- title_exp("cool")

    return(HTML(curbcut::cc_t(
      lang = lang,
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Heat-related natural ",
      "infrastructure functions include:</p><ul>{c_heat}{heat}{cool}</ul>")))

  }
}

explore_graph_natural_inf <- function(data, vars, lang, ...) {

  if (vars$var_left == "c_priority") {

    labels <- sapply(c("Flood", "Biodiversity",
                       "Heat island"), cc_t, lang = lang,
                     USE.NAMES = FALSE)

    data.frame(labels = labels,
               cons_levels = c(data$c_flood,
                               data$c_biodiversity,
                               data$c_heat_island),
               total_cons = c(1, 1, 1)) |>
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(labels, total_cons), stat = "identity",
                        fill = colours_dfs$qual$fill[c(2, 1, 6)], alpha = 0.2) +
      ggplot2::geom_bar(ggplot2::aes(labels, cons_levels), stat = "identity",
                        fill = colours_dfs$qual$fill[c(2, 1, 6)]) +
      ggplot2::scale_y_continuous(name = NULL,
                                  labels = scales::percent) +
      ggplot2::scale_x_discrete(name = curbcut::cc_t(lang = lang, "Amount protected")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                     legend.position = "none",
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank())

  } else {

    var_titles <- variables[c("var_code", "var_short")]

    data$var_short <- sapply(data$name, \(x) {
      curbcut::var_get_info(var = x, what = "var_short", translate = TRUE,
                            lang = lang)
    })

    var_names <- data$var_short[c(4, 9, 3, 6, 2, 5, 7, 8, 1)]
    data$var_short <- factor(data$var_short, levels = var_names)

    colours_dfs$left_5

    pal <- rev(c(colours_dfs$bivar$fill[c(1,2,3,5,8,9)],
                 colours_dfs$qual$fill[2:length(colours_dfs$qual$fill)]))

    data |>
      ggplot2::ggplot(ggplot2::aes(value_pct, var_short, fill = var_short)) +
      ggplot2::geom_col() +
      ggplot2::geom_hline(yintercept = data$var_short[data$name == vars$var_left],
                          colour = "black", lwd = 1) +
      ggplot2::scale_x_continuous(name = curbcut::cc_t(lang = lang, "Share of Montreal area"),
                                  labels = scales::label_percent(1)) +
      ggplot2::scale_y_discrete(name = NULL) +
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

  }

}

# UI ----------------------------------------------------------------------

natural_inf_UI <- function(id) {
  id_map <- paste0(id, "-map")
  page <- modules[modules$id == id, ]
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))

  shiny::tagList(
    shiny::div(
      `data-theme` = theme_lowercased,
      # Sidebar
      curbcut::sidebar_UI(
        shiny::NS(id, id),
        curbcut::label_indicators(id = shiny::NS(id, "label")),
        curbcut::picker_UI(shiny::NS(id, id),
                           picker_id = "d1",
                           var_list = var_left_list_1_natural_inf,
                           label = curbcut::cc_t("Theme")),
        curbcut::picker_UI(shiny::NS(id, id),
                           picker_id = "d2",
                           var_list = list("----" = " "),
                           label = curbcut::cc_t("Indicator")),
        curbcut::slider_UI(shiny::NS(id, id),
                           label = curbcut::cc_t("Amount of territory to protect"),
                           min = 0,
                           max = 25,
                           step = 1,
                           value = 17,
                           post = "%"),
        curbcut::advanced_controls_UI(
          id = id,
          label = curbcut::cc_t("Custom priorities"),
          curbcut::slider_text_UI(shiny::NS(id, id),
                                  slider_text_id = "bio",
                                  label = curbcut::cc_t("Biodiversity conservation"),
                                  choices = custom_slider_choices,
                                  selected = "Important"),
          curbcut::slider_text_UI(shiny::NS(id, id),
                                  slider_text_id = "hea",
                                  label = curbcut::cc_t("Heat island reduction"),
                                  choices = custom_slider_choices,
                                  selected = "Important"),
          curbcut::slider_text_UI(shiny::NS(id, id),
                                  slider_text_id = "flo",
                                  label = curbcut::cc_t("Flood prevention"),
                                  choices = custom_slider_choices,
                                  selected = "Important")
        ),
        bottom = shiny::tagList(legend_UI(NS(id, id)))),

    # Map
    curbcut::map_js_UI(shiny::NS(id, id)),

    # Right panel
    curbcut::right_panel(
      id = id,
      curbcut::explore_UI(NS(id, id)))
  )
  )
}


# Server ------------------------------------------------------------------

natural_inf_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Populate the empty container created in map_js_UI
    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[1],
        latitude = map_loc[2],
        zoom = map_zoom,
        map_style_id = map_base_style,
        inst_prefix = inst_prefix,
        stories = stories,
        stories_min_zoom = 13
      )
    })

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
      selected = shiny::reactive(var_left_2_list()[[1]]),
      var_list = var_left_2_list)

    var_left <- shiny::reactive({
      if (var_left_2() != " ") var_left_2() else var_left_1()
      })


    # Main slider value
    main_slider <- curbcut::slider_server(id = id)

    # Custom priorities
    custom_priorities <- advanced_controls_server(
      id = id,
      r = r,
      label = shiny::reactive("Custom priorities"))

    # Hide and show UI elements depending on the picked choices
    shiny::observeEvent(var_left_1(), {
      shinyjs::toggle(NS(id, "ccpicker_d2"), condition = var_left_1() != "c_priority")
      shinyjs::toggle(NS(id, "cccheckbox_cbx"), condition = var_left_1() == "c_priority")
      shinyjs::toggle("advanced_controls", condition = var_left_1() == "c_priority")
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

    custom_sl_ch <- shiny::reactive({
      sapply(custom_slider_choices, cc_t, lang = r$lang(), USE.NAMES = FALSE)
    })

    # Custom priority sliders. Addition of a shiny namespace, as these sliders
    # are now withing the advanced control (Additional namespacing).
    s_bio <- curbcut::slider_text_server(
      id = id,
      r = r,
      slider_text_id = "bio",
      choices = custom_sl_ch)
    s_hea <- curbcut::slider_text_server(
      id = id,
      r = r,
      slider_text_id = "hea",
      choices = custom_sl_ch)
    s_flo <- curbcut::slider_text_server(
      id = id,
      r = r,
      slider_text_id = "flo",
      choices = custom_sl_ch)
    ni_slider <- shiny::reactive(process_ni_sliders(s_bio(), s_hea(), s_flo()))

    # Sidebar
    curbcut::sidebar_server(id = id, r = r)

    # SQL retrieval
    data <- shiny::reactive({
      ni_data_get(var_left = var_left(),
                  custom_priorities = custom_priorities(),
                  main_slider = main_slider(),
                  ni_slider = ni_slider())
    })

    # Map custom colours
    natural_inf_colours <- reactive({

      ni_colour_table <- colours_dfs$viridis_25
      # Test with left_5 instead of viridis
      ni_colour_table$fill <- unlist(sapply(colours_dfs$left_5$fill[2:6], rep, 5, simplify = FALSE))
      ni_colour_table$fill <- paste0(ni_colour_table$fill, "FF")

      if (var_left() == "c_priority") {

        if (!custom_priorities()) {

          remove <- 50 - main_slider()
          # ni_colour_table$fill <- gsub("FF$", "", ni_colour_table$fill)
          ni_colour_table$fill[ni_colour_table$group <= remove] <-
            paste0(substr(ni_colour_table$fill[
              ni_colour_table$group <= remove], 1, 7), "00")
          ni_colour_table

        } else {

          if (main_slider() == 0) {
            data.frame(group = "ABCD", fill = "#FFFFFF00")
          } else {
            group_values <- db_get(select = "group_values", from = "natural_inf_custom",
                                   where = list(slider = main_slider(),
                                                biodiversity = ni_slider()[1],
                                                heat_island = ni_slider()[2],
                                                flood = ni_slider()[3]))[[1]]

            out <- data.frame(group = names(group_values[[1]]),
                              fill = unlist(group_values))

            # Switch the viridis colour scale to the left_5
            viridis <- colours_dfs$viridis_25
            names(viridis)[2] <- "fill_viridis"
            viridis <- merge(viridis, ni_colour_table, by = "group")
            viridis <- viridis[2:3]
            out <- merge(out, viridis, by.x = "fill", by.y = "fill_viridis")

            out <- out[c("group", "fill.y")]
            names(out)[2] <- "fill"

            out
          }
        }
      } else ni_colour_table

    })
    
    # Update the `r[[id]]$vars` reactive
    vars <- shiny::reactive(vars_build(var_left = var_left(), scale = "raster", time = NULL)$vars)

    # Update the `r[[id]]$df` reactive as some module want it to not bel NULL
    shiny::observe({
      r[[id]]$scale <- shiny::reactiveVal("raster")
    })

    # Legend
    curbcut::legend_server(
      id = id,
      r = r,
      vars = vars,
      data = data,
      scale = shiny::reactive("raster"),
      time = shiny::reactive(NULL)
    )

    # Composite variable for map
    map_var <- shiny::reactive(if (var_left() == "c_priority" & custom_priorities()) "ID" else var_left())

    # Choose tileset
    tile <- shiny::reactive({
      if (var_left() == "c_priority" & custom_priorities()) {
        return("natural_inf-custom")
      }
      return(paste0("natural_inf-", var_left()))})

    # Map
    map_viewstate <- curbcut::map_js_server(
      id = id,
      r = r,
      tile = tile,
      data_colours = data,
      select_id = r[[id]]$select_id,
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      fill_fun = shiny::reactive(scale_fill_natural_inf),
      fill_fun_args = shiny::reactive(list(map_var(), natural_inf_colours())),
      pickable = shiny::reactive(FALSE),
      stories = NULL,
      vars = vars
    )

    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = shiny::reactive(NULL),
      vars = vars,
      scale = shiny::reactive(NULL),
      select_id = shiny::reactive(NA),
      time = shiny::reactive(NULL),
      zoom_levels = shiny::reactive(NULL),
      table_fun = shiny::reactive(info_table_natural_inf),
      table_args = shiny::reactive(list(data = data(), vars = vars(), lang = r$lang())),
      graph_fun = shiny::reactive(explore_graph_natural_inf),
      graph_args = shiny::reactive(list(data = data(), vars = vars(), lang = r$lang()))
    )

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )
  })
}
