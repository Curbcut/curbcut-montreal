### ACCESS PAGE ################################################################

# GLOBAL ------------------------------------------------------------------

`access_default_region` <- unlist(modules$regions[modules$id == "access"])[1]
`access_mzp` <-
  eval(parse(text = paste0("map_zoom_levels_", `access_default_region`)))


# UI ----------------------------------------------------------------------

`access_UI` <- function(id) {
  id_map <- paste0(id, "-map")

  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        auto_vars_UI(NS(id, id), var_list = make_dropdown(only = NULL, only_vars = c(
          "access_foot_30_arenas_amusement", "access_foot_30_cinemas_motion", "access_foot_30_communitycentres_individual",
          "access_foot_30_education_elementary", "access_foot_30_education_colleges", "access_foot_30_education_other",
          "access_foot_30_fooddistribution_retail", "access_foot_30_fooddistribution_grocery", "access_foot_30_fooddistribution_fruit",
          "access_foot_30_fooddistribution_meat", "access_foot_30_fooddistribution_miscellaneous", "access_foot_30_fooddistribution_dairy",
          "access_foot_30_policeservices_police", "access_foot_30_religiousbuildings_religious", "access_foot_30_retail_department",
          "access_foot_30_retail_hardware", "access_foot_30_wifihotspots_public", "access_foot_30_healthcare_doctors",
          "access_foot_30_healthcare_nursing", "access_foot_30_healthcare_hospitals", "access_foot_30_healthcare_other",
          "access_foot_30_education_total", "access_foot_30_fooddistribution_total", "access_foot_30_healthcare_total",
          "access_foot_30_retail_total", "access_bicycle_30_arenas_amusement", "access_bicycle_30_cinemas_motion",
          "access_bicycle_30_communitycentres_individual", "access_bicycle_30_education_elementary", "access_bicycle_30_education_colleges",
          "access_bicycle_30_education_other", "access_bicycle_30_fooddistribution_retail", "access_bicycle_30_fooddistribution_grocery",
          "access_bicycle_30_fooddistribution_fruit", "access_bicycle_30_fooddistribution_meat", "access_bicycle_30_fooddistribution_miscellaneous",
          "access_bicycle_30_fooddistribution_dairy", "access_bicycle_30_policeservices_police", "access_bicycle_30_religiousbuildings_religious",
          "access_bicycle_30_retail_department", "access_bicycle_30_retail_hardware", "access_bicycle_30_wifihotspots_public",
          "access_bicycle_30_healthcare_doctors", "access_bicycle_30_healthcare_nursing", "access_bicycle_30_healthcare_hospitals",
          "access_bicycle_30_healthcare_other", "access_bicycle_30_education_total", "access_bicycle_30_fooddistribution_total",
          "access_bicycle_30_healthcare_total", "access_bicycle_30_retail_total", "access_car_30_arenas_amusement",
          "access_car_30_cinemas_motion", "access_car_30_communitycentres_individual", "access_car_30_education_elementary",
          "access_car_30_education_colleges", "access_car_30_education_other", "access_car_30_fooddistribution_retail",
          "access_car_30_fooddistribution_grocery", "access_car_30_fooddistribution_fruit", "access_car_30_fooddistribution_meat",
          "access_car_30_fooddistribution_miscellaneous", "access_car_30_fooddistribution_dairy", "access_car_30_policeservices_police",
          "access_car_30_religiousbuildings_religious", "access_car_30_retail_department", "access_car_30_retail_hardware",
          "access_car_30_wifihotspots_public", "access_car_30_healthcare_doctors", "access_car_30_healthcare_nursing",
          "access_car_30_healthcare_hospitals", "access_car_30_healthcare_other", "access_car_30_education_total",
          "access_car_30_fooddistribution_total", "access_car_30_healthcare_total", "access_car_30_retail_total",
          "access_foot_30_daycarespots", "access_bicycle_30_daycarespots", "access_car_30_daycarespots"
        )), label = curbcut::cc_t(translation = translation, "Access"))
      ),
      bottom = div(
        class = "bottom_sidebar",
        tagList(
          legend_UI(NS(id, id)),
          zoom_UI(NS(id, id), `access_mzp`)
        )
      )
    ),

    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id))
    )
  )
}


# Server ------------------------------------------------------------------

`access_server` <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")

    # Initial reactives
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, `access_mzp`))
    poi <- reactiveVal(NULL)

    # Map
    output[[id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = isolate(r[[id]]$zoom())
      ))
    })

    # Zoom and POI reactives
    observe({
      r[[id]]$zoom(get_zoom(get_view_state(id_map)$zoom))
      new_poi <- observe_map(get_view_state(id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
        (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi())))) {
        poi(new_poi)
      }
    }) |> bindEvent(get_view_state(id_map))

    # Map zoom levels change depending on r$geo()
    map_zoom_levels <- eventReactive(r$geo(), {
      get_zoom_levels(
        default = `access_default_region`,
        geo = r$geo(),
        var_left = isolate(var_left())
      )
    })

    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(
        r[[id]]$zoom(), map_zoom_levels()$levels,
        map_zoom_levels()$region
      )
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom(), map_zoom_levels()$levels)

    # Click reactive
    observe({
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) &&
        selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else {
        r[[id]]$select_id(selection)
      }
    }) |> bindEvent(get_clicked_object(id_map))

    # Default location
    observe({
      if (is.null(r$default_select_id())) {
        return(NULL)
      }
      new_id <- data()$ID[data()$ID %in%
        r$default_select_id()[[gsub("_.*", "", r[[id]]$df())]]]
      if (length(new_id) == 0) {
        return(NULL)
      }
      r[[id]]$select_id(new_id)
    }) |> bindEvent(r$default_select_id(), r[[id]]$df())

    # Choose tileset
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = map_zoom_levels
    )

    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |>
      bindEvent(tile(), zoom_string())





    # Time variable
    time <- eventReactive(var_left_1(), {
      tb <- tables_in_sql[[r[[id]]$df()]]
      # Get the available table
      avl_table <- tb[grepl(var_left_1(), tb)]
      # Flag if there's more than one time
      if (length(avl_table) != 1) {
        stop(paste0(
          "There are 0 or more than 1 table corresponding to `",
          var_left_1(), "`. The assumption is that there is only one ",
          "possible time()."
        ))
      }
      # Return the time
      return(gsub(paste0(var_left_1(), "_"), "", avl_table))
    })

    # Left variable server
    var_left_1 <- auto_vars_server(
      id = id,
      r = r,
      var_list = make_dropdown(
        only = NULL,
        only_vars = c(
          "access_foot_30_arenas_amusement", "access_foot_30_cinemas_motion", "access_foot_30_communitycentres_individual",
          "access_foot_30_education_elementary", "access_foot_30_education_colleges", "access_foot_30_education_other",
          "access_foot_30_fooddistribution_retail", "access_foot_30_fooddistribution_grocery", "access_foot_30_fooddistribution_fruit",
          "access_foot_30_fooddistribution_meat", "access_foot_30_fooddistribution_miscellaneous", "access_foot_30_fooddistribution_dairy",
          "access_foot_30_policeservices_police", "access_foot_30_religiousbuildings_religious", "access_foot_30_retail_department",
          "access_foot_30_retail_hardware", "access_foot_30_wifihotspots_public", "access_foot_30_healthcare_doctors",
          "access_foot_30_healthcare_nursing", "access_foot_30_healthcare_hospitals", "access_foot_30_healthcare_other",
          "access_foot_30_education_total", "access_foot_30_fooddistribution_total", "access_foot_30_healthcare_total",
          "access_foot_30_retail_total", "access_bicycle_30_arenas_amusement", "access_bicycle_30_cinemas_motion",
          "access_bicycle_30_communitycentres_individual", "access_bicycle_30_education_elementary", "access_bicycle_30_education_colleges",
          "access_bicycle_30_education_other", "access_bicycle_30_fooddistribution_retail", "access_bicycle_30_fooddistribution_grocery",
          "access_bicycle_30_fooddistribution_fruit", "access_bicycle_30_fooddistribution_meat", "access_bicycle_30_fooddistribution_miscellaneous",
          "access_bicycle_30_fooddistribution_dairy", "access_bicycle_30_policeservices_police", "access_bicycle_30_religiousbuildings_religious",
          "access_bicycle_30_retail_department", "access_bicycle_30_retail_hardware", "access_bicycle_30_wifihotspots_public",
          "access_bicycle_30_healthcare_doctors", "access_bicycle_30_healthcare_nursing", "access_bicycle_30_healthcare_hospitals",
          "access_bicycle_30_healthcare_other", "access_bicycle_30_education_total", "access_bicycle_30_fooddistribution_total",
          "access_bicycle_30_healthcare_total", "access_bicycle_30_retail_total", "access_car_30_arenas_amusement",
          "access_car_30_cinemas_motion", "access_car_30_communitycentres_individual", "access_car_30_education_elementary",
          "access_car_30_education_colleges", "access_car_30_education_other", "access_car_30_fooddistribution_retail",
          "access_car_30_fooddistribution_grocery", "access_car_30_fooddistribution_fruit", "access_car_30_fooddistribution_meat",
          "access_car_30_fooddistribution_miscellaneous", "access_car_30_fooddistribution_dairy", "access_car_30_policeservices_police",
          "access_car_30_religiousbuildings_religious", "access_car_30_retail_department", "access_car_30_retail_hardware",
          "access_car_30_wifihotspots_public", "access_car_30_healthcare_doctors", "access_car_30_healthcare_nursing",
          "access_car_30_healthcare_hospitals", "access_car_30_healthcare_other", "access_car_30_education_total",
          "access_car_30_fooddistribution_total", "access_car_30_healthcare_total", "access_car_30_retail_total",
          "access_foot_30_daycarespots", "access_bicycle_30_daycarespots", "access_car_30_daycarespots"
        )
      ),
      auto_disable = FALSE
    )
    var_left <- reactive(paste(var_left_1(), time(), sep = "_"))

    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = make_dropdown(
        multi_year = FALSE,
        only_vars = c(
          "housing_tenant", "housing_rent", "housing_repairs",
          "housing_value", "housing_unafford", "housing_unsuit",
          "housing_stress_renter", "housing_stress_owner", "housing_mobility_one",
          "housing_mobility_five", "housing_single_detached", "inc_median_income",
          "inc_50", "inc_100", "inc_high",
          "inc_limat", "iden_imm", "iden_imm_new",
          "iden_vm", "iden_aboriginal", "trans_car",
          "trans_walk_or_bike", "trans_transit", "trans_t_15",
          "trans_t_45", "trans_t_45_plus", "emp_professional",
          "emp_creative", "family_children", "family_one_person",
          "lang_french_only", "lang_eng_only", "lang_french_eng",
          "lang_no_official", "age_0_14", "age_15_64",
          "age_65_plus", "edu_bachelor_above", "edu_no_degree"
        ),
        only = NULL,
        exclude = NULL,
        compare = TRUE
      ),
      time = time
    )

    # Sidebar
    sidebar_server(id = id, r = r)

    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      geo = map_zoom_levels()$region,
      var_left = var_left(),
      var_right = var_right()
    ))

    # Data for tile coloring
    data_color <- reactive(get_data_color(
      map_zoom_levels = map_zoom_levels()$levels,
      geo = map_zoom_levels()$region,
      var_left = var_left(),
      var_right = var_right()
    ))

    # Year disclaimer
    year_disclaimer_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      time = time
    )

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      geo = reactive(map_zoom_levels()$region)
    )

    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi
    )

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile,
      data_color = data_color,
      zoom_levels = reactive(map_zoom_levels()$levels)
    )

    # Update map labels
    label_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile
    )

    # Explore panel
    explore_content <- explore_server(
      id = id,
      r = r,
      data = data,
      geo = reactive(map_zoom_levels()$region),
      var_left = var_left,
      var_right = var_right
    )

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(id_map)),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c())
    )

    # Data transparency and export
    r[[id]]$export_data <- reactive(data_export(
      id = id,
      data = data(),
      var_left = var_left(),
      var_right = var_right(),
      df = r[[id]]$df()
    ))
  })
}
