### HOUSING PAGE ###############################################################

# GLOBAL ------------------------------------------------------------------

`housing_default_region` <- unlist(modules$regions[modules$id == "housing"])[1]
`housing_mzp` <-
  eval(parse(text = paste0("map_zoom_levels_", `housing_default_region`)))
default_region <- modules$regions[modules$id == "housing"][[1]][1]

# UI ----------------------------------------------------------------------

`housing_UI` <- function(id) {
  shiny::tagList(
    tags$head(tags$script(src = "../../rightmenu.js")),
    # Sidebar
    curbcut::sidebar_UI(
      id = shiny::NS(id, id),
      curbcut::picker_UI(id = NS(id, id), var_list = c(
        "housing_tenant", "housing_rent", "housing_repairs",
        "housing_value", "housing_unafford", "housing_unsuit",
        "housing_stress_renter", "housing_stress_owner", "housing_mobility_one",
        "housing_mobility_five", "housing_single_detached"
      )),
      curbcut::slider_UI(id = NS(id, id), slider_id = "slu", min = 1996, max = 2021),
      curbcut::slider_UI(id = NS(id, id), slider_id = "slb", min = 1996, max = 2021, 
                         label = cc_t("Select two years"), value = c(2006, 2021)),
      curbcut::checkbox_UI(id = NS(id, id), label = curbcut::cc_t("Compare dates"), value = FALSE),
      year_disclaimer_UI(NS(id, id)),
      bottom = shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        curbcut::zoom_UI(shiny::NS(id, id), `housing_mzp`)
      )
    ),
    
    # Map
    curbcut::map_UI(NS(id, id)),
    
    # Change view (Map/Data/Place explorer)
    curbcut::panel_view_UI(id = NS(id, id)),
    
    # Right panel
    curbcut::right_panel(
      id = id,
      curbcut::compare_UI(
        id = NS(id, id),
        var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
      ),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id))
    )
  )
}


# Server ------------------------------------------------------------------

`housing_server` <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    rv_zoom_string <- reactiveVal(
      curbcut::zoom_get_string(
        zoom = map_zoom,
        zoom_levels = `housing_mzp`,
        region = default_region
      )
    )
    
    # Zoom and POI reactives when the view state of the map changes.
    observeEvent(map_viewstate(), {
      r[[id]]$zoom(curbcut::zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(curbcut::update_poi(
        id = id, poi = r[[id]]$poi(),
        map_viewstate = map_viewstate()
      ))
    })
    
    # Map zoom levels change depending on r$region()
    zoom_levels <-
      reactive(curbcut::zoom_get_levels(id = id, region = r$region()))
    
    # Zoom string reactive
    observe({
      rv_zoom_string({
        curbcut::zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = zoom_levels()$zoom_levels,
          region = zoom_levels()$region
        )
      })
    })
    
    # Update selected ID
    curbcut::update_select_id(id = id, r = r, data = data)
    
    # Choose tileset
    tile <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels
    )
    
    # Get df
    observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        r[[id]]$df(curbcut::update_df(
          tile = tile(),
          zoom_string = rv_zoom_string()
        ))
      }
    )
    
    # Time
    slider_uni <- curbcut::slider_server(id = id, slider_id = "slu")
    slider_bi <- curbcut::slider_server(id = id, slider_id = "slb")
    slider_switch <- curbcut::checkbox_server(id = id, r = r, label = shiny::reactive("Compare dates"))
    time <- shiny::reactive(if (slider_switch()) slider_bi() else slider_uni())
    
    # Enable or disable first and second slider
    shiny::observeEvent(slider_switch(), {
      shinyjs::toggle(NS(id, "ccslider_slu"), condition = !slider_switch())
      shinyjs::toggle(NS(id, "ccslider_slb"), condition = slider_switch())
    })
    
    # Left variable
    var_left <- curbcut::picker_server(
      id = id,
      r = r,
      var_list = curbcut::dropdown_make(
        vars = c(
          "housing_tenant", "housing_rent", "housing_repairs",
          "housing_value", "housing_unafford", "housing_unsuit",
          "housing_stress_renter", "housing_stress_owner", "housing_mobility_one",
          "housing_mobility_five", "housing_single_detached"
        ),
        compare = FALSE
      ),
      time = time
    )
    
    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = curbcut::dropdown_make(
        vars = c(
          "inc_median_income", "inc_50", "inc_100",
          "inc_high", "inc_limat", "iden_imm",
          "iden_imm_new", "iden_vm", "iden_aboriginal",
          "trans_car", "trans_walk_or_bike", "trans_transit",
          "trans_t_15", "trans_t_45", "trans_t_45_plus",
          "family_children", "family_one_person", "lang_french_only",
          "lang_eng_only", "lang_french_eng", "lang_no_official",
          "age_0_14", "age_15_64", "age_65_plus",
          "edu_bachelor_above", "edu_no_degree"
        ),
        compare = TRUE
      ),
      time = time
    )
    
    # The `vars` reactive
    vars <- reactive(curbcut::vars_build(
      var_left = var_left(),
      var_right = var_right(),
      df = r[[id]]$df()
    ))
    
    # Sidebar
    sidebar_server(id = id, r = r)
    
    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Data
    data <- reactive(curbcut::data_get(
      vars = vars(),
      df = r[[id]]$df()
    ))
    
    # Data for tile coloring
    data_colours <- reactive(curbcut::data_get_colours(
      vars = vars(),
      region = zoom_levels()$region,
      zoom_levels = zoom_levels()$zoom_levels
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
    curbcut::legend_server(
      id = id,
      r = r,
      vars,
      data = data,
      df = r[[id]]$df
    )
    
    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = r[[id]]$poi
    )
    
    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_server(
      id = id,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords
    )
    
    # Update map labels
    curbcut::label_server(
      id = id,
      tile = tile,
      zoom = r[[id]]$zoom,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      region = reactive(zoom_levels()$region)
    )
    
    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = reactive(zoom_levels()$region),
      vars = vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id)
    
    # Bookmarking
    curbcut::bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )
    
    # Change view
    curbcut::panel_view_server(id = id,
                               r = r,
                               vars = vars,
                               data = data,
                               zoom_levels = reactive(zoom_levels()$zoom_levels))
    
  })
}
