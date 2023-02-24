### CANALE PAGE ################################################################

# GLOBAL ------------------------------------------------------------------

`canale_default_region` <- unlist(modules$regions[modules$id == "canale"])[1]
`canale_mzp` <-
  eval(parse(text = paste0("map_zoom_levels_", `canale_default_region`)))
default_region <- modules$regions[modules$id == "canale"][[1]][1]


# UI ----------------------------------------------------------------------

`canale_UI` <- function(id) {
  id_map <- paste0(id, "-map")

  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      id = NS(id, id),
      bottom =  shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        curbcut::zoom_UI(shiny::NS(id, id), `canale_mzp`)
      )
    ),

    # Map
    curbcut::map_UI(NS(id, id)),

    # Right panel
    curbcut::right_panel(
      id = id,
      compare_UI(NS(id, id), curbcut::dropdown_make(vars = " ", compare = TRUE)),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id))
    )
  )
}


# Server ------------------------------------------------------------------

`canale_server` <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    rv_zoom_string <- reactiveVal(
      curbcut::zoom_get_string(zoom = map_zoom, 
                               zoom_levels = `canale_mzp`, 
                               region = default_region))
    
    # Zoom and POI reactives when the view state of the map changes.
    observeEvent(rdeck::get_view_state(id_map), {
      r[[id]]$zoom(curbcut::zoom_get(rdeck::get_view_state(id_map)$zoom))
      r[[id]]$poi(curbcut::update_poi(id = id, poi = r[[id]]$poi()))
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
          region = zoom_levels()$region)
      })
    })

    # Click reactive
    observeEvent(rdeck::get_clicked_object(id_map)$ID, {
      r[[id]]$select_id({
        curbcut::update_select_id(id = id, select_id = r[[id]]$select_id())
      })
    })
    
    # Default location
    observe({
      r[[id]]$select_id({
        curbcut::update_select_id_from_default(
          data = data(),
          default_select_ids = r$default_select_ids(),
          select_id = isolate(r[[id]]$select_id()))
      })
    })

    # Choose tileset
    tile <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels
    )
    
    # Get df 
    observeEvent({
      tile()
      rv_zoom_string()}, {
        r[[id]]$df(curbcut::update_df(tile = tile(), 
                                      zoom_string = rv_zoom_string()))
      })
    
    # Time
    time <- reactive("2016")

    # Left variable
    var_left <- reactive(paste("canale", time(), sep = "_"))

    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = curbcut::dropdown_make(vars = 
        c(
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
        compare = TRUE
      ),
      time = time
    )
    
    vars <- reactive(curbcut::vars_build(var_left = var_left(),
                                         var_right = var_right(),
                                         df = r[[id]]$df()))
    
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
    curbcut::map_server(id = id,
                        tile = tile,
                        data_colours = data_colours,
                        select_id = r[[id]]$select_id,
                        zoom_levels = reactive(zoom_levels()$zoom_levels),
                        zoom = r[[id]]$zoom)

    # Update map labels
    curbcut::label_server(id = id,
                          tile = tile,
                          zoom = r[[id]]$zoom,
                          zoom_levels = reactive(zoom_levels()$zoom_levels),
                          region = reactive(zoom_levels()$region))

    # # Explore panel
    # explore_content <- explore_server(
    #   id = id,
    #   r = r,
    #   data = data,
    #   geo = reactive(zoom_levels()$region),
    #   var_left = var_left,
    #   var_right = var_right
    # )

    # # Bookmarking
    # bookmark_server(
    #   id = id,
    #   r = r,
    #   s_id = r[[id]]$select_id,
    #   df = r[[id]]$df,
    #   map_viewstate = reactive(get_view_state(id_map)),
    #   var_left = var_left,
    #   var_right = var_right,
    #   more_args = reactive(c())
    # )

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
