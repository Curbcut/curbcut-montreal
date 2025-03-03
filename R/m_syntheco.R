### syntheco PAGE ###########################################################

# Mapping of variable names to titles based on the PDF
variable_titles <- list(
  AGEGRP = "Age Group",
  MarStH = "Marital Status",
  SEX = "Sex",
  Mob1 = "Mobility Status 1 Year Ago",
  Mob5 = "Mobility Status 5 Years Ago",
  PR1 = "Province/Territory of Residence 1 Year Ago",
  PR5 = "Province/Territory of Residence 5 Years Ago",
  ABOID = "Indigenous Identity",
  BFNMEMB = "Membership in a First Nation or Indian Band",
  REGIND = "Registered or Treaty Indian Status",
  ETHDER = "Ethnic Origin",
  VISMIN = "Visible Minority",
  FOL = "First Official Language Spoken",
  HLAEN = "Home Language - English",
  HLAFR = "Home Language - French",
  HLANO = "Home Language - Non-official Language",
  HLBEN = "Home Language Regularly Spoken - English",
  HLBFR = "Home Language Regularly Spoken - French",
  HLBNO = "Home Language Regularly Spoken - Non-official Language",
  KOL = "Knowledge of Official Languages",
  LWAEN = "Language of Work - English",
  LWAFR = "Language of Work - French",
  LWANO = "Language of Work - Non-official Language",
  LWBEN = "Language of Work Regularly Spoken - English",
  LWBFR = "Language of Work Regularly Spoken - French",
  LWBNO = "Language of Work Regularly Spoken - Non-official Language",
  MTNEn = "Mother Tongue - English",
  MTNFr = "Mother Tongue - French",
  MTNNO = "Mother Tongue - Non-official Language",
  AGEIMM = "Age at Immigration",
  CITIZEN = "Citizenship Status",
  CITOTH = "Other Country of Citizenship",
  GENSTAT = "Generation Status",
  IMMSTAT = "Immigrant Status",
  POB = "Place of Birth",
  POBF = "Place of Birth of Father",
  POBM = "Place of Birth of Mother",
  YRIMM = "Year of Immigration",
  ATTSCH = "School Attendance",
  CIP2011 = "Major Field of Study",
  HDGREE = "Highest Certificate, Diploma or Degree",
  LOC_ST_RES = "Location of Study Compared with Residence",
  LOCSTUD = "Location of Study",
  SSGRAD = "Secondary School Graduation Certificate",
  COW = "Class of Worker",
  FPTWK = "Full-time or Part-time Weeks Worked",
  HRSWRK = "Hours Worked",
  LFTAG = "Labour Force Status",
  LSTWRK = "When Last Worked",
  NAICS = "Industry",
  NOCS = "Occupation",
  WKSWRK = "Weeks Worked",
  WRKACT = "Work Activity",
  DIST = "Distance from Home to Work",
  DUR = "Commuting Duration",
  LEAVE = "Time Leaving for Work",
  MODE = "Main Mode of Commuting",
  OCC = "Commuting Vehicle Occupancy",
  POWST = "Place of Work Status",
  PWPR = "Place of Work Province",
  EFDECILE = "National Economic Family After-tax Income Decile",
  EFDIMBM = "Disposable Income for MBM",
  EMPIN = "Employment Income",
  GTRFS = "Government Transfers",
  INCTAX = "Income Taxes",
  LOLICOA = "Low-income Status Based on LICO-AT",
  LOLICOB = "Low-income Status Based on LICO-BT",
  LOLIMA = "Low-income Status Based on LIM-AT",
  LOLIMB = "Low-income Status Based on LIM-BT",
  LOMBM = "Low-income Status Based on MBM",
  MRKINC = "Market Income",
  TOTINC = "Total Income",
  TOTINC_AT = "Total After-tax Income",
  CF_RP = "Census Family Reference Person",
  CFSTAT = "Household Living Arrangements",
  CFSTRUCT = "Census Family Structure",
  EF_RP = "Economic Family Reference Person",
  HHMAINP = "Person responsible for household payments",
  PRIHM = "Primary Household Maintainer",
  BEDRM = "Bedrooms",
  BUILT = "Period of Construction",
  CONDO = "Condominium Status",
  DTYPE = "Structural Type of Dwelling",
  FCOND = "Condominium fees",
  NOS = "Housing Suitability",
  PRESMORTG = "Presence of Mortgage",
  REPAIR = "Dwelling Condition",
  ROOM = "Number of Rooms",
  SHELCO = "Shelter Cost",
  SUBSIDY = "Subsidized Housing",
  TENUR = "Tenure",
  VALUE = "Value of Dwelling",
  CMA = "Census Metropolitan Area",
  PR = "Province or Territory",
  HH_ID = "Household ID",
  EF_ID = "Economic Family ID",
  CF_ID = "Census Family ID",
  PP_ID = "Person ID",
  WEIGHT = "Weighting Factor"
)

# Grouping variables into categories
categories <- list(
  Demography = c("AGEGRP", "MarStH", "SEX"),
  Mobility = c("Mob1", "Mob5", "PR1", "PR5"),
  `Aboriginal Population` = c("ABOID", "BFNMEMB", "REGIND"),
  `Ethnic origin and visible minority` = c("ETHDER", "VISMIN"),
  Language = c("FOL", "HLAEN", "HLAFR", "HLANO", "HLBEN", "HLBFR", "HLBNO", "KOL", "LWAEN", "LWAFR", "LWANO", "LWBEN", "LWBFR", "LWBNO", "MTNEn", "MTNFr", "MTNNO"),
  `Place of birth, immigration and citizenship` = c("AGEIMM", "CITIZEN", "CITOTH", "GENSTAT", "IMMSTAT", "POB", "POBF", "POBM", "YRIMM"),
  Education = c("ATTSCH", "CIP2011", "HDGREE", "LOC_ST_RES", "LOCSTUD", "SSGRAD"),
  `Labour Market Activities` = c("COW", "FPTWK", "HRSWRK", "LFTAG", "LSTWRK", "NAICS", "NOCS", "WKSWRK", "WRKACT"),
  `Journey to Work` = c("DIST", "DUR", "LEAVE", "MODE", "OCC", "POWST", "PWPR"),
  Income = c("EFDECILE", "EFDIMBM", "EMPIN", "GTRFS", "INCTAX", "LOLICOA", "LOLICOB", "LOLIMA", "LOLIMB", "LOMBM", "MRKINC", "TOTINC", "TOTINC_AT"),
  `Families and family composition` = c("CF_RP", "CFSTAT", "CFSTRUCT", "EF_RP"),
  Households = c("HHMAINP", "PRIHM"),
  `Dwelling: Housing and shelter cost` = c("BEDRM", "BUILT", "CONDO", "DTYPE", "FCOND", "NOS", "PRESMORTG", "REPAIR", "ROOM", "SHELCO", "SUBSIDY", "TENUR", "VALUE"),
  Weighting = c(paste0("WT",1:16))
)

dwelling_description <- list(
  BEDRM = data.frame(
    Code = c(0:5, 8),
    Description = c(
      "No bedrooms", "1 bedroom", paste(2:5, "bedrooms"), "Not available"
    )
  ),
  BUILT = data.frame(
    Value = c(1:11, 88),
    Label = c(
      "1920 or before", "1921 to 1945", "1946 to 1960", "1961 to 1970", 
      "1971 to 1980", "1981 to 1990", "1991 to 1995", "1996 to 2000", 
      "2001 to 2005", "2006 to 2010", "2011 to 2016", "Not available"
    )
  ),
  CONDO = data.frame(
    Code = c(0,1,8),
    Description = c(
      "Not condominium", "Condominium", "Not available"
    )
  ),
  DTYPE = data.frame(
    Code = c(1:3,8),
    Description = c(
      "Single-detached house", "Apartment", "Other dwelling", "Not available"
    )
  ),
  FCOND = data.frame(
    Code = c(99999999, 200, 617, 300, 100 ,250, 150, 350, 50, 400, 1),
    Description = c(
      "Not available", curbcut:::convert_unit.dollar(x = c(200, 617, 300, 100 ,250, 150, 350, 50, 400, 1))
    )
  ),
  NOS = data.frame(
    Code = c(1,2,8),
    Description = c(
      "Suitable", "Not suitable", "Not available"
    )
  ),
  PRESMORTG = data.frame(
    Code = c(1,2,8,9),
    Description = c(
      "Without mortgage", "With mortgage", "Not available", "Not applicable"
    )
  ),
  REPAIR = data.frame(
    Code = c(1:3),
    Description = c(
      "Regular maintenance needed", "Minor repairs needed", "Major repairs needed"
    )
  ),
  ROOM = data.frame(
    Code = c(1:11, 88),
    Description = c(
      "1 room", paste(2:10, "rooms"), "11 rooms or more", "Not available"
    )
  ),
  SHELCO = data.frame(
    Code = c(1600L, 2417L, 800L, 500L, 0L, 1200L, 600L, 300L, 400L, 900L, 
             1500L, 1000L, 100L, 1400L, 1100L, 700L, 1300L, 1700L, 1800L, 
             1L, 200L),
    Description = c(
      curbcut:::convert_unit.dollar(
        x = c(1600L, 2417L, 800L, 500L, 0L, 1200L, 600L, 300L, 400L, 900L, 
              1500L, 1000L, 100L, 1400L, 1100L, 700L, 1300L, 1700L, 1800L, 
              1L, 200L))
    )
  ),
  SUBSIDY = data.frame(
    Code = c(0,1,8,9),
    Description = c(
      "Not a subsidized dwelling", "A subsidized dwelling", "Not available",
      "Not applicable"
    )
  ),
  TENUR = data.frame(
    Code = c(1,2,8),
    Description = c(
      "Owned by a member of the household", "Rented or Band housing",
      "Not available"
    )
  ),
  VALUE = data.frame(
    Code = c(99999999L, 420000L, 200000L, 270000L, 300000L, 310000L, 240000L, 
             410000L, 360000L, 220000L, 951579L, 210000L, 460000L, 520000L, 
             500000L, 280000L, 350000L, 250000L, 400000L, 330000L, 430000L, 
             480000L, 50000L, 290000L, 230000L, 60000L, 40000L, 340000L, 170000L, 
             370000L, 260000L, 150000L, 490000L, 450000L, 180000L, 510000L, 
             190000L, 100000L, 550000L, 160000L, 570000L, 10000L, 140000L, 
             380000L, 390000L, 320000L, 470000L, 560000L, 600000L, 440000L, 
             30000L, 530000L, 540000L, 580000L, 110000L, 130000L, 20000L, 
             1L, 120000L, 90000L, 80000L, 70000L, 590000L),
    Description = c(
      "Missing",
      curbcut:::convert_unit.dollar(
        x = c(420000L, 200000L, 270000L, 300000L, 310000L, 240000L, 
              410000L, 360000L, 220000L, 951579L, 210000L, 460000L, 520000L, 
              500000L, 280000L, 350000L, 250000L, 400000L, 330000L, 430000L, 
              480000L, 50000L, 290000L, 230000L, 60000L, 40000L, 340000L, 170000L, 
              370000L, 260000L, 150000L, 490000L, 450000L, 180000L, 510000L, 
              190000L, 100000L, 550000L, 160000L, 570000L, 10000L, 140000L, 
              380000L, 390000L, 320000L, 470000L, 560000L, 600000L, 440000L, 
              30000L, 530000L, 540000L, 580000L, 110000L, 130000L, 20000L, 
              1L, 120000L, 90000L, 80000L, 70000L, 590000L))
      
    )
  )
)

# Example of the qualitative descriptions for variables
variables_description <- list(
  # Demography
  AGEGRP = data.frame(
    Code = c(1:13, 88),
    Description = c(
      "0 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years",
      "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years",
      "45 to 49 years", "50 to 54 years", "55 to 64 years", "65 to 74 years",
      "75 years and over", "Not available"
    )
  ),
  MarStH = data.frame(
    Code = c(1, 2, 3, 4, 5, 6, 8),
    Description = c(
      "Married (and not separated)", "Living common law", 
      "Separated, but still legally married", "Divorced", "Widowed", 
      "Never married (single)", "Not available"
    )
  ),
  SEX = data.frame(
    Code = c(1, 2, 9),
    Description = c(
      "Male", "Female", "Not available"
    )
  ),
  # Mobility
  Mob1 = data.frame(
    Code = c(1, 2, 3, 4, 5, 6, 8, 9),
    Description = c(
      "Non-movers", "Non-migrants", "Different CSD, same census division", 
      "Different CD, same province", "Interprovincial migrants", 
      "External migrants", "Not available", "Not applicable"
    )
  ),
  Mob5 = data.frame(
    Code = c(1, 2, 3, 4, 5, 6, 9),
    Description = c(
      "Non-movers", "Non-migrants", "Different CSD, same census division", 
      "Different CD, same province", "Interprovincial migrants", 
      "External migrants", "Not applicable"
    )
  ),
  PR1 = data.frame(
    Code = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 70, 88, 99),
    Description = c(
      "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
      "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
      "Alberta", "British Columbia", "Northern Canada", "Not available", 
      "Not applicable"
    )
  ),
  PR5 = data.frame(
    Code = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 70, 99),
    Description = c(
      "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
      "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
      "Alberta", "British Columbia", "Northern Canada", "Not applicable"
    )
  ),
  # Aboriginal Population
  ABOID = data.frame(
    Code = c(1:6, 7),
    Description = c(
      "First Nations (North American Indian)",
      "Métis",
      "Inuk (Inuit)",
      "Multiple Indigenous responses",
      "Aboriginal responses not included elsewhere",
      "Non-Aboriginal identity",
      "Not available"
    )
  ),
  BFNMEMB = data.frame(
    Code = c(1, 2, 8),
    Description = c(
      "Not a member of a First Nation or Indian band", 
      "Member of a First Nation or Indian band", "Not available"
    )
  ),
  REGIND = data.frame(
    Code = c(1, 2, 8),
    Description = c(
      "Not a Registered or Treaty Indian", "Registered or Treaty Indian", "Not available"
    )
  ),
  # Ethnic Origin and Visible Minority
  ETHDER = data.frame(
    Code = c(1:8, 88),
    Description = c(
      "British Isles origins", 
      "French origins", 
      "North American Aboriginal origins", 
      "Other North American origins", 
      "European origins (excluding British Isles and French origins)", 
      "Asian origins", 
      "Other single origins", 
      "Multiple origins", 
      "Not available"
    )
  ),
  VISMIN = data.frame(
    Code = c(1,2,8),
    Description = c(
      "Visible minority", "Not a visible minority", "Not available"
    )
  )
)


# Function to map code to description
get_variable_description <- function(var_name, code) {
  if (!var_name %in% names(variables_description)) return(as.character(code))
  desc <- variables_description[[var_name]]$Description[
    variables_description[[var_name]]$Code == code]
  if (length(desc) == 0) return("Unknown")
  return(desc)
}

# Function to map code to description
get_dwelling_description <- function(var_name, code) {
  if (!var_name %in% names(dwelling_description)) return(as.character(code))
  desc <- dwelling_description[[var_name]]$Description[
    dwelling_description[[var_name]]$Code == code]
  if (length(desc) == 0) return("Unknown")
  return(desc)
}

syntheco_explore_text <- function(select_id) {
  if (is.na(select_id)) return(paste0("<p>No household selected. Zoom in and click on a ",
                                      "point on the map to view detailed ",
                                      "characteristics of a synthetic household.</p>"))
  
  out <- db_get(select = "*", from = "syntheco_pop", where = list(HH_ID = select_id))
  
  # Number of households
  verb <- if (nrow(out) == 1) "is" else "are"
  first_part <- 
    sprintf(paste0("<p>In the synthetic population dataset, there %s <b>%s</b> ",
                   "individual%s living in household #<b>%s</b>.</p>"),
            verb, nrow(out), if (nrow(out) > 1) "s" else "", 
            gsub("^syntheco_hh_", "", select_id))
  
  # Dwelling description
  dwelling_title <- sprintf("%s - %s", 
                            get_dwelling_description("DTYPE", out[1,]$DTYPE), 
                            get_dwelling_description("BEDRM", out[1,]$BEDRM))
  dwelling_table <- "<table style='width:100%; border-collapse: collapse;margin-bottom:10px; font-size:small;'>"
  dwelling_table <- paste0(dwelling_table, "<tr style='background-color: #f2f2f2;'><th style='border: 1px solid black; padding: 5px;'>Attribute</th><th style='border: 1px solid black; padding: 5px;'>Description</th></tr>")
  for (i in seq_along(categories$`Dwelling: Housing and shelter cost`)) {
    var <- categories$`Dwelling: Housing and shelter cost`[i]
    if (!is.na(out[1, var])) {
      description <- get_dwelling_description(var, out[1, var])
      title <- if (var %in% names(variable_titles)) variable_titles[[var]] else var
      row_style <- if (i %% 2 == 0) "background-color: #ffffff;" else "background-color: #f9f9f9;"
      dwelling_table <- paste0(
        dwelling_table, 
        sprintf("<tr style='%s'><td style='border: 1px solid black; padding: 5px;'><b>%s</b></td><td style='border: 1px solid black; padding: 5px;'>%s</td></tr>", row_style, title, description)
      )
    }
  }
  dwelling_table <- paste0(dwelling_table, "</table>")
  dwelling_part <- sprintf("<h4>Dwelling: %s</h4>%s", dwelling_title, dwelling_table)
  
  # Details for each individual with styling
  individuals_part <- "<h4 style='margin-top: 20px; margin-bottom: 10px;'>Residents</h4>"
  for (i in 1:nrow(out)) {
    individual <- out[i, ]
    individual_title <- sprintf("%s - %s", get_variable_description("SEX", individual$SEX), get_variable_description("AGEGRP", individual$AGEGRP))
    categories_part <- ""
    for (category in names(categories)) {
      if (category == "Dwelling: Housing and shelter cost") next
      category_variables <- categories[[category]]
      category_details <- ""
      for (var in category_variables) {
        if (!is.na(individual[[var]])) {
          description <- get_variable_description(var, individual[[var]])
          title <- if (var %in% names(variable_titles)) variable_titles[[var]] else var
          category_details <- paste0(
            category_details, 
            sprintf("<li style='padding: 2px 0; font-size:small'><b>%s:</b> %s</li>", title, description)
          )
        }
      }
      # Note for understanding value meanings
      if (category %in% c("Demography", "Mobility", "Aboriginal Population", "Ethnic origin and visible minority")) {
        categories_part <- paste0(
          categories_part,
          sprintf(
            "<details style='cursor: pointer; margin: 4px; margin-left: 7px; font-size:small;'>
            <summary style='display: list-item; list-style-type: disclosure-closed;'>%s</summary>
            <ul style='padding-left: 20px;'>%s</ul>
            </details>",
            category, category_details
          )
        )
      } else {
        categories_part <- paste0(
          categories_part,
          sprintf(
            "<details style='cursor: pointer; margin: 4px; margin-left: 7px; font-size:small;'>
            <summary style='display: list-item; list-style-type: disclosure-closed;'>%s</summary>
            <p style='margin-top: 10px;'><b>Note:</b> For detailed value descriptions, refer to the PDF available at the following link: 
            <a href='https://s3.amazonaws.com/curbcut.public.resources/syntheco/pumf-98M0002-E-2016-hierarchical.pdf' target='_blank'>Download PDF</a>.</p>
            <ul style='padding-left: 20px;'>%s</ul>
            </details>",
            category, category_details
          )
        )
      }
    }
    individual_info <- sprintf(
      "<details style='cursor: pointer; margin-bottom: 10px; border: 1px solid #ccc; padding: 10px; border-radius: 5px;'>
      <summary style='display: list-item; list-style-type: disclosure-closed; font-weight: bold; font-size: 1em;'>%s</summary>
      %s
      </details>",
      individual_title, categories_part
    )
    individuals_part <- paste0(individuals_part, individual_info)
  }
  
  # Combine all parts
  full_text <- paste0(first_part, dwelling_part, individuals_part)
  
  return(full_text)
}




syntheco_UI <- function(id) {
  page <- modules[modules$id == id, ]
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))

  # Grab the possible regions for the module
  possible_regions <- page$regions[[1]][1]
  
  shiny::tagList(
    tags$head(
      tags$style(HTML("
      #syntheco-right_panel {
        transition: min-width 0.5s ease;
      }
    "))),
    
    # Sidebar
    shiny::div(
      `data-theme` = theme_lowercased,
      curbcut::sidebar_UI(
        id = shiny::NS(id, id),
        shiny::tags$a(
          class = "btn btn-default shiny-download-link", 
          style = "font-size:small;padding:25px;",
          href = "https://s3.amazonaws.com/curbcut.public.resources/syntheco/syntheco_population.zip",
          shiny::tags$div(
            style = "display: flex; align-items: center;",
            shiny::tags$div(
              shiny::icon("download"),
              style = "flex-shrink: 0; margin-right: 10px;"
            ),
            shiny::tags$div(
              style = "display: flex; flex-direction: column; justify-content: center;",
              shiny::tags$span("Download the synthetic"),
              shiny::tags$span("population (269.1 MB)")
            )
          )
        ),
        bottom = shiny::tagList()
      ),
      
      # Map
      curbcut::map_js_UI(shiny::NS(id, id)),
      
      # Right panel
      curbcut::right_panel(
        id = id,
        curbcut::explore_UI(shiny::NS(id, id))
      )
    )
  )
}

# Create the basic server function
syntheco_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    # shiny::observe(shinyjs::hide("syntheco-left_widgets"))
    shiny::observeEvent(r[[id]]$select_id(), {
      if (!is.na(r[[id]]$select_id())) {
        shinyjs::runjs('document.getElementById("syntheco-right_panel").style.setProperty("min-width", "500px", "important");')
      } else {
        shinyjs::runjs('document.getElementById("syntheco-right_panel").style.setProperty("min-width", "var(--w-panel)", "important");')
      }
    })
    
    # Sidebar
    sidebar_server(id = id, r = r)
    
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
    
    # # SQL retrieval
    # data <- shiny::reactive({
    #   ni_data_get(var_left = var_left(),
    #               custom_priorities = custom_priorities(),
    #               main_slider = main_slider(),
    #               ni_slider = ni_slider())
    # })
    
    # Update the `r[[id]]$df` reactive as some module want it to not bel NULL
    shiny::observe({
      r[[id]]$scale <- shiny::reactiveVal("raster")
    })

    # Choose tileset
    tile <- shiny::reactive("syntheco6")
    
    # Update map in response to variable changes or zooming
    map_viewstate <- syntheco_map_js_server(
      id = id,
      r = r,
      tile = tile,
      coords = r[[id]]$coords,
      zoom = r[[id]]$zoom
    )
    
    # Update selected ID
    update_select_id(id = id, r = r)

    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      table_fun = shiny::reactive(syntheco_explore_text),
      table_args = shiny::reactive(list(select_id = r[[id]]$select_id())),
      graph_fun = shiny::reactive(\(x) NULL),
      graph_args = shiny::reactive(list(NULL))
    )
    # 
    # # Bookmarking
    # bookmark_server(
    #   id = id,
    #   r = r,
    #   select_id = r[[id]]$select_id,
    #   map_viewstate = map_viewstate
    # )
  })
}
