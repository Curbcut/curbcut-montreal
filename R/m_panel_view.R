#' Renders a floating panel with a map view and data view buttons.
#'
#' This function renders a Shiny server module for the `panel_view_UI` UI
#' element which displays a floating panel with buttons to toggle between the map
#' an table/data info. It shows a map when the map button is clicked and a table
#' with data when the data button is clicked. When there is a selection, it also
#' shows a magnifying glass linking to the place explorer.
#'
#' @param id <`character`> The ID of the page in which the panel will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#'
#' @return Panel view module
#' @export
panel_view_server <- function(id, r, vars, data) {
  
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Show the map when the right button is clicked
    shiny::observeEvent(input$panel_map, {
      shinyjs::show(id = "map_div", anim = TRUE, animType = "fade")
      shinyjs::hide(id = "view_data", anim = TRUE, animType = "fade")
    })
    
    # Hide the map and show the data when the right button is clicked
    shiny::observeEvent(input$panel_data, {
      shinyjs::hide(id = "map_div", anim = TRUE, animType = "fade")
      shinyjs::show(id = "view_data", anim = TRUE, animType = "fade")
    })
    
    # Bring the user to the place explorer when there is a selection
    shiny::observeEvent(r[[id]]$select_id(), {
      shinyjs::toggle(id = "panel_selection",
                      condition = !is.na(r[[id]]$select_id()),
                      anim = TRUE, animType = "fade")
    })
    
    # Show the data in the same spot as the map (`map_div`)
    output$data_info <- shiny::renderUI({
      # TKTK data show text
    })
    
    # If the data is private
    private_data <- shiny::reactive({
      vars_ <- vars()[vars() != " "]
      private <- all(sapply(vars_, var_get_info, what = "private"))
      return(private)
    })
    
    # Toggle the table depending if the info is private
    shiny::observeEvent(private_data(), {
      shinyjs::toggle(id = "data_table", condition = !private_data())
    })
    
    # Hide the data if it is private. Prepare the data to be shown and to
    # be downloaded
    datas <- shiny::reactive({
      # If the dataset is private, return an empty dataframe
      if (private_data()) return(data.frame())
      
      # Prepare the pretty table and the download table
      dat <- curbcut:::table_view_prep_table(vars = vars(),
                                            data = data(),
                                            df = r[[id]]$df(),
                                            lang = r$lang())
      
      # Return
      return(dat)
    })
    
    # Place the selection first in the pretty_data
    pretty_data <- shiny::reactive({
      if (is.na(r[[id]]$select_id())) return(datas()$pretty_data)
      
      # Place the selection first
      dat <- datas()$pretty_data
      s_id <- which(dat$ID == r[[id]]$select_id())
      no_s_id <- which(dat$ID != r[[id]]$select_id())
      dat <- dat[c(s_id, no_s_id), ]
      
      return(dat)
    })
    
    # If there is a selection, pre-select it
    update_selection_list <- shiny::reactive({
      if (is.na(r[[id]]$select_id())) return("single")
      sel <- which(pretty_data()$ID == r[[id]]$select_id())
      list(mode = "single", selected = sel, target = "row")
    })
    
    # Make the data a `DT::datatable` and style every column
    datatable_styled <- shiny::reactive({
      dat <- DT::datatable(pretty_data(),
                           selection = update_selection_list(),
                           options = list(autoWidth = TRUE),
                           rownames = FALSE)
      
      for (i in datas()$title_vars) {
        dat <- panel_view_style_cols(var = i, table = dat)
      }
      
      return(dat)
    })
    
    # Show the table
    output$data_table <- DT::renderDT({
      # Recalculate every time the button is pressed
      input$panel_data
      datatable_styled()
    })
    
    # If there is a selection in the table, update the selection
    shiny::observeEvent(input$data_table_rows_selected, {
      # If deselected from the table, return an NA selection
      if (is.null(input$data_table_rows_selected))
        return(r[[id]]$select_id(NA))
      
      # If there is a selection, update the selected id
      new_id <- pretty_data()$ID[input$data_table_rows_selected]
      r[[id]]$select_id(new_id)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observe(print(datas()$data |> class()))
    
    # When the user clicks to download the .csv
    output$download_csv <-
      shiny::downloadHandler(
        filename = paste0(id, "_data.csv"),
        content = function(file) {
          data <- datas()$data
          utils::write.csv(data, file, row.names = FALSE)
        }, contentType = "text/csv")
    
  })
}

#' @describeIn panel_view_server Create the UI for the legend module
#' @export
panel_view_UI <- function(id) {
  
  shiny::tagList(
    shiny::tags$div(
      class = "floating-panel",
      shiny::tags$div(
        class = "hidden-icons",
        # Map
        shiny::tags$button(
          class = "action-button btn1",
          style = "background-color:transparent;border:none;",
          id = shiny::NS(id, "panel_map"),
          icon_material("map", style = "color:white;font-size:40px;"),
          shiny::tags$span(class = "help-text",
                           style = "color:white;padding:20px;",
                           cc_t("View map"))),
        # Data
        shiny::tags$button(
          class = "action-button btn2",
          style = "background-color:transparent;border:none;",
          id = shiny::NS(id, "panel_data"),
          icon_material("table_view", style = "color:white;font-size:40px;"),
          shiny::tags$span(class = "help-text",
                           style = "color:white;padding:20px;",
                           cc_t("View/export data"))),
        # Explore data link
        shinyjs::hidden(shiny::tags$button(
          class = "action-button btn3",
          style = "background-color:transparent;border:none;",
          id = shiny::NS(id, "panel_selection"),
          icon_material("search", style = "color:white;font-size:40px;"),
          shiny::tags$span(class = "help-text",
                           style = "color:white;padding:20px;",
                           cc_t("Regional portrait"))))
      )
    ),
    
    shiny::tags$head(tags$style(shiny::HTML(".download_csv {display:inline;margin-right:10px;}"))),
    shiny::tags$head(tags$style(shiny::HTML(".download_shp {display:inline;}"))),
    
    # To accompany the panel data button, create the div
    shinyjs::hidden(
      shiny::div(
        class = "panel_view",
        style = "margin-right:500px;margin-left:500px",
        id = shiny::NS(id, "view_data"),
        shiny::htmlOutput(
          outputId = shiny::NS(id, "data_info"),
          fill = TRUE),
        shiny::div(style = "margin-bottom:20px;",
                   DT::DTOutput(
                     outputId = shiny::NS(id, "data_table"))),
        shiny::div(
          style = "text-align:right",
          shiny::downloadButton(class = "download_csv",
                                outputId = shiny::NS(id, "download_csv"), 
                                label = cc_t("Download '.csv'")),
          shiny::downloadButton(class = "download_shp",
                                outputId = shiny::NS(id, "download_shp"), 
                                label = cc_t("Download '.shp'")))))
  )
}
