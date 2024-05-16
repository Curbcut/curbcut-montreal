# data_get_colours_db_bbox <- function(vars, scale, time, breaks, bounding_box,
#                                      inst_prefix = get_from_globalenv("inst_prefix")) {
#   # Define the bounding box directly in the function call for clarity
#   southWest_lon <- bounding_box$southWest$lon - 0.05
#   southWest_lat <- bounding_box$southWest$lat - 0.05
#   northEast_lon <- bounding_box$northEast$lon + 0.05
#   northEast_lat <- bounding_box$northEast$lat + 0.05
# 
#   # Build the combined SQL query
#   sql_query <- sprintf(
#     paste0(
#       "SELECT t1.\"ID\", t2.\"%s_%s\" as val_col FROM %s.\"%s\" t1 JOIN ",
#       "%s.\"%s_%s\" t2 ON t1.\"ID\" = t2.\"ID\" WHERE ",
#       "ST_Within(t1.geometry, ST_MakeEnvelope(%f, %f, %f, %f, 4326))"
#     ),
#     vars$var_left, time, inst_prefix, scale, inst_prefix, scale, vars$var_left,
#     southWest_lon, southWest_lat, northEast_lon, northEast_lat
#   )
# 
#   # Execute the query using dbGetQuery() and store the result
#   data <- db_get_helper(sql_query)
# 
#   # Rework breaks just for assembling (we want to include ALL observations)
#   breaks[1] <- -Inf
#   breaks[length(breaks)] <- Inf
# 
#   # Assemble output
#   data$group <- .bincode(data$val_col, breaks, include.lowest = TRUE)
# 
#   # Deal with colours
#   colours <- curbcut:::colours_get()
#   colour_table <- colours$left_5
#   mapping <- match(data$group, colour_table$group)
#   data$fill <- colour_table$fill[mapping]
#   data <- data[c("ID", "fill")]
# 
#   ## Do not use ID_color, instead stay on ID. The colouring `fill` function uses
#   ## the first column name as identifier. It might be useful in a case where ex.
#   ## scale is DB, but the DB tile has the ID_color set to the DA in which the DB
#   ## falls. When that's the case, there won't be a weird lag of empty color when
#   ## the user zooms into DB scale.
#   # names(data)[1] <- c("ID_color")
# 
#   # Switch NA to the right "NA" colour
#   data$fill[is.na(data$fill)] <- colour_table$fill[grepl("^NA", colour_table$group)][[1]]
# 
#   return(data)
# }
# 
# data_colours_server <- function(id, r, vars, scale, region, data, time,
#                                 zoom_levels, schemas, map_viewstate,
#                                 scales_as_DA = shiny::reactive(c("building", "street"))) {
#   shiny::moduleServer(id, function(input, output, session) {
#     inst_prefix <- get_from_globalenv("inst_prefix")
# 
#     # Get variable breaks from data attribute 'breaks_var_left'
#     breaks_var_left <- shiny::reactive(attr(data(), "breaks_var_left"))
# 
#     # Determine if the current scale is in the database or on disk
#     is_scale_in_db <- shiny::reactive({
#       scale <- r[[id]]$scale()
#       in_db_scales <- get_from_globalenv("db_scales")
#       scale %in% in_db_scales && !(scale %in% scales_as_DA())
#     })
# 
#     # Fetch data colors if scale is on disk
#     qs_data_colour <- shiny::reactive({
#       data_get_colours(
#         vars = vars(), region = region(), time = time(),
#         zoom_levels = zoom_levels(), schemas = schemas()
#       )
#     })
# 
#     # Fetch data colors if scale is in the database
#     sql_colour <- shiny::reactive({
#       if (!is_scale_in_db()) {
#         return(NULL)
#       }
#       out <- data_get_colours_db_bbox(
#         vars = vars(), scale = scale(),
#         time = time()$var_left, breaks = breaks_var_left(),
#         bounding_box = map_viewstate()$boundingbox
#       )
#     })
# 
#     # Data for tile coloring
#     data_colours <- shiny::reactive({
#       disk_data <- qs_data_colour()
#       db_data <- sql_colour()
# 
#       if (!is_scale_in_db()) {
#         return(disk_data)
#       }
# 
#       names(disk_data) <- names(db_data)
#       rbind(db_data, disk_data)
#     })
# 
#     # Return the fetched data colors
#     return(data_colours)
#   })
# }
