# #### PERMITS INFO TABLE MODULE ############################################
# 
# permits_info_table <- function(id, x, select_id, var_left, ...) {
#   
#   moduleServer(id, function(input, output, session) {
#     reactive({
#       
#       type <- unique(str_extract(var_left(), "(?<=permits_).*(?=_count)"))
#       type <- switch(type, "combination" = "dwellings combination",
#                      "conversion" = "condo conversion",
#                      "demolition" = "demolition",
#                      "new_construction" = "new construction",
#                      "renovation" = "renovatios",
#                      "total")
#       time <- unique(str_extract(var_left(), "(?<=count_).*"))
#       
#       
#       cat <- if (type == "total" && is.na(select_id())) {
#         "total"
#       } else if (type != "total" && is.na(select_id())) {
#         "uni_type"
#       } else if (!is.na(select_id())) {
#         "selected"
#       }
#       
#       if (cat == "total") {
#         enum <-
#           x() |>
#           count(type) |>
#           mutate(type =
#                    case_when(type == "combination" ~ "dwellings combination",
#                              type == "conversion" ~ "condo conversion",
#                              type == "demolition" ~ "demolition",
#                              type == "new_construction" ~ "new construction",
#                              type == "renovation" ~ "renovation")) |>
#           arrange(-n) |>
#           mutate(text = cc_t(r = r, "{sapply(n, convert_unit, compact = TRUE)} ",
#                                       "permits for {type}")) |>
#           pull(text) |>
#           paste(collapse = ", ") |>
#           stringi::stri_replace_last_fixed(",", ", and")
#         
#         if (length(time) == 1) {
#           HTML(cc_t(r = r, 
#             "<p>At the scale of the City of Montreal, there were a ",
#             "total of {convert_unit(nrow(x()))} permits issued related ",
#             "to housing changes in {time}</p>",
#             "<p>In {time}, there were ",
#             "{str_replace(enum, ' for', ' issued for')}.</p>"))
#         } else {
#           HTML(cc_t(r = r, 
#             "<p>At the scale of the City of Montreal, there were a ",
#             "total of {convert_unit(nrow(x()))} permits issued related ",
#             "to housing changes between {time[1]} and {time[2]}.</p>",
#             "<p>Between {time[1]} and {time[2]}, there were ",
#             "{str_replace(enum, ' for', ' issued for')}.</p>"))
#         }
#         
#         
#       } else if (cat == "uni_type") {
#         
#         first_p <- if (length(time) == 1) {
#           cc_t(r = r, 
#             "<p>At the scale of the City of Montreal, there were a ",
#             "total of {convert_unit(nrow(x()))} {type} permits issued ",
#             "in {time}.</p>")
#         } else {
#           cc_t(r = r, 
#             "<p>At the scale of the City of Montreal, there were a ",
#             "total of {convert_unit(nrow(x()))} {type} permits issued ",
#             "between {time[1]} and {time[2]}.</p>")
#         }
#         
#         sec_p <- if (type %in% c("dwellings combination", "demolition")) {
#           loss <- convert_unit(abs(sum(pull(x(), nb_dwellings), na.rm = TRUE)))
#           cc_t(r = r, 
#             "<p>These {type}s resulted in the loss of {loss} ",
#             "dwellings.</p>")
#         } else if (type == "new construction") {
#           new <- convert_unit(abs(sum(pull(x(), nb_dwellings), na.rm = TRUE)))
#           cc_t(r = r, 
#             "<p>These {type}s resulted in the addition of {new} ",
#             "dwellings.</p>")
#         }
#         
#         HTML(paste0(first_p, sec_p))
#         
#         
#       }  else if (cat == "selected") {
#         z <- permits[permits$ID == select_id(), ]
#         z <- filter(z, !is.na(ID))
#         
#         z <- z |>
#           mutate(type =
#                    case_when(type == "combination" ~ "dwelling combination",
#                              type == "conversion" ~ "condo conversion",
#                              type == "demolition" ~ "demolition",
#                              type == "new_construction" ~ "new construction",
#                              type == "renovation" ~ "renovation"))
#         
#         first_p <-
#           cc_t(r = r, 
#             "<p><b>Permit {z$ID}</b><p>",
#             "<p>The issued permit {z$ID} was for `{z$type}` in {z$year}",
#             ".</p>")
#         
#         sec_p <- if (type %in% c("dwelling combination", "demolition")) {
#           loss <- convert_unit(abs(sum(pull(z, nb_dwellings), na.rm = TRUE)))
#           cc_t(r = r, 
#             "<p>It resulted in the loss of {loss} dwellings.</p>")
#         } else if (z$type == "new construction") {
#           new <- convert_unit(abs(sum(pull(z, nb_dwellings), na.rm = TRUE)))
#           cc_t(r = r, 
#             "<p>It resulted in the addition of {new} dwellings.</p>")
#         } else ""
#         
#         HTML(paste0(first_p, sec_p))
#       }
#       
#     })
#   })
# }
