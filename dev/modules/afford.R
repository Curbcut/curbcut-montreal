### Housing affordability data setup ###########################################

# 2 tables

# Grouping
grouping <-  c("households", "individuals")

shelter_cost <- list("total" = "total", 
                     "more_30_per" = c("30-50%", "50%-80%", ">80%"),
                     "more_50_per" = c("50%-80%", ">80%"),
                     "more_80_per" = ">80%")

#HOUSEHOLDS
tenure_statuses <- list("total" = "total", 
                        "tenant" = "tenant",
                        "owner" = "owner")
# Dwelling characteristics
dwelling <- c("total",
              "single-detached house",
              "semi-detached house",
              "row house",
              "apartment or flat in a duplex",
              "apartment in a building that has five or more storeys",
              "apartment in a building that has fewer than five storeys",
              "other single-attached house",
              "mobile homes and other movable dwellings ")
# OR
# Family characteristics
characteristics <- list("total" = "total",
                        "kids_3_plus" = "Families with 3 or more children",
                        "low income after tax",
                        "unsuitable" = "unsuitable",
                        "repairs" = "major repairs needed")


# INDIVIDUALS
sexes <- list("total" = "total", 
              "female" = "female", 
              "male" = "male")

imm_statuses <- list("total" = "total", 
                     "immigrants" = "immigrants",
                     "non_immigrants" = c("non-immigrants", "non-permanent"))

# Immigration characteristics
# Appears and disappears depending if immigrant is selected
#'  "Before 2001"
#'  "2001 to 2010"
#'  "2011 to 2016"
#'  "Economic immigrants"
#'  "Immigrants sponsored by family"
#'  "Refugees"
#'  "Other immigrants"

# Visible minority / Indigenous
#'  "Visible minority"
#'  "Does not belong to a visible minority group"
#'  "Aboriginal"

# Family characteristics
household_statuses <- list("total" = "total", 
                           "lone_parents" = "Lone parents (lone-parent families)",
                           "living_alone" = "Persons living alone",
                           "low_inc" = "low income after tax")
