#### Build MCP Rmd documents  ##################################################

process_mcp <- function(intro) {
  rmarkdown::render(paste0("dev/mcp/intro.Rmd"), output_dir = "www/mcp")
  "www/mcp/intro.html" <- paste0("www/mcp/intro.html")
  intro <- readLines("www/mcp/intro.html")
  head_1 <- str_which(intro, "<head>")
  head_2 <- str_which(intro, "</head>")
  intro <- intro[-c(head_1:head_2)]
  writeLines(intro, "www/mcp/intro.html")
  
  rmarkdown::render(paste0("dev/mcp/adaptation.Rmd"), output_dir = "www/mcp")
  "www/mcp/adaptation.html" <- paste0("www/mcp/adaptation.html")
  adaptation <- readLines("www/mcp/adaptation.html")
  head_1 <- str_which(adaptation, "<head>")
  head_2 <- str_which(adaptation, "</head>")
  adaptation <- adaptation[-c(head_1:head_2)]
  writeLines(adaptation, "www/mcp/adaptation.html")
  
  rmarkdown::render(paste0("dev/mcp/community.Rmd"), output_dir = "www/mcp")
  "www/mcp/community.html" <- paste0("www/mcp/community.html")
  community <- readLines("www/mcp/community.html")
  head_1 <- str_which(community, "<head>")
  head_2 <- str_which(community, "</head>")
  community <- community[-c(head_1:head_2)]
  writeLines(community, "www/mcp/community.html")
  
  rmarkdown::render(paste0("dev/mcp/economy.Rmd"), output_dir = "www/mcp")
  "www/mcp/economy.html" <- paste0("www/mcp/economy.html")
  economy <- readLines("www/mcp/economy.html")
  head_1 <- str_which(economy, "<head>")
  head_2 <- str_which(economy, "</head>")
  economy <- economy[-c(head_1:head_2)]
  writeLines(economy, "www/mcp/economy.html")
  
  rmarkdown::render(paste0("dev/mcp/equity.Rmd"), output_dir = "www/mcp")
  "www/mcp/equity.html" <- paste0("www/mcp/equity.html")
  equity <- readLines("www/mcp/equity.html")
  head_1 <- str_which(equity, "<head>")
  head_2 <- str_which(equity, "</head>")
  equity <- equity[-c(head_1:head_2)]
  writeLines(equity, "www/mcp/equity.html")
  
  rmarkdown::render(paste0("dev/mcp/food.Rmd"), output_dir = "www/mcp")
  "www/mcp/food.html" <- paste0("www/mcp/food.html")
  food <- readLines("www/mcp/food.html")
  head_1 <- str_which(food, "<head>")
  head_2 <- str_which(food, "</head>")
  food <- food[-c(head_1:head_2)]
  writeLines(food, "www/mcp/food.html")
  
  rmarkdown::render(paste0("dev/mcp/greening.Rmd"), output_dir = "www/mcp")
  "www/mcp/greening.html" <- paste0("www/mcp/greening.html")
  greening <- readLines("www/mcp/greening.html")
  head_1 <- str_which(greening, "<head>")
  head_2 <- str_which(greening, "</head>")
  greening <- greening[-c(head_1:head_2)]
  writeLines(greening, "www/mcp/greening.html")
  
  rmarkdown::render(paste0("dev/mcp/innovation.Rmd"), output_dir = "www/mcp")
  "www/mcp/innovation.html" <- paste0("www/mcp/innovation.html")
  innovation <- readLines("www/mcp/innovation.html")
  head_1 <- str_which(innovation, "<head>")
  head_2 <- str_which(innovation, "</head>")
  innovation <- innovation[-c(head_1:head_2)]
  writeLines(innovation, "www/mcp/innovation.html")
  
  rmarkdown::render(paste0("dev/mcp/landuse.Rmd"), output_dir = "www/mcp")
  "www/mcp/landuse.html" <- paste0("www/mcp/landuse.html")
  landuse <- readLines("www/mcp/landuse.html")
  head_1 <- str_which(landuse, "<head>")
  head_2 <- str_which(landuse, "</head>")
  landuse <- landuse[-c(head_1:head_2)]
  writeLines(landuse, "www/mcp/landuse.html")
  
  rmarkdown::render(paste0("dev/mcp/mobility.Rmd"), output_dir = "www/mcp")
  "www/mcp/mobility.html" <- paste0("www/mcp/mobility.html")
  mobility <- readLines("www/mcp/mobility.html")
  head_1 <- str_which(mobility, "<head>")
  head_2 <- str_which(mobility, "</head>")
  mobility <- mobility[-c(head_1:head_2)]
  writeLines(mobility, "www/mcp/mobility.html")
  
  rmarkdown::render(paste0("dev/mcp/regionalism.Rmd"), output_dir = "www/mcp")
  "www/mcp/regionalism.html" <- paste0("www/mcp/regionalism.html")
  regionalism <- readLines("www/mcp/regionalism.html")
  head_1 <- str_which(regionalism, "<head>")
  head_2 <- str_which(regionalism, "</head>")
  regionalism <- regionalism[-c(head_1:head_2)]
  writeLines(regionalism, "www/mcp/regionalism.html")
  
  rmarkdown::render(paste0("dev/mcp/sustainability.Rmd"), output_dir = "www/mcp")
  "www/mcp/sustainability.html" <- paste0("www/mcp/sustainability.html")
  sustainability <- readLines("www/mcp/sustainability.html")
  head_1 <- str_which(sustainability, "<head>")
  head_2 <- str_which(sustainability, "</head>")
  sustainability <- sustainability[-c(head_1:head_2)]
  writeLines(sustainability, "www/mcp/sustainability.html")
}
