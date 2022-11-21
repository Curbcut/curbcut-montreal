#### title_text data setup #####################################################

library(dplyr)
library(qs)


# Canale ------------------------------------------------------------------

title_text <-
  tibble(tab = character(), type = character(), text = character()) |> 
  add_row(tab = "canale", type = "title", 
          text = "Active living potential: the CanALE index") |> 
  add_row(tab = "canale", type = "main", text = paste0(
    "The CanALE dataset (developed by Prof. Nancy Ross ",
    "and her team) captures four key elements related to ",
    "active living environments: population density, ",
    "points of interest, street grid, and proximity of transit service.")) |> 
  add_row(tab = "canale", type = "extra", text = paste0(
    "<p>A safe and inviting pedestrian environment is not ",
    "a given in all neighbourhoods: it is influenced by ",
    "socio-economic factors. The risks of pedestrian ",
    "injuries and fatalities are higher in low-income and ",
    "racialized communities where residents often rely on ",
    "walking as a daily mode of transport but where the ",
    "local environment is not necessarily inviting and ",
    "safe.<p>In addition to evidence pointing towards ",
    "large discrepancies in the provision of walkable ",
    "urban space across income and racial lines, concern ",
    "has been raised with regard to the possible ",
    "gentrification and displacement impacts of ",
    "improved pedestrian infrastructure. In other words, ",
    "who can afford to live in walkable neighbourhoods?",
    "<br><p>Further resources: <ul><li><a href='https://",
    "www150.statcan.gc.ca/n1/pub/82-003-x/2019005/",
    "article/00002-eng.htm'>Thomas Herrmann, William ",
    "Gleckner, Rania A. Wasfi, Benoît Thierry, Yan ",
    "Kestens and Nancy A. Ross. 2019. 'A pan-Canadian ",
    "measure of active living environments using open ",
    "data. Statistics Canada Health Reports, 82-003-X.",
    "</a><li>Kevin Manaugh, Linnea Soli, Samuel Kohn, ",
    "Robin Basalaev-Binder, Ty Tuff, David Wachsmuth. ",
    "2020. 'Montreal's response to COVID-19: An equity ",
    "analysis of new active transport infrastructure.' ",
    "Transportation Research Board working paper. ",
    "<b>(MSSI research)</b></ul><br><p><i>Module lead ",
    "authors: David Wachsmuth, Robin Basalaev-Binder</i>")) |> 

# CanBICS -----------------------------------------------------------------

  add_row(tab = "canbics", type = "title", text = "Bikeway Comfort and Safety") |> 
  add_row(tab = "canbics", type = "main", text = paste0(
    "Can-BICS, or Canadian Bikeway Comfort and Safety, is a classification ",
    "system for cycling infrastructure in Canada. This system is based on ",
    "three tiers that considers safety and user comfort: high-comfort bikeways, ",
    "medium-comfort bikeways, and low-comfort bikeways. In this module, explore ",
    "how areas in the Montreal region rank in accordance with the Can-BICS index.")) |> 
  add_row(tab = "canbics", type = "extra", text = paste0(
    "<p>The information seen in this module is based on data from CANUE. ",
    "Can-BICS was developed by Meghan Winters, PhD, Moreno Zanotto, MSc, ",
    "and Gregory Butler, MSc. In selecting different areas on the map, ",
    "insights can be gained about the type and quality of cycling infrastructure. ",
    "Using the panel on the right, you can compare the Can-BICS index to ",
    "variables linked to housing, income, identity, transport, households, ",
    "language, age, and education. Understanding the spatialization of cycling ",
    "infrastructure as classified by Can-BICS can help to highlight the ",
    "availability and infrastructure types across the Montreal region and ",
    "support efforts in improving bikeways. <p>For more information about ",
    "Can-BICS visit: <a href='https://www.canada.ca/en/public-health/services/",
    "reports-publications/health-promotion-chronic-disease-prevention-canada-",
    "research-policy-practice/vol-40-no-9-2020/canbics-classification-system-",
    "naming-convention-cycling-infrastructure.html'>At-a-glance – The Canadian Bikeway Comfort and Safety (Can-BICS) Classification System: a common naming convention for cycling infrastructure</a>")) |> 

  
# Climate risk ------------------------------------------------------------

  add_row(tab = "climate_risk", type = "title", text = "Climate change risk") |> 
  add_row(tab = "climate_risk", type = "main", text = paste0(
    "Climate change will have increasingly negative ",
    "impacts on communities in Montreal, but these ",
    "impacts will vary significantly by both geography ",
    "and social factors. The distribution of five ",
    "different climate risks—heat waves, flooding, heavy ",
    "rain, drought, and destructive storms—is visualized here.")) |>
  add_row(tab = "climate_risk", type = "extra", text = paste0(
    "<p>The Climate Change Risk datasets come from the City of Montreal's ",
    "efforts to examine potential climate risks for the Montreal region in ",
    "the 2015-2020 Urban Agglomeration Climate Change Adaptation Plan. The ",
    "plan identifies the five variables of heat waves, flooding, heavy rain, ",
    "drought, and destructive storms as the primary climate risk factors ",
    "(alongside rising temperatures) for the Montreal agglomeration. The ",
    "Adaptation Plan includes projections for potential climate-change ",
    "impacts on buildings, municipal operations, the local environment, ",
    "and Montreal communities.<p>The datasets visualized here are publicly ",
    "available through the Montreal Open Data Portal.<ul><li>Heat waves ",
    "include a range of extreme heat events based on temperature and ",
    "duration. Montreal has generally seen an upward trend in extreme heat ",
    "events, most noticeably during the 2000s. Heat waves are especially ",
    "of concern in Montreal due to more than one quarter (28%) of the ",
    "island containing heat islands.<li>Flooding, specifically river ",
    "flooding, refers to flow rate or river level exceeding the critical ",
    "threshold. The Montreal agglomeration's flood risk is concentrated ",
    "along the Des Prairies River.<li>Heavy rain can cause rivers to ",
    "overflow, put strain on infrastructures, cause public health problems, ",
    "and negatively affect natural environments. Episodes of heavy rain ",
    "are on the upward trend in Quebec.<li>Drought includes meteorological ",
    "drought (amount of precipitation), agricultural drought (soil dryness), ",
    "hydrological drought (surface and groundwaters), and socioeconomic ",
    "drought (actions of humans on water resources). Montreal has seen a very ",
    "slight upward trend in meteorological droughts. <li>Destructive storms ",
    "include wind storms, hail storms, heavy snowstorms, and freezing rain. ",
    "Events of freezing rain increased 26% from 1979 to 2008, and heavy ",
    "snowstorms have also increased over the past 70 years.</ul>")) |> 

  
# Housing -----------------------------------------------------------------
  
  add_row(tab = "housing", type = "title", text = "The housing system") |> 
  add_row(tab = "housing", type = "main", text = paste0(
    "Housing is at the centre of our lives. Our ability to find affordable, ",
    "adequate and healthy accommodations profoundly affects our life ",
    "chances.")) |> 
  add_row(tab = "housing", type = "extra", text = paste0(
    "<p>Access to affordable and adequate housing is a core element of ",
    "social equity in cities. In Canada, the National Housing Strategy aims ",
    "to housing needs and houselessness through modernization, new ",
    "construction, and innovation and research. Within the City of Montreal, ",
    "important housing initiatives include the Diverse Metropolis by-law and ",
    "the 12,000 housing unit strategy. <p>This module presents housing data ",
    "from the Census from 1996 to the present, and explores relationships ",
    "with demographic patterns.<br><p><i>Further reading:</i></p><ul><li>",
    "<a href = 'https://www.cmhc-schl.gc.ca/en/nhs/'>CMHC. (n.d.). National ",
    "Housing Strategy.</a><li><a href ='https://montreal.ca/articles/",
    "metropole-mixte-les-grandes-lignes-du-reglement-7816'>Ville de ",
    "Montréal. (4 octobre 2021). Métropole Mixte: Les grandes lignes du ",
    "règlement.</a><li>Madden, D., & Marcuse, P. (2016). <i>In Defense of ",
    "Housing: The Politics of Crisis</i>. New York and London: Verso ",
    "Books.</ul>")) |> 

# Vacancy rate ---------------------------------------------------

add_row(tab = "vac_rate", type = "title", text = "Vacancy rate") |> 
  add_row(tab = "vac_rate", type = "main", text = paste0(
    "Examining residential rental vacancy rates is an important part of ",
    "understanding the housing landscape in Montreal. In this module, explore ",
    "and compare information about vacancy rates for specific types of units, ",
    "the year of construction, or within a rent range. Select one or two years ",
    "to compare the dates for which you wish to view the spatialized data.")) |> 
  add_row(tab = "vac_rate", type = "extra", text = paste0(
    "The comparative analysis seen in this module is based on data from ",
    "the CMHC. In selecting different options from the drop-down menus, ",
    "insights can be gained on how vacancy rates vary over time and spatially ",
    "by type of unit, year of construction, and rent range. Using the panel ",
    "on the right, you can compare these options to variables linked to ",
    "housing, income, identity, transport, households, language, age, and ",
    "education. Information about vacancy rates and its related variables ",
    "can help define past and current trends in the housing market and what ",
    "is needed to better provide adequate rental housing.")) |> 
  
# Housing affordability ---------------------------------------------------

  add_row(tab = "afford", type = "title", text = "Housing Affordability") |> 
  add_row(tab = "afford", type = "main", text = paste0(
    "Having access to affordable and equitable shelter is essential. ",
    "Affordable housing is often broadly defined as spending less than 30% of ",
    "household income on shelter costs. The reality of each household ",
    "consists of many different factors and characteristics, such as, the ",
    "people living there, their income, their shelter costs, and their tenure ",
    "status. In this module, explore and compare housing affordability by ",
    "city or borough, or census tract.")) |> 
  add_row(tab = "afford", type = "extra", text = paste0(
    "<p> The comparative analysis that you see in this module is based on ",
    "housing data from the 2016 Census. In selecting different options from ",
    "the drop-down menus, insights can be gained on how affordability varies ",
    "by household or individuals, their shelter costs, whether they are ",
    "tenants or owners and various family, immigration, and dwelling ",
    "characteristics. Using the panel on the right, you can compare these ",
    "housing affordability variables with access to different amenities by ",
    "mode of transportation. It is important to understand the geographic and ",
    "socioeconomic patterns associated with housing affordability to inform ",
    "policies and actions to best address housing needs. </ul>")) |> 


# Tenure status -----------------------------------------------------------

  add_row(tab = "tenure", type = "title", text = "Tenure Status") |> 
  add_row(tab = "tenure", type = "main", text = paste0(
    "The categorization of housing by tenure status, tenancy or ownership, ",
    "helps to gain a clearer picture of the housing landscape in Montreal. ",
    "This is especially the case when compared with other factors. In this ",
    "module, tenure status can be explored in relation to shelter costs and ",
    "additional variables such as family characteristics and dwelling ",
    "types.")) |> 
  add_row(tab = "tenure", type = "extra", text = paste0(
    "<p> The comparative analysis that you see in this module is based on ", 
    "housing data from the 2016 Census. In selecting different options from ",
    "the drop-down menus, insights can be gained on how tenure status ", 
    "interacts with shelter costs, and various family, immigration, and ",
    "dwelling characteristics. Using the panel on the right, you can compare ",
    "these tenure status variables with access to different amenities by mode ",
    "of transportation. Understanding housing needs by tenure status can help ",
    "to inform what is to be improved specifically for tenants or owners as ",
    "they might be experiencing different difficulties and ",
    "advantages. </ul>")) |> 
  

# Dwelling types ----------------------------------------------------------

  add_row(tab = "dw_types", type = "title", text = "Dwelling Types") |> 
  add_row(tab = "dw_types", type = "main", text = paste0(
    "Whether a household is living in a single-detached house or an ",
    "apartment in a building of 5+ stories is an important aspect of ", 
    "understanding people’s housing realities. In selecting different types ",
    "of dwellings, you can simply explore and compare them in relation to ",
    "tenure status and shelter cost.")) |> 
  add_row(tab = "dw_types", type = "extra", text = paste0(
    "<p> The comparative analysis that you see in this module is based on ",
    "housing data from the 2016 Census. In selecting different options from ",
    "the drop-down menus, insights can be gained on how dwelling types ",
    "interact with tenure status and shelter costs. Using the panel on the ",
    "right, you can compare the dwelling type variables with access to ",
    "different amenities by mode of transportation. Exploring the housing ",
    "system in Montreal through dwelling types adds a level of understanding ",
    "to the overall housing situation. </ul>")) |> 
  
# Place explorer ----------------------------------------------------------

  add_row(tab = "place_explorer", type = "title", text = "Place explorer") |> 
  add_row(tab = "place_explorer", type = "main", text = paste0(
    "Select a location by entering a postal code or clicking on the map, ",
    "and see how it compares to the rest of the Montreal region or island ",
    "across a variety of sustainability indicators.")) |> 
  add_row(tab = "place_explorer", type = "extra", text = paste0(
    "The data in the Place Explorer is taken from other Curbcut modules, ",
    "with two exceptions: <a href = 'https://www.canuedata.ca/tmp/",
    "CANUE_METADATA_NO2LUR_A_YY.pdf'>Air pollution</a> and ",
    "<a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf'>",
    "green space</a> data are taken from <a href = 'https://www.canuedata.ca'>",
    "CANUE</a>.")) |> 
  

# Crash -------------------------------------------------------------------

  # add_row(tab = "crash", type = "title", text = "Road safety") |> 
  # add_row(tab = "crash", type = "main", text = paste0(
  #   "Road safety is an important consideration for wellbeing ",
  #   "and safety in cities. This module ",
  #   "provides an overview and analysis of road collisions ",
  #   "in the City of Montreal, ranging from 2012 to today.")) |> 
  # add_row(tab = "crash", type = "extra", text = paste0(
  #   "<p>Data is collected by the Service de Police de la ","
  #   Ville de Montréal (SPVM) and compiled by the Société ",
  #   "d’Assurance Automobile du Québec (SAAQ), and contains ",
  #   "information related to every road collision, including the date, ",
  #   "location and type of parties involved (i.e. cars, bicycles ",
  #   "or pedestrians) and injury severity. ",
  #   "<p>For more information on road collisions and a temporal ",
  #   "analysis of the data, please consult the ", 
  #   "<i>Road safety analysis</i> above.</p>",
  #   "<p>References:</p><ul><li><a href = https://www.pietons.quebec/",
  #   "sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapproche",
  #   "visionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir ",
  #   "l’approche vision zéro en sécurité routière. Piétons Québec. Online:</a>",
  #   "<li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-",
  #   "routieres'>Ville de Montréal. (2021). Collisions routières. ",
  #   "Données Ouvertes Montréal.</a></ul>")) |> 
  

# Green alley -------------------------------------------------------------
  
  add_row(tab = "alley", type = "title",
          text = "Green alleys") |> 
  add_row(tab = "alley", type = "main", text = paste0(
    "Green alleys are back alleys which have been transforme",
    "d into spaces that improve people’s living environments",
    ". When adequately designed, these public spaces can hel",
    "p reduce heat island effects, noise and air pollution, ",
    "and improve water management. They can also enhance soc",
    "ial interactions and become recreational spaces for chi",
    "ldren where car circulation is reduced.")) |> 
  add_row(tab = "alley", type = "extra", text = paste0(
    "<p>The Green Alley Program in Montreal was implemented ",
    "in 1995. At latest count  there were a total of 573 gre",
    "en alleys in the city with more scheduled to be  create",
    "d . First put in place in Plateau-Mont-Royal and Rosemo",
    "nt-La-Petite-Patrie, the program was then extended to m",
    "ost of Montreal’s boroughs. The program promotes public",
    " participation and citizen governance; the initiative t",
    "o create a green alley comes from the residents themsel",
    "ves. The Ecoquartiers (most boroughs have at least one ",
    "of them) are key actors in green alley development: the",
    "y receive applications, select the submissions, and fol",
    "low the evolution of the projects.</p><p>The Green All",
    "ey dataset is a mix of data from the City of Montreal O",
    "pen Data Portal, from boroughs’ Eco-quartiers, newspape",
    "r articles and personal observations (68 alleys were vi",
    "sited and classified). No fully complete list of green ",
    "alleys in Montreal exists.</p><p>Green alleys are dive",
    "rse in activities, design, dimensions, and dwellings de",
    "nsity in the surrounding area. Within that diversity, w",
    "e classify four types of alleys:</p><ul><li>Green all",
    "eys: most of the alley includes greenery such as permea",
    "ble surfaces, gardens, and green walls that provide env",
    "ironmental benefits like cooling the street temperature",
    ", filtrating rainwater, and contributing to enriching t",
    "he biodiversity of the alleys. They are generally close",
    "d to circulation.<li>Community-oriented alleys: their ",
    "main characteristic is the existence of gathering space",
    "s that enrich community life. This type of alley presen",
    "ts activities organized by the residents, and includes ",
    "the appropriation of the alley with furniture and art, ",
    "and play areas for children.<li>Mixed alleys: these in",
    "clude green elements and are also spaces that allow div",
    "erse community activities, especially interventions tha",
    "t create safe areas for children, such as measures that",
    " reduce car traffic and, in some cases, parking.<li>Ne",
    "ither green nor community-oriented alleys: they are aba",
    "ndoned or used as parking spaces or as ways to access p",
    "rivate garages, without any environmental or social-com",
    "munity benefit.</ul>")) |> 

# Demographics -------------------------------------------------------------

add_row(tab = "demographics", type = "title",
        text = "Demographics") |> 
  add_row(tab = "demographics", type = "main", text = paste0(
    "Certain factors about a population can reveal interesting socioeconomic ",
    "information. In this module, learn and explore different demographics ",
    "statistically expressed per square kilometre, percentage of population, ",
    "or simply by count. Choose various factors to examine such as gender, ",
    "immigration status, shelter cost, and additional immigration, visible ",
    "minority and family characteristics.")) |> 
  add_row(tab = "demographics", type = "extra", text = paste0(
    "<p>The comparative analysis that you see in this module is based on ", 
    "demographic data from the 2016 Census. In selecting different options ",
    "from the drop-down menus, insights can be gained on the prevalence of ",
    "different factors by the three types of groupings. Using the panel on ",
    "the right, you can compare these demographic variables with access to ",
    "different amenities by mode of transportation. Examining and comparing ",
    "demographic variables can provide valuable information about the ",
    "Montreal population. </ul>")) |> 

# Covid -------------------------------------------------------------------

  # add_row(tab = "covid", type = "title",
  #         text = "2020 and 2021 Covid interventions") |> 
  # add_row(tab = "covid", type = "main", text = paste0(
  #   "The onset of the COVID-19 pandemic prompted municipalities ",
  #   "across the world to restrict public transit systems and advise citizens ",
  #   "to make only necessary trips. For many without access to motorized ",
  #   "vehicles, active transportation, walking and cycling, became the primary ",
  #   "mode of transport. To accommodate the increased demand for these modes ",
  #   "of safe travel, and to ensure the possibility to practice physical ",
  #   "distancing while travelling, many cities made rapid changes to cycling ",
  #   "and pedestrian networks, including constructing new bike lanes, ",
  #   "creating temporary infrastructure, and shutting down streets ",
  #   "to motor vehicle traffic. These changes served to minimize viral ",
  #   "transmission during travel and to more safely connect people to ",
  #   "essential services, health care, and greenspace.")) |> 
  # add_row(tab = "covid", type = "extra", text = paste0(
  #   "In May 2020, the City of Montréal announced plans to establish ",
  #   "over 300 kilometers of active transport infrastructure by the ",
  #   "end of the summer. These plans included six distinct types of ",
  #   "street changes: active transportation circuits, family and active ",
  #   "streets, partially closed streets, closed streets, expanded ",
  #   "pedestrian corridors, and planned corridors. The active ",
  #   "transportation circuits are intended to link green spaces, ",
  #   "provide increased access to parks for densely populated areas, ",
  #   "and increase access to local commerce, while promoting physical ",
  #   "distancing during travel. Family and active streets define ",
  #   "portions of the street which have been closed to cars entirely ",
  #   "to encourage physical distancing while residents spend time outdoors. ",
  #   "These streets were chosen based on proximity to green space and ",
  #   "population density. While some streets have partially closed to ",
  #   "traffic, pedestrianizing a portion of the available car lanes, ",
  #   "other streets have been closed entirely to motor vehicles to maximize ",
  #   "pedestrian space. Expanded pedestrian corridors are locations where ",
  #   "sidewalks have been widened in order to allow greater capacity for ",
  #   "physical distancing while walking. The last group of street changes ",
  #   "are labelled as planned corridors and  encompass all types of street ",
  #   "changes, not specifically defined by the city.<p> The City of Montréal ",
  #   "made multiple revisions to the plans over the course of the summer ",
  #   "of 2020. The initial plans released in May were ambitious, providing ",
  #   "cycling and pedestrian infrastructure to nearly 30% of the city ",
  #   "population and envisioning a cohesive city-wide network of health ",
  #   "corridors supplemented by local, small-scale interventions to support ",
  #   "physical distancing. By July the plans were reduced from 311 kilometers ",
  #   "to 104 kilometers and by fall just 80 kilometers, primarily due to the ",
  #   "removal of nearly all the active transportation circuits. These circuits ",
  #   "were intended to provide an alternative to public transit, creating a ",
  #   "comprehensive system connecting Montreal's various boroughs.<p> The City ",
  #   "of Montreal has announced more public space interventions for summer ",
  #   "2021 in response to the lasting effects of COVID-19 and ongoing demands ",
  #   "for greater physical distancing capacity in dense urban areas. However, ",
  #   "rather than prioritizing mobility and access to essential services as in ",
  #   "the previous year, the rollout of vaccines and the gradual easing of ",
  #   "restrictions has allowed for the city's scope to narrow. The 2021 plans ",
  #   "consist of pedestrianization interventions on 13 commercial streets ",
  #   "around Montréal, an effort to revitalize the local economy and provide ",
  #   "safer public spaces for comfortable shopping. The implementation of ",
  #   "pedestrian-only zones on streets with high concentrations of ",
  #   "restaurants, bars, and other retail shops creates appealing destinations ",
  #   "for Montréal residents and promotes recovery for the businesses which ",
  #   "have been affected by COVID-19.")) |> 
  

# Access ------------------------------------------------------------------
  
  add_row(tab = "access", type = "title", 
          text = "Accessibility to opportunities") |> 
  add_row(tab = "access", type = "main", text = paste0(
    "Accessibility (the ease of reaching desired destination",
    "s) is the most important element of a transport network",
    ". This module displays what is accessible on public tra",
    "nsit within 30 minutes’ “door-to-door” travel time. Cli",
    "cking on individual census tracts shows which other cen",
    "sus tracts are reachable within a given travel time and",
    " at a chosen period of the day (peak, off-peak, and nig",
    "ht). To return to the accessibility map, click on the s",
    "ame census tract again or on ‘Clear selection’ in the E",
    "xplore panel.")) |> 
  add_row(tab = "access", type = "extra", text = paste0(
    "<p>This module focuses on public transit accessibility to a set of key ",
    "destinations: ", 
    "all jobs, ‘low-skill’ jobs (those not requiring a university degree), ", 
    "‘high-skill’ jobs (those usually requiring a university degree), jobs ",
    "with an annual salary of <$30,000, and schools and healthcare facilities.",
    "<p> This analysis allows for the identification of areas of the city or ",
    "population groups that are in need of improved service, it also allows ", 
    "for the analysis of change throughout the day and week. It is based on ",
    "'Cumulative Opportunities' accessibility, meaning it displays the total ",
    "number of destinations available within 30 minutes ‘door-to-door’ travel ",
    "time (including walking to reach station, waiting time, in-vehicle ",
    "time). For more info on how accessibility metrics are calculated see ",
    "<a href = 'https://conservancy.umn.edu/bitstream/handle/11299/199892/",
    "CTS13-20_Access-Across-America.pdf'>'Access Across America'</a>.")) |> 
  
# Access to amenities ------------------------------------------------------------------

add_row(tab = "amenities", type = "title", 
        text = "Access to amenities") |> 
  add_row(tab = "amenities", type = "main", text = paste0(
    "Being able to access amenities and services in our nearby urban ",
    "environment can greatly impact our daily experiences and quality of ",
    "life. The time and mode of transportation needed to reach these amenities ",
    "plays a large role in this. In this module, explore information about ",
    "access to schools, food distributors, health care facilities, ",
    "municipal parks, and daycare spots by walk, bike, transit, or car.")) |> 
  add_row(tab = "amenities", type = "extra", text = paste0(
    "The spatialized analysis seen in this module is based on data from ",
    "Données Québec and DMTI. In selecting different options from the ",
    "drop-down menus, insights can be gained about access to different types ",
    "of amenities by a certain mode of transportation within a given amount ",
    "of time. Using the panel on the right, you can compare these options to ",
    "variables linked to housing, income, identity, transport, households, ",
    "language, age, and education. Understanding access to amenities by mode ",
    "of transportation gives a glimpse into how different areas are serviced ",
    "and what that might imply for residents.")) |> 
  

# Montreal stories --------------------------------------------------------
  
  add_row(tab = "stories", type = "title", text = "Montreal stories") |> 
  add_row(tab = "stories", type = "main", text = paste0(
    "Explore narrative case studies on sustainability issues in Montreal's ",
    "neighborhoods. In this module, read text-based stories and view their ",
    "adjoining visual media.")) |> 
  add_row(tab = "stories", type = "extra", text = paste0(
    "These stories, written by Curbcut contributors, examine Montreal ",
    "sustainability issues that aren't well suited to representation in our ",
    "standard interactive map format. Learn more about stories rooted in ",
    "specific geographic locations across the city or those that have had ",
    "an impact on the whole of Montreal.")) |> 
  

# Gentrification ----------------------------------------------------------

  # add_row(tab = "gentrification", type = "title", 
  #         text = "Neighbourhood change pattern: Gentrification index") |> 
  # add_row(tab = "gentrification", type = "main", text = paste0(
  #   "This gentrification index shows neighbourhood change patterns over time ",
  #   "based on census data. Seven key indicators are chosen to capture changes ",
  #   "in resident and housing characteristics: median household income, ",
  #   "educational attainment, occupation, visible minority proportion, tenant ",
  #   "proportion, average property values, and average rent.")) |> 
  # add_row(tab = "gentrification", type = "extra", text = paste0(
  #   "<p>Gentrification is a process of neighbourhood upgrading, but the 
  #   benefits of improvements are experienced differently by different classes ",
  #   "of residents. While the middle-upper class residents benefit from the ",
  #   "better living quality, lower-income and marginalized groups could face ","
  #   the pressure of unaffordable housing and displacement and be forced to ",
  #   "move out.</p>",
  #   "<p>Tracing the pattern of gentrification in a continually gentrifying ",
  #   "city helps to identify the force that drives people's mobility and dig ",
  #   "into the inequality caused by the redevelopment. Additionally, with the ",
  #   "comparison of categorized indicators, the analysis helps to answer what ",
  #   "roles those factors play in the procedure of gentrification.</p>",
  #   "<p>Further resources:</p><ul><li>Firth, Caislin L., Benoit Thierry, ",
  #   "Daniel Fuller, Meghan Winters, and Yan Kestens. 'Gentrification, Urban ",
  #   "Interventions and Equity (GENUINE): A map-based gentrification tool ",
  #   "for Canadian metropolitan areas.' Health reports 32.5 (2021): 15-28.",
  #   "<li>Walks, Alan R., and Richard Maaranen. The timing, patterning, & ",
  #   "forms of gentrification & neighbourhood upgrading in Montreal, Toronto, ",
  #   "& Vancouver, 1961 to 2001. Toronto, ON: Centre for Urban and Community ",
  #   "Studies, Cities Centre, University of Toronto, 2008.</ul>")) |> 
  

# Green space -------------------------------------------------------------

  # add_row(tab = "green_space", type = "title", text = "Green spaces") |> 
  # add_row(tab = "green_space", type = "main", text = paste0(
  #   "There are around 1500 parks in the City of Montreal, ",
  #   "which combined represent a surface of over 45 km^2. ",
  #   "It is widely acknowledged that accessibility to such ",
  #   "green spaces can contribute to physical health, ",
  #   "mental health, and social well-being. However, these ",
  #   "areas may not be equally accessible to all. This ",
  #   "module offers an outlook on parks and other public ",
  #   "spaces with possible comparison to housing, ",
  #   "socio-demographic, and other census variables.")) |> 
  # add_row(tab = "green_space", type = "extra", text = paste0("TKTK")) |> 
  

# Marketed sustainability -------------------------------------------------

  # add_row(tab = "marketed_sustainability", type = "title", 
  #         text = "Sustainability marketing in new housing developments") |> 
  # add_row(tab = "marketed_sustainability", type = "main", text = paste0(
  #   "<p>The idea of a “creative class” proposes that the societal ",
  #   "elite do not solely exist as small groups of wealthy ",
  #   "individuals operating in the shadows, but rather that ",
  #   "a large group of individuals made up of scientists, ",
  #   "engineers, professors, writers, and artists as well ",
  #   "as people in knowledge-intensive industries like high tech, ",
  #   "finance, law, health care, and business management ",
  #   "constitute an elite class with unique influence over ",
  #   "society who “create, manage, and problem-solve” ",
  #   "(Florida 2002/2019, Berry & Portney 2016). Cities with ",
  #   "large creative class populations tend to pursue ",
  #   "sustainability policies and have active local environmental ",
  #   "groups involved in policy making, although these policies ",
  #   "may have disproportionately negative effects on marginalized ",
  #   "groups (Berry & Portney 2016). Gould and Lewis (2018) ",
  #   "propose a variation of the existing definition of creative ",
  #   "class by arguing that individuals with sustainability-focused ",
  #   "values who have the financial means to act on their ",
  #   "environmental concerns through luxury consumption constitute ",
  #   "their own class, the “sustainability class”. This term has ",
  #   "remained largely unexplored in the literature with only ",
  #   "brief mention in discussions of green gentrification and ",
  #   "its impacts on marginalized groups (Anguelovski et al. 2019, ",
  #   "Berry & Portney 2016, Gould & Lewis 2018, Kuo 2017, ",
  #   "Mullenbach et al. 2021, Tubridy 2021).</p>",
  #   "<p>This module explores the marketing language used by ",
  #   "recently constructed housing developments in Montreal as ",
  #   "a way to examine targeted marketing towards members of the ",
  #   "“sustainability class”. Text from development websites was ",
  #   "analyzed using a constructed sustainability dictionary to ",
  #   "assign each development a sustainability score.")) |>
  # add_row(tab = "marketed_sustainability", type = "extra", text = paste0(
  #   "180 housing developments constructed after 2015 on the ",
  #   "island of Montreal were identified from Montreal Open ",
  #   "Data “Unités d’évaluation foncière” property assessment data. ",
  #   "Marketing websites for 137 developments were found to be ",
  #   "used for text mining. A sustainability dictionary was ",
  #   "created using keywords from existing criteria used to ",
  #   "designate sustainability accolades, including Leadership ",
  #   "in Energy and Environmental Design (LEED), B Corps, ",
  #   "Building Research Establishment Environmental Assessment ",
  #   "Method (BREEAM), Comprehensive Assessment System for Built ",
  #   "Environmental Efficiency (CASBEE), International ",
  #   "Organization for Standardization (ISO), Living ",
  #   "Building Challenge, and Green Building Initiative (GBI). ",
  #   "Dictionary entries were categorized into nine categories: ",
  #   "accessibility, economic, energy, environment, jargon, ",
  #   "materials & waste, social, technological, agriculture & ",
  #   "food. Each entry was also labeled as green or grey based ",
  #   "on Wachsmuth and Angelo’s (2018) concepts of green and ",
  #   "grey urban nature. Text was scraped from development ",
  #   "websites using the rvest package and subsequently cleaned ",
  #   "by removing punctuation and stopwords. The text was ",
  #   "initially analyzed using existing dictionaries of positive ",
  #   "and negative words from the qdapDictionaries package to ",
  #   "perform sentiment analysis and ensure that analysis would ",
  #   "function well. The constructed sustainability dictionary ",
  #   "was then used to assign a sustainability score to each of ",
  #   "the housing development websites, first counting the number ",
  #   "of sustainability-related terms in each piece of text and ",
  #   "dividing this count by the total number of words from each ",
  #   "website. This results in a score with a maximum of 1.00, ",
  #   "representing the percentage of words in the website text ",
  #   "that correspond to words in the dictionary. Resulting ",
  #   "sustainability scores were used in conjunction with ",
  #   "existing data to determine the average score per number of ",
  #   "units in a development and the average score per year.")) |> 
  

  # Natural infrastructure --------------------------------------------------

add_row(tab = "natural_inf", type = "title", 
        text = "Natural infrastructure") |> 
  add_row(tab = "natural_inf", type = "main", text = paste0(
    "Natural ecosystems contribute to well-being, quality of life and public ",
    "health. This module quantifies the benefits provided by urban trees and ",
    "wooded areas to biodiversity conservation, flood prevention, and ",
    "heat-island reduction. The natural infrastructure included in this study ",
    "covers approximately 25% of the Montreal region. Montreal's natural ",
    "infrastructure is threatened by forces including pollution, pests, ",
    "climate change, and urban sprawl. Increasing the percentage of protected ",
    "natural infrastructure increases the supply of these ecosystem benefits.")) |> 
  add_row(tab = "natural_inf", type = "extra", text = paste0(
    "<p>Data made available by <a href='https://www.habitat-nature.com/'>",
    "Habitat</a>. For more information on the methods and data used for this ",
    "module, see <a href = 'https://fr.davidsuzuki.or",
    "g/publication-scientifique/le-role-des-infrastructures-natu",
    "relles-dans-la-prevention-des-inondations-dans-la-communaut",
    "e-metropolitaine-de-montreal/' target = '_blank'>",
    "Maure et al., 2018, Le rôle des infrastructures naturelles dans la ",
    "prévention des inondations dans la Communauté métropolitaine de ",
    "Montréal, Fondation David Suzuki.")) |> 

# Short distance city -------------------------------------------------------

add_row(tab = "city_amenities", type = "title", 
        text = "Short-distance city") |> 
  add_row(tab = "city_amenities", type = "main", text = paste0(
    "This module presents accessibility indicators for walking and cycling, calculated on the City of Montreal's territory. This project is led by Polytechnique Montréal, McGill University and the Institut national de la recherche scientifique (INRS) and funded by the City of Montreal and Mitacs. This module is a preliminary version and will be the subject of future developments.")) |> 
  add_row(tab = "city_amenities", type = "extra", text = paste0(
    "In an urban planning context, accessibility refers to the ease with which individuals can reach opportunities dispersed throughout the territory. High walking and cycling accessibility is associated with high quality of life and social equity. A good access to destinations by walk and by bike also promotes active travel and thus, leads to greenhouse gases (GHG) emissions reductions, savings in transportation costs and improved population health. Therefore, accessibility indicators contribute to the integration of transportation and land-use in planning for sustainable mobility.",
    "<br><br><p>Further resources: <ul><li><a href='https://github.com/VilledeMontreal/MontrealEnCommun'>Montréal En Commun</a>.</ul>",
    "<p><i>Module lead authors: Geneviève Boisjoly, Kevin Manaugh, Owen Waygood, Philippe Apparicio, José Arturo Jasso Chávez, Julien Verdier, Karl El-Murr</i>"))


