#### `Home`, `About`, `Why dashboard` preparation for translation ##############


# Import existing translation ---------------------------------------------

# home_and_about_translated <- 
#   read.csv("dev/translation/csv/home_and_about_translated.csv") |> 
#   as_tibble()

# # Fix weird encoding
# home_and_about_translated$en <- char_fix(home_and_about_translated$en)
# home_and_about_translated$fr <- char_fix(home_and_about_translated$fr)


# Home and about translations ---------------------------------------------

# MISSING TRANSLATIONS
# home_and_about_translated <- 
#   tibble(en = c(
#     
#     # ABOUT | Why a dashboard?
#     paste0("Why a dashboard? The science behind Sus"),
#     paste0("Urban sustainability is a complex, multidimensional ",
#            "concept encompassing a variety of stakeholders, values ",
#            "and priorities. Individuals, households, and larger ",
#            "entities make decisions about mobility, accessibility, ",
#            "and other aspects of urban life  in the context of ",
#            "the built and natural environment as well as policies, ",
#            "regulations, and social norms, much of which are under ",
#            "some degree of control of local, regional, and federal ",
#            "decision makers. Sustainability permeates every ",
#            "decision we make and every relationship we maintain, ",
#            "but the connections between individual decisions and ",
#            "long-term outcomes are complex and involve many ",
#            "subjective comparisons between every aspect of urban ",
#            "living."),
#     paste0("Dashboards offer a tool for communicating ",
#            "sustainability data in a visually based digital ",
#            "platform. We see a gap in current dashboards going ",
#            "beyond the visualization of pre-existing data at ",
#            "static scales, leaving room for a more ",
#            "future-oriented, scalable, and interactive model. ",
#            "Sustainability dashboards should build upon best ",
#            "practices to provide useful tools for individuals ",
#            "and cities alike to examine the many facets of urban ",
#            "sustainability and question existing assumptions. ",
#            "Maintaining transparency with data and methodologies, ",
#            "ensuring public participation and accurate ",
#            "representation of underprivileged communities, and ",
#            "using engaging and accessible tools contribute to the ",
#            "success of a dashboard (Cosgrave et al. 2013; Deutz ",
#            "and Ioppolo 2015; Kitchin 2015; Otten et al. 2015; ",
#            "Pettit et al. 2012)."),
#     paste0("SUS - a comprehensive and inclusive urban ",
#            "sustainability dashboard - aims to develop a platform ",
#            "for integrating, exploring, and analyzing the widest ",
#            "possible range of urban sustainability data sources ",
#            "pertaining to the Montreal region, and, on the basis ",
#            "of this platform, provide a robust set of tools for ",
#            "scenario modelling and analysis which can be of ",
#            "significant use to researchers, policymakers, ",
#            "communities and individuals. In addition to the raw, ",
#            "reproducible datasets, SUS incorporates the necessary ",
#            "contextualization for its users to place their ",
#            "personal or organisational questions into a larger ",
#            "framework of socioeconomic equity and accessibility. ",
#            "It therefore serves as both a data-exploration tool, ",
#            "as well as a knowledge and information-sharing ",
#            "resource, designed to encourage greater reflection ",
#            "on different urban sustainability issues, and which ",
#            "communities are most impacted by them."),
#     paste0("SUS stands at the intersection of two ongoing ",
#            "transformations of urban life, urban governance, and ",
#            "urban research. First, cities are increasingly ",
#            "understood as key arenas for determining the planet’s ",
#            "environmental trajectory, and academic, policy, and ",
#            "public interest in urban sustainability has risen ",
#            "accordingly (Angelo and Wachsmuth 2019). Second, as ",
#            "part of a trend towards data-driven governance, cities ",
#            "are becoming increasingly prominent laboratories for ",
#            "“smart city” initiatives which incorporate digital ",
#            "technologies into local decision making and indeed the ",
#            "physical fabric of the city (Greenfield 2006; ",
#            "Mattern 2015)."),
#     paste0("SUS relies on a unified data infrastructure to help ",
#            "different people get different answers about common ",
#            "sustainability questions. The majority of these data ",
#            "are publicly available and reproducible, aggregated ",
#            "into thematic and place-based modules to allow a ",
#            "range of users and stakeholders greater accessibility ",
#            "to answering sustainability questions. A household ",
#            "considering a move may want to know which ",
#            "neighbourhoods support walkable lifestyles. ",
#            "Government planners may also be interested in ",
#            "neighbourhood walkability, but they want to understand ",
#            "aggregate patterns, and identify neighbourhoods which ",
#            "are outliers in terms of physical characteristics and ",
#            "observed travel patterns of residents. SUS is also a ",
#            "hub for academic innovation, using the questions ",
#            "generated by users to drive the research we do. This ",
#            "feedback creates something we have never seen before: ",
#            "a clearinghouse for urban sustainability decision ",
#            "making."),
#     paste0("While the primary implementation of SUS is focused on ",
#            "the Montreal region, the infrastructure we developed ",
#            "is designed to scale to other cities and regions in ",
#            "Canada, with a streamlined pathway for new modules or ",
#            "research questions to be explored and added in."),
#     paste0("SUS mobilizes quantitative data and analysis and ",
#            "qualitative data and narrative within a firm ",
#            "understanding of the injustices and disparities in ",
#            "urban life. Each module within SUS encourages a ",
#            "critical lens toward inequity in cities, and the ",
#            "potential for the data to be used to reinforce ",
#            "discriminatory power structures. SUS actively seeks ",
#            "to question the status quo and the institutional ",
#            "discrimination it sustains, and provides its users ",
#            "tools to do the same."),
#     paste0("SUS catalyzes new understandings of urban ",
#            "sustainability, transforms conventional thinking about ",
#            "the relationship between humans and the natural ",
#            "environment in cities, and positively shifts the ",
#            "trajectory of local sustainability decision making ",
#            "across Canada."),
#     paste0("Further resources:"),
#     paste0("<ul><li><a href= ''>Robin Basalaev-Binder ",
#            "and David Wachsmuth. 2020. 'Progress in ",
#            "data-driven urban sustainability'. ",
#            "Working paper.</a> <b>(MSSI research)</b></ul>"),
#     
#     # HOME
#     paste0("Towards a"),
#     paste0("sustainable city"),
#     paste0("Learn more"),
#     paste0("Start Exploring Maps"),
#     paste0("Statement"),
#     paste0("SUS is a platform for integrating, exploring, and analyzing a ", 
#            "wide range of urban sustainability data sources for the Montreal ",
#            "region across multiple spatial and temporal scales. SUS offers ",
#            "a robust set of tools for scenario modelling and analysis which ",
#            "will be useful for researchers, policymakers, communities, and ",
#            "individuals."),
#     paste0("SUS embraces an inclusive vision of urban sustainability, allowing ",
#            "users to contextualize questions into larger frameworks of equity and ",
#            "accessibility. It serves as both a data-exploration tool and a knowledge ",
#            "and information-sharing resource, designed to encourage greater ",
#            "reflection on different urban sustainability issues, and on the ",
#            "communities which are most impacted by them."),
#     paste0("The majority of the data used are publicly available and aggregated ",
#            "into thematic and place-based modules to allow a range of stakeholders ",
#            "greater accessibility to answering sustainability questions. SUS ",
#            "further mobilizes both qualitative and quantitative research to bring ",
#            "questions without clear datasets into the discussion."),
#     paste0("SUS aims to engage Montrealers to harness the momentum surrounding ",
#            "technologically-based approaches to sustainability for public good with ",
#            "a vision towards making the City more socially inclusive and less ",
#            "environmentally impactful."),
#     paste0("An initiative of the"),
#     paste0("McGill Sustainability Systems Initiative"),
#     paste0("More")
#   )) |> 
#   left_join(home_and_about_translated, by = "en")
#   


# REVERT TO R SCRIPT ------------------------------------------------------

home_and_about_translated <- 
  tibble(en = character(), fr = character()) |>
  add_row(en = paste0("Towards a"), 
          fr = paste0("Vers une")) |> 
  add_row(en = paste0("sustainable city"), 
          fr = paste0("ville durable")) |> 
  add_row(en = paste0("Learn more"), 
          fr = paste0("En savoir plus")) |> 
  add_row(en = paste0("Start Exploring Maps"), 
          fr = paste0("Explorez les cartes")) |> 
  add_row(en = paste0("Statement"), 
          fr = paste0("Mission")) |> 
  add_row(en = paste0("SUS is a platform for integrating, exploring, and analy",
                      "zing a wide range of urban sustainability data sources ",
                      "for the Montreal region across multiple spatial and tem",
                      "poral scales. SUS offers a robust set of tools for scen",
                      "ario modelling and analysis which will be useful for re",
                      "searchers, policymakers, communities, and individuals."), 
          fr = paste0("SUS est une plateforme qui intègre, explore et analyse ",
                      "un large éventail de sources de données en lien avec la",
                      " durabilité urbaine dans la région de Montréal et ce, à",
                      " travers multiples échelles spatiales et temporelles. S",
                      "US offre une gamme d'outils robustes pour la modélisati",
                      "on et l'analyse de scénarios qui seront utiles pour les",
                      " chercheurs, les décideurs politiques, les communautés ",
                      "et les individus.")) |> 
  add_row(en = paste0("SUS embraces an inclusive vision of urban sustainabilit",
                      "y, allowing users to contextualize questions into large",
                      "r frameworks of equity and accessibility. It serves as ",
                      "both a data-exploration tool and a knowledge and inform",
                      "ation-sharing resource, designed to encourage greater r",
                      "eflection on different urban sustainability issues, and",
                      " on the communities which are most impacted by them."), 
          fr = paste0("SUS adopte une vision inclusive de la durabilité urbain",
                      "e qui permet aux utilisateurs de contextualiser certain",
                      "es questions dans des cadres plus larges d'équité et d'",
                      "accessibilité. À la fois un outil d'exploration de donn",
                      "ées et une ressource de partage de connaissances et d'i",
                      "nformation, il a été conçu pour encourager une réflexio",
                      "n plus poussée sur différents enjeux liés à la durabili",
                      "té urbaine ainsi que sur les communautés qui sont le pl",
                      "us affectées par ceux-ci. ")) |> 
  add_row(en = paste0("The majority of the data used are publicly available an",
                      "d aggregated into thematic and place-based modules to a",
                      "llow a range of stakeholders greater accessibility to a",
                      "nswering sustainability questions. SUS further mobilize",
                      "s both qualitative and quantitative research to bring q",
                      "uestions without clear datasets into the discussion."), 
          fr = paste0("La majorité des données utilisées sont accessibles au p",
                      "ublic et regroupées en modules thématiques et géographi",
                      "ques afin de permettre à un large éventail de parties p",
                      "renantes de mieux répondre aux questions de durabilité.",
                      " SUS mobilise également des recherches qualitatives et ",
                      "quantitatives afin d'intégrer au débat des questions po",
                      "ur lesquelles il n'existe pas suffisamment de données. ",
                      "")) |> 
  add_row(en = paste0("SUS aims to engage Montrealers to harness the momentum ",
                      "surrounding technologically-based approaches to sustain",
                      "ability for public good with a vision towards making th",
                      "e City more socially inclusive and less environmentally",
                      " impactful."), 
          fr = paste0("SUS vise à encourager les Montréalais à exploiter le mo",
                      "mentum entourant les approches technologiques de la dur",
                      "abilité pour le bien public, dans le but de rendre la v",
                      "ille plus inclusive sur le plan social et moins dommage",
                      "able sur le plan environnemental. ")) |> 
  add_row(en = paste0("An initiative of the "), 
          fr = paste0("Une initiative de l'")) |> 
  add_row(en = paste0("McGill Sustainability Systems Initiative"), 
          fr = paste0("Initiative systémique de McGill sur la durabilité")) |> 
  add_row(en = paste0("More"), 
          fr = paste0("Plus"))

# Save --------------------------------------------------------------------

# Also encode english for `Kanehsatà:ke`
# Encoding(home_and_about_translated$en) <- "UTF-8"
# Encoding(home_and_about_translated$fr) <- "UTF-8"
# write_csv(home_and_about_translated, 
#           file = "dev/translation/csv/home_and_about_translated.csv")
