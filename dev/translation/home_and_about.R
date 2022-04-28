#### `Home`, `About`, `Why dashboard` preparation for translation ##############

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
  add_row(en = paste0("Sus is a platform for exploring urban sustainability in",
                      " the Montreal region across multiple spatial and tempor",
                      "al scales. Sus offers a justice- and inclusivity-focuse",
                      "d approach to sustainability which integrates the wides",
                      "t possible range of data sources to help researchers, p",
                      "olicymakers, communities, and individuals."), 
          fr = paste0("Sus est une plateforme permettant d'explorer la durabil",
                      "ité urbaine dans la région de Montréal à de multiples é",
                      "chelles spatiales et temporelles. Sus propose une appro",
                      "che de la durabilité axée sur la justice et l'inclusion",
                      " qui intègre le plus large éventail possible de sources",
                      " de données pour aider les chercheurs, les décideurs po",
                      "litiques, les communautés et les individus.")) |> 
  add_row(en = paste0("Sus embraces an inclusive vision of urban sustainabilit",
                      "y, allowing users to pose questions about environmental",
                      " issues and contextualize them within larger frameworks",
                      " of equity and accessibility. It serves as both a data-",
                      "exploration tool and a knowledge and information-sharin",
                      "g resource, designed to encourage greater reflection on",
                      " urban sustainability challenges, and on the communitie",
                      "s which are most affected by them."), 
          fr = paste0("Sus adopte une vision inclusive de la durabilité urbain",
                      "e, permettant aux utilisateurs de poser des questions s",
                      "ur les problèmes environnementaux et de les contextuali",
                      "ser dans des cadres plus larges d'équité et d'accessibi",
                      "lité. Elle sert à la fois d'outil d'exploration des don",
                      "nées et de ressource de partage des connaissances et de",
                      "s informations, conçue pour encourager une plus grande ",
                      "réflexion sur les défis de la durabilité urbaine, ainsi",
                      " que sur les communautés qui en sont le plus affectées.",
                      "")) |> 
  add_row(en = paste0("Sus is organized into thematic and place-based “modules",
                      "”, each of which takes a narrow slice of our data and p",
                      "resents it in a way designed to answer existing questio",
                      "ns and provoke new ones. What is the relationship betwe",
                      "en heat risk and housing tenure? Does my neighbourhood ",
                      "have better or worse active transport options than the ",
                      "rest of the city? What is the history of environmental ",
                      "gentrification in Little Burgundy? The majority of the ",
                      "data is publicly available, and over time we will be ad",
                      "ding more tools for users to export the data and use it",
                      " themselves."), 
          fr = paste0("Sus est organisée en “modules” thématiques et géographi",
                      "ques, chacun d'entre eux présentant une petite partie d",
                      "e nos données de manière à répondre aux questions exist",
                      "antes, mais aussi pour en susciter de nouvelles. Quelle",
                      " est la relation entre le risque de canicule et le mode",
                      " d'occupation du logement? Mon quartier dispose-t-il de",
                      " meilleures ou de moins bonnes options de transport act",
                      "if que le reste de la ville? Quelle est l'histoire de l",
                      "a gentrification environnementale dans la Petite Bourgo",
                      "gne? La majorité des données sont accessibles au public",
                      " et, au fil du temps, nous ajouterons des outils permet",
                      "tant aux utilisateurs d'exporter les données et de les ",
                      "utiliser eux-mêmes.")) |> 
  
  # add_row(en = paste0("See the “How to use” page for more information on how S",
  #                     "us works. And see the “Authors” page to learn more abou",
  #                     "t our team."), 
  #         fr = paste0("Consultez la page “Mode d'emploi” pour en savoir plus s",
  #                     "ur le fonctionnement de SUS. Et consultez la page “Aute",
  #                     "urs” pour en savoir plus sur notre équipe.")) |> 
  add_row(en = paste0("See the "),
          fr = paste0("Consultez la page")) |> 
  add_row(en = paste0("“How to use”"),
          fr = paste0("“Mode d'emploi”")) |> 
  add_row(en = paste0("page for more information on how Sus works. And see the "),
          fr = paste0("pour en savoir plus sur le fonctionnement de Sus. Et consultez la page ")) |> 
  add_row(en = paste0("“Authors”"),
          fr = paste0("“Auteurs”")) |> 
  add_row(en = paste0(" page to learn more about our team."),
          fr = paste0("pour en savoir plus sur notre équipe")) |> 
  
  add_row(en = paste0("McGill Sustainability Systems Initiative"), 
          fr = paste0("Initiative systémique de McGill sur la durabilité")) |> 
  add_row(en = paste0("More"), 
          fr = paste0("Plus")) |> 
  add_row(en = paste0("An initiative of the "), 
          fr = paste0("Une initiative de l'"))