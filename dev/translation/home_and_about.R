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
  add_row(en = paste0("Curbcut is a platform for exploring urban sustainability in",
                      " the Montreal region across multiple spatial and tempor",
                      "al scales. Curbcut offers a justice- and inclusivity-focuse",
                      "d approach to sustainability which integrates the wides",
                      "t possible range of data sources to help researchers, p",
                      "olicymakers, communities, and individuals."), 
          fr = paste0("Curbcut est une plateforme permettant d'explorer la durabil",
                      "ité urbaine dans la région de Montréal à de multiples é",
                      "chelles spatiales et temporelles. Curbcut propose une appro",
                      "che de la durabilité axée sur la justice et l'inclusion",
                      " qui intègre le plus large éventail possible de sources",
                      " de données pour aider les chercheurs, les décideurs po",
                      "litiques, les communautés et les individus.")) |> 
  add_row(en = paste0("Curbcut embraces an inclusive vision of urban sustainabilit",
                      "y, allowing users to pose questions about environmental",
                      " issues and contextualize them within larger frameworks",
                      " of equity and accessibility. It serves as both a data-",
                      "exploration tool and a knowledge and information-sharin",
                      "g resource, designed to encourage greater reflection on",
                      " urban sustainability challenges, and on the communitie",
                      "s which are most affected by them."), 
          fr = paste0("Curbcut adopte une vision inclusive de la durabilité urbain",
                      "e, permettant aux utilisateurs de poser des questions s",
                      "ur les problèmes environnementaux et de les contextuali",
                      "ser dans des cadres plus larges d'équité et d'accessibi",
                      "lité. Elle sert à la fois d'outil d'exploration des don",
                      "nées et de ressource de partage des connaissances et de",
                      "s informations, conçue pour encourager une plus grande ",
                      "réflexion sur les défis de la durabilité urbaine, ainsi",
                      " que sur les communautés qui en sont le plus affectées.",
                      "")) |> 
  add_row(en = paste0("Curbcut is organized into thematic and place-based “modules",
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
          fr = paste0("Curbcut est organisée en “modules” thématiques et géographi",
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
  
  # Get notified about the 2021 census
  add_row(en = paste0("Get notified about"),
          fr = paste0("Soyez informé du")) |>
  add_row(en = paste0("the 2021 Census"),
          fr = paste0("recensement de 2021")) |>
  add_row(en = paste0("Many of the topics that can be explored on Curbcut draw on data ",
                      "from the Canadian Census. The Census of Population is ",
                      "conducted every five years and provides statistical ",
                      "information about demographic, social and economic ",
                      "characteristics."),
          fr = paste0("Plusieurs des sujets qui peuvent être explorés sur ",
                      "Curbcut s’appuient sur des données provenant du Recensement ",
                      "canadien. Le Recensement de la population est effectué ",
                      "tous les cinq ans et fournit des informations statistiques ",
                      "sur les caractéristiques démographiques, sociales et ",
                      "économiques.")) |>
  add_row(en = paste0("Sign up to our newsletter to get notified when the 2021 ",
                      "Census data is available on Curbcut!"),
          fr = paste0("Inscrivez-vous à notre infolettre pour être informé ",
                      "lorsque les données du Recensement de 2021 seront ",
                      "disponibles sur Curbcut !")) |>
  add_row(en = paste0("Sign up!"),
          fr = paste0("Inscrivez-vous !")) |>
  
  # Centraide
  add_row(en = paste0("In a novel collaboration, Centraide of Greater Montreal ",
                      "is partnering with Curbcut on a series of housing maps. ",
                      "Centraide is using its social expertise and data to help ",
                      "target and interpret housing issues, a decisive factor ",
                      "in poverty and social exclusion."),
          fr = paste0("Dans une collaboration inédite, Centraide du Grand ",
                      "Montréal s’associe à Curbcut à travers une série de ",
                      "cartes dédiées au logement. Centraide met son expertise ",
                      "sociale et ses données au profit de la plateforme pour ",
                      "mieux cibler et interpréter les enjeux liés au logement, ",
                      "qui est un facteur déterminant sur la pauvreté et ",
                      "l’exclusion sociale.  ")) |>
  
  # add_row(en = paste0("See the “How to use” page for more information on how S",
  #                     "us works. And see the “Authors” page to learn more abou",
  #                     "t our team."), 
  #         fr = paste0("Consultez la page “Mode d'emploi” pour en savoir plus s",
  #                     "ur le fonctionnement de Curbcut. Et consultez la page “Aute",
  #                     "urs” pour en savoir plus sur notre équipe.")) |> 
  add_row(en = paste0("See the "),
          fr = paste0("Consultez la page")) |> 
  add_row(en = paste0("“How to use”"),
          fr = paste0("“Mode d'emploi”")) |> 
  add_row(en = paste0("page for more information on how Curbcut works. And see the "),
          fr = paste0("pour en savoir plus sur le fonctionnement de Curbcut. Et consultez la page ")) |> 
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
