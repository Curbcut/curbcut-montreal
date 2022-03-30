#### Green alleys translation ##################################################

green_alleys_translated <- 
tibble(en = character(), fr = character()) |>
  add_row(en = paste0("The alley is mainly covered with grass, and is very wel",
                      "l maintained with several signs demonstrating the wish ",
                      "to build a sense of community (name of the alley, small",
                      " community garden, play areas)."), 
          fr = paste0("La ruelle est principalement couverte d'herbe, et est t",
                      "rès bien maintenue: plusieurs signes démontrent une vol",
                      "onté de développer un sentiment de communauté (nom de l",
                      "a ruelle, petit jardin communautaire, espaces de jeux).",
                      "")) |> 
  add_row(en = paste0("The alley is a 'ruelle champêtre'. Most of it is permea",
                      "ble, with a green strip in the middle and lateral green",
                      "ery on both sides. It is a well-maintained alley with m",
                      "any plants and flowers, trees and green walls."), 
          fr = paste0("La ruelle est une 'ruelle champêtre'. La plupart de la ",
                      "ruelle est perméable, avec une bande d'herbe au milieu ",
                      "et de la végétation latérale des deux côtés. C'est une ",
                      "ruelle bien maintenue avec beaucoup de plantes et de fl",
                      "eurs, des arbres et des murs végétalisés.")) |> 
  add_row(en = paste0("There is a large green strip in the middle of the alley",
                      " and multiple green elements on the sides. There is als",
                      "o a play area for children."), 
          fr = paste0("Il y a une large bande d'herbe au milieu de la ruelle e",
                      "t beaucoup de végétation sur les côtés. Il y a égalemen",
                      "t un espace de jeux pour enfants.")) |> 
  add_row(en = paste0("It is one of the most famous green alleys in Montreal. ",
                      "Most of its surface is permeable, with a lot of greener",
                      "y."), 
          fr = paste0("C'est une des ruelles vertes les plus connues à Montréa",
                      "l. La plupart de la surface de la ruelle est perméable,",
                      " avec beaucoup de végétation.")) |> 
  add_row(en = paste0("The alley has a sign saying that it is a 'family and ac",
                      "tive street'. It has green elements, a community garden",
                      ", spaces where children can play, and murals."), 
          fr = paste0("Il y a un signe dans la ruelle indiquant qu'il s'agit d",
                      "'une 'rue active et familiale' avec de la végétation, u",
                      "n jardin communautaire, des espaces où les enfants peuv",
                      "ent jouer, et des murales.")) |> 
  add_row(en = paste0("This alley is very green and safe for children."), 
          fr = paste0("La ruelle est très verte et sécuritaire pour les enfant",
                      "s.")) |> 
  add_row(en = paste0("The alley has trees, plants and flowers on the sides. E",
                      "xcavation works were undertaken in 2012 to create perme",
                      "able sections. Benches, toys and a drawing exhibition a",
                      "dd a sense of community."), 
          fr = paste0("Il y a des arbres, des plantes et des fleurs sur les cô",
                      "tés de la ruelle. Des travaux d'excavation ont été entr",
                      "epris en 2012 pour créer des sections de surface perméa",
                      "ble. Des bancs, des jeux et une exposition de dessins a",
                      "joutent un sentiment de  communauté.")) |> 
  add_row(en = paste0("The alley has very green sections with permeable coveri",
                      "ng. There is also one larger section open to circulatio",
                      "n but with paintings, murals, and handmade colourful si",
                      "gns that add a sense of community."), 
          fr = paste0("La ruelle a des sections très végétalisées avec une sur",
                      "face perméable. Il y aussi une large section ouverte à ",
                      "la circulation mais où il y a des peintures, des murale",
                      "s, et des panneaux colorés faits à la main qui ajoutent",
                      " un sentiment de communauté.")) |> 
  add_row(en = paste0("There are three very green sections with green walls, f",
                      "lower pots, green strips. The main section is very wide",
                      " and permeable."), 
          fr = paste0("Il y a trois sections très vertes avec des murs végétal",
                      "isés, des pots à fleurs et des bandes d'herbe. La secti",
                      "on principale est très large et perméable.")) |> 
  add_row(en = paste0("There is a permeable strip in the middle of the alley. ",
                      "An important part of the greenery comes from private ga",
                      "rdens or courtyards. Murals and games for children add ",
                      "a sense of community."), 
          fr = paste0("Il y a une bande perméable au milieu de la ruelle. Une ",
                      "part importante de la végétation  vient de jardins ou c",
                      "ours privés. Des murales et des jeux pour enfants ajout",
                      "ent un sentiment de communauté.")) |> 
  add_row(en = paste0("It is more a community garden than a green alley. It ha",
                      "s arts and craft, as well as explanatory boards that pr",
                      "ovide environment-oriented information."), 
          fr = paste0("Il s'agit davantage d'un jardin communautaire que d'une",
                      " ruelle verte. Il y a de l'artisanat ainsi que des pann",
                      "eaux explicatifs qui fournissent des informations en li",
                      "en avec l'environnement.")) |> 
  add_row(en = paste0("There are multiple elements that create a sense of comm",
                      "unity: play areas for kids, colourful signs, crafts and",
                      " floor games. Activities with the residents seem to be ",
                      "frequently organised."), 
          fr = paste0("Il y a de nombreux éléments qui créent un sentiment de ",
                      "communauté: des espaces de jeux pour enfants, des signe",
                      "s colorés, de l'artisanat et des jeux au sol. Des activ",
                      "ités entre résidents semblent être fréquemment organisé",
                      "es.")) |> 
  add_row(en = paste0("The alley has lateral greenery on both sides, and games",
                      " for children such as hockey sticks."), 
          fr = paste0("Il y a de la végétation latérale des deux côtés de la r",
                      "uelle, des jouets pour enfants tels que des crosses de ",
                      "hockey.")) |> 
  add_row(en = paste0("The alley is very grey with only few green elements com",
                      "ing from private gardens."), 
          fr = paste0("La ruelle est très grise avec seulement peu de végétati",
                      "on qui vient de jardins privés.")) |> 
  add_row(en = paste0("The alley has lateral greenery (mainly green walls), sm",
                      "all basketball baskets for kids, and murals."), 
          fr = paste0("Il y a de la végétation latérale (principalement des mu",
                      "rs végétalisés) dans la ruelle, des petits paniers de b",
                      "asketball pour enfants, et des murales.")) |> 
  add_row(en = paste0("It is a narrow alley mainly covered with grass."), 
          fr = paste0("C'est une ruelle étroite principalement couverte d'herb",
                      "e.")) |> 
  add_row(en = paste0("The alley is located next to René-Lévesque Boulevard. M",
                      "ost of it is covered with permeable material and it see",
                      "ms to be well maintained. It has a lot of greenery: flo",
                      "wer, plants, trees and green walls."), 
          fr = paste0("La ruelle est située près du boulevard René-Lévesque. L",
                      "a majorité de la surface de la ruelle est  perméable et",
                      " la ruelle a l'air bien entretenue. La ruelle a beaucou",
                      "p de végétation: fleurs, plantes, arbres et murs végéta",
                      "lisés.")) |> 
  add_row(en = paste0("it is a very green alley, with stairs: it has gardens o",
                      "n both sides, with big trees that provide shade."), 
          fr = paste0("La ruelle très verte, avec des marches: il y a des jard",
                      "ins des deux côtés avec des grands arbres qui apportent",
                      " de l'ombre.")) |> 
  add_row(en = paste0("The alley is very small and is situated next to a parki",
                      "ng lot. It is a curving path bordered by greenery."), 
          fr = paste0("La ruelle est très petite et est située près d'un espac",
                      "e de parking. C'est un chemin en courbe bordé de végéta",
                      "tion.")) |> 
  add_row(en = paste0("Bordered by four hotels, this alley is very grey and do",
                      "es not seem to be regularly maintained. There are only ",
                      "a few green elements."), 
          fr = paste0("Bordée par quatre hôtels, cette ruelle est très grise e",
                      "t ne semble pas être entretenue de façon régulière. Il ",
                      "y a peu de végétation.")) |> 
  add_row(en = paste0("The alley has lateral greenery (mainly green walls), an",
                      "d few benches. Children were playing in the alley when ",
                      "we visited it."), 
          fr = paste0("Il y a de la végétation latérale (principalement des mu",
                      "rs végétalisés) dans la ruelle, et quelques bancs. Des ",
                      "enfants jouaient dans la ruelle quand on l'a visitée.")) |> 
  add_row(en = paste0("The alley has lateral greenery, trees and an excavated ",
                      "permeable section. There are many elements that foster ",
                      "community life: a shared library, play areas for childr",
                      "en, and chairs."), 
          fr = paste0("La ruelle a de la végétation latérale, des arbres et un",
                      "e section perméable excavée. Il y a de nombreux élément",
                      "s qui favorisent une vie de communauté: une bibliothèqu",
                      "e partagée, des espaces de jeux pour enfants, et des ch",
                      "aises.")) |> 
  add_row(en = paste0("Greenery mainly comes from private gardens or courtyard",
                      "s. The alley has some benches."), 
          fr = paste0("La végétation vient principalement de jardins ou cours ",
                      "privés. Il y a quelques bancs dans la ruelle.")) |> 
  add_row(en = paste0("Greenery mainly come from private gardens or courtyards",
                      ". The alley has some benches and chairs."), 
          fr = paste0("La végétation vient principalement de jardins ou cours ",
                      "privés. Il y a quelques bancs et chaises dans la ruelle",
                      ".")) |> 
  add_row(en = paste0("It only has a few green elements and a table."), 
          fr = paste0("Il y a un peu de végétation et une table dans la ruelle",
                      ".")) |> 
  add_row(en = paste0("The alley is somehow divided into two parts: while the ",
                      "first half is very grey, with a lot of parking spots an",
                      "d very few vegetation, the second half is more children",
                      "-oriented with toys, bikes, and is greener."), 
          fr = paste0("La ruelle est en quelque sorte divisée en deux parties:",
                      " alors que la première partie est très grise, avec de n",
                      "ombreux espaces de stationnement et très peu de végétat",
                      "ion, la seconde partie est davantage pour les enfants a",
                      "vec des jouets, des vélos, et a plus de végétation.")) |> 
  add_row(en = paste0("It is a recent green alley. It does not have a lot of g",
                      "reenery, but it has some benches and toys for kids."), 
          fr = paste0("Il s'agit d'une ruelle verte récente. Il n'y a pas beau",
                      "coup de végétation, mais il y a quelques bancs et des j",
                      "ouets pour enfants.")) |> 
  add_row(en = paste0("It has greenery and is very community oriented. It is a",
                      " colourful alley with activities for children of differ",
                      "ent ages. There are basketball baskets, a ping-pong tab",
                      "le as well as swings and signs made by the residents."), 
          fr = paste0("Il s'agit d'une ruelle verte et communautaire. C'est un",
                      "e ruelle colorée avec des activités pour des enfants d'",
                      "âges divers. Il y a des paniers de basketball, une tabl",
                      "e de ping-pong ainsi que des balançoires et des panneau",
                      "x faits par les résidents.")) |> 
  add_row(en = paste0("There is not a lot of greenery, but elements such as be",
                      "nches and a basketball basket foster a sense of communi",
                      "ty."), 
          fr = paste0("Il y a peu de végétation, mais plusieurs éléments tels ",
                      "que des bancs et un panier de basketball favorisent un ",
                      "sentiment de communauté.")) |> 
  add_row(en = paste0("The alley has lateral greenery (flowers, trees), as wel",
                      "l as benches, chairs and a shared library."), 
          fr = paste0("La ruelle a de la végétation latérale (fleurs, arbres),",
                      " ainsi que des bancs, des chaises et une bibliothèque p",
                      "artagée.")) |> 
  add_row(en = paste0("The alley has lateral greenery, and a space with a bask",
                      "etball basket where kids can play."), 
          fr = paste0("La ruelle a de la végétation latérale, un espace avec u",
                      "n panier de basketball où les enfants peuvent jouer.")) |> 
  add_row(en = paste0("This is a short alley with very few greenery, but with ",
                      "benches, games for children and a shared library."), 
          fr = paste0("Il s'agit d'une ruelle courte avec très peu de végétati",
                      "on, mais avec des bancs, des jeux pour enfants et une b",
                      "ibliothèque partagée.")) |> 
  add_row(en = paste0("The alley has lateral greenery, a shared library and ch",
                      "airs."), 
          fr = paste0("La ruelle a de la végétation latérale, une bibliothèque",
                      " partagée et des chaises.")) |> 
  add_row(en = paste0("The alley has two different sections. It has permeable ",
                      "covering and lateral greenery. There are also benches a",
                      "nd spaces for children that reinforce the community dim",
                      "ension."), 
          fr = paste0("La ruelle a deux sections distinctes. La surface est pe",
                      "rméable et il y a de la végétation latérale. Il y a aus",
                      "si des bancs et des espaces pour enfants qui renforcent",
                      " la dimension communautaire.")) |> 
  add_row(en = paste0("The alley has lateral greenery (plants and trees)."), 
          fr = paste0("La ruelle a de la végétation latérale (plantes et arbre",
                      "s).")) |> 
  add_row(en = paste0("It is a very green alley, with lateral greenery on each",
                      " side and a grass strip in the middle."), 
          fr = paste0("très verte, avec de la végétation latérale des deux côt",
                      "és et une bande d'herbe au milieu.")) |> 
  add_row(en = paste0("This alley is very grey."), 
          fr = paste0("Cette ruelle très grise.")) |> 
  add_row(en = paste0("The alley is a curving path bordered by plants. It also",
                      " has a shared library."), 
          fr = paste0("La ruelle est un chemin en courbe bordé par des plantes",
                      ". La ruelle a aussi une bibliothèque partagée.")) |> 
  add_row(en = paste0("The alley is a curving path bordered by plants. It also",
                      " has a shared library."), 
          fr = paste0("La ruelle est un chemin en courbe bordé par des plantes",
                      ". La ruelle a aussi une bibliothèque partagée.")) |> 
  add_row(en = paste0("The alley is very grey, with only a few elevated pots w",
                      "ith plants."), 
          fr = paste0("La ruelle est très grise, avec seulement quelques pots ",
                      "de plantes surélevés.")) |> 
  add_row(en = paste0("The alley has permeable covering and it has lateral gre",
                      "enery on both sides."), 
          fr = paste0("La surface de la ruelle est perméable et il y a de la v",
                      "égétation latérale des deux côtés.")) |> 
  add_row(en = paste0("The alley is very grey with very few greenery and a lot",
                      " of parking spots."), 
          fr = paste0("La ruelle est très grise avec très peu de végétation et",
                      " beaucoup de places de stationnement.")) |> 
  add_row(en = paste0("The alley is located next to the Ville-Marie Expressway",
                      ". It has many trees, plants and flowers. It is well mai",
                      "ntained and also has spaces where people can gather."), 
          fr = paste0("La ruelle est située près de l'autoroute Ville-Marie. I",
                      "l y a de nombreux arbres, plantes et fleurs dans la rue",
                      "lle. Elle est bien maintenue et il y a également des es",
                      "paces où les gens peuvent se rassembler.")) |> 
  add_row(en = paste0("There are trees and plants in the alley, as well as gra",
                      "ss strips in the middle."), 
          fr = paste0("Il y a des arbres et des plantes dans la ruelle, ainsi ",
                      "que des bandes d'herbe au milieu.")) |> 
  add_row(en = paste0("The alley has lateral greenery on each side and there i",
                      "s a strip of permeable surface. One smaller section is ",
                      "closed to circulation and has flower pots."), 
          fr = paste0("La ruelle a de la végétation latérale de chaque côté et",
                      " il y a une bande de surface perméable. Une plus petite",
                      " section est fermée à la circulation et a des pots à fl",
                      "eurs.")) |> 
  add_row(en = paste0("The alley has some greenery and permeable sections. It ",
                      "is located next to a playground."), 
          fr = paste0("La ruelle a un peu de végétation et des sections perméa",
                      "bles. La ruelle est située près d'un terrain de jeux.")) |> 
  add_row(en = paste0("The alley is very grey, except for the small green stri",
                      "p at one entrance."), 
          fr = paste0("La ruelle est très grise, à l'exception de la bande d'h",
                      "erbe à une des entrées.")) |> 
  add_row(en = paste0("The alley has lateral greenery in pots, grass strips in",
                      " the middle, and a small park with permeable covering."), 
          fr = paste0("La ruelle a de la végétation latérale dans des pots de ",
                      "fleurs, des bandes d'herbe au milieu, et un petit parc ",
                      "avec un revêtement perméable.")) |> 
  add_row(en = paste0("The alley has lateral greenery (plants, trees and flowe",
                      "rs), and a mural."), 
          fr = paste0("La ruelle a de la végétation latérale (plantes, arbres ",
                      "et fleurs), et une murale.")) |> 
  add_row(en = paste0("The alley has trees, plants, flowers, and a large commu",
                      "nity garden. It is also community-oriented as there are",
                      " tables and chairs, and a play area for kids."), 
          fr = paste0("Il y a des arbres, des plantes, des fleurs, et un large",
                      " jardin communautaire dans la ruelle. La ruelle est aus",
                      "si communautaire étant donné qu'il y a des tables et de",
                      "s chaises, ainsi qu'un espace de jeux pour enfants.")) |> 
  add_row(en = paste0("The alley has lateral greenery (trees, and plants and f",
                      "lowers in pots), as well as a permeable section."), 
          fr = paste0("La ruelle a de la végétation latérale (arbres, et plant",
                      "es et fleurs en pots), ainsi qu'une section perméable.")) |> 
  add_row(en = paste0("It is a wide paved alley with lateral greenery and big ",
                      "trees."), 
          fr = paste0("Il s'agit d'une large ruelle pavée avec de la végétatio",
                      "n latérale et des grands arbres.")) |> 
  add_row(en = paste0("It is a wild green alley covered with grass, with many ",
                      "plants and trees."), 
          fr = paste0("Il s'agit d'une ruelle verte sauvage couverte d'herbe, ",
                      "avec de nombreux arbres et plantes.")) |> 
  add_row(en = paste0("The alley has some lateral greenery that mainly comes f",
                      "rom private gardens or courtyards, and a permable secti",
                      "on in the middle."), 
          fr = paste0("La ruelle a un peu de végétation latérale, venant princ",
                      "ipalement de jardins ou cours privés, et une section pe",
                      "rméable au milieu.")) |> 
  add_row(en = paste0("There is a lot of lateral greenery, both in the alley a",
                      "nd coming from private gardens or courtyards. There are",
                      " also murals and colourful signs."), 
          fr = paste0("Il y a beaucoup de végétation latérale, à la fois dans ",
                      "la ruelle et venant de jardins ou cours privés. Il y a ",
                      "aussi des murales et des panneaux colorés.")) |> 
  add_row(en = paste0("It is a wide paved alley, with lateral greenery and an ",
                      "area where children can play."), 
          fr = paste0("Il s'agit d'une large ruelle pavée, avec de la végétati",
                      "on latérale et un espace où les enfants peuvent jouer.")) |> 
  add_row(en = paste0("This alley is located next to Decarie highway. To a cer",
                      "tain extent, the lateral greenery blocks the view and t",
                      "he noise of the highway."), 
          fr = paste0("Cette ruelle est située près de l'autoroute Décarie. Da",
                      "ns une certaine mesure, la végétation latérale bloque l",
                      "a vue sur l'autoroute et le bruit.")) |> 
  add_row(en = paste0("The first half of the alley is very grey and for cars, ",
                      "but the second half has lateral greenery coming from pr",
                      "ivate gardens or courtyards and many trees that bring s",
                      "hade."), 
          fr = paste0("La première moitié de la ruelle est très grise et dédié",
                      "e aux voitures, mais la seconde moitié a de la végétati",
                      "on latérale venant de jardins ou cours privés, et de no",
                      "mbreux arbres qui apportent de l'ombre.")) |> 
  add_row(en = paste0("The alley has lateral greenery in pots and in excavated",
                      " sections, trees, and grass strips in the middle."), 
          fr = paste0("La ruelle a de la végétation latérale dans des pots et ",
                      "dans des sections excavées, des arbres, et des bandes d",
                      "'herbe au milieu.")) |> 
  add_row(en = paste0("The alley has lateral greenery, trees, a community gard",
                      "en, and murals"), 
          fr = paste0("La ruelle a de la végétation latérale, des arbres, un j",
                      "ardin communautaire et des murales")) |> 
  add_row(en = paste0("The alley has lateral greenery in excavated sections, t",
                      "rees and grass strips in the middle."), 
          fr = paste0("La ruelle a de la végétation latérale dans des sections",
                      " excavées, des arbres et des bandes d'herbe au milieu.")) |> 
  add_row(en = paste0("The alley has lateral greenery, permeable strips, trees",
                      " and flower pots. There are also murals, benches and sp",
                      "aces for children."), 
          fr = paste0("La ruelle a de la végétation latérale, des bandes permé",
                      "ables, des arbres et des pots de fleurs. Il y a aussi d",
                      "es murales, des bancs et des espaces pour enfants")) |> 
  add_row(en = paste0("There are plants and trees on the sides. The alley also",
                      " has benches and murals."), 
          fr = paste0("Il y a des surfaces vertes latérales avec des plantes e",
                      "t des arbres. Il y a aussi des bancs et des murales dan",
                      "s la ruelle.")) |> 
  add_row(en = paste0("There are lateral excavations with greenery, permeable ",
                      "pavement and green walls."), 
          fr = paste0("Il y a des excavations latérales avec de la végétation,",
                      " un revêtement perméable et des murs végétalisés")) |> 
  add_row(en = paste0("The alley has a lateral section with greenery and an ex",
                      "cavated segment."), 
          fr = paste0("La ruelle a une section latérale avec de la  végétation",
                      " et un segment excavé.")) |> 
  add_row(en = paste0("There are plant pots, benches, and floor games in the a",
                      "lley."), 
          fr = paste0("Il y a des pots de plantes, des bancs, et des jeux au s",
                      "ol dans la ruelle.")) |> 
  add_row(en = paste0("There are plant pots, lateral greenery, trees, and a mu",
                      "ral in the alley."), 
          fr = paste0("Il y a des pots de plantes, de la végétation latérale, ",
                      "des arbres, et des murales dans la ruelle.")) |> 
  add_row(en = paste0("closed"), 
          fr = paste0("fermée")) |> 
  add_row(en = paste0("open, with some parking spots"), 
          fr = paste0("ouverte, avec quelques places de stationnement")) |> 
  add_row(en = paste0("partially closed"), 
          fr = paste0("partiellement fermée")) |> 
  add_row(en = paste0("open"), 
          fr = paste0("ouverte")) |> 
  add_row(en = paste0("closed"), 
          fr = paste0("fermée")) |> 
  add_row(en = paste0("open, with a speed limit of 10 km/h"), 
          fr = paste0("ouverte, avec une limitation de vitesse à 10 km/h")) |> 
  add_row(en = paste0("open, with traffic calming infrastructures at both entr",
                      "ances"), 
          fr = paste0("ouverte, avec des infrastructures de modération du traf",
                      "ic à chaque entrée")) |> 
  add_row(en = paste0("open, with many parking spots"), 
          fr = paste0("ouverte, avec de nombreuses places de stationnement")) |> 
  add_row(en = paste0("closed, with fences at both entrances"), 
          fr = paste0("fermée, avec des barrières à chaque entrée")) |> 
  add_row(en = paste0("open, with several parking spots"), 
          fr = paste0("ouverte, avec plusieurs places de stationnement")) |> 
  add_row(en = paste0("open, with traffic calming infrastructure"), 
          fr = paste0("ouverte, avec des infrastructures de modération du traf",
                      "ic")) |> 
  add_row(en = paste0("open, with a speed limit of 15 km/h"), 
          fr = paste0("ouverte, avec une limitation de vitesse à 15 km/h")) |> 
  add_row(en = paste0("open, with many parking spots."), 
          fr = paste0("ouverte, avec de nombreuses places de stationnement.")) |> 
  add_row(en = paste0("partially closed to circulation as there is one garage ",
                      "at the end of the alley."), 
          fr = paste0("partiellement fermée à la circulation puisqu'il y a un ",
                      "garage à chaque entrée de la ruelle.")) |> 
  add_row(en = paste0("open, with several garages"), 
          fr = paste0("ouverte, avec plusieurs garages")) |> 
  add_row(en = paste0("Inauguration date: "), 
          fr = paste0("Date d'inauguration: ")) |> 
  add_row(en = paste0("We do not have information available on this green alley"), 
          fr = paste0("Nous n'avons actuellement pas d'informations disponible",
                      "s sur cette ruelle verte"))
  