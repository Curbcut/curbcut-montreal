### PIECES OF TRANSLATION NEEDED BEFORE SWITCH DESIGN ##########################

translation_temp <- tibble(en = character(), fr = character()) |> 
  add_row(en = "Centraide",
          fr = "Centraide") |> 
  add_row(en = "Home",
          fr = "Accueil") |> 
  add_row(en = "Maps",
          fr = "Cartes") |> 
  add_row(en = "Montréal stories",
          fr = "Histoires de Montréal") |> 
  add_row(en = "Montreal",
          fr = "Montréal") |> 
  add_row(en = "About",
          fr = "À propos") |> 
  add_row(en = "Contact/feedback",
          fr = "Contact/retour") |> 
  add_row(en = "SEE MAP",
          fr = "VOIR CARTE") |> 
  add_row(en = "Back to the map",
          fr = "Retour à la carte") |> 
  add_row(en = "Choose themes:",
          fr = "Choisissez des thèmes :") |> 
  add_row(en = "About Curbcut",
          fr = "À propos de Curbcut") |> 
  add_row(en = "How to use",
          fr = "Mode d'emploi") |> 
  add_row(en = "Authors",
          fr = "Auteurs") |> 
  add_row(en = "Stories",
          fr = "Histoires") |> 
  add_row(en = "Themes",
          fr = "Thèmes")