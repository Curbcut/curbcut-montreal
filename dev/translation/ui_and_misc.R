#### `Info table` preparation for translation ##################################


# Import existing translation ---------------------------------------------

ui_and_misc_translated <-
  read.csv("dev/translation/csv/ui_and_misc_translated.csv") |>
  as_tibble()


# Translate UI strings ----------------------------------------------------
ui_and_misc_translated <- 
  c(c("Learn more" = "En savoir plus"),
    c("250-metre grid" = "Grille de 250 mètres"),
    c("Compare" = "Comparez"),
    c("Hide" = "En voir moins"),
    c("Explore" = "Explorez"),
    c("Select a year" = "Sélectionnez une année"),
    c("Select two years" = "Sélectionnez deux années"),
    c("Compare dates" = "Comparaison de dates"),
    c("Review a single variable part of the index" = 
        "Examiner une variable unique comprise dans l'indice"),
    c("Grouping" = "Regroupement"),
    c("Type of permits" = "Type de permis"),
    c("Select two dates" = "Sélectionnez deux dates"),
    c("Timing" = "Timing"),
    c("Destination type" = "Type de destination"),
    c("Time threshold" = "Seuil de temps"),
    c("Road safety analysis" = "Analyse de la sécurité routière"),
    c("Type of crash" = "Type d'accident"),
    c("Focus on green alleys visited by our team" = 
        "Focus sur les allées vertes visitées par notre équipe"),
    c("Type of green space" = "Type d'espace vert"),
    c("Back to the map" = "Retour à la carte"),
    c("Maps" = "Cartes"),
    c("Housing" = "Logement"),
    c("Housing system" = "Le système de logement"),
    c("Gentrification" = "Gentrification"),
    c("Permits" = "Permis"),
    c("Marketed Sustainability" = "Durabilité commercialisée"),
    c("Urban Life" = "Vie urbaine"),
    c("Active living potential" = "Potentiel de vie active"),
    c("Green alleys" = "Allées vertes"),
    c("Green spaces" = "Espaces verts"),
    c("Transport" = "Transport"),
    c("Accessibility" = "Accessibilité"),
    c("Road safety" = "Sécurité routière"),
    c("Climate" = "Climat"),
    c("Climate risk" = "Risque climatique"),
    c("Covid" = "Covid"),
    c("Covid interventions" = "Mesures Covid"),
    c("Policy" = "Politiques"),
    c("Montréal climate plans" = "Plans climatiques de Montréal"),
    c("More" = "Davantage"),
    c("Montréal stories" = "Histoires de Montréal"),
    c("Place explorer" = "Explorez un lieu"),
    c("About" = "À propos"),
    c("Terms & Conditions" = "Termes et conditions"),
    c("Privacy Policy" = "Politique de confidentialité"),
    c("Contact" = "Contactez-nous"),
    c("Bookmark" = "Signet"),
    c("Data explanation and export" = "Explication et export des données"),
    c("Generate a report" = "Générez un rapport"),
    # Covid legend (mapdeck's tooltip doesn't deal with accents)
    c("Active and safe lane circuit" = "Circuit des voies actives \net securitaires"),
    c("Expanded pedestrian corridor" = "Corridor pieton elargi"),
    c("Projected corridor" = "Corridor projete"),
    c("Framed queue" = "File d'attente encadree"),
    c("Street partially closed" = "Rue partiellement fermee"),
    c("Family and active street" = "Rue familiale et active"),
    c("Closed street" = "Rue fermee"),
    c("Local circulation" = "Circulation locale"),
    c("Shared street" = "Rue partagee"),
    c("May 2020" = "Mai 2020"),
    c("July 2020" = "Juillet 2020"),
    c("October 2020" = "Octobre 2020"))

ui_and_misc_translated <- 
  tibble(en = names(ui_and_misc_translated),
         fr = ui_and_misc_translated)


# "Translate" names -------------------------------------------------------

ui_and_misc_translated <- 
  bind_rows(ui_and_misc_translated,
            tibble(en = borough$name,
                   fr = borough$name)
  )

# Save --------------------------------------------------------------------

Encoding(ui_and_misc_translated$en) <- "UTF-8"
Encoding(ui_and_misc_translated$fr) <- "UTF-8"
write_csv(ui_and_misc_translated, 
          file = "dev/translation/csv/ui_and_misc_translated.csv")
