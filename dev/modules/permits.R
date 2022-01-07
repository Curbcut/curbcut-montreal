#### Permits data setup ###################################################

# This script relies on objects created in dev/build_geometries.R

# Import and clean permits and uef data ----------------------------------

permits <-
  read_sf("dev/data/permis-construction/permis-construction.shp") |> 
  st_transform(4326) |> 
  st_set_agr("constant") |> 
  select(-c(longitude:loc_y)) |> 
  set_names(c("no_demande", "id", "start_date", "issued_date",
              "address", "borough", "type", "description", "category1",
              "category2", "text", "nb_dwellings", "geometry")) |> 
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"),
         issued_date = as.Date(issued_date, format = "%Y-%m-%d")) |> 
  filter(!st_is_empty(geometry))

# Clean the description and categories field for consistent naming -------

permits <-
  permits |> 
  mutate(description = case_when(str_detect(description, "Construc|construc|CONSTRUC|Nouveau bâtiment|Mur de soutènement") ~ "Construction",
                                 str_detect(description, "Transfo|transfo|Transf|MO PRAM Decarie Modif Int/ Ext|A1:Taux implantation modifié|Mise aux normes|A2:Taux implantation inchangé|MO Modif Int & Ext|AI - Aménagement intérieur|Permis d'améliorations|RP- réparation|MI PRAM Decarie Modif Int") ~ "Transformation",
                                 str_detect(description, "DÉMO|Démo|démo|DEMO|Z - Déménag Bat Princ HP") ~ "Demolition",
                                 str_detect(description, "AGRANDISSEMENT|Agrandissement|agrandissement") ~ "Agrandissement",
                                 str_detect(description, "Abattage|abattage") ~ "Abattage",
                                 str_detect(description, "Dégarnissage intérieur|Travaux intérieurs|MI Modifications Intérieures|Réaménagement de local|Rénovation") ~ "Renovation interieures",
                                 str_detect(description, "ML Modif LOGEMENT ADD SS|Lotissement|LS - Logement sous-sol|Nouveau logement") ~ "Ajout/Combination logement",
                                 str_detect(description, "Piscine|piscine|PISCINE|spa|Spa") ~ "Piscine et spa",
                                 str_detect(description, "Affichage|Enseigne|enseigne|affichage|ENSEIGNE|ET Etalage Extérieur|Z - Panneau Réclame HP") ~ "Affichage",
                                 str_detect(description, "Garage|garage|abri|Abri|Cabanon|CABANON|cabanon|Solarium|Batiment Accessoire|Z - Déménag Dépendance HP|EM- Embarcadère|ACCESSOIRE|accessoire|Bâtiment temporaire|Temporaire|remorque|Remise|remise|BÂTIMENTS ACCESSOIRES|EA Equipement Accessoire") ~ "Garage/Access/Bat. Temporaire",
                                 str_detect(description, "Vente|Foire|vente|Foire|promo|Sollicitation|Place de marché") ~ "Permis vente",
                                 str_detect(description, "Chat|chat|Chien|chien|CHATS") ~ "Permis animaux",
                                 str_detect(description, "Clôture|Cloture|cloture|clôture|CLÔTURE|muret|pieux|Occupation permanente") ~ "Cloture/Muret",
                                 str_detect(description, "Stationnement|stationnement|Fond compensation stat.") ~ "Stationnement",
                                 str_detect(description, "Antenne|antenne|Satellite|satellite|ANTENNE") ~ "Antenne/Satellite",
                                 str_detect(description, "Terrasse|terrasse") ~ "Terrasse",
                                 str_detect(description, "TR- transport de bâtiments|DB Déplacement Batiment") ~ "Transport batiment",
                                 str_detect(description, "Décontaminer|Décontamination|Decontamination|DÉCONTAMINATION") ~ "Decontamination",
                                 str_detect(description, "Renouvellement|Certificat d'autorisation|Autres Certificats|Certificat Autre|Droit acquis") ~ "Certificats et renouvellement",
                                 str_detect(description, "Domaine Public|domaine public|dom. pub|Règl. Art. 89 Charte|T6: ATT. EN MODIFICATION|Recherche de plans|Etude préliminaire") ~ "Domaine public",
                                 str_detect(description, "Drain|Égout|Aqueduc|RACCORD|Raccord|raccord|Plomberie|Sanitaire|sanitaire") ~ "Plomberie/Raccord/Sanitaire",
                                 str_detect(description, "Boites de Dons|Boite de Dons|CC - Conteneur de cueillettes|Boites de dons|Conteneur de dons|Boîte récupération tissus|Boites de dons|Boîte de dons") ~ "Boites de dons",
                                 str_detect(description, "adresse civique|Numérotage|Adresse civ|Numéro Civique|numéro civique|Numéro civique") ~ "Adresse civique ~ demande",
                                 str_detect(description, "RÉSERVOIRS|Réservoir|réservoir|PU- puits") ~ "Reservoir",
                                 str_detect(description, "CA CERTIFICATS|Z-Autre - Const Accessoire HP") ~ "Thermopompe/Gaz",
                                 str_detect(description, "Art mural") ~ "Art mural",
                                 str_detect(description, "Entrée charretière|Entrée Charretière|Entrée de service|Aire de chargement") ~ "Entrees",
                                 str_detect(description, "Excavation|remblai|Remblais|Remblai") ~ "Excavation/Remblais",
                                 str_detect(description, "Renaturalisation|renaturalisation|Stabilisation de rive|bande riveraine") ~ "Renaturalisation",
                                 str_detect(description, "pesticide") ~ "Pesticide",
                                 str_detect(description, "Occupation temporaire|Occupation périodique|ce type de permis n'est plus v") ~ "Occupation temporaire",
                                 str_detect(description, "PPCMOI|Changement d'usage|Dérogation copropriété divise|Dérogation mineure|Usage conditionnel") ~ "Derogations",
                                 str_detect(description, "sinistre|incendie") ~ "Reparations post sinistre",
                                 str_detect(description, "AP Aménagement Paysager|aménagement paysager|Modification terrain|Aménagement du terrain|Travaux extérieurs|ME Modifications Extérieures|ME PRAM Decarie Modif Ext|MA Modif Ext - Acc: Resid|MA Modif Ext - Acc: Comm Ind|Aménagement de terrain|ME PRAM Poirier Modif Ext|Z-Autre - Amélioration HP") ~ "Travaux exterieurs",
                                 str_detect(description, "Appareil mécanique|EQ Equip Elect/Méc: Resid|Équipement mécanique|APPAREILS MÉCANIQUE|TG - Travaux de génie|EQ Equip Elect/Méc: Comm Ind") ~ "Mecanique/Electrique"),
         category1 = case_when(str_detect(category1, "Commercial|Commerce|commercial|COMMERCE") ~ "Commercial",
                               str_detect(category1, "Résidentiel - Mixte|Résidentiel-Mixte|Usage Multiple") ~ "Mixte",
                               str_detect(category1, "Industriel|INDUSTRIE|Industrie|industriel") ~ "Industriel",
                               str_detect(category1, "PUBLIC|Public|public|publique") ~ "Public",
                               str_detect(category1, "Résidentiel|Habitation|habitation|HABITATION|bâtiment accessoire") ~ "Residentiel",
                               str_detect(category1, "Institutionnel|collectif") ~ "Institutionnel",
                               str_detect(category1, "Autre|agricole|Non") ~ "Autre",
                               str_detect(category1, "Parc|PARC") ~ "Parcs et espaces verts",
                               str_detect(category1, "VACANT|Vacant") ~ "Terrain vacant"),
         category2 = case_when(str_detect(category2, "4 à 8 logements|4 Logements|Multifamilial|(4logs)|(5à11logs)|multifamilial|quadruplex|Multi|5 à 11 Logements|12 Logements +|8 à 12 Logements|Condominium|Condos|36 logements et plus|12 à 36 logements") ~ "Multifamilial",
                               str_detect(category2, "Commercial|Commer léger|commercial|Commerce|commerce|Bureaux|Bureau|bureau|commerciaux|C1 Léger|prohibés") ~ "Commercial",
                               str_detect(category2, "1 Logement|Unifamilial|unifamilial|Bungalow|Cottage") ~ "Unifamilial",
                               str_detect(category2, "Garage|accessoire|garage|Remise|Abri|Dépendance") ~ "Garage/Bat. Access.",
                               str_detect(category2, "Bifamilial|Trifamilial|Trif|Bif|2 Logements|Jumelée|3 Logements|trifamilial|(3logs)|Duplex|Triplex|Unif. avec log. au sous-sol") ~ "Bi- et trifamilial",
                               str_detect(category2, "Culte|culte") ~ "Culte",
                               str_detect(category2, "pétroliers|industrie|Industrie|lourd|Manufactu|exploitation|I6 Primaire et de récupération") ~ "Industriel",
                               str_detect(category2, "communautaire|Communautaire|civil") ~ "Communautaire",
                               str_detect(category2, "École|Université|Enseignement|Collegial|Collégial|enseignement|Garderie") ~ "Scolaire et garderie",
                               str_detect(category2, "gouvernemental|gouv|municipal|Gouv") ~ "Gouvernemental",
                               str_detect(category2, "Institu|institu|Administration") ~ "Institutionnel",
                               str_detect(category2, "Personnes agées|retraite|résidence|personnes âgées") ~ "Retraite",
                               str_detect(category2, "Mixte|mixte|multiple|Résid. & Comm.") ~ "Mixte",
                               str_detect(category2, "Parc|parc|Berge|Golf|conservation|Conservation") ~ "Parcs et espaces verts",
                               str_detect(category2, "Vacant|vacant|VACANT") ~ "Terrains et lots vacants",
                               str_detect(category2, "publics|public|loisir|récréa|Récréation|sportif|récréation|PUBLIC|Publique") ~ "Public et recreatif",
                               str_detect(category2, "alcool|Restauration|restauration") ~ "Restaurants et bars",
                               str_detect(category2, "Agricole|agriculture") ~ "Agricole",
                               str_detect(category2, "hébergement|chambre|d'héb") ~ "Centres hebergement",
                               str_detect(category2, "Batiments en hauteur >6 étages|Résidentiel|Habitation collective|chalet|Residentiel|Familiale|Maison mobile") ~ "Residentiel, general",
                               str_detect(category2, "Santé|hospitalier") ~ "Sante et hopitaux",
                               str_detect(category2, "Permis ancien système") ~ "Permis expires",
                               str_detect(category2, "Cour de triage|Structure/Fondations") ~ "Infrastructures",
                               str_detect(category2, "I1 Recherche et développement|Entrepreneur général|conservation (p4)|artériel léger|Utilité légère|Catégorisé|Atelier spécialisé|Voisinage|Quartier|Légère|Prestige|Lourde|artériel léger (c3)|Utilité légère (u1)|Collective|À VENIR") ~ "Transformations et renovations",
                               str_detect(category2, "Fabrication et assemblage|Urbain|Non catégorisé|Ateliers") ~ "Construction et demolition",
                               str_detect(category2, "Piscine|paysager|contraignant|habitation collective") ~ "Amenagements exterieurs",
                               str_detect(category2, "détail|Vente|services|Services") ~ "Services et commerce au detail",
                               str_detect(category2, "Carburant|essence|Pétrolier|Transport|véhicule|Hotel|Véhicule|station-service|Stationnnement|Stationnement|Recherche et développement") ~ "Transport et auxiliaires"))


# Clean the text field so it is all lowercase and without accents ---------


permits <- 
  permits |> 
  mutate(text = stringi::stri_trans_general(str_to_lower(text), "Latin-ASCII"))


# Select only condo conversions -------------------------------------------

condo_conversions <- 
  permits |> 
  mutate(conversion = case_when(str_detect(text, "transfo") & str_detect(text, "copropri") ~ TRUE,
                                str_detect(text, "conver") & str_detect(text, "copropri") ~ TRUE,
                                str_detect(text, "transfo") & str_detect(text, "condo") ~ TRUE,
                                str_detect(text, "ream") & str_detect(text, "condo") ~ TRUE,
                                TRUE ~ FALSE)) |> 
  filter(conversion) |>
  mutate(issued_date = lubridate::year(issued_date))



# Select permits for combining of two units into one ----------------------


combined_dwellings <- 
  permits |> 
  filter(nb_dwellings < 0) |> 
  filter(category1 == "Residentiel" | category1 == "Mixte") |> 
  mutate(combining = case_when(str_detect(text, "transfo") ~ TRUE, 
                               str_detect(text, "reuni") & str_detect(text, "redu") ~ TRUE,
                               str_detect(text, "ream") & str_detect(text, "redu") ~ TRUE,
                               str_detect(text, "redu") & str_detect(text, "log") ~ TRUE,
                               str_detect(text, "ream") & str_detect(text, "redu") ~ TRUE,
                               str_detect(text, "loge") & str_detect(text, "conver") ~ TRUE,
                               str_detect(text, "modif") & str_detect(text, "suppression") & str_detect(text, "civique") ~ TRUE,
                               str_detect(text, "loge") & str_detect(text, "diminuer") ~ TRUE,
                               str_detect(text, "ream") & str_detect(text, "typologie") ~ TRUE,
                               str_detect(text, "ream") & str_detect(text, "unifamilial") ~ TRUE,
                               str_detect(text, "amenager") & str_detect(text, "plex") & str_detect(text, "unifamilial") ~ TRUE,
                               str_detect(text, "amenager") & str_detect(text, "unifamilial") ~ TRUE,
                               str_detect(text, "enlever") & str_detect(text, "loge") ~ TRUE,
                               str_detect(text, "jumeler") ~ TRUE,
                               str_detect(text, "fusion") ~ TRUE,
                               str_detect(text, "transf") & str_detect(text, "log") ~ TRUE,
                               str_detect(text, "transf") & str_detect(text, "commerce") ~ TRUE,
                               str_detect(text, "transf") & str_detect(text, "garderie") ~ TRUE,
                               str_detect(text, "transf") & str_detect(text, "hotel") ~ TRUE, 
                               str_detect(text, "transf") & str_detect(text, "bureau") ~ TRUE,
                               TRUE ~ FALSE)) |> 
  filter(combining) |>
  mutate(issued_date = lubridate::year(issued_date))



# Demolition permits ------------------------------------------------------

demolitions <- 
  permits |> 
  filter(type == "DE") |> 
  filter(!str_detect(text, "hangar")) |> 
  filter(!str_detect(text, "piscine") & !str_detect(text, "demolir")) |> 
  filter(!str_detect(text, "garage") & !str_detect(text, "demo")) |> 
  filter(category2 != "Garage/Bat. Access.") |> 
  mutate(issued_date = lubridate::year(issued_date))



# Renovation permits ------------------------------------------------------

renovations <- 
  permits |> 
  filter(type == "TR") |> 
  filter(category1 == "Residentiel" | category1 == "Mixte") |> 
  filter(description != "Abattage") |>
  filter(!str_detect(text, "fissure") & !str_detect(text, "fondation")) |> 
  filter(!str_detect(text, "remplac") & !str_detect(text, "drain")) |> 
  filter(!str_detect(text, "fissure") & !str_detect(text, "fondation")) |> 
  filter(!str_detect(text, "pieux") & !str_detect(text, "structure")) |> 
  mutate(issued_date = lubridate::year(issued_date))

# New construction, uef data ----------------------------------------------

uef_geom <-
  read_sf("dev/data/uniteevaluationfonciere/uniteevaluationfonciere.shp") |>
  st_transform(4326) |> 
  st_set_agr("constant") |> 
  st_centroid()

uef <- 
  uef_geom |>
  st_drop_geometry() |> 
  filter(!is.na(NOMBRE_LOG), NOMBRE_LOG > 0, between(ANNEE_CONS, 1990, 2020)) |> 
  group_by(CIVIQUE_DE, CIVIQUE_FI, NOM_RUE, ANNEE_CONS) |> 
  summarise(number_dwellings = sum(NOMBRE_LOG, na.rm = TRUE), .groups = "drop")

new_construction <- 
  uef_geom |> 
  filter(!is.na(NOMBRE_LOG), NOMBRE_LOG > 0, between(ANNEE_CONS, 1990, 2020)) |> 
  group_by(CIVIQUE_DE, CIVIQUE_FI, NOM_RUE, ANNEE_CONS) |> 
  # For all the group above formed, there is a unique point. Retrieving the 
  # unique point rather than a MULTIPOINT geometry of 20x the same point.
  slice(1) |> 
  select(CIVIQUE_DE, CIVIQUE_FI, NOM_RUE, ANNEE_CONS) |> 
  left_join(uef, by = c("CIVIQUE_DE", "CIVIQUE_FI", "NOM_RUE", "ANNEE_CONS")) |> 
  st_transform(4326) |> 
  st_set_agr("constant")


# Combine five categories into one ----------------------------------------


condo_conversions <- 
  condo_conversions |> 
  mutate(type = "conversion") |> 
  select(id, issued_date, nb_dwellings, type) |> 
  set_names(c("id", "year", "nb_dwellings", "type", "geometry"))

renovations <- 
  renovations |> 
  mutate(type = "renovation") |> 
  select(id, issued_date, nb_dwellings, type) |> 
  set_names(c("id", "year", "nb_dwellings", "type", "geometry"))

demolitions <- 
  demolitions |> 
  mutate(type = "demolition") |> 
  select(id, issued_date, nb_dwellings, type) |> 
  set_names(c("id", "year", "nb_dwellings", "type", "geometry"))

combined_dwellings <- 
  combined_dwellings |> 
  mutate(type = "combination") |> 
  select(id, issued_date, nb_dwellings, type) |> 
  set_names(c("id", "year", "nb_dwellings", "type", "geometry"))

new_construction <- 
  new_construction |> 
  tibble::rowid_to_column(var = "id") |> 
  mutate(id = paste0("uef", id, collapse = "_")) |> 
  mutate(type = "new_construction") |> 
  select(id, ANNEE_CONS, number_dwellings, type) |> 
  rename(year = ANNEE_CONS,
         nb_dwellings = number_dwellings)

permits <- 
  rbind(condo_conversions, combined_dwellings,
        demolitions, renovations, new_construction)


# Add to existing geographies ---------------------------------------------

process_permits <- function(x) {
  
  island_csduid <- c("2466007", "2466023_1",  "2466023_10", "2466023_11",
                     "2466023_12", "2466023_13", "2466023_14", "2466023_15", 
                     "2466023_16", "2466023_17", "2466023_18", "2466023_19",
                     "2466023_2", "2466023_3", "2466023_4", "2466023_5",  
                     "2466023_6", "2466023_7", "2466023_8", "2466023_9",
                     "2466032", "2466047", "2466058", "2466062", "2466087", 
                     "2466092", "2466097", "2466102", "2466107", "2466112",
                     "2466117", "2466127", "2466142", "2466072", "2466023")
  
  y <- 
    x |> 
    select(ID) |> 
    st_join(permits) |> 
    st_drop_geometry() |> 
    count(ID, year, type) |> 
    filter(!is.na(year), !is.na(type)) |> 
    group_by(ID, year) |> 
    summarize(type = c(type, "total"), n = c(n, sum(n, na.rm = TRUE)),
              .groups = "drop") |> 
    tidyr::pivot_wider(id_cols = "ID",
                       names_from = c("type", "year"), 
                       names_prefix = "permits_",
                       names_sep = "_", 
                       values_from = n) |> 
    (\(z) select(z, sort(names(z))))() |> 
    # Make sure that a missing geometry shows up as 0. Missing means no permits.
    full_join(select(x, any_of(c("ID", "CSDUID")), population), by = "ID") |> 
    filter(!is.na(ID)) %>%
    {if (nrow(.) == nrow(borough))
      filter(., ID %in% island_csduid)
      else filter(., CSDUID %in% island_csduid)} |>
    arrange(ID) |> 
    st_as_sf() |> 
    rename_with(~paste0(., "_count"), starts_with("permits")) |> 
    mutate(across(starts_with("permits"), 
                  .fns = list(
                    sqkm = ~{1000000 * .x / 
                        units::drop_units(st_area(geometry))},
                    per1k = ~{1000 * .x / population}),
                  .names = "{str_remove(.col, '_count')}_{.fn}"), 
           .before = geometry) |> 
    mutate(across(starts_with("permits"), ~replace(., is.na(.), 0))) |>
    mutate(across(starts_with("permits"), ~replace(., is.infinite(.), 0))) |>
    rename_with(~paste0(str_remove(., "_\\d{4}"),
                        str_extract(., "_\\d{4}")), starts_with("permits")) |>
    select(-population, -any_of(c("CSDUID"))) |>
    st_drop_geometry() |> 
    full_join(select(st_drop_geometry(x), "ID"), by = "ID")
  
  # Make sure the order isn't lost, so we can cbind in m_permits.R
  y[match(x$ID, y$ID),] |> 
    select(-ID)
}


permits_results <- map(list("borough" = borough, "CT" = CT, "DA" = DA, 
                            "grid" = grid), process_permits)


# Add breaks --------------------------------------------------------------

permits_results <- map(permits_results, add_q3)
permits_q3 <- map(permits_results, get_breaks_q3)
permits_q5 <- map(permits_results, get_breaks_q5)
permits_choropleth <- map2(permits_results, permits_q5, ~bind_cols(.x, add_q5(.x, .y)))


# Data testing ------------------------------------------------------------

# Doesn't make sense to look at year difference, when numbers might be so low
# ex. permits_choropleth$borough |> 
#     select(contains("permits_conversion_count") & contains(c("1992", "1993"))
# 1 in 1992, and 3 in 1993.
data_testing(permits_choropleth, ignore_year_diff = TRUE)


# Join to geometries ------------------------------------------------------

# In this case, we join geometries only once the module is activated.


# Add additional fields to permits ------------------------------------------

permits <-
  permits |>
  arrange(year) |>
  mutate(fill = case_when(type == "combination" ~ "#008533EE",
                          type == "conversion" ~ "#F59600EE",
                          type == "demolition" ~ "#7C0082EE",
                          type == "new_construction" ~ "#992400EE",
                          type == "renovation" ~ "#0E6399EE")) |> 
  relocate(geometry, .after = last_col())


# Meta testing ------------------------------------------------------------

meta_testing()


# Add to variables table --------------------------------------------------

var_list <- 
  permits_choropleth |> 
  map(~names(select(.x, -ID, -contains(c("q3", "q5"))))) |> 
  unlist() |> 
  unique()

var_list_no_dates <- str_remove(var_list, "_\\d{4}$") |> unique()

# Get breaks_q3
breaks_q3_active <-
  map2(set_names(var_list), str_extract(var_list, "\\d{4}$"),  function(var_name, year) {
    map2_dfr(permits_q3, names(permits_choropleth), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, date = year, rank = 0:3,
                                   .before = everything())}) |> 
      select(scale, date, rank, var = all_of(var_name))})

names(breaks_q3_active) <- str_remove(names(breaks_q3_active), "_\\d{4}$")

breaks_q3_active <-
  map(set_names(var_list_no_dates), ~{
    breaks_q3_active[names(breaks_q3_active) == .x] |> 
      reduce(rbind)
  })

# Get breaks_q5
breaks_q5_active <-
  map2(set_names(var_list), str_extract(var_list, "\\d{4}$"),  function(var_name, year) {
    map2_dfr(permits_q5, names(permits_choropleth), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, date = year, rank = 0:5,
                                   .before = everything())}) |> 
      select(scale, date, rank, var = all_of(var_name))})

names(breaks_q5_active) <- str_remove(names(breaks_q5_active), "_\\d{4}$")

breaks_q5_active <- 
  map(set_names(var_list_no_dates), ~{
    breaks_q5_active[names(breaks_q5_active) == .x] |> 
      reduce(rbind)
  })


# Add variable explanations -----------------------------------------------

# construct green space variables table
permits_table <- 
  map_dfr(unique(str_remove(var_list, "_\\d{4}$")), ~{
    type <- case_when(str_detect(.x, "combination") ~ "Dwellings combination permits",
                      str_detect(.x, "conversion") ~ "Condo conversion permits",
                      str_detect(.x, "demolition") ~ "Demolition permits",
                      str_detect(.x, "new_construction") ~ "New construction permits",
                      str_detect(.x, "renovation") ~ "Renovation permits",
                      str_detect(.x, "total") ~ "Total permits")
    
    group <- 
      if (str_detect(.x, "sqkm")) {
        "per sq km"
      } else if (str_detect(.x, "per1k")) {
        "per 1,000"
      } else "count"
    
    tibble(
      var_code = .x,
      var_title = paste(type, group),
      var_short = str_remove_all(paste(type, group), "per |count|Dwellings | permits") |> 
        str_replace("sq km", "sqkm") |> 
        (\(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))(),
      explanation = paste("the number of", 
                          str_to_lower(type), "emitted",
                          str_replace(group, "sq km", "square kilometre") |> 
                            str_replace("1,000", "1,000 residents") |> 
                            str_remove("count")),
      category = NA,
      private = FALSE,
      dates = list(as.character(1990:2021)),
      scales = list(c("borough", "building", "CT", "DA", "grid")),
      breaks_q3 = list(breaks_q3_active[[.x]]),
      breaks_q5 = list(breaks_q5_active[[.x]]),
      source = "VdM")
    
  }) |> 
  mutate(explanation = ifelse(str_detect(explanation, "new construction"), 
                              str_replace(explanation, "the number of new construction permits emitted",
                                          "the number of owner and renter residential buildings built"), 
                              explanation))

# Join green space variable table to variables table
variables <-
  variables |>
  bind_rows(permits_table)


# To save output, run dev/build_geometries.R, which calls this script
