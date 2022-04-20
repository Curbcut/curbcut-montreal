# #### `Variables` preparation for translation ###################################

variables_translated <- 
  tibble(en = character(), fr = character()) |>
  add_row(en = paste0("Tenant-occupied (%)"), 
          fr = paste0("Occupé par un locataire (%)")) |> 
  add_row(en = paste0("Average rent ($)"), 
          fr = paste0("Loyer moyen ($)")) |> 
  add_row(en = paste0("Housing requiring major repairs (%)"), 
          fr = paste0("Logement nécessitant des réparations majeures (%)")) |> 
  add_row(en = paste0("Average property value ($)"), 
          fr = paste0("Valeur moyenne des logements ($)")) |> 
  add_row(en = paste0("One-year housing mobility (%)"), 
          fr = paste0("Mobilité résidentielle au cours de la dernière année (%",
                      ")")) |> 
  add_row(en = paste0("Five-year housing mobility (%)"), 
          fr = paste0("Mobilité résidentielle au cours des cinq dernières anné",
                      "es (%)")) |> 
  add_row(en = paste0("Single-detached (%)"), 
          fr = paste0("Maison individuelle non attenantes (%)")) |> 
  add_row(en = paste0("Median household income ($)"), 
          fr = paste0("Revenu médian des ménages ($)")) |> 
  add_row(en = paste0("Income under $50k (%)"), 
          fr = paste0("Revenu inférieur à 50k (%)")) |> 
  add_row(en = paste0("Income between $50k-$100k (%)"), 
          fr = paste0("Revenu entre 50k-100k (%)")) |> 
  add_row(en = paste0("Income above $100k (%)"), 
          fr = paste0("Revenu supérieur à 100k (%)")) |> 
  add_row(en = paste0("Immigrants (%)"), 
          fr = paste0("Immigrants (%)")) |> 
  add_row(en = paste0("New immigrants (%)"), 
          fr = paste0("Nouveaux immigrants (%)")) |> 
  add_row(en = paste0("Visible minorities (%)"), 
          fr = paste0("Minorités visibles (%)")) |> 
  add_row(en = paste0("Aboriginal (%)"), 
          fr = paste0("Autochtone (%)")) |> 
  add_row(en = paste0("Drive to work (%)"), 
          fr = paste0("Se rend au travail en voiture (%)")) |> 
  add_row(en = paste0("Walk or cycle to work (%)"), 
          fr = paste0("Se rend au travail à pied ou à vélo (%)")) |> 
  add_row(en = paste0("Public transit to work (%)"), 
          fr = paste0("Se rend au travail en transport en commun (%)")) |> 
  add_row(en = paste0("Living alone"), 
          fr = paste0("Habite seul")) |> 
  add_row(en = paste0("French only (%)"), 
          fr = paste0("Français seulement (%)")) |> 
  add_row(en = paste0("English only (%)"), 
          fr = paste0("Anglais seulement (%)")) |> 
  add_row(en = paste0("French and English (%)"), 
          fr = paste0("Français et anglais (%)")) |> 
  add_row(en = paste0("Neither French nor English (%)"), 
          fr = paste0("Ni français ni anglais (%)")) |> 
  add_row(en = paste0("Aged between 0 and 14 (%)"), 
          fr = paste0("Âgés de 0 à 14 ans (%)")) |> 
  add_row(en = paste0("Aged between 15 and 64 (%)"), 
          fr = paste0("Âgés de 15 à 64 ans (%)")) |> 
  add_row(en = paste0("Aged 65 and above (%)"), 
          fr = paste0("Âgés de 65 ans et plus (%)")) |> 
  add_row(en = paste0("Bachelor and above (%)"), 
          fr = paste0("Baccalauréat et plus (%)")) |> 
  add_row(en = paste0("No certificate, diploma or degree (%)"), 
          fr = paste0("Aucun certificat, diplôme ou grade (%)")) |> 
  add_row(en = paste0("Managerial and professional occupations (%)"), 
          fr = paste0("Cadres et professions libérales (%)")) |> 
  add_row(en = paste0("Creative occupations (%)"), 
          fr = paste0("Professions de la classe créative (%)")) |> 
  add_row(en = paste0("Families with children (%)"), 
          fr = paste0("Familles avec enfants (%)")) |> 
  add_row(en = paste0("Unsuitable housing (%)"), 
          fr = paste0("Logements inadéquats (%)")) |> 
  add_row(en = paste0("Renter housing stress (%)"), 
          fr = paste0("Locataires dépensant plus de 30% de leur revenu en frai",
                      "s de logement (%)")) |> 
  add_row(en = paste0("Owner housing stress (%)"), 
          fr = paste0("Propriétaires dépensant plus de 30% de leur revenu en f",
                      "rais de logement (%)")) |> 
  add_row(en = paste0("Unaffordable housing (%)"), 
          fr = paste0("Logements inabordables (%)")) |> 
  add_row(en = paste0("Prevalence of low income (after-tax) (%)"), 
          fr = paste0("Prévalence de faible revenu (après impôt) (%)")) |> 
  add_row(en = paste0("Commute under 15 minutes (%)"), 
          fr = paste0("Trajet de moins de 15 minutes (%)")) |> 
  add_row(en = paste0("Commute 15-45 minutes (%)"), 
          fr = paste0("Trajet entre 15 et 45 minutes (%)")) |> 
  add_row(en = paste0("Commute more than 45 minutes (%)"), 
          fr = paste0("Trajet de plus de 45 minutes (%)")) |> 
  add_row(en = paste0("CanALE index"), 
          fr = paste0("Indice AVA-Can")) |> 
  add_row(en = paste0("Drought vulnerability"), 
          fr = paste0("Vulnérabilité aux sécheresses")) |> 
  add_row(en = paste0("Flood vulnerability"), 
          fr = paste0("Vulnérabilité aux crues")) |> 
  add_row(en = paste0("Heavy rain vulnerability"), 
          fr = paste0("Vulnérabilité aux pluies abondantes")) |> 
  add_row(en = paste0("Destructive storm vulnerability"), 
          fr = paste0("Vulnérabilité aux tempêtes destructrices")) |> 
  add_row(en = paste0("Heat wave vulnerability"), 
          fr = paste0("Vulnérabilité aux vagues de chaleur")) |> 
  add_row(en = paste0("Collisions (cyclists)"), 
          fr = paste0("Collisions (cyclistes) ")) |> 
  add_row(en = paste0("Collisions (other)"), 
          fr = paste0("Collisions (autres)")) |> 
  add_row(en = paste0("Collisions (pedestrians)"), 
          fr = paste0("Collisions (piétons)")) |> 
  add_row(en = paste0("Collisions"), 
          fr = paste0("Collisions")) |> 
  add_row(en = paste0("Collisions per 1,000 (cyclists)"), 
          fr = paste0("Collisions pour 1 000 (cyclistes)")) |> 
  add_row(en = paste0("Collisions per 1,000 (other)"), 
          fr = paste0("Collisions pour 1 000 (autres)")) |> 
  add_row(en = paste0("Collisions per 1,000 (pedestrians)"), 
          fr = paste0("Collisions pour 1 000 (piétons)")) |> 
  add_row(en = paste0("Collisions per 1,000"), 
          fr = paste0("Collisions pour 1 000")) |> 
  add_row(en = paste0("Collisions per sq km (cyclists)"), 
          fr = paste0("Collisions par km2 (cyclistes)")) |> 
  add_row(en = paste0("Collisions per sq km (other)"), 
          fr = paste0("Collisions par km2 (autres)")) |> 
  add_row(en = paste0("Collisions per sq km (pedestrians)"), 
          fr = paste0("Collisions par km2 (piétons)")) |> 
  add_row(en = paste0("Collisions per sq km"), 
          fr = paste0("Collisions par km2 ")) |> 
  add_row(en = paste0("Total jobs (weekday peak)"), 
          fr = paste0("Tous les emplois (pointe en semaine)")) |> 
  add_row(en = paste0("Total jobs (weekday off-peak)"), 
          fr = paste0("Tous les emplois (hors pointe en semaine)")) |> 
  add_row(en = paste0("Total jobs (weekday night)"), 
          fr = paste0("Tous les emplois (nuit de semaine)")) |> 
  add_row(en = paste0("Total jobs (weekend peak)"), 
          fr = paste0("Tous les emplois (pointe de fin de semaine)")) |> 
  add_row(en = paste0("Total jobs (weekend off-peak)"), 
          fr = paste0("Tous les emplois (hors pointe de fin de semaine)")) |> 
  add_row(en = paste0("Total jobs (weekend night)"), 
          fr = paste0("Tous les emplois (nuit de fin de semaine)")) |> 
  add_row(en = paste0("Low-skill jobs (weekday peak)"), 
          fr = paste0("Emplois peu qualifiés (pointe en semaine)")) |> 
  add_row(en = paste0("Low-skill jobs (weekday off-peak)"), 
          fr = paste0("Emplois peu qualifiés (hors pointe en semaine)")) |> 
  add_row(en = paste0("Low-skill jobs (weekday night)"), 
          fr = paste0("Emplois peu qualifiés (nuit de semaine)")) |> 
  add_row(en = paste0("Low-skill jobs (weekend peak)"), 
          fr = paste0("Emplois peu qualifiés (pointe de fin de semaine)")) |> 
  add_row(en = paste0("Low-skill jobs (weekend off-peak)"), 
          fr = paste0("Emplois peu qualifiés (hors pointe de fin de semaine)")) |> 
  add_row(en = paste0("Low-skill jobs (weekend night)"), 
          fr = paste0("Emplois peu qualifiés (nuit de fin de semaine)")) |> 
  add_row(en = paste0("High-skill jobs (weekday peak)"), 
          fr = paste0("Emplois hautement qualifiés (pointe en semaine)")) |> 
  add_row(en = paste0("High-skill jobs (weekday off-peak)"), 
          fr = paste0("Emplois hautement qualifiés (hors pointe en semaine)")) |> 
  add_row(en = paste0("High-skill jobs (weekday night)"), 
          fr = paste0("Emplois hautement qualifiés (nuit de semaine)")) |> 
  add_row(en = paste0("High-skill jobs (weekend peak)"), 
          fr = paste0("Emplois hautement qualifiés (pointe de fin de semaine)")) |> 
  add_row(en = paste0("High-skill jobs (weekend off-peak)"), 
          fr = paste0("Emplois hautement qualifiés (hors pointe de fin de sema",
                      "ine)")) |> 
  add_row(en = paste0("High-skill jobs (weekend night)"), 
          fr = paste0("Emplois hautement qualifiés (nuit de fin de semaine)")) |> 
  add_row(en = paste0("Low-income jobs (weekday peak)"), 
          fr = paste0("Emplois à faible revenu (pointe en semaine)")) |> 
  add_row(en = paste0("Low-income jobs (weekday off-peak)"), 
          fr = paste0("Emplois à faible revenu (hors pointe en semaine)")) |> 
  add_row(en = paste0("Low-income jobs (weekday night)"), 
          fr = paste0("Emplois à faible revenu (nuit de semaine)")) |> 
  add_row(en = paste0("Low-income jobs (weekend peak)"), 
          fr = paste0("Emplois à faible revenu (pointe de fin de semaine)")) |> 
  add_row(en = paste0("Low-income jobs (weekend off-peak)"), 
          fr = paste0("Emplois à faible revenu (hors pointe de fin de semaine)",
                      "")) |> 
  add_row(en = paste0("Low-income jobs (weekend night)"), 
          fr = paste0("Emplois à faible revenu (nuit de fin de semaine)")) |> 
  add_row(en = paste0("Schools (weekday peak)"), 
          fr = paste0("Écoles (pointe en semaine)")) |> 
  add_row(en = paste0("Schools (weekday off-peak)"), 
          fr = paste0("Écoles (hors pointe en semaine)")) |> 
  add_row(en = paste0("Schools (weekday night)"), 
          fr = paste0("Écoles (nuit de semaine)")) |> 
  add_row(en = paste0("Schools (weekend peak)"), 
          fr = paste0("Écoles (pointe de fin de semaine)")) |> 
  add_row(en = paste0("Schools (weekend off-peak)"), 
          fr = paste0("Écoles (hors pointe de fin de semaine)")) |> 
  add_row(en = paste0("Schools (weekend night)"), 
          fr = paste0("Écoles (nuit de fin de semaine)")) |> 
  add_row(en = paste0("Healthcare (weekday peak)"), 
          fr = paste0("Établissements de soins de santé (pointe en semaine)")) |> 
  add_row(en = paste0("Healthcare (weekday off-peak)"), 
          fr = paste0("Établissements de soins de santé  (hors pointe en semai",
                      "ne)")) |> 
  add_row(en = paste0("Healthcare (weekday night)"), 
          fr = paste0("Établissements de soins de santé  (nuit de semaine)")) |> 
  add_row(en = paste0("Healthcare (weekend peak)"), 
          fr = paste0("Établissements de soins de santé  (pointe de fin de sem",
                      "aine)")) |> 
  add_row(en = paste0("Healthcare (weekend off-peak)"), 
          fr = paste0("Établissements de soins de santé  (hors pointe de fin d",
                      "e semaine)")) |> 
  add_row(en = paste0("Healthcare (weekend night)"), 
          fr = paste0("Établissements de soins de santé  (nuit de fin de semai",
                      "ne)")) |> 
  add_row(en = paste0("Green alleys per sq km"), 
          fr = paste0("Ruelles vertes par km2")) |> 
  add_row(en = paste0("Green alleys per 1,000"), 
          fr = paste0("Ruelles vertes pour 1 000")) |> 
  add_row(en = paste0("Borough park per sq km"), 
          fr = paste0("Parcs d'arrondissements par km2")) |> 
  add_row(en = paste0("Borough park per 1,000"), 
          fr = paste0("Parcs d'arrondissements pour 1 000")) |> 
  add_row(en = paste0("Large park per sq km"), 
          fr = paste0("Grands parcs par km2")) |> 
  add_row(en = paste0("Large park per 1,000"), 
          fr = paste0("Grands parcs pour 1 000")) |> 
  add_row(en = paste0("Other park per sq km"), 
          fr = paste0("Autres parcs par km2")) |> 
  add_row(en = paste0("Other park per 1,000"), 
          fr = paste0("Autres parcs pour 1 000")) |> 
  add_row(en = paste0("Road space per sq km"), 
          fr = paste0("Espace routier par km2")) |> 
  add_row(en = paste0("Road space per 1,000"), 
          fr = paste0("Espace routier pour 1 000")) |> 
  add_row(en = paste0("Under validation per sq km"), 
          fr = paste0("En cours de validation par km2")) |> 
  add_row(en = paste0("Under validation per 1,000"), 
          fr = paste0("En cours de validation pour 1 000")) |> 
  add_row(en = paste0("Total green space per sq km"), 
          fr = paste0("Total d'espaces verts par km2")) |> 
  add_row(en = paste0("Total green space per 1,000"), 
          fr = paste0("Total d'espaces verts pour 1 000")) |> 
  add_row(en = paste0("Gentrification index"), 
          fr = paste0("Indice de gentrification")) |> 
  add_row(en = paste0("Dwellings combination permits count"), 
          fr = NA) |> 
  add_row(en = paste0("Condo conversion permits count"), 
          fr = NA) |> 
  add_row(en = paste0("Demolition permits count"), 
          fr = NA) |> 
  add_row(en = paste0("New construction permits count"), 
          fr = NA) |> 
  add_row(en = paste0("Renovation permits count"), 
          fr = NA) |> 
  add_row(en = paste0("Total permits count"), 
          fr = NA) |> 
  add_row(en = paste0("Dwellings combination permits per sq km"), 
          fr = NA) |> 
  add_row(en = paste0("Dwellings combination permits per 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Condo conversion permits per sq km"), 
          fr = NA) |> 
  add_row(en = paste0("Condo conversion permits per 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Demolition permits per sq km"), 
          fr = NA) |> 
  add_row(en = paste0("Demolition permits per 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("New construction permits per sq km"), 
          fr = NA) |> 
  add_row(en = paste0("New construction permits per 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Renovation permits per sq km"), 
          fr = NA) |> 
  add_row(en = paste0("Renovation permits per 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Total permits per sq km"), 
          fr = NA) |> 
  add_row(en = paste0("Total permits per 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Tenant"), 
          fr = paste0("Locataire")) |> 
  add_row(en = paste0("Avg. rent"), 
          fr = paste0("Loyer moy.")) |> 
  add_row(en = paste0("Repairs"), 
          fr = paste0("Réparations")) |> 
  add_row(en = paste0("Avg. value"), 
          fr = paste0("Valeur moy.")) |> 
  add_row(en = paste0("1-year mob."), 
          fr = paste0("Mob. rés. 1 an")) |> 
  add_row(en = paste0("5-year mob."), 
          fr = paste0("Mob. rés. 5 ans")) |> 
  add_row(en = paste0("Single-detached"), 
          fr = paste0("Maison ind.")) |> 
  add_row(en = paste0("Med. inc."), 
          fr = paste0("Rev. méd.")) |> 
  add_row(en = paste0("Inc. <$50k"), 
          fr = paste0("Rev. <50k$")) |> 
  add_row(en = paste0("Inc. $50-100k"), 
          fr = paste0("Rev. <50k-100k$")) |> 
  add_row(en = paste0("Inc. >$100k"), 
          fr = paste0("Rev. >100k$")) |> 
  add_row(en = paste0("Immigrants"), 
          fr = paste0("Immigrants")) |> 
  add_row(en = paste0("New immigrants"), 
          fr = paste0("Nouv. immigrants")) |> 
  add_row(en = paste0("Vis. minorities"), 
          fr = paste0("Minorités vis.")) |> 
  add_row(en = paste0("Aboriginal"), 
          fr = paste0("Autochtone")) |> 
  add_row(en = paste0("Drive"), 
          fr = paste0("Conduit")) |> 
  add_row(en = paste0("Walk or cycle"), 
          fr = paste0("Pied ou vélo")) |> 
  add_row(en = paste0("Transit"), 
          fr = paste0("Transp. commun")) |> 
  add_row(en = paste0("TKTK"), 
          fr = NA) |> 
  add_row(en = paste0("Fr. only"), 
          fr = paste0("Fr. seul.")) |> 
  add_row(en = paste0("Eng. only"), 
          fr = paste0("Ang. seul.")) |> 
  add_row(en = paste0("Fr. and Eng."), 
          fr = paste0("Fr. et ang.")) |> 
  add_row(en = paste0("Non-official"), 
          fr = paste0("Non-officiel")) |> 
  add_row(en = paste0("0-14 yo"), 
          fr = paste0("0-14 ans")) |> 
  add_row(en = paste0("15-64 yo"), 
          fr = paste0("15-64 ans")) |> 
  add_row(en = paste0("6+5 yo"), 
          fr = NA) |> 
  add_row(en = paste0("Bachelor+"), 
          fr = paste0("Bac.+")) |> 
  add_row(en = paste0("No degree"), 
          fr = paste0("Aucun diplôme")) |> 
  add_row(en = paste0("Professional"), 
          fr = paste0("Professionel")) |> 
  add_row(en = paste0("Creative"), 
          fr = paste0("Classe créative")) |> 
  add_row(en = paste0("With child"), 
          fr = paste0("Avec enfant")) |> 
  add_row(en = paste0("Unsuitable"), 
          fr = paste0("Inadéquat")) |> 
  add_row(en = paste0("Renter stress"), 
          fr = paste0("Inabordable")) |> 
  add_row(en = paste0("Owner stress"), 
          fr = paste0("Inabordable")) |> 
  add_row(en = paste0("Unaffordable"), 
          fr = paste0("Inabordable")) |> 
  add_row(en = paste0("Low income"), 
          fr = paste0("Faible revenu")) |> 
  add_row(en = paste0("Commute <15m"), 
          fr = paste0("Trajet <15min")) |> 
  add_row(en = paste0("Commute 14-45m"), 
          fr = paste0("Trajet 14-45min")) |> 
  add_row(en = paste0("Commute >45m"), 
          fr = paste0("Trajet >45min")) |> 
  add_row(en = paste0("CanALE"), 
          fr = paste0("AVA-Can")) |> 
  add_row(en = paste0("Drought"), 
          fr = paste0("Sécheresses")) |> 
  add_row(en = paste0("Flood"), 
          fr = paste0("Inondations")) |> 
  add_row(en = paste0("Heavy rain"), 
          fr = paste0("Pluies abond.")) |> 
  add_row(en = paste0("Destr. storm"), 
          fr = paste0("Tempêtes destr.")) |> 
  add_row(en = paste0("Heat wave"), 
          fr = paste0("Vagues de chaleur")) |> 
  add_row(en = paste0("Cyclists"), 
          fr = paste0("Cyclistes")) |> 
  add_row(en = paste0("Other"), 
          fr = paste0("Autre")) |> 
  add_row(en = paste0("Pedestrian"), 
          fr = paste0("Piéton")) |> 
  add_row(en = paste0("Total"), 
          fr = paste0("Total")) |> 
  add_row(en = paste0("Crash /1k cyc."), 
          fr = paste0("Coll. /1k cycl.")) |> 
  add_row(en = paste0("Crash /1k other"), 
          fr = paste0("Coll. /1k autre")) |> 
  add_row(en = paste0("Crash /1k ped."), 
          fr = paste0("Coll. /1k piéton")) |> 
  add_row(en = paste0("Crash /1k total"), 
          fr = paste0("Coll. /1k total")) |> 
  add_row(en = paste0("Crash /sqkm cyc"), 
          fr = paste0("Coll. /km2 cycl.")) |> 
  add_row(en = paste0("Crash /sqkm oth"), 
          fr = paste0("Coll. /km2 autre")) |> 
  add_row(en = paste0("Crash /sqkm ped"), 
          fr = paste0("Coll. /km2 piéton")) |> 
  add_row(en = paste0("Crash /sqkm tot"), 
          fr = paste0("Coll. /km2 tot.")) |> 
  add_row(en = paste0("Total WKP"), 
          fr = paste0("Total PS")) |> 
  add_row(en = paste0("Total WKOP"), 
          fr = paste0("Total HPS")) |> 
  add_row(en = paste0("Total WKN"), 
          fr = paste0("Total NS")) |> 
  add_row(en = paste0("Total WEP"), 
          fr = paste0("Total PFDS")) |> 
  add_row(en = paste0("Total WEOP"), 
          fr = paste0("Total HPFDS")) |> 
  add_row(en = paste0("Total WEN"), 
          fr = paste0("Total NFDS")) |> 
  add_row(en = paste0("Low-skill WKP"), 
          fr = paste0("Peu qual. PS")) |> 
  add_row(en = paste0("Low-skill WKOP"), 
          fr = paste0("Peu qual. HPS")) |> 
  add_row(en = paste0("Low-skill WKN"), 
          fr = paste0("Peu qual. NS")) |> 
  add_row(en = paste0("Low-skill WEP"), 
          fr = paste0("Peu qual. PFDS")) |> 
  add_row(en = paste0("Low-skill WEOP"), 
          fr = paste0("Peu qual. HPFDS")) |> 
  add_row(en = paste0("Low-skill WEN"), 
          fr = paste0("Peu qual. NFDS")) |> 
  add_row(en = paste0("Hi-skill WKP"), 
          fr = paste0("Haut. qual. PS")) |> 
  add_row(en = paste0("Hi-skill WKOP"), 
          fr = paste0("Haut. qual. HPS")) |> 
  add_row(en = paste0("Hi-skill WKN"), 
          fr = paste0("Haut. qual. NS")) |> 
  add_row(en = paste0("Hi-skill WEP"), 
          fr = paste0("Haut. qual. PFDS")) |> 
  add_row(en = paste0("Hi-skill WEOP"), 
          fr = paste0("Haut. qual. HPFDSH")) |> 
  add_row(en = paste0("Hi-skill WEN"), 
          fr = paste0("Haut. qual. NFDS")) |> 
  add_row(en = paste0("Low-inc WKP"), 
          fr = paste0("Faib. rev. PS")) |> 
  add_row(en = paste0("Low-inc WKOP"), 
          fr = paste0("Faib. rev. HPS")) |> 
  add_row(en = paste0("Low-inc WKN"), 
          fr = paste0("Faib. rev. NS")) |> 
  add_row(en = paste0("Low-inc WEP"), 
          fr = paste0("Faib. rev. PFDS")) |> 
  add_row(en = paste0("Low-inc WEOP"), 
          fr = paste0("Faib. rev. HPFDSH")) |> 
  add_row(en = paste0("Low-inc WEN"), 
          fr = paste0("Faib. rev. NFDS")) |> 
  add_row(en = paste0("Schools WKP"), 
          fr = paste0("Écoles PS")) |> 
  add_row(en = paste0("Schools WKOP"), 
          fr = paste0("Écoles HPS")) |> 
  add_row(en = paste0("Schools WKN"), 
          fr = paste0("Écoles NS")) |> 
  add_row(en = paste0("Schools WEP"), 
          fr = paste0("Écoles PFDS")) |> 
  add_row(en = paste0("Schools WEOP"), 
          fr = paste0("Écoles HPFDSH")) |> 
  add_row(en = paste0("Schools WEN"), 
          fr = paste0("Écoles NFDS")) |> 
  add_row(en = paste0("Healthcare WKP"), 
          fr = paste0("Santé PS")) |> 
  add_row(en = paste0("Healthcare WKOP"), 
          fr = paste0("Santé HPS")) |> 
  add_row(en = paste0("Healthcare WKN"), 
          fr = paste0("Santé NS")) |> 
  add_row(en = paste0("Healthcare WEP"), 
          fr = paste0("Santé PFDS")) |> 
  add_row(en = paste0("Healthcare WEOP"), 
          fr = paste0("Santé HPFDSH")) |> 
  add_row(en = paste0("Healthcare WEN"), 
          fr = paste0("Santé NFDS")) |> 
  add_row(en = paste0("Alleys sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Alleys 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Borough park sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Borough park 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Large park sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Large park 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Other park sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Other park 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Road space sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Road space 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Under validation sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Under validation 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Total sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Total 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Gentrification"), 
          fr = paste0("Gentrification")) |> 
  add_row(en = paste0("Combination "), 
          fr = NA) |> 
  add_row(en = paste0("Condo conversion "), 
          fr = NA) |> 
  add_row(en = paste0("Demolition "), 
          fr = NA) |> 
  add_row(en = paste0("New construction "), 
          fr = NA) |> 
  add_row(en = paste0("Renovation "), 
          fr = NA) |> 
  add_row(en = paste0("Total "), 
          fr = NA) |> 
  add_row(en = paste0("Combination sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Combination 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Condo conversion sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Condo conversion 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Demolition sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Demolition 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("New construction sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("New construction 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("Renovation sqkm"), 
          fr = NA) |> 
  add_row(en = paste0("Renovation 1,000"), 
          fr = NA) |> 
  add_row(en = paste0("the percentage of private dwellings occupied by tenants",
                      ""), 
          fr = paste0("le pourcentage de logements privés occupés par des loca",
                      "taires")) |> 
  add_row(en = paste0("the average rent paid by tenants per month"), 
          fr = paste0("le loyer moyen payé par les locataires par mois")) |> 
  add_row(en = paste0("the percentage of households living in dwellings requir",
                      "ing major repairs"), 
          fr = paste0("le pourcentage de ménages vivant dans des logements néc",
                      "essitant des réparations importantes")) |> 
  add_row(en = paste0("the average value of owner-occupied dwellings"), 
          fr = paste0("la valeur moyenne des logements occupés par leur propri",
                      "étaire")) |> 
  add_row(en = paste0("the percentage of households that have moved in the pas",
                      "t year"), 
          fr = paste0("le pourcentage de ménages qui ont déménagé au cours de ",
                      "l'année dernière")) |> 
  add_row(en = paste0("the percentage of households that have moved in the pas",
                      "t five years"), 
          fr = paste0("le pourcentage de ménages qui ont déménagé au cours des",
                      " cinq dernières années")) |> 
  add_row(en = paste0("the percentage of occupied private dwellings that are s",
                      "ingle-detached houses"), 
          fr = paste0("le pourcentage de logements privés occupés qui sont des",
                      " maisons individuelles non attenantes")) |> 
  add_row(en = paste0("median before-tax household income"), 
          fr = paste0("revenu médian des ménages avant impôt")) |> 
  add_row(en = paste0("the percentage of households with an income less then $",
                      "50,000"), 
          fr = paste0("le pourcentage de ménages dont le revenu est inférieur ",
                      "à 50 000 $")) |> 
  add_row(en = paste0("the percentage of households with an income between $50,",
                      "000 and $100,000"), 
          fr = paste0("le pourcentage de ménages dont le revenu est entre ",
                      "$50 000 et 100 000 $")) |> 
  add_row(en = paste0("the percentage of households with an income higher than",
                      " $100,000"), 
          fr = paste0("le pourcentage de ménages dont le revenu est supérieur ",
                      "à 100 000 $")) |> 
  add_row(en = paste0("the percentage of residents who are foreign-born"), 
          fr = paste0("le pourcentage de résidents nés à l'étranger")) |> 
  add_row(en = paste0("the percentage of people who have immigrated in the las",
                      "t five years"), 
          fr = paste0("le pourcentage de personnes qui ont immigré au cours de",
                      "s cinq dernières années")) |> 
  add_row(en = paste0("the percentage of people who identify as part of one or",
                      " more visible minority groups"), 
          fr = paste0("le pourcentage de personnes qui s'identifient comme fai",
                      "sant partie d'un ou plusieurs groupes de minorités visi",
                      "bles")) |> 
  add_row(en = paste0("the percentage of people who are of aboriginal identity",
                      ""), 
          fr = paste0("le pourcentage de personnes ayant une identité autochto",
                      "ne")) |> 
  add_row(en = paste0("the percentage of people who drive a privately owned ca",
                      "r or truck to work"), 
          fr = paste0("le pourcentage de personnes qui conduisent une voiture ",
                      "ou un camion privé pour se rendre au travail")) |> 
  add_row(en = paste0("the percentage of people who walk or cycle to work"), 
          fr = paste0("le pourcentage de personnes qui se rendent au travail à",
                      " pied ou à vélo")) |> 
  add_row(en = paste0("the percentage of people who use public transit to get ",
                      "to work"), 
          fr = paste0("le pourcentage de personnes qui utilisent les transport",
                      "s en commun pour se rendre au travail")) |> 
  add_row(en = paste0("the percentage of one person households out of total ho",
                      "useholds"), 
          fr = paste0("le pourcentage de ménages composés d'une personne par r",
                      "apport au total des ménages")) |> 
  add_row(en = paste0("the percentage of individuals that only know French as ",
                      "an official language"), 
          fr = paste0("le pourcentage d'individus qui ne connaissent que le fr",
                      "ançais comme langue officielle")) |> 
  add_row(en = paste0("the percentage of individuals that only know English as",
                      " an official language"), 
          fr = paste0("le pourcentage d'individus qui ne connaissent que l'ang",
                      "lais comme langue officielle")) |> 
  add_row(en = paste0("the percentage of individuals that know both official l",
                      "anguages (French and English)"), 
          fr = paste0("le pourcentage de personnes qui connaissent les deux la",
                      "ngues officielles (français et anglais)")) |> 
  add_row(en = paste0("the percentage of individuals that do not know either o",
                      "f the official languages (French or English)"), 
          fr = paste0("le pourcentage d'individus qui ne connaissent aucune de",
                      "s deux langues officielles (français ou anglais)")) |> 
  add_row(en = paste0("the percentage of the population aged between 0 and 14 ",
                      "years old"), 
          fr = paste0("le pourcentage de la population âgée de 0 à 14 ans")) |> 
  add_row(en = paste0("the percentage of the population aged between 15 and 64",
                      " years old"), 
          fr = paste0("le pourcentage de la population âgée de 15 à 64 ans")) |> 
  add_row(en = paste0("the percentage of the population aged 65 and above"), 
          fr = paste0("le pourcentage de la population âgée de 65 ans et plus")) |> 
  add_row(en = paste0("the percentage of the population aged 15 and over holdi",
                      "ng a degree at bachelor level or above"), 
          fr = paste0("le pourcentage de la population âgée de 15 ans et plus ",
                      "détenant un diplôme de niveau baccalauréat ou plus")) |> 
  add_row(en = paste0("the percentage of the population aged 15 and over with ",
                      "no certificate, diploma or degree"), 
          fr = paste0("le pourcentage de la population âgée de 15 ans et plus ",
                      "ne possédant aucun certificat, diplôme ou grade")) |> 
  add_row(en = paste0("the percentage of the workforce in professional and man",
                      "agerial occupations, based on the North American Indust",
                      "ry Classification System"), 
          fr = paste0("le pourcentage de la main-d'Å“uvre dans les professions",
                      " libérales et les postes de gestion, selon le Système d",
                      "e classification des industries de l'Amérique du Nord")) |> 
  add_row(en = paste0("the percentage of the workforce in artistic and cultura",
                      "l occupations, based on the North American Industry Cla",
                      "ssification System"), 
          fr = paste0("le pourcentage de la main-d'Å“uvre dans les professions",
                      " artistiques et culturelles, selon le Système de classi",
                      "fication des industries de l'Amérique du Nord")) |> 
  add_row(en = paste0("the percentage of census families with children out of ",
                      "total households"), 
          fr = paste0("le pourcentage de familles de recensement avec enfants ",
                      "par rapport au total des ménages")) |> 
  add_row(en = paste0("the percentage of households living in accommodations w",
                      "ithout enough bedrooms"), 
          fr = paste0("le pourcentage de ménages vivant dans des logements ave",
                      "c un nombre insuffisant de chambres à coucher")) |> 
  add_row(en = paste0("the percentage of renter households that spend more tha",
                      "n 30% of their income on shelter costs"), 
          fr = paste0("le pourcentage de ménages locataires qui consacrent plu",
                      "s de 30 % de leur revenu aux frais de logement")) |> 
  add_row(en = paste0("the percentage of owner households that spend more than",
                      " 30% of their income on shelter costs"), 
          fr = paste0("le pourcentage de ménages propriétaires qui consacrent ",
                      "plus de 30 % de leur revenu aux frais de logement")) |> 
  add_row(en = paste0("the percentage of dwellings for which residents pay mor",
                      "e than 30% of income on housing costs"), 
          fr = paste0("le pourcentage de logements pour lesquels les résidents",
                      " consacrent plus de 30 % de leurs revenus aux frais de ",
                      "logement")) |> 
  add_row(en = paste0("the prevalence of low income in private households base",
                      "d on the Low-income measure, after-tax (LIM-AT)"), 
          fr = paste0("la prévalence de ménages privés à faible revenu, basée ",
                      "sur la Mesure de faible revenu après impôt (MFR-ApI)")) |> 
  add_row(en = paste0("the percentage of people whose commute time is less tha",
                      "n 15 minutes"), 
          fr = paste0("le pourcentage de personnes dont le temps de trajet est",
                      " inférieur à 15 minutes")) |> 
  add_row(en = paste0("the percentage of people whose commute time is between ",
                      "15 and 45 minutes"), 
          fr = paste0("le pourcentage de personnes dont le temps de trajet est",
                      " compris entre 15 et 45 minutes")) |> 
  add_row(en = paste0("the percentage of people whose commute time is longer t",
                      "han 45 minutes"), 
          fr = paste0("le pourcentage de personnes dont le temps de trajet est",
                      " supérieur à 45 minutes")) |> 
  add_row(en = paste0("the potential for active living"), 
          fr = paste0("le potentiel de la vie active")) |> 
  add_row(en = paste0("the vulnerability to climate-change related drought"), 
          fr = paste0("la vulnérabilité aux changements climatiques reliés aux",
                      " sécheresses")) |> 
  add_row(en = paste0("the vulnerability to climate-change related flooding"), 
          fr = paste0("la vulnérabilité aux changements climatiques reliés aux",
                      " crues")) |> 
  add_row(en = paste0("the vulnerability to climate-change related heavy rain"), 
          fr = paste0("la vulnérabilité aux changements climatiques reliés aux",
                      " pluies abondantes")) |> 
  add_row(en = paste0("the vulnerability to climate-change related destructive",
                      " storms"), 
          fr = paste0("la vulnérabilité aux changements climatiques reliés aux",
                      " tempêtes destructrices")) |> 
  add_row(en = paste0("the vulnerability to climate-change related heat waves"), 
          fr = paste0("la vulnérabilité aux changements climatiques reliés aux",
                      " vagues de chaleur")) |> 
  add_row(en = paste0("the total number of car collisions involving cyclists"), 
          fr = paste0("le nombre total de collisions de voitures impliquant de",
                      "s cyclistes")) |> 
  add_row(en = paste0("the total number of car collisions involving neither pe",
                      "destrians or cyclists"), 
          fr = paste0("le nombre total de collisions de voitures n'impliquant ",
                      "ni piétons, ni cyclistes")) |> 
  add_row(en = paste0("the total number of car collisions involving pedestrian",
                      "s"), 
          fr = paste0("le nombre total de collisions de voitures impliquant de",
                      "s piétons")) |> 
  add_row(en = paste0("the total number of car collisions"), 
          fr = paste0("le nombre total de collisions de voitures")) |> 
  add_row(en = paste0("the total number of car collisions involving cyclists p",
                      "er 1,000 residents"), 
          fr = paste0("le nombre total de collisions de voitures impliquant de",
                      "s cyclistes pour 1 000 résidents")) |> 
  add_row(en = paste0("the total number of car collisions involving neither pe",
                      "destrians or cyclists per 1,000 residents"), 
          fr = paste0("le nombre total de collisions de voitures n'impliquant ",
                      "ni piétons, ni cyclistes pour 1 000 résidents")) |> 
  add_row(en = paste0("the total number of car collisions involving pedestrian",
                      "s per 1,000 residents"), 
          fr = paste0("le nombre total de collisions de voitures impliquant de",
                      "s piétons pour 1 000 résidents")) |> 
  add_row(en = paste0("the total number of car collisions per 1,000 residents"), 
          fr = paste0("le nombre total de collisions de voitures pour 1 000 ré",
                      "sidents")) |> 
  add_row(en = paste0("the total number of car collisions involving cyclists p",
                      "er square kilometre"), 
          fr = paste0("le nombre total de collisions de voitures impliquant de",
                      "s cyclistes par km2")) |> 
  add_row(en = paste0("the total number of car collisions involving neither pe",
                      "destrians or cyclists per square kilometre"), 
          fr = paste0("le nombre total de collisions de voitures n'impliquant ",
                      "ni piétons ni cyclistes par km2")) |> 
  add_row(en = paste0("the total number of car collisions involving pedestrian",
                      "s per square kilometre"), 
          fr = paste0("le nombre total de collisions de voitures impliquant de",
                      "s piétons par km2")) |> 
  add_row(en = paste0("the total number of car collisions per square kilometre",
                      ""), 
          fr = paste0("le nombre total de collisions de voitures par km2")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes a",
                      "t weekday peak service"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et ",
                      "moins à l'heure de pointe en semaine ")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes a",
                      "t weekday off-peak service"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et ",
                      "moins hors heure de pointe en semaine ")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes a",
                      "t weekday night service"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et ",
                      "moins la nuit en semaine")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes a",
                      "t weekend peak service"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et ",
                      "moins à l'heure de pointe la fin de semaine")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes a",
                      "t weekend off-peak service"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et ",
                      "moins hors heure de pointe la fin de semaine")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes a",
                      "t weekend night service"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et ",
                      "moins la nuit la fin de semaine")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minut",
                      "es at weekday peak service"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en ",
                      "30 minutes et moins à l'heure de pointe en semaine ")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minut",
                      "es at weekday off-peak service"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en ",
                      "30 minutes et moins hors heure de pointe en semaine ")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minut",
                      "es at weekday night service"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en ",
                      "30 minutes et moins la nuit en semaine")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minut",
                      "es at weekend peak service"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en ",
                      "30 minutes et moins à l'heure de pointe la fin de semai",
                      "ne")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minut",
                      "es at weekend off-peak service"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en ",
                      "30 minutes et moins hors heure de pointe la fin de sema",
                      "ine")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minut",
                      "es at weekend night service"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en ",
                      "30 minutes et moins la nuit la fin de semaine")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minu",
                      "tes at weekday peak service"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibl",
                      "es en 30 minutes et moins à l'heure de pointe en semain",
                      "e ")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minu",
                      "tes at weekday off-peak service"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibl",
                      "es en 30 minutes et moins hors heure de pointe en semai",
                      "ne ")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minu",
                      "tes at weekday night service"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibl",
                      "es en 30 minutes et moins la nuit en semaine")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minu",
                      "tes at weekend peak service"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibl",
                      "es en 30 minutes et moins à l'heure de pointe la fin de",
                      " semaine")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minu",
                      "tes at weekend off-peak service"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibl",
                      "es en 30 minutes et moins hors heure de pointe la fin d",
                      "e semaine")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minu",
                      "tes at weekend night service"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibl",
                      "es en 30 minutes et moins la nuit la fin de semaine")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible ",
                      "within 30 minutes at weekday peak service"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de ",
                      "30 000$ accessibles en 30 minutes et moins à l'heure de",
                      " pointe en semaine ")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible ",
                      "within 30 minutes at weekday off-peak service"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de ",
                      "30 000$ accessibles en 30 minutes et moins hors heure d",
                      "e pointe en semaine ")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible ",
                      "within 30 minutes at weekday night service"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de ",
                      "30 000$ accessibles en 30 minutes et moins la nuit en s",
                      "emaine")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible ",
                      "within 30 minutes at weekend peak service"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de ",
                      "30 000$ accessibles en 30 minutes et moins à l'heure de",
                      " pointe la fin de semaine")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible ",
                      "within 30 minutes at weekend off-peak service"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de ",
                      "30 000$ accessibles en 30 minutes et moins hors heure d",
                      "e pointe la fin de semaine")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible ",
                      "within 30 minutes at weekend night service"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de ",
                      "30 000$ accessibles en 30 minutes et moins la nuit la f",
                      "in de semaine")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes at w",
                      "eekday peak service"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et m",
                      "oins à l'heure de pointe en semaine ")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes at w",
                      "eekday off-peak service"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et m",
                      "oins hors heure de pointe en semaine ")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes at w",
                      "eekday night service"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et m",
                      "oins la nuit en semaine")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes at w",
                      "eekend peak service"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et m",
                      "oins à l'heure de pointe la fin de semaine")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes at w",
                      "eekend off-peak service"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et m",
                      "oins hors heure de pointe la fin de semaine")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes at w",
                      "eekend night service"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et m",
                      "oins la nuit la fin de semaine")) |> 
  add_row(en = paste0("the number of healthcare facilities accessible within 3",
                      "0 minutes at weekday peak service"), 
          fr = paste0("le nombre total d'établissements de santé accessibles e",
                      "n 30 minutes et moins à l'heure de pointe en semaine ")) |> 
  add_row(en = paste0("the number of healthcare facilities accessible within 3",
                      "0 minutes at weekday off-peak service"), 
          fr = paste0("le nombre total d'établissements de santé accessibles e",
                      "n 30 minutes et moins hors heure de pointe en semaine ")) |> 
  add_row(en = paste0("the number of healthcare facilities within 30 minutes a",
                      "t weekday night service"), 
          fr = paste0("le nombre total d'établissements de santé accessibles e",
                      "n 30 minutes et moins la nuit en semaine")) |> 
  add_row(en = paste0("the number of healthcare facilities accessible within 3",
                      "0 minutes at weekend peak service"), 
          fr = paste0("le nombre total d'établissements de santé accessibles e",
                      "n 30 minutes et moins à l'heure de pointe la fin de sem",
                      "aine")) |> 
  add_row(en = paste0("the number of healthcare facilities accessible within 3",
                      "0 minutes at weekend off-peak service"), 
          fr = paste0("le nombre total d'établissements de santé accessibles e",
                      "n 30 minutes et moins hors heure de pointe la fin de se",
                      "maine")) |> 
  add_row(en = paste0("the number of healthcare facilities accessible within 3",
                      "0 minutes at weekend night service"), 
          fr = paste0("le nombre total d'établissements de santé accessibles e",
                      "n 30 minutes et moins la nuit la fin de semaine")) |> 
  add_row(en = paste0("the number of square metres of green alley per square k",
                      "ilometre"), 
          fr = paste0("le nombre de m2 de ruelle verte par km2")) |> 
  add_row(en = paste0("the number of square metres of green alley per 1,000 re",
                      "sidents"), 
          fr = paste0("le nombre de m2 de ruelle verte pour 1 000 résidents")) |> 
  add_row(en = paste0("the number of square metres of borough park per square ",
                      "kilometre"), 
          fr = paste0("le nombre de m2 de parc d'arrondissement par km2")) |> 
  add_row(en = paste0("the number of square metres of borough park per 1,000 r",
                      "esidents"), 
          fr = paste0("le nombre de m2 de parc d'arrondissement pour 1 000 rés",
                      "idents")) |> 
  add_row(en = paste0("the number of square metres of large park per square ki",
                      "lometre"), 
          fr = paste0("le nombre de m2 de grand parc par km2")) |> 
  add_row(en = paste0("the number of square metres of large park per 1,000 res",
                      "idents"), 
          fr = paste0("le nombre de m2 de grand parc pour 1 000 résidents")) |> 
  add_row(en = paste0("the number of square metres of other park per square ki",
                      "lometre"), 
          fr = paste0("le nombre de m2 d'autre parc par km2")) |> 
  add_row(en = paste0("the number of square metres of other park per 1,000 res",
                      "idents"), 
          fr = paste0("le nombre de m2 d'autre parc pour 1 000 résidents")) |> 
  add_row(en = paste0("the number of square metres of road space per square ki",
                      "lometre"), 
          fr = paste0("le nombre de m2 d'espace routier par km2")) |> 
  add_row(en = paste0("the number of square metres of road space per 1,000 res",
                      "idents"), 
          fr = paste0("le nombre de m2 d'espace routier pour 1 000 résidents")) |> 
  add_row(en = paste0("the number of square metres of green space under valida",
                      "tion per square kilometre"), 
          fr = paste0("le nombre de m2 d'espace vert en cours de validation pa",
                      "r km2")) |> 
  add_row(en = paste0("the number of square metres of green space under valida",
                      "tion per 1,000 residents"), 
          fr = paste0("le nombre de m2 d'espace vert en cours de validation po",
                      "ur 1 000 résidents")) |> 
  add_row(en = paste0("the number of square metres of total green space per sq",
                      "uare kilometre"), 
          fr = paste0("le nombre de m2 d'espace vert total par km2")) |> 
  add_row(en = paste0("the number of square metres of total green space per 1,",
                      "000 residents"), 
          fr = paste0("le nombre de m2 d'espace vert total pour 1 000 résident",
                      "s")) |> 
  add_row(en = paste0("the gentrification pressure an area is experiencing"), 
          fr = paste0("la pression de gentrification que subit un secteur")) |> 
  add_row(en = paste0("the number of dwellings combination permits emitted "), 
          fr = NA) |> 
  add_row(en = paste0("the number of condo conversion permits emitted "), 
          fr = NA) |> 
  add_row(en = paste0("the number of demolition permits emitted "), 
          fr = NA) |> 
  add_row(en = paste0("the number of owner and renter residential buildings bu",
                      "ilt "), 
          fr = NA) |> 
  add_row(en = paste0("the number of renovation permits emitted "), 
          fr = NA) |> 
  add_row(en = paste0("the number of total permits emitted "), 
          fr = NA) |> 
  add_row(en = paste0("the number of dwellings combination permits emitted per",
                      " square kilometre"), 
          fr = NA) |> 
  add_row(en = paste0("the number of dwellings combination permits emitted per",
                      " 1,000 residents"), 
          fr = NA) |> 
  add_row(en = paste0("the number of condo conversion permits emitted per squa",
                      "re kilometre"), 
          fr = NA) |> 
  add_row(en = paste0("the number of condo conversion permits emitted per 1,00",
                      "0 residents"), 
          fr = NA) |> 
  add_row(en = paste0("the number of demolition permits emitted per square kil",
                      "ometre"), 
          fr = NA) |> 
  add_row(en = paste0("the number of demolition permits emitted per 1,000 resi",
                      "dents"), 
          fr = NA) |> 
  add_row(en = paste0("the number of owner and renter residential buildings bu",
                      "ilt per square kilometre"), 
          fr = NA) |> 
  add_row(en = paste0("the number of owner and renter residential buildings bu",
                      "ilt per 1,000 residents"), 
          fr = NA) |> 
  add_row(en = paste0("the number of renovation permits emitted per square kil",
                      "ometre"), 
          fr = NA) |> 
  add_row(en = paste0("the number of renovation permits emitted per 1,000 resi",
                      "dents"), 
          fr = NA) |> 
  add_row(en = paste0("the number of total permits emitted per square kilometr",
                      "e"), 
          fr = NA) |> 
  add_row(en = paste0("the number of total permits emitted per 1,000 residents",
                      ""), 
          fr = NA) |> 
  add_row(en = paste0("Housing"), 
          fr = paste0("Logement")) |> 
  add_row(en = paste0("Income"), 
          fr = paste0("Revenu")) |> 
  add_row(en = paste0("Identity"), 
          fr = paste0("Identité")) |> 
  add_row(en = paste0("Transport"), 
          fr = paste0("Transport")) |> 
  add_row(en = paste0("Household"), 
          fr = paste0("Ménage")) |> 
  add_row(en = paste0("Language"), 
          fr = paste0("Langue")) |> 
  add_row(en = paste0("Age"), 
          fr = paste0("Âge")) |> 
  add_row(en = paste0("Education"), 
          fr = paste0("Éducation")) |> 
  add_row(en = paste0("Employment"), 
          fr = paste0("Emploi")) |> 
  add_row(en = paste0("Urban life"), 
          fr = paste0("La vie urbaine")) |> 
  add_row(en = paste0("Climate"), 
          fr = paste0("Climat")) |> 
  add_row(en = paste0("Insignificant"), 
          fr = paste0("Insignifiant")) |> 
  add_row(en = paste0("Minor"), 
          fr = paste0("Mineur")) |> 
  add_row(en = paste0("Moderate"), 
          fr = paste0("Modéré")) |> 
  add_row(en = paste0("Elevated"), 
          fr = paste0("Élevé")) |> 
  add_row(en = paste0("Major"), 
          fr = paste0("Majeur")) |> 
  add_row(en = paste0("None"), 
          fr = paste0("Aucun")) |> 
  add_row(en = paste0("Insig."), 
          fr = paste0("Insign.")) |> 
  add_row(en = paste0("Mod."), 
          fr = paste0("Mod.")) |> 
  add_row(en = paste0("Elev."), 
          fr = paste0("Élev.")) |> 
  add_row(en = paste0("Community"), 
          fr = paste0("Communautaire")) |> 
  add_row(en = paste0("Green"), 
          fr = paste0("Verte")) |> 
  add_row(en = paste0("Mixed"), 
          fr = paste0("Mixte")) |> 
  add_row(en = paste0("Unmaintained"), 
          fr = paste0("Non-maintenue")) |> 
  add_row(en = paste0("Commun."), 
          fr = paste0("Commun.")) |> 
  add_row(en = paste0("Green"), 
          fr = paste0("Verte")) |> 
  add_row(en = paste0("Mixed"), 
          fr = paste0("Mixte")) |> 
  add_row(en = paste0("Unmain."), 
          fr = paste0("Non-maint."))
