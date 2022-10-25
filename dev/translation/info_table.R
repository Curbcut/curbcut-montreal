# #### `Info table` preparation for translation ##################################

info_table_translated <- 
  tibble(en = character(), fr = character()) |>
  add_row(en = paste0("borough/city"), 
          fr = paste0("de l'arrondissement/de la ville")) |> 
  add_row(en = paste0("census tract"), 
          fr = paste0("du secteur de recensement")) |> 
  add_row(en = paste0("dissemination area"), 
          fr = paste0("de l'aire de diffusion")) |> 
  add_row(en = paste0("dissemination block"), 
          fr = paste0("de l'îlot de diffusion")) |> 
  add_row(en = paste0("250-m"), 
          fr = paste0("de 250-m")) |> 
  add_row(en = paste0("Sector"), 
          fr = paste0("Secteur")) |> 
  add_row(en = paste0("building"), 
          fr = paste0("du bâtiment")) |> 
  add_row(en = paste0("street"), 
          fr = paste0("rue")) |> 
  add_row(en = paste0("boroughs or cities"), 
          fr = paste0("arrondissements ou villes")) |> 
  add_row(en = paste0("census tracts"), 
          fr = paste0("secteurs de recensement")) |> 
  add_row(en = paste0("dissemination areas"), 
          fr = paste0("aires de diffusion")) |> 
  add_row(en = paste0("dissemination blocks"), 
          fr = paste0("îlots de diffusion")) |> 
  add_row(en = paste0("areas"), 
          fr = paste0("zones")) |> 
  add_row(en = paste0("buildings"), 
          fr = paste0("bâtiments")) |> 
  add_row(en = paste0("streets"), 
          fr = paste0("rues")) |> 
  add_row(en = paste0("The dissemination area around {select_name$name}"), 
          fr = paste0("L'aire de diffusion autour de {select_name$name}")) |> 
  add_row(en = paste0("Census tract {select_name$name}"), 
          fr = paste0("Secteur de recensement {select_name$name} ")) |> 
  add_row(en = paste0("Dissemination area {select_name$name}"), 
          fr = paste0("Aire de diffusion {select_name$name} ")) |> 
  add_row(en = paste0("Dissemination block {select_name$name}"), 
          fr = paste0("Îlot de diffusion {select_name$name} ")) |> 
  add_row(en = paste0("The area around {select_name$name}"), 
          fr = paste0("La zone autour de {select_name$name} ")) |> 
  add_row(en = paste0("{select_name$name_2} of {out$place_name}"), 
          fr = paste0("{select_name$name_2} de {out$place_name}")) |> 
  add_row(en = paste0("much larger than"), 
          fr = paste0("beaucoup plus grand que")) |> 
  add_row(en = paste0("larger than"), 
          fr = paste0("plus grand que")) |> 
  add_row(en = paste0("almost the same as"), 
          fr = paste0("presque identique à")) |> 
  add_row(en = paste0("smaller than"), 
          fr = paste0("plus petit que")) |> 
  add_row(en = paste0("much smaller than"), 
          fr = paste0("beaucoup plus petit que")) |> 
  add_row(en = paste0("high"), 
          fr = paste0("élevé/e")) |> 
  add_row(en = paste0("low"), 
          fr = paste0("faible")) |> 
  add_row(en = paste0("moderate"), 
          fr = paste0("modéré/e")) |> 
  add_row(en = paste0("increased"), 
          fr = paste0("augmenté")) |> 
  add_row(en = paste0("decreased"), 
          fr = paste0("diminué")) |> 
  add_row(en = paste0("majority"), 
          fr = paste0("majorité")) |> 
  add_row(en = paste0("plurality"), 
          fr = paste0("pluralité")) |> 
  add_row(en = paste0("positive"), 
          fr = paste0("positive")) |> 
  add_row(en = paste0("negative"), 
          fr = paste0("négative")) |> 
  add_row(en = paste0("strong"), 
          fr = paste0("forte")) |> 
  add_row(en = paste0("weak"), 
          fr = paste0("faible")) |> 
  add_row(en = paste0("higher"), 
          fr = paste0("plus grand/e")) |> 
  add_row(en = paste0("lower"), 
          fr = paste0("plus petit/e")) |> 
  add_row(en = paste0("with only a few exceptions"), 
          fr = paste0("à quelques exceptions près")) |> 
  add_row(en = paste0("although with some exceptions"), 
          fr = paste0("bien qu'avec des exceptions")) |> 
  add_row(en = paste0("although with many exceptions"), 
          fr = paste0("bien qu'avec beaucoup d'exceptions")) |> 
  add_row(en = paste0("dramatically different"), 
          fr = paste0("radicalement différents")) |> 
  add_row(en = paste0("substantially different"), 
          fr = paste0("sensiblement différents")) |> 
  add_row(en = paste0("considerably different"), 
          fr = paste0("modérément différents")) |> 
  add_row(en = paste0("similar"), 
          fr = paste0("similaires")) |> 
  add_row(en = paste0(" and "), 
          fr = paste0(" et ")) |> 
  add_row(en = paste0("several different dates"), 
          fr = paste0("plusieurs dates différentes")) |> 
  add_row(en = paste0("increasing"), 
          fr = paste0("augmentant")) |> 
  add_row(en = paste0("decreasing"), 
          fr = paste0("diminuant")) |> 
  add_row(en = paste0("No data available."), 
          fr = paste0("Aucune donnée disponible.")) |> 
  add_row(en = paste0("<i>(Data from {date_left}.)</i>"), 
          fr = paste0("<i>(Données de {date_left}.)</i>")) |> 
  
  add_row(en = paste0("in the Montreal region"), 
          fr = paste0("de la région de Montréal")) |> 
  add_row(en = paste0("in the City of Montreal"), 
          fr = paste0("de la Ville de Montréal")) |> 
  add_row(en = paste0("on the island of Montreal"), 
          fr = paste0("de l'île de Montréal")) |> 
  add_row(en = paste0("in the Centraide of Greater Montreal territory"), 
          fr = paste0("du territoire de Centraide Grand Montréal")) |> 
  add_row(en = paste0("<strong>Kahnawake Mohawk Territory</strong><p>Statistic",
                      "s Canada does not gather the same data for indigenous r",
                      "eserves in the Census as it does for other jurisdiction",
                      "s, so we cannot display findings here."), 
          fr = paste0("<strong>Kahnawake (Réserve indienne)</strong><p>Dans le",
                      " cadre du recensement, Statistique Canada ne recueille ",
                      "pas les mêmes données pour les réserves autochtones que",
                      " dans les autres juridictions, nous ne pouvons donc pas",
                      " afficher de résultats ici.")) |> 
  add_row(en = paste0("<strong>Kanehsatà:ke</strong><p>Statistics Canada does ",
                      "not gather the same data for indigenous reserves in the",
                      " Census as it does for other jurisdictions, so we canno",
                      "t display findings here."), 
          fr = paste0("<strong>Kanehsatà:ke</strong><p>Dans le cadre du recens",
                      "ement, Statistique Canada ne recueille pas les mêmes do",
                      "nnées pour les réserves autochtones que dans les autres",
                      " juridictions, nous ne pouvons donc pas afficher de rés",
                      "ultats ici.")) |> 
  add_row(en = paste0("{z$place_name} has no data available on {z$exp_left}."), 
          fr = paste0("{z$place_name} n'a pas de données disponibles sur {z$ex",
                      "p_left}.")) |> 
  add_row(en = paste0("{z$place_name} has no data available on {z$exp_left} an",
                      "d {z$exp_right}."), 
          fr = paste0("{z$place_name} n'a pas de données disponibles sur {z$ex",
                      "p_left} et {z$exp_right} .")) |> 
  add_row(en = paste0("At the {z$scale_sing} scale, {z$exp_left} varies from {",
                      "z$min_val} to {z$max_val}, with an average value of {z$",
                      "mean_val} and a median value of {z$median_val}. Two thi",
                      "rds of {z$scale_plural} have a score between {z$quant_l",
                      "ow} and {z$quant_high}."), 
          fr = paste0("À l'échelle {z$scale_sing}, {z$exp_left} varie de {z$mi",
                      "n_val} à {z$max_val} avec une valeur moyenne de {z$mean",
                      "_val} et une valeur médiane de {z$median_val} . Les deu",
                      "x tiers des {z$scale_plural} ont un score compris entre",
                      " {z$quant_low} et {z$quant_high} .")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} has",
                      " a population of {z$pop} and a '{z$title_left}' score (",
                      "{z$exp_left}) of {z$val_left}, which is {z$larger} the ",
                      "territory-wide median of {z$median_val}.<p>{z$place_name} ",
                      "has a {z$high} relative score for this indicator, with ",
                      "'{z$exp_left}' higher than {z$percentile} of {z$scale_p",
                      "lural} {z$geo}."), 
          fr = paste0("<strong>{z$place_heading}</strong><p> {z$place_name} a ",
                      "une population de {z$pop} et un score pour '{z$title_le",
                      "ft}' ({z$exp_left}) de {z$val_left}, ce qui est {z$larg",
                      "er} la médiane territoriale de {z$median_val}.<p>{z$place_",
                      "name} a une valeur relativement {z$high} pour cet indic",
                      "ateur, avec '{z$exp_left}' plus élevé/e que {z$percenti",
                      "le} des {z$scale_plural} {z$geo}.")) |> 
  add_row(en = paste0("At the {z$scale_sing} scale, {z$exp_left} varies from '",
                      "{z$min_val}' to '{z$max_val}'. A {z$majority} of {z$sca",
                      "le_plural} ({z$mode_prop}) have a value of '{z$mode_val",
                      "}', while {z$mode_prop_2} have a value of '{z$mode_val_",
                      "2}'."), 
          fr = paste0("À l'échelle {z$scale_sing}, {z$exp_left} varie de {z$mi",
                      "n_val} à {z$max_val}. Une {z$majority} des {z$scale_plu",
                      "ral} ({z$mode_prop}) ont une valeur de '{z$mode_val}', ",
                      "tandis que {z$mode_prop_2} ont une valeur de '{z$mode_v",
                      "al_2}'.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} has",
                      " a population of {z$pop} and a '{z$title_left}' value o",
                      "f '{z$val_left}', which is shared by {z$other_with_val}",
                      " of {z$scale_plural} {z$geo}."), 
          fr = paste0("<strong>{z$place_heading}</strong><p> {z$place_name} a ",
                      "une population de {z$pop} et une valeur '{z$title_left}",
                      "' de ' {z$val_left}, ce qui est partagé par {z$other_wi",
                      "th_val} des {z$scale_plural} {z$geo}.")) |> 
  add_row(en = paste0("At the {z$scale_sing} scale, the change in {z$exp_left}",
                      " between {z$start_date_left} and {z$end_date_left} vari",
                      "ed from {z$min_val} to {z$max_val}, with an average cha",
                      "nge of {z$mean_val} and a median change of {z$median_va",
                      "l}. Two thirds of {z$scale_plural} saw a change between",
                      " {z$quant_low} and {z$quant_high}."), 
          fr = paste0("À l'échelle {z$scale_sing}, la variation pour '{z$exp_l",
                      "eft}' entre {z$start_date_left} et {z$end_date_left} a ",
                      "varié de {z$min_val} à {z$max_val}, avec une variation ",
                      "moyenne de {z$mean_val} et une variation médiane de {z$",
                      "median_val}. Deux tiers des {z$scale_plural} ont connu ",
                      "une variation comprise entre {z$quant_low} et {z$quant_",
                      "high}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>{sentence(z$exp_le",
                      "ft)} in {z$place_name} {z$increase} by {sub('-', '', z$",
                      "val_left)} between {z$start_date_left} and {z$end_date_",
                      "left}, which is {z$larger} the territory-wide median chang",
                      "e of {z$median_val}.<p>{z$place_name} had a {z$high} re",
                      "lative change for this indicator, with a change in {z$e",
                      "xp_left} larger than {z$percentile} of {z$scale_plural}",
                      " {z$geo}."), 
          fr = paste0("<strong>{z$place_heading}</strong><p>{sentence(z$exp_le",
                      "ft)} à {z$place_name} a {z$increase} de {sub('-', '', z",
                      "$val_left)} entre {z$start_date_left} et {z$end_date_le",
                      "ft}, ce qui est {z$larger} la variation médiane à l",
                      "'échelle du territoire ({z$median_val}).<p>{z$place_name",
                      "} a connu un changement relatif {z$high} pour cet indic",
                      "ateur, avec une variation pour '{z$exp_left}' plus gran",
                      "d que {z$percentile} des {z$scale_plural} {z$geo}.")) |> 
  add_row(en = paste0("TKTK At the {z$scale_sing} scale, {z$exp_left} varies f",
                      "rom '{z$min_val}' to '{z$max_val}'. A {z$majority} of {",
                      "z$scale_plural} ({z$mode_prop}) have a value of '{z$mod",
                      "e_val}', while {z$mode_prop_2} have a value of '{z$mode",
                      "_val_2}'."), 
          fr = paste0("TKTK")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>TKTK {z$place_name",
                      "} has a population of {z$pop} and a '{z$title_left}' va",
                      "lue of '{z$val_left}', which is shared by {z$other_with",
                      "_val} of {z$scale_plural} {z$geo}."), 
          fr = paste0("TKTK")) |> 
  add_row(en = paste0("<p>'{z$title_left}' has effectively no correlation ({z$",
                      "corr_disp}) with '{z$title_right}' at the {z$scale_sing",
                      "} scale.<p>This means that, at the {z$scale_sing} scale",
                      ", there is no relationship between the two variables."), 
          fr = paste0("<p>'{z$title_left}' n'a effectivement aucune corrélatio",
                      "n ({z$corr_disp}) avec '{z$title_right}' à l'échelle {z",
                      "$scale_sing}.<p>Cela signifie qu'à l'échelle {z$scale_s",
                      "ing}, il n'y a pas de relation entre les deux variables",
                      ".")) |> 
  add_row(en = paste0("<p>'{z$title_left}' has a {z$strong} {z$pos} correlatio",
                      "n ({z$corr_disp}) with '{z$title_right}' at the {z$scal",
                      "e_sing} scale.<p>This means that, in general, {z$scale_",
                      "plural} with a higher {sub('^the ', '', z$exp_right)} t",
                      "end to have a {z$higher} {sub('^the ', '', z$exp_left)}",
                      ", {z$high_low_disclaimer}."), 
          fr = paste0("<p>'{z$title_left}' présente une corrélation {z$strong}",
                      " et {z$pos} ({z$corr_disp}) avec '{z$title_right}' à l'éch",
                      "elle {z$scale_sing}.<p>Cela signifie qu'en général, les",
                      " {z$scale_plural} avec un/e plus grand/e {sub('^le |^la",
                      " ', '', z$exp_right)} ont tendance à avoir un/e {z$high",
                      "er} {sub('^le |^la ', '', z$exp_left)}, {z$high_low_dis",
                      "claimer}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} has",
                      " a population of {z$pop}, a '{z$title_left}' value of {",
                      "z$val_left}, and a '{z$title_right}' value of {z$val_ri",
                      "ght}. <p>These two scores are {z$relative_position}, in",
                      " relative terms. {z$place_name} has {sub('^the', 'a', z",
                      "$exp_left)} higher than {z$perc_left} of {z$scale_plura",
                      "l} and {sub('^the', 'a', z$exp_right)} higher than {z$p",
                      "erc_right} of {z$scale_plural} {z$geo}."), 
          fr = paste0("<strong>{z$place_heading}</strong><p> {z$place_name} a ",
                      "une population de {z$pop}, une valeur  '{z$title_left}'",
                      " de {z$val_left} et une valeur  '{z$title_right}' de {z",
                      "$val_right}. <p>Ces deux scores sont {z$relative_positi",
                      "on}, en termes relatifs. {z$place_name} a {sub('^le', '",
                      "un', z$exp_left)} supérieur à {z$perc_left} des {z$scal",
                      "e_plural} et {sub('^le', 'un', z$exp_right)} supérieur ",
                      "à {z$perc_right} des {z$scale_plural} {z$geo}.")) |> 
  add_row(en = paste0("<p>'{z$title_left}' has effectively no correlation (Spe",
                      "arman's rho: {z$corr_disp}) with '{z$title_right}' at t",
                      "he {z$scale_sing} scale.<p>This means that, at the {z$s",
                      "cale_sing} scale, there is no relationship between the ",
                      "two variables."), 
          fr = paste0("<p>'{z$title_left}' n'a effectivement aucune corrélatio",
                      "n (rho de Spearman: {z$corr_disp}) avec '{z$title_right",
                      "}' à l'échelle {z$scale_sing}.<p>Cela signifie qu'à l'é",
                      "chelle {z$scale_sing} scale, il n'y a pas de relation e",
                      "ntre les deux variables")) |> 
  add_row(en = paste0("<p>'{z$title_left}' has a {z$strong} {z$pos} correlatio",
                      "n (Spearman's rho: {z$corr_disp}) with '{z$title_right}",
                      "' at the {z$scale_sing} scale.<p>This means that, in ge",
                      "neral, {z$scale_plural} with a higher {sub('^the ', '',",
                      " z$exp_right)} tend to have a {z$higher} {sub('^the ', ",
                      "'', z$exp_left)}, {z$high_low_disclaimer}."), 
          fr = paste0("<p>'{z$title_left}' présente une corrélation {z$strong}",
                      " et {z$pos} (rho de Spearman: {z$corr_disp}) avec '{z$titl",
                      "e_right}' à l'échelle {z$scale_sing}.<p>Cela signifie q",
                      "u'en général, les {z$scale_plural} avec un/e plus grand",
                      "/e {sub('^le |^la ', '', z$exp_right)} ont tendance à a",
                      "voir un/e {z$higher} {sub('^le |^la ', '', z$exp_left)}",
                      ", {z$high_low_disclaimer}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} has",
                      " a population of {z$pop}, a '{z$title_left}' value of '",
                      "{z$val_left}', and a '{z$title_right}' value of {z$val_",
                      "right}. <p>{z$place_name} has {sub('^the', 'a', z$exp_r",
                      "ight)} higher than {z$perc} of other {z$scale_plural} w",
                      "ith {sub('^the', 'a', z$exp_left)} of '{z$val_left}' {z$geo}."), 
          fr = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} a u",
                      "ne population de {z$pop}, une valeur '{z$title_left}' d",
                      "e '{z$val_left}', et une valeur '{z$title_right}' de {z",
                      "$val_right}. <p>{z$place_name} a {sub('^le', 'un', z$ex",
                      "p_right)} plus élevé que {z$perc} des autres {z$scale_p",
                      "lural} avec {sub('^le', 'un', z$exp_left)} de '{z$val_l",
                      "eft}' {z$geo}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} has",
                      " a population of {z$pop}, a {z$title_left} value of '{z",
                      "$val_left}', and a '{z$title_right}' value of '{z$val_r",
                      "ight}'. <p>{z$place_name} has {sub('^the', 'a', z$exp_l",
                      "eft)} higher than {z$perc} of other {z$scale_plural} wi",
                      "th {sub('^the', 'a', z$exp_right)} of '{z$val_right}' {z$geo}."), 
          fr = paste0("TKTK")) |> 
  add_row(en = paste0("<p>From {z$start_date_left} to {z$end_date_left}, the c",
                      "hange in '{z$title_left}' had effectively no correlatio",
                      "n ({z$corr_disp}) with the change in '{z$title_right}' ",
                      "at the {z$scale_sing} scale.<p>This means that, at the ",
                      "{z$scale_sing} scale, there was no relationship between",
                      " the change in the two variables."), 
          fr = paste0("<p>De {z$start_date_left} à {z$end_date_left}, la varia",
                      "tion de la valeur '{z$title_left}' n'a effectivement au",
                      "cune corrélation avec la variation de la valeur '{z$tit",
                      "le_right}' à l'échelle {z$scale_sing}.<p>Cela signifie ",
                      "qu'à l'échelle {z$scale_sing}, il n'y avait pas de rela",
                      "tion entre la variation des deux variables.")) |> 
  add_row(en = paste0("<p>From {z$start_date_left} to {z$end_date_left}, the c",
                      "hange in '{z$title_left}' had a {z$strong} {z$pos} corr",
                      "elation ({z$corr_disp}) with the change in '{z$title_ri",
                      "ght}' at the {z$scale_sing} scale.<p>This means that, i",
                      "n general, {z$scale_plural} with a higher change in {z$",
                      "exp_left} tended to have a {z$higher} change in {z$exp_",
                      "right}, {z$high_low_disclaimer}."), 
          fr = paste0("<p>De {z$start_date_left} à {z$end_date_left}, la varia",
                      "tion de la valeur '{z$title_left}' avait une corrélation",
                      " {z$strong} et {z$pos} ({z$corr_disp}) avec la variation ",
                      "de la valeur '{z$title_right}' à l'échelle {z$scale_sin",
                      "g}.<p>Cela signifie qu'en général, les {z$scale_plural}",
                      " présentant une plus grande variation pour '{z$exp_left",
                      "}' ont tendance à avoir un/e {z$higher} variation pour ",
                      "'{z$exp_right}', {z$high_low_disclaimer}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>From {z$start_date",
                      "_left} to {z$end_date_left}, {z$place_name} had a chang",
                      "e in its '{z$title_left}' value of {z$val_left}, and a ",
                      "change in its '{z$title_right}' value of {z$val_right}.",
                      " <p>These two scores are {z$relative_position}, in rela",
                      "tive terms. {z$place_name} had a change in {z$exp_left}",
                      " higher than {z$perc_left} of {z$scale_plural} and a ch",
                      "ange in {z$exp_right} higher than {z$perc_right} of {z$",
                      "scale_plural} {z$geo}."), 
          fr = paste0("<strong>{z$place_heading}</strong><p>De {z$start_date_l",
                      "eft} à {z$end_date_left}, {z$place_name} a connu une va",
                      "riation de sa valeur '{z$title_left}' de {z$val_left}, ",
                      "et une variation de sa valeur '{z$title_right}' de {z$v",
                      "al_right}. <p>Ces deux résultats sont {z$relative_posit",
                      "ion}, en termes relatifs. {z$place_name} a eu une varia",
                      "tion pour '{z$exp_left}' supérieure à {z$perc_left} des",
                      " {z$scale_plural} et une variation pour '{z$exp_right}'",
                      " supérieure à {z$perc_right} des {z$scale_plural} {z$geo}.")) |> 
  add_row(en = paste0("<p>From {z$start_date_left} to {z$end_date_left}, the c",
                      "hange in '{z$title_left}' had effectively no correlatio",
                      "n (Spearman's rho: {z$corr_disp}) with the change in '{",
                      "z$title_right}' at the {z$scale_sing} scale.<p>This mea",
                      "ns that, at the {z$scale_sing} scale, there was no rela",
                      "tionship between the change in the two variables."), 
          fr = paste0("<p>Entre {z$start_date_left} et {z$end_date_left}, la v",
                      "ariation de la valeur '{z$title_left}' n'avait effectiv",
                      "ement aucune corrélation (rho de Spearman: {z$corr_disp",
                      "}) avec la variation de la valeur '{z$title_right}' à l",
                      "'échelle {z$scale_sing}.<p>Cela signifie qu'à l'échelle",
                      " {z$scale_sing}, il n'y avait aucune relation entre la ",
                      "variation des deux variables.")) |> 
  add_row(en = paste0("<p>From {z$start_date_left} to {z$end_date_left}, the c",
                      "hange in '{z$title_left}' had a {z$strong} {z$pos} corr",
                      "elation (Spearman's rho: {z$corr_disp}) with the change",
                      " in '{z$title_right}' at the {z$scale_sing} scale.<p>Th",
                      "is means that, in general, {z$scale_plural} with a high",
                      "er change in {z$exp_left} tended to have a {z$higher} c",
                      "hange in {z$exp_right}, {z$high_low_disclaimer}."), 
          fr = paste0("<p>De {z$start_date_left} à {z$end_date_left}, la varia",
                      "tion de la valeur '{z$title_left}' avait une",
                      " corrélation {z$strong} et {z$pos} (rho de Spearman: {z$corr_disp}) a",
                      "vec variation de la valeur '{z$title_right}' à l'échell",
                      "e {z$scale_sing}.<p>Cela signifie qu'en général, les {z",
                      "$scale_plural} présentant une plus grande variation du ",
                      "{sub('^le |^la ', '', z$exp_left)} avaient tendance à a",
                      "voir une {z$higher} variation du {sub('^le |^la ', '', ",
                      "z$exp_right)}, {z$high_low_disclaimer}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong><p>TKTK {z$place_name",
                      "} has a population of {z$pop}, a '{z$title_left}' value",
                      " of '{z$val_left}', and a '{z$title_right}' value of {z",
                      "$val_right}. <p>{z$place_name} has {sub('^the', 'a', z$",
                      "exp_right)} higher than {z$perc} of other {z$scale_plur",
                      "al} with {sub('^the', 'a', z$exp_left)} of '{z$val_left",
                      "}' {z$geo}."), 
          fr = paste0("TKTK")) |> 
  add_row(en = paste0("<p>During {z$date_left}, {z$exp_left} averaged {z$mean_",
                      "val} per day. The maximum value was {z$max_val} on {z$m",
                      "ax_date}, and the minimum value was {z$min_val} on {z$m",
                      "in_date}. There was no growth trend during this time pe",
                      "riod."), 
          fr = paste0("<p>En {z$date_left}, {z$exp_left} était en moyenne de {",
                      "z$mean_val} par jour. La valeur maximale était de {z$ma",
                      "x_val} le/les/à {z$max_date}, et la valeur minimale éta",
                      "it de {z$min_val} le/les/à {z$min_date}. Il n'y a pas e",
                      "u de tendances de croissance/décroissance pendant cette",
                      " période.")) |> 
  add_row(en = paste0("<p>During {z$date_left}, {z$exp_left} averaged {z$mean_",
                      "val} per day. The maximum value was {z$max_val} on {z$m",
                      "ax_date}, and the minimum value was {z$min_val} on {z$m",
                      "in_date}. There was a {z$strong} {z$pos} growth trend d",
                      "uring this time period, with {z$exp_left} {z$coef_incre",
                      "asing} an average of {z$coef} each day."), 
          fr = paste0("<p>En {z$date_left}, {z$exp_left} était en moyenne de {",
                      "z$mean_val} par jour. La valeur maximale était de {z$ma",
                      "x_val} le/les/à {z$max_date}, et la valeur minimale éta",
                      "it de {z$min_val} le/les/à {z$min_date}. Une {z$strong}",
                      " tendance à la croissance {z$pos} a été observée au cou",
                      "rs de cette période, {z$exp_left} {z$coef_increasing} e",
                      "n moyenne de {z$coef} chaque jour.")) |> 
  add_row(en = paste0("<p><b>STRONG CORRELATION</b></p>"), 
          fr = paste0("<p><b>FORTE CORRÉLATION</b></p>")) |> 
  add_row(en = paste0("<p><i>Data from {date_left} for '{z$title_left}' and ", 
                      "{date_right} for '{z$title_right}'.</i></p>"), 
          fr = paste0("<p><i>Données de {date_left} pour '{z$title_left}' et ", 
                      "de {date_right} pour '{z$title_right}'.</i></p>")) |> 
  
  add_row(en = paste0("<p>From {z$start_date_left} to {z$end_date_left}, the change in ", 
                      "'{z$title_left}' had effectively no correlation ({z$corr_disp}) ", 
                      "with {z$date_right}'s '{z$title_right}' at the {z$scale_sing} scale.",
                      "<p>This means that, at the {z$scale_sing} scale, there was no ",
                      "relationship between the change in {z$exp_left} with {z$exp_right}."), 
          fr = paste0("<p>De {z$start_date_left} à {z$end_date_left}, la ", 
                      "variation de la valeur '{z$title_left}' ne présente aucune ", 
                      "corrélation ({z$corr_disp}) avec ", 
                      "la valeur '{z$title_right}' de {z$date_right} à ", 
                      "l'échelle {z$scale_sing}. <p>Cela signifie qu'à l'échelle des",
                      " {z$scale_plural}, il n'y avait aucune relation entre ",
                      "la variation de/du {sub('^le |^la", " ', '', ", 
                      "z$exp_left)} et {z$exp_right}, {z$high_low_disclaimer}.")) |> 
  add_row(en = paste0("<p>From {z$start_date_left} to {z$end_date_left}, the ", 
                      "change in '{z$title_left}' had a {z$strong} {z$pos} ", 
                      "correlation ({z$corr_disp}) with {z$date_right}'s ", 
                      "'{z$title_right}' at the {z$scale_sing} scale.<p>This ", 
                      "means that, in general, {z$scale_plural} with a higher ", 
                      "change in {z$exp_left} tended to have a {z$higher} ", 
                      "value in {z$exp_right}, {z$high_low_disclaimer}."), 
          fr = paste0("<p>De {z$start_date_left} à {z$end_date_left}, la ", 
                      "variation de la valeur '{z$title_left}' présente une ", 
                      "corrélation {z$strong} et {z$pos} ({z$corr_disp}) avec ", 
                      "la valeur '{z$title_right}' de {z$date_right} à ", 
                      "l'échelle {z$scale_sing}. <p>Cela signifie qu'en ", 
                      "général, les {z$scale_plural} présentant une variation ", 
                      "plus importante de/du {sub('^le |^la", " ', '', ", 
                      "z$exp_left)} ont tendance à avoir une valeur {z$higher} ", 
                      "avec {z$exp_right}, {z$high_low_disclaimer}.")) |> 
  add_row(en = paste0("<strong>{z$place_heading}</strong>",
                      "<p>From {z$start_date_left} to {z$end_date_left}, {z$place_name} had ",
                      "a change in its '{z$title_left}' value of {z$val_left}. ",
                      "In {z$date_right}, '{z$title_right}' had a value of {z$val_right}. ",
                      "<p>These two scores are {z$relative_position}, in relative ",
                      "terms. {z$place_name} had a change in {z$exp_left} higher ",
                      "than {z$perc_left} of {z$scale_plural}. It also had ",
                      "{sub('^the', 'a', z$exp_right)} higher than {z$perc_right} ",
                      "of {z$scale_plural} {z$geo}."),
          fr = paste0("<strong>{z$place_heading}</strong><p>De {z$start_date_left} à ",
                      "{z$end_date_left}, {z$place_name} a connu une variation de sa valeur ",
                      "'{z$title_left}' de {z$val_left}. En {z$date_right}, ",
                      "‘{z$title_right}' avait une valeur de {z$val_right}.<p>",
                      "Ces deux scores sont {z$relative_position} en termes ",
                      "relatifs. {z$place_name} avait une variation pour ",
                      "{z$exp_left} supérieure à {z$perc_left} des {z$scale_plural}. ",
                      "Elle avait également {sub('^le', 'un', z$exp_right)} ",
                      "supérieur à celui de {z$perc_right} des {z$scale_plural} ",
                      "{z$geo}.")) |> 
          
  # Natural infrastructure
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% ",
                      "of the territory of the Montreal region. Preserving {sl",
                      "ider}% of the overall territory as natural infrastructu",
                      "re means that {conservation_pct}% of the natural infras",
                      "tructure of the region would be protected.</p><p>This l",
                      "evel of protection allows for the conservation of {floo",
                      "d} of the runoff reduction, {biodiversity} of the biodi",
                      "versity conservation, and {heat_island} of the urban he",
                      "at island reduction effects provided by natural infrast",
                      "ructure in the region.</p>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ",
                      "25 % du territoire de la région de Montréal. En préserv",
                      "ant {slider} % du territoire global en tant qu'infrastru",
                      "cture naturelle, {conservation_pct} % de l'infrastructur",
                      "e naturelle de la région serait protégée.</p>",
                      "<p>Ce niveau de protection permet de conserver {flood}",
                      " de la réduction du ruissellement, {biodiversity} de",
                      " la conservation de la biodiversité et {heat_island} ",
                      "des effets de réduction des îlots de chaleur urbains fo",
                      "urnis par les infrastructures naturelles de la région.<",
                      "/p>")) |> 
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% of the ",
                      "territory of the Montreal region. Biodiversity-related natural ", 
                      "infrastructure functions include:</p><ul>{c_bio}{habitat_qual}",
                      "{habitat_con}{favorable_cc}</ul>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ",
                      "25 % du territoire de la région de Montréal. Les foncti",
                      "ons des infrastructures naturelles liées à la biodivers",
                      "ité sont les suivantes :</p><ul>{c_bio}{habitat_qual}",
                      "{habitat_con}{favorable_cc}</ul>")) |> 
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% of the ",
                      "territory of the Montreal region. Flood-related natural ", 
                      "infrastructure functions include:</p><ul>{c_flood}{flood}</ul>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ", 
                      "25 % du territoire de la région de Montréal. Les foncti", 
                      "ons des infrastructures naturelles liées aux inondation", 
                      "s sont les suivantes :</p><ul>{c_flood}{flood}</ul>")) |> 
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% of the ",
                      "territory of the Montreal region. Heat-related natural ", 
                      "infrastructure functions include:</p><ul>{c_heat}{heat}{cool}</ul>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ", 
                      "25 % du territoire de la région de Montréal. Les foncti", 
                      "ons des infrastructures naturelles liées à la températu", 
                      "re sont les suivantes:</p><ul>{c_heat}{heat}{cool}</ul>")) |> 
  # Centraide related
  add_row(en = paste0("centraide zone"), 
          fr = paste0("du quartier centraide")) |> 
  add_row(en = paste0("Centraide zone"), 
          fr = paste0("Quartier centraide")) |> 
  add_row(en = paste0("centraide zones"), 
          fr = paste0("quartiers Centraide")) |> 
  add_row(en = paste0("Centraide zone {select_name$name}"), 
          fr = paste0("Quartier Centraide {select_name$name}")) |> 
  # CMHC
  add_row(en = paste0("cmhc zone"), 
          fr = paste0("des zones SCHL")) |> 
  add_row(en = paste0("CMHC zone"), 
          fr = paste0("de la zone SCHL")) |> 
  add_row(en = paste0("CMHC Zone"), 
          fr = paste0("Zone SCHL")) |> 
  add_row(en = paste0("CMHC zones"), 
          fr = paste0("zones SCHL")) |> 
  add_row(en = paste0("CMHC zone {select_name$name}"), 
          fr = paste0("Zone SCHL {select_name$name}")) |> 
  
  add_row(en = paste0("We have no data on {z$exp_left} at the {z$scale_sing} sc",
                      "ale."), 
          fr = paste0("Nous ne disposons pas de données sur {z$exp_left} à l'éc",
                      "helle {z$scale_sing}.")) |> 
  add_row(en = paste0("We have no data on {z$exp_left} or on {z$exp_right} at the",
                      " {z$scale_sing} scale."), 
          fr = paste0("Nous ne disposons pas de données sur {z$exp_left} ou sur ",
                      "{z$exp_right} à l'échelle {z$scale_sing}."))

