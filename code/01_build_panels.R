# =============================================================================
# 01_build_panels.R
# Political Alignment and Natural Disaster Recognition in France
#
# Builds all analysis panels from raw data sources:
#   - panel_couleur_cat_elec : annual panel, large municipalities (Period 1)
#   - panel_cat_elec         : annual panel, all municipalities (Period 2)
#   - panel_maire_heck       : electoral-cycle panel for Heckman model
#
# Output files are saved to PATH_OUTPUT.
# Raw data sources are described in data/README_data.md.
# =============================================================================

# -----------------------------------------------------------------------------
# -- 0. PATHS — set PATH_ROOT to your local data directory
# -----------------------------------------------------------------------------
PATH_ROOT        <- "C:/sdrive/CATNAT/couleur_catnat/REPO"



PATH_CATNAT_DIR  <- file.path("C:/sdrive/CATNAT/couleur_catnat/DATA_Catnat/")
PATH_COM_NEW     <- file.path(PATH_ROOT, "raw_data/com_new.csv")          # distributed in repo
PATH_EMDAT       <- file.path(PATH_ROOT, "raw_data/EMDAT_FR_2000_2026.xlsx")
PATH_METEO_YEAR  <- file.path(PATH_ROOT, "raw_data/meteo_commune_year_2000_2023.csv")
PATH_METEO_MONTH <- file.path(PATH_ROOT, "raw_data/meteo_commune_month_2000_2023.csv")
PATH_PLUIE_JOUR  <- file.path(PATH_ROOT, "raw_data/pluie_jour.csv")
PATH_CIC         <- file.path(PATH_ROOT, "raw_data/comptes-individuels-des-communes-fichier-global-a-compter-de-2000.csv")
PATH_PPRN        <- file.path(PATH_ROOT, "raw_data/pprn_gaspar.csv")
PATH_REVENU      <- file.path(PATH_ROOT, "raw_data/REVENU_MEDIAN_2000_2024.csv")
PATH_POPULATION  <- file.path(PATH_ROOT, "raw_data/POPULATION.csv")
PATH_EPCI        <- file.path(PATH_ROOT, "raw_data/epci23.csv")
PATH_MAIRE_COULEUR   <- file.path(PATH_ROOT, "raw_data/panel_maire_couleur.csv")
PATH_MAIRE_ELECTION  <- file.path(PATH_ROOT, "raw_data/panel_maire_election.csv")
PATH_PRESIDENT       <- file.path(PATH_ROOT, "raw_data/panel_president.csv")
PATH_LEGISLATIVE     <- file.path(PATH_ROOT, "raw_data/panel_legislative_commune.csv")
PATH_OUTPUT      <- file.path(PATH_ROOT, "output/")

# -----------------------------------------------------------------------------
# -- 1. PACKAGES ----
# -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)
library(data.table)
library(stringr)
library(stringi)
library(jsonlite)
library(zoo)

# -----------------------------------------------------------------------------
# -- 2. RAW CATNAT DATA  -------- 
# (source: CCR, https://catastrophes-naturelles.ccr.fr)
# Download annual CSV files from the CCR website and store them in
# PATH_CATNAT_DIR/YYYY/ subfolders (one subfolder per year).
# -----------------------------------------------------------------------------
liste_donnees <- list()
for (annee in 2000:2024) {
  fichiers <- list.files(file.path(PATH_CATNAT_DIR, annee),
                         pattern = "\\.csv$", full.names = TRUE)
  for (fichier in fichiers) {
    data <- read_delim(
      fichier, delim = ";", escape_double = FALSE,
      col_types = cols(
        `N° Insee`          = col_character(),
        `Début d'Evénement` = col_date(format = "%d/%m/%Y"),
        `Fin d'Evénement`   = col_date(format = "%d/%m/%Y"),
        `Arrêté du`         = col_date(format = "%d/%m/%Y"),
        `Parution au JO du` = col_date(format = "%d/%m/%Y")
      ),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    ) %>% mutate(Annee = annee)
    liste_donnees <- append(liste_donnees, list(data))
  }
}

catnat_raw <- bind_rows(liste_donnees) %>%
  arrange(`N° Insee`, Annee) %>%
  rename(cod_commune = `N° Insee`, an = Annee) %>%
  mutate(
    dep      = substr(cod_commune, 1, 2),
    year_cat = year(`Début d'Evénement`),
    year_jo  = year(`Parution au JO du`),
    catnat_T = as.integer(`Décision` != "Non reconnue"),
    catnat_F = as.integer(`Décision` == "Non reconnue"),
    catnat   = 1,
    delai_Jo = as.Date(`Parution au JO du`) - as.Date(`Fin d'Evénement`),
    cat_type = case_when(
      `Nom du péril` %in% c("Sécheresse", "Inondations Remontée Nappe")                                          ~ "Drought",
      `Nom du péril` %in% c("Inondations et/ou Coulées de Boue", "Lave Torrentielle")                            ~ "Floods",
      `Nom du péril` %in% c("Chocs Mécaniques liés à l'action des Vagues","Vents Cycloniques","Raz de Marée")    ~ "Storm",
      `Nom du péril` %in% c("Mouvement de Terrain","Effondrement et/ou Affaisement",
                            "Glissement de Terrain","Eboulement et/ou Chute de Blocs")                          ~ "Landslide",
      `Nom du péril` == "Secousse Sismique"                                                                      ~ "Seismic",
      `Nom du péril` %in% c("Algues Sargasses","Eruption Volcanique","Avalanche","Grêle","Poids de la Neige")    ~ "Other",
      TRUE ~ `Nom du péril`
    )
  ) %>%
  filter(year_cat > 1999, !dep %in% c("97", "98")) %>%
  mutate(cod_commune = str_pad(as.character(cod_commune), 5, pad = "0"),
         year_cat    = as.integer(year_cat))

# --- commune mergers ---
com_new <- read_csv(PATH_COM_NEW)

catnat_init <- catnat_raw %>%
  full_join(com_new) %>%
  filter(!is.na(catnat_T)) %>%
  mutate(
    cod_commune_new = ifelse(is.na(cod_commune_new), cod_commune, cod_commune_new),
    cod_commune     = ifelse(year_cat > 2015, cod_commune_new, cod_commune)
  ) %>%
  select(-cod_commune_new) %>%
  distinct()

# -----------------------------------------------------------------------------
# -- 3. EM-DAT MATCHING ----
# flag extreme events at arrêté level 
# -----------------------------------------------------------------------------
EMDAT_FR <- read_excel(PATH_EMDAT)
K <- 7  # date tolerance in days

norm <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower() %>% gsub("[^a-z0-9]", "", .)

dep_lookup <- c(
  "ain"="01","aisne"="02","allier"="03","alpesdehauteprovence"="04","hautesalpes"="05",
  "alpesmaritimes"="06","ardeche"="07","ardennes"="08","ariege"="09","aube"="10","aude"="11",
  "aveyron"="12","bouchesdurhone"="13","calvados"="14","cantal"="15","charente"="16",
  "charentemaritime"="17","cher"="18","correze"="19","corsedusud"="2A","hautecorse"="2B",
  "cotedor"="21","cotesdarmor"="22","creuse"="23","dordogne"="24","doubs"="25","drome"="26",
  "eure"="27","eureetloir"="28","finistere"="29","gard"="30","hautegaronne"="31","gers"="32",
  "gironde"="33","herault"="34","illeetvilaine"="35","indre"="36","indreetloire"="37",
  "isere"="38","jura"="39","landes"="40","loiretcher"="41","loire"="42","hauteloire"="43",
  "loireatlantique"="44","loiret"="45","lot"="46","lotetgaronne"="47","lozere"="48",
  "maineetloire"="49","manche"="50","marne"="51","hautemarne"="52","mayenne"="53",
  "meurtheetmoselle"="54","meuse"="55","morbihan"="56","moselle"="57","nievre"="58","nord"="59",
  "oise"="60","orne"="61","pasdecalais"="62","puydedome"="63","pyreneesatlantiques"="64",
  "pyreneesatlantique"="64","hautespyrenees"="65","pyreneesorientales"="66","basrhin"="67",
  "hautrhin"="68","rhone"="69","hautesaone"="70","saoneetloire"="71","sarthe"="72","savoie"="73",
  "hautesavoie"="74","paris"="75","seinemaritime"="76","seineetmarne"="77","yvelines"="78",
  "deuxsevres"="79","somme"="80","tarn"="81","tarnetgaronne"="82","var"="83","vaucluse"="84",
  "vendee"="85","vienne"="86","hautevienne"="87","vosges"="88","yonne"="89",
  "territoiredebelfort"="90","essonne"="91","hautsdeseine"="92","seinesaintdenis"="93",
  "valdemarne"="94","valdoise"="95"
)

reg_lookup <- list(
  alsace=c("67","68"), aquitaine=c("24","33","40","47","64"), auvergne=c("03","15","43","63"),
  bassenormandie=c("14","50","61"), bourgogne=c("21","58","71","89"), bretagne=c("22","29","35","56"),
  centre=c("18","28","36","37","41","45"), champagneardenne=c("08","10","51","52"), corse=c("2A","2B"),
  franchecomte=c("25","39","70","90"), hautenormandie=c("27","76"),
  iledefrance=c("75","77","78","91","92","93","94","95"),
  languedocrousillon=c("11","30","34","48","66"), limousin=c("19","23","87"),
  lorraine=c("54","55","57","88"), midipyrenees=c("09","12","31","32","46","65","81","82"),
  nordpasdecalais=c("59","62"), paysdelaloire=c("44","49","53","72","85"),
  picardie=c("02","60","80"), poitoucharentes=c("16","17","79","86"),
  provencealpescotedazur=c("04","05","06","13","83","84"),
  rhonealpes=c("01","07","26","38","42","69","73","74")
)

extract_deps <- function(js) {
  if (is.na(js) || js == "") return(character(0))
  p <- tryCatch(fromJSON(js), error = function(e) NULL)
  if (is.null(p)) return(character(0))
  out <- character(0)
  if (!is.null(p$adm2_name)) {
    codes <- dep_lookup[norm(na.omit(p$adm2_name))]
    out   <- c(out, codes[!is.na(codes)])
  }
  if (length(out) == 0 && !is.null(p$adm1_name)) {
    for (rn in norm(na.omit(p$adm1_name)))
      if (!is.null(reg_lookup[[rn]])) out <- c(out, reg_lookup[[rn]])
  }
  unique(out)
}

grp_emdat <- function(t) dplyr::case_when(
  t == "Flood"  ~ "Floods",
  t == "Storm"  ~ "Storm",
  t %in% c("Drought", "Wildfire", "Extreme temperature") ~ "Drought",
  TRUE ~ NA_character_
)
grp_catnat <- function(t) dplyr::case_when(
  t %in% c("Floods", "Storm", "Drought") ~ t,
  TRUE ~ NA_character_
)

emdat_iv <- EMDAT_FR %>%
  filter(`Start Year` %in% 2000:2024) %>%
  transmute(
    type_grp = grp_emdat(`Disaster Type`),
    sd  = make_date(`Start Year`, `Start Month`, coalesce(`Start Day`, 1L)),
    ed  = if_else(
      is.na(`End Day`),
      ceiling_date(make_date(`End Year`, `End Month`, 1L), "month") - days(1),
      make_date(`End Year`, `End Month`, `End Day`)
    ),
    dep = lapply(`Admin Units`, extract_deps)
  ) %>%
  unnest(dep) %>%
  transmute(dep, type_grp, win_start = sd - days(K), win_end = ed + days(K)) %>%
  as.data.table()
setkey(emdat_iv, dep, type_grp, win_start, win_end)

catnat <- as.data.table(catnat_init)
catnat[, dep      := substr(cod_commune, 1, 2)]
catnat[, type_grp := grp_catnat(cat_type)]
catnat[, cd_end   := as.Date(`Fin d'Evénement`)]
catnat[, cd_start := as.Date(`Début d'Evénement`)]
catnat[is.na(cd_start), cd_start := cd_end]
catnat[, rid := .I]

ov <- foverlaps(
  catnat[!is.na(cd_start) & !is.na(cd_end), .(rid, dep, type_grp, cd_start, cd_end)],
  emdat_iv,
  by.x = c("dep", "type_grp", "cd_start", "cd_end"),
  by.y = c("dep", "type_grp", "win_start", "win_end"),
  type = "any", nomatch = 0L
)

catnat[, emdat_extreme := as.integer(rid %in% ov$rid)]
catnat <- as_tibble(catnat) %>% select(-rid, -cd_start, -cd_end, -type_grp)

# -----------------------------------------------------------------------------
# -- 4. WEATHER DATA ----
# -----------------------------------------------------------------------------
meteo_commune <- read_csv(PATH_METEO_YEAR) %>%
  mutate(dep = substr(cod_commune, 1, 2)) %>%
  filter(!dep %in% "97") %>% select(-dep)

pluie_jour <- read_csv(PATH_PLUIE_JOUR) %>%
  mutate(dep = substr(cod_commune, 1, 2)) %>%
  filter(!dep %in% "97") %>% select(-dep) %>%
  filter(year < 2022) %>% select(cod_commune, year, rr)
meteo_commune <- meteo_commune %>% left_join(pluie_jour)

meteo_month <- read_csv(PATH_METEO_MONTH) %>%
  mutate(year = year(datenum), diff_temp = TX - TN) %>%
  group_by(cod_commune, year) %>%
  summarise(diff_temp = max(diff_temp), .groups = "drop")
meteo_commune <- meteo_commune %>% left_join(meteo_month)

# Duplicate weather for old communes (before mergers)
meteo_anciennes <- com_new %>%
  left_join(meteo_commune, by = c("cod_commune_new" = "cod_commune")) %>%
  rename(cod_commune = cod_commune)

meteo_complete <- bind_rows(meteo_commune, meteo_anciennes) %>%
  distinct(cod_commune, year, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# -- 5. MUNICIPAL BUDGET DATA -----
# -----------------------------------------------------------------------------
CIC <- read.csv(PATH_CIC, sep = ";") %>%
  select(an, dep, icom, pop1, fprod, fimpo1, fimpo2, fdgf, fcharge, fperso, fachat,
         ffin, fsubv, frecinv, femp, fsubr, fdepinv, fequip, fremb, fdette) %>%
  arrange(dep, icom, an) %>%
  mutate(
    dep        = substr(dep, nchar(dep) - 1, nchar(dep)),
    com        = str_pad(as.character(icom), 3, "left", "0"),
    cod_commune = paste0(dep, com)
  ) %>%
  rename(year = an) %>%
  select(-dep, -icom) %>%
  mutate(
    totalex   = fcharge + fdepinv,
    totalgrant = fdgf + fsubr,
    totalrev  = fprod + frecinv,
    totaltax  = fimpo1 + fimpo2,
    debt      = fdette
  )

# -----------------------------------------------------------------------------
# -- 6. AGGREGATE CATNAT TO ANNUAL PANEL ----
# -----------------------------------------------------------------------------
cols_zero <- c(
  "catnat", "catnat_T", "catnat_F",
  "Drought_T", "Drought_F", "Drought_T_ext", "Drought_F_ext", "Drought_T_noex", "Drought_F_noex",
  "Floods_T", "Floods_F", "Floods_T_ext", "Floods_F_ext", "Floods_T_noex", "Floods_F_noex",
  "catnat_T_ext", "catnat_F_ext", "catnat_T_noex", "catnat_F_noex",
  "catnat_ext", "catnat_noex", "n_emdat"
)

catnat_an <- catnat %>%
  mutate(emdat_extreme = replace_na(emdat_extreme, 0)) %>%
  group_by(cod_commune, year_jo) %>%
  summarise(
    year_cat_max   = max(year_cat),
    year_cat_min   = min(year_cat),
    Drought_T      = sum(cat_type == "Drought" & catnat_T == 1),
    Drought_F      = sum(cat_type == "Drought" & catnat_F == 1),
    Drought_T_ext  = sum(cat_type == "Drought" & catnat_T == 1 & emdat_extreme == 1),
    Drought_F_ext  = sum(cat_type == "Drought" & catnat_F == 1 & emdat_extreme == 1),
    Drought_T_noex = sum(cat_type == "Drought" & catnat_T == 1 & emdat_extreme == 0),
    Drought_F_noex = sum(cat_type == "Drought" & catnat_F == 1 & emdat_extreme == 0),
    Floods_T      = sum((is.na(cat_type) | cat_type != "Drought") & catnat_T == 1),
    Floods_F      = sum((is.na(cat_type) | cat_type != "Drought") & catnat_F == 1),
    Floods_T_ext  = sum((is.na(cat_type) | cat_type != "Drought") & catnat_T == 1 & emdat_extreme == 1),
    Floods_F_ext  = sum((is.na(cat_type) | cat_type != "Drought") & catnat_F == 1 & emdat_extreme == 1),
    Floods_T_noex = sum((is.na(cat_type) | cat_type != "Drought") & catnat_T == 1 & emdat_extreme == 0),
    Floods_F_noex = sum((is.na(cat_type) | cat_type != "Drought") & catnat_F == 1 & emdat_extreme == 0),
    catnat_T_ext  = sum(catnat_T == 1 & emdat_extreme == 1),
    catnat_F_ext  = sum(catnat_F == 1 & emdat_extreme == 1),
    catnat_T_noex = sum(catnat_T == 1 & emdat_extreme == 0),
    catnat_F_noex = sum(catnat_F == 1 & emdat_extreme == 0),
    catnat_ext    = sum(emdat_extreme == 1),
    catnat_noex   = sum(emdat_extreme == 0),
    n_emdat       = sum(emdat_extreme),
    catnat_T      = sum(catnat_T),
    catnat_F      = sum(catnat_F),
    catnat        = sum(catnat),
    .groups = "drop"
  )

# Derived demand-side variables
catnat_an <- catnat_an %>%
  mutate(
    Drought_req = Drought_T + Drought_F,
    Floods_req  = Floods_T  + Floods_F,
    catnat_noex2 = Drought_T + Floods_T_noex + Drought_F + Floods_F_noex
  )

# Merge with weather
catnat_an_meteo <- catnat_an %>%
  rename(year = year_jo) %>%
  full_join(meteo_commune) %>%
  arrange(cod_commune, year)

# Merge with budget
catnat_an_meteo_cic <- catnat_an_meteo %>%
  left_join(CIC) %>%
  filter(year < 2023) %>%
  select(
    cod_commune, year, year_cat_max, year_cat_min,
    catnat, catnat_T, catnat_F,
    Drought_T, Drought_F, Drought_T_ext, Drought_F_ext, Drought_T_noex, Drought_F_noex,
    Floods_T, Floods_F, Floods_T_ext, Floods_F_ext, Floods_T_noex, Floods_F_noex,
    catnat_T_ext, catnat_F_ext, catnat_T_noex, catnat_F_noex,
    catnat_ext, catnat_noex, catnat_noex2, n_emdat,
    Drought_req, Floods_req,
    rr, diff_temp, pluie, max_pluie, t_max_mean, t_max_max, t_min_min,
    ampli_temp_max, vite_vent_max, soleil,
    totalex, fdepinv, totalrev, totalgrant, fsubr, totaltax, debt, fdgf
  ) %>%
  mutate(an = year)

# -----------------------------------------------------------------------------
# -- 7. BUILD base_complete (municipality x year panel) ----
# -----------------------------------------------------------------------------
base_complete <- expand.grid(
  cod_commune = unique(meteo_complete$cod_commune),
  an          = 2000:2022
) %>%
  mutate(dep = substr(cod_commune, 1, 2)) %>%
  arrange(cod_commune, an) %>%
  full_join(catnat_an_meteo_cic, by = c("cod_commune", "an")) %>%
  arrange(cod_commune, an)

# Median income
revenu <- read_csv(PATH_REVENU) %>% mutate(dep = substr(cod_commune, 1, 2))
base_complete <- base_complete %>% full_join(revenu) %>% distinct()

# Population (interpolated)
pop_full <- read_csv(PATH_POPULATION) %>%
  rename(an = year) %>%
  group_by(cod_commune) %>%
  complete(an = 2000:2021) %>%
  arrange(cod_commune, an) %>%
  mutate(
    total_pop = na.approx(total_pop, an, na.rm = FALSE),
    pop_20    = na.approx(pop_20,    an, na.rm = FALSE),
    pop_65    = na.approx(pop_65,    an, na.rm = FALSE)
  ) %>%
  ungroup()

base_complete <- base_complete %>%
  full_join(pop_full) %>%
  group_by(cod_commune) %>%
  fill(total_pop, pop_20, pop_65, MEDREV, .direction = "down") %>%
  ungroup()

# PPRN (prevention plan)
pprn <- read_delim(PATH_PPRN, delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  arrange(cod_commune) %>%
  mutate(date_pprn = year(dat_approbation)) %>%
  group_by(cod_commune) %>%
  summarise(date_pprn = min(date_pprn, na.rm = TRUE)) %>%
  mutate(date_pprn = ifelse(date_pprn == Inf, NA, date_pprn))

base_complete <- base_complete %>%
  full_join(pprn) %>%
  mutate(PPRN = ifelse(is.na(date_pprn) | date_pprn > an, 0, 1))

# Zero-fill catnat counts for municipality-years with no events
base_complete <- base_complete %>%
  mutate(across(all_of(cols_zero), ~ replace_na(., 0))) %>%
  distinct() %>%
  arrange(cod_commune, an)

# EPCI membership
epci <- read_csv(PATH_EPCI) %>%
  mutate(EPCI = case_when(
    nj_epci == "CC" ~ "CC",
    is.na(nj_epci)  ~ "isolated",
    TRUE            ~ "integrated"
  ))
base_complete <- base_complete %>%
  left_join(epci %>% select(cod_commune, an, EPCI))

# Additional variables
base_complete <- base_complete %>%
  mutate(
    diff_temperature = t_max_max - t_min_min,
    p_pop_65 = pop_65 / total_pop,
    p_pop_20 = pop_20 / total_pop
  )

# -----------------------------------------------------------------------------
# -- 8. PANEL FOR PERIOD 1 (large municipalities, mayor color known) ----
# -----------------------------------------------------------------------------
nettoyer_chaine <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^A-Z0-9]", "")
}

panel_maire_couleur <- read_csv(PATH_MAIRE_COULEUR) %>%
  mutate(dep = substr(cod_commune, 1, 2)) %>%
  filter(!dep %in% c("ZA","ZB","ZC","ZD","ZM","ZN","ZP","ZS", NA)) %>%
  mutate(across(c(nom, prenom), nettoyer_chaine))

maires_couleurs <- panel_maire_couleur %>%
  mutate(nuance = na_if(nuance, "LNC")) %>%
  arrange(cod_commune, nom, prenom, nuance) %>%
  group_by(cod_commune, nom, prenom) %>%
  summarise(nuance_maire = first(na.omit(nuance)), .groups = "drop")

panel_couleur_cat_elec <- expand.grid(
  cod_commune = unique(panel_maire_couleur$cod_commune),
  an          = 2008:2022
) %>%
  arrange(cod_commune, an) %>%
  left_join(panel_maire_couleur %>% mutate(nuance = na_if(nuance, "LNC")),
            by = c("cod_commune", "an")) %>%
  left_join(maires_couleurs, by = c("cod_commune", "nom", "prenom")) %>%
  mutate(nuance = coalesce(nuance, "LNC")) %>%
  select(-couleur_pol) %>%
  mutate(
    nuance = case_when(
      nuance_maire == "LSOC" ~ nuance_maire,
      nuance == "LNC"        ~ nuance_maire,
      TRUE                   ~ nuance
    ),
    nuance = ifelse(is.na(nuance), "LNC", nuance),
    couleur_pol = case_when(
      nuance %in% c("LDVG","LFG","LSOC","LEXG","LRDG","LVEC","LUG","LGC","LPG","LECO","LCOM") ~ "Gauche",
      nuance %in% c("LDVD","LUD","LUDI","LUMP","LLR","LMAJ","LDIV")                           ~ "Droite",
      nuance %in% c("LCMD","LMC","LMDM","LREM","LDVC","LUC")                                  ~ "Centre",
      nuance %in% c("LRN","LEXD")                                                              ~ "Extreme droite",
      nuance %in% c("LNC","NC")                                                                ~ "Sans etiquette",
      is.na(nuance)                                                                            ~ "Non renseigne",
      TRUE                                                                                     ~ "Divers"
    )
  ) %>%
  select(-nuance_maire) %>%
  distinct() %>%
  arrange(cod_commune, an)

panel_president        <- read_csv(PATH_PRESIDENT)
panel_legislative      <- read_csv(PATH_LEGISLATIVE) %>% select(-Nom, -Prénom)

panel_couleur_cat_elec <- panel_couleur_cat_elec %>%
  select(-dep) %>%
  left_join(base_complete, by = c("cod_commune", "an")) %>%
  distinct() %>%
  filter(an < 2023) %>%
  left_join(panel_president,   by = c("cod_commune", "an")) %>%
  left_join(panel_legislative, by = c("cod_commune", "an")) %>%
  distinct() %>%
  mutate(
    tx_reco          = catnat_T / catnat * 100,
    tx_reco_noex2    = (Drought_T + Floods_T_noex) /
      (Drought_T + Drought_F + Floods_T_noex + Floods_F_noex) * 100,
    tx_reco_drought  = Drought_T / (Drought_T + Drought_F) * 100,
    tx_reco_drought_noex = Drought_T_noex / (Drought_T_noex + Drought_F_noex) * 100,
    tx_reco_floods   = Floods_T / (Floods_T + Floods_F) * 100,
    tx_reco_floods_noex  = Floods_T_noex / (Floods_T_noex + Floods_F_noex) * 100
  ) %>%
  distinct(cod_commune, an, nom, prenom, nuance, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# -- 9. PANEL FOR PERIOD 2 (all municipalities, presidential alignment) ----
# -----------------------------------------------------------------------------
panel_cat_elec <- base_complete %>%
  full_join(panel_president,   by = c("cod_commune", "an")) %>%
  full_join(panel_legislative, by = c("cod_commune", "an")) %>%
  distinct() %>%
  mutate(
    tx_reco         = catnat_T / catnat * 100,
    tx_reco_noex2   = (Drought_T + Floods_T_noex) /
      (Drought_T + Drought_F + Floods_T_noex + Floods_F_noex) * 100,
    tx_reco_drought = Drought_T / (Drought_T + Drought_F) * 100,
    tx_reco_drought_noex = Drought_T_noex / (Drought_T_noex + Drought_F_noex) * 100,
    tx_reco_floods  = Floods_T / (Floods_T + Floods_F) * 100,
    tx_reco_floods_noex  = Floods_T_noex / (Floods_T_noex + Floods_F_noex) * 100
  )

# -----------------------------------------------------------------------------
# -- 10. HECKMAN PANEL (electoral-cycle panel, 2008 / 2014 / 2020) -----
# -----------------------------------------------------------------------------
panel4ans <- base_complete %>%
  filter(an > 2000) %>%
  mutate(mandat_maire = case_when(
    an %in% 2001:2007 ~ 2001,
    an %in% 2008:2013 ~ 2008,
    an %in% 2014:2019 ~ 2014,
    an > 2019         ~ 2020
  )) %>%
  group_by(cod_commune, mandat_maire) %>%
  summarise(
    catnat_T = sum(catnat_T), catnat_F = sum(catnat_F), catnat = sum(catnat),
    Drought_T = sum(Drought_T), Drought_F = sum(Drought_F),
    Floods_T  = sum(Floods_T),  Floods_F  = sum(Floods_F),
    catnat_T_noex = sum(catnat_T_noex), catnat_F_noex = sum(catnat_F_noex),
    catnat_noex = sum(catnat_noex), n_emdat = sum(n_emdat),
    pluie = max(pluie, na.rm = TRUE), t_max_max = max(t_max_max, na.rm = TRUE),
    t_min_min = min(t_min_min, na.rm = TRUE), vite_vent_max = max(vite_vent_max, na.rm = TRUE),
    totalex = mean(totalex, na.rm = TRUE), fdepinv = mean(fdepinv, na.rm = TRUE),
    debt = mean(debt, na.rm = TRUE), MEDREV = mean(MEDREV, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE), pop_65 = mean(pop_65, na.rm = TRUE),
    date_pprn = min(date_pprn, na.rm = TRUE), PPRN = max(PPRN), EPCI = first(EPCI),
    .groups = "drop"
  ) %>%
  rename(an = mandat_maire) %>%
  mutate(p_pop_65 = pop_65 / total_pop)

panel4ans[panel4ans == -Inf] <- NA
panel4ans[panel4ans ==  Inf] <- NA

panel_maire_election <- read_csv(PATH_MAIRE_ELECTION)

panel_maire_election <- panel4ans %>%
  right_join(panel_maire_election) %>%
  left_join(panel_president)       %>%
  left_join(panel_legislative)     %>%
  distinct()

catnat_heck <- catnat_raw %>%
  select(-an) %>% rename(an = year_cat) %>%
  filter(an %in% 2000:2020) %>%
  mutate(
    mandat_maire = case_when(
      an %in% 2000:2008 ~ 2008,
      an %in% 2009:2014 ~ 2014,
      an %in% 2015:2020 ~ 2020
    ),
    catnat = 1
  ) %>%
  group_by(cod_commune, mandat_maire) %>%
  summarise(catnat = sum(catnat), catnat_T = sum(catnat_T), catnat_F = sum(catnat_F),
            .groups = "drop") %>%
  rename(an = mandat_maire)

panel_maire_heck <- catnat_heck %>%
  full_join(
    panel_maire_election %>%
      filter(an %in% c(2008, 2014, 2020)) %>%
      distinct(cod_commune, an, .keep_all = TRUE) %>%
      select(-catnat, -catnat_T, -catnat_F),   # <-- retirer les colonnes en conflit
    by = c("cod_commune", "an")
  ) %>%
  arrange(cod_commune, an) %>%
  mutate(
    catnat   = ifelse(is.na(catnat),   0, catnat),
    catnat_T = ifelse(is.na(catnat_T), 0, catnat_T),
    catnat_F = ifelse(is.na(catnat_F), 0, catnat_F),
    catnat_cycle_u   = as.integer(catnat   > 0),
    catnat_T_cycle_u = as.integer(catnat_T > 0),
    catnat_F_cycle_u = as.integer(catnat_F > 0),
    age2          = age^2,
    seniority2    = seniority^2,
    couleur_maire = ifelse(is.na(couleur_pol), "SANSETIQUETTE", couleur_pol),
    gender        = as.integer(genre == "H")
  ) %>%
  distinct()

# -----------------------------------------------------------------------------
# -- 11. SAVE OUTPUT PANELS ----
# -----------------------------------------------------------------------------
fwrite(panel_couleur_cat_elec, file = file.path(PATH_OUTPUT, "panel_couleur_cat_elec_EMDAT.csv"), na = "NA")
fwrite(panel_cat_elec,         file = file.path(PATH_OUTPUT, "panel_cat_elec_EMDAT.csv"),         na = "NA")
fwrite(panel_maire_heck,       file = file.path(PATH_OUTPUT, "panel_maire_heck.csv"),             na = "NA")
fwrite(base_complete,          file = file.path(PATH_OUTPUT, "base_complete_EMDAT.csv"),          na = "NA")

message("01_build_panels.R done. Output files written to ", PATH_OUTPUT)
