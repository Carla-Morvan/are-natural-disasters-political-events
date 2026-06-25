# Data Documentation

## Raw Data Sources

### Natural Disaster Data (CatNat)
- **Source**: Caisse Centrale de Réassurance (CCR)
- **URL**: https://catastrophes-naturelles.ccr.fr/les-arretes
- **Coverage**: All natural disaster recognition requests and decisions in France, 2000--2024
- **Format**: Annual CSV files, one subfolder per year. Download manually and store in `raw_data/catnat/YYYY/`
- **Key variables**: municipality code, disaster type, start/end date, JO publication date, decision (recognized/not recognized)

### EM-DAT Major Disaster Events
- **Source**: EM-DAT International Disaster Database, Centre for Research on the Epidemiology of Disasters (CRED)
- **URL**: https://www.emdat.be
- **Coverage**: Major disaster events in France, 2000--2026
- **Format**: `.xlsx` file, downloaded manually. Store as `raw_data/EMDAT_FR_2000_2026.xlsx`
- **Note**: Used to flag extreme events (>= 10 fatalities, >= 100 affected, state of emergency, or international assistance call). Droughts are excluded from this filter due to geographic scale mismatch with CatNat data.

### Weather Data
- **Source**: Météo-France, aggregated at municipality level
- **URL**: https://donneespubliques.meteofrance.fr/
- **Files**:
  - `meteo_commune_year_2000_2023.csv` — annual weather statistics per municipality
  - `meteo_commune_month_2000_2023.csv` — monthly weather statistics per municipality
  - `pluie_jour.csv` — daily rainfall aggregated to annual per municipality
- **Key variables**: `pluie` (annual rainfall), `max_pluie` (maximum daily rainfall), `t_max_max` (maximum temperature), `t_min_min` (minimum temperature), `vite_vent_max` (maximum wind speed)

### Municipal Budget Data (CIC)
- **Source**: Direction Générale des Finances Publiques (DGFiP)
- **URL**: https://data.economie.gouv.fr/explore/?search=comptes-individuels-des-communes
- **Coverage**: All French mainland municipalities, 2000--2022
- **Format**: CSV file. Store as `raw_data/comptes-individuels-des-communes.csv`
- **Key variables**: `totalex` (total expenditures per capita), `fdepinv` (investment expenditures), `fdgf` (general grant), `fdette` (debt stock)

### Natural Hazard Prevention Plans (PPRN)
- **Source**: GASPAR database, French Ministry of Environment
- **URL**: https://www.georisques.gouv.fr/donnees/bases-de-donnees/procedures-administratives-relatives-aux-risques
- **Format**: CSV file. Store as `raw_data/pprn_gaspar.csv`
- **Key variable**: `dat_approbation` (approval date of the prevention plan)

### Socioeconomic Data
- **Median household income**: INSEE, Filosofi survey
  - URL: https://www.insee.fr/fr/statistiques/6036907
  - File: `raw_data/REVENU_MEDIAN_2000_2024.csv`
- **Population**: INSEE, legal population estimates
  - URL: https://www.insee.fr/fr/statistiques/1893204
  - File: `raw_data/POPULATION.csv`
  - Key variables: `total_pop`, `pop_20` (under 20), `pop_65` (over 65)

### Intercommunality (EPCI)
- **Source**: INSEE
- **URL**: https://www.insee.fr/fr/information/2510634
- **File**: `raw_data/epci23.csv`
- **Key variable**: `nj_epci` — intercommunality type (CC = community of municipalities, integrated, isolated)

### Commune Mergers (Communes Nouvelles)
- **Source**: INSEE, Code Officiel Géographique
- **URL**: https://www.insee.fr/fr/information/2549968
- **File**: `raw_data/com_new.csv` — crosswalk table mapping old commune codes to new codes after mergers
- **Note**: Used to harmonize municipality codes across years in the CatNat data

### Electoral Data — Presidential Elections
- **Source**: French Ministry of the Interior
- **URL**: https://www.interieur.gouv.fr/Elections/Les-resultats
- **Coverage**: Presidential elections 2002, 2007, 2012, 2017 at the municipality level
- **File**: `raw_data/panel_president.csv` (constructed from raw Ministry files)
- **Key variable**: `president_en_tete` — candidate with the highest vote share in the first round

### Electoral Data — Municipal Elections and Mayor Characteristics
- **Source**: French Ministry of the Interior
- **URL**: https://www.interieur.gouv.fr/Elections/Les-resultats
- **Coverage**: Municipal elections 2008, 2014, 2020
- **Files**:
  - `raw_data/panel_maire_couleur.csv` — mayor political affiliation (large municipalities only), constructed from Ministry raw files
  - `raw_data/panel_maire_election.csv` — mayor characteristics (age, gender, seniority, candidacy, reelection), constructed from Ministry raw files
- **Note**: Construction scripts are not distributed in this repository. These files are available upon request from the authors.

### Electoral Data — Legislative Elections
- **Source**: French Ministry of the Interior
- **URL**: https://www.interieur.gouv.fr/Elections/Les-resultats
- **File**: `raw_data/panel_legislative_commune.csv`
- **Key variables**: `groupe_parlementaire` (parliamentary group of the deputy), `ALIGN_legis` (alignment with president)

### Geographic Data (required for map figures only)
- **Municipal boundaries**: `raw_data/geo_commune_2022.shp`
  - URL: https://www.data.gouv.fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap
- **National boundaries**: `raw_data/fr.shp` — available from the same source

---

## Output Panels

The following panels are produced by `code/01_build_panels.R` and used as inputs by the analysis scripts. They are not distributed in this repository but can be reconstructed by running `01_build_panels.R`.

| File | Description | Used by |
|------|-------------|---------|
| `panel_couleur_cat_elec_EMDAT.csv` | Annual panel, large municipalities with mayor political color known | `02_did_period1.R` |
| `panel_cat_elec_EMDAT.csv` | Annual panel, all municipalities, presidential alignment | `03_did_period2.R` |
| `panel_maire_heck.csv` | Electoral-cycle panel (2008, 2014, 2020) for Heckman model | `04_heckman.R` |
| `base_complete_EMDAT.csv` | Full annual panel, all municipalities, all years | Internal |

---

## Variable Dictionary

### CatNat variables (in analysis panels)

| Variable | Description |
|----------|-------------|
| `cod_commune` | Municipality identifier (5-digit INSEE code) |
| `an` | Year |
| `catnat` | Total number of disaster requests (recognized + not recognized) |
| `catnat_T` | Number of recognized disaster requests |
| `catnat_F` | Number of rejected disaster requests |
| `tx_reco` | Recognition rate = `catnat_T / catnat * 100` |
| `tx_reco_noex2` | Recognition rate excluding EM-DAT extreme events (droughts kept intact) |
| `tx_reco_drought` | Recognition rate for drought only |
| `tx_reco_floods` | Recognition rate for floods only |
| `Drought_T` / `Drought_F` | Recognized / rejected drought requests |
| `Floods_T` / `Floods_F` | Recognized / rejected floods and storms requests |
| `emdat_extreme` | Dummy = 1 if the arrêté matches an EM-DAT major event |
| `Drought_req` | Total drought requests = `Drought_T + Drought_F` |
| `Floods_req` | Total floods requests = `Floods_T + Floods_F` |

### Political variables

| Variable | Description |
|----------|-------------|
| `nuance` | Mayor political label (official Ministry classification) |
| `couleur_pol` | Mayor political orientation (Gauche / Droite / Centre / Sans étiquette) |
| `treated` | Period 1 treatment: 1 if socialist mayor (LSOC) elected in 2012 |
| `TREAT` | Period 2 treatment: 1 if Macron came first in municipality in 2017 presidential election |
| `POST` | Post-treatment dummy |
| `president_en_tete` | Candidate with highest vote share in first round of presidential election |
| `groupe_parlementaire` | Parliamentary group of the local deputy |
| `ALIGN_legis` | Legislative alignment with the president |

### Controls

| Variable | Description |
|----------|-------------|
| `PPRN` | Dummy = 1 if municipality has an approved natural hazard prevention plan |
| `max_pluie` | Maximum daily rainfall in the year (mm) |
| `pluie` | Annual rainfall (mm) |
| `MEDREV` | Median household income |
| `total_pop` | Total population |
| `p_pop_65` | Share of population aged 65 and over |
| `totalex` | Total municipal expenditures per capita |
| `EPCI` | Intercommunality type (CC / integrated / isolated) |
| `lag_catnat` | Number of disaster requests in the past year |

### Heckman panel variables

| Variable | Description |
|----------|-------------|
| `candi_all` | Dummy = 1 if the incumbent ran for reelection |
| `reelect_all` | Dummy = 1 if the incumbent was reelected |
| `catnat_cycle_u` | Dummy = 1 if at least one disaster occurred during the electoral cycle |
| `catnat_categ` | Categorical: "No shock" / "declared" / "undeclared" |
| `age` | Mayor age at election |
| `age2` | Mayor age squared (exclusion restriction) |
| `nb_candi_lists` | Number of competing lists (exclusion restriction) |
| `gender` | Dummy = 1 if mayor is male |
| `seniority` | Number of years as mayor |
| `fdepinv` | Investment expenditures per capita |
| `debt` | Outstanding debt stock per capita |
| `voix_pres` | Presidential vote share of the aligned candidate |
| `couleur_maire2` | Mayor political color (simplified, reference = SANSETIQUETTE) |
