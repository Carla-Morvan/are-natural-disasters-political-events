# =============================================================================
# 04_heckman.R
# Political Alignment and Natural Disaster Recognition in France
# Heckman selection model: effect of CatNat recognition on mayor reelection
#
# Input:  panel_maire_heck.csv  (from 01_build_panels.R)
# =============================================================================

# -----------------------------------------------------------------------------
# -- 0. PATHS -----
# -----------------------------------------------------------------------------
PATH_ROOT        <- "C:/sdrive/CATNAT/couleur_catnat/REPO"
PATH_PANEL_HECK <- file.path(PATH_ROOT, "output/panel_maire_heck.csv")

# -----------------------------------------------------------------------------
# -- 1. PACKAGES ----
# -----------------------------------------------------------------------------
library(dplyr); library(tidyr)
library(sampleSelection); library(fixest); library(texreg)
library(car); library(boot)

# -----------------------------------------------------------------------------
# -- 2. LOAD AND PREPARE DATA ----
# -----------------------------------------------------------------------------
panel_heck <- read.csv(PATH_PANEL_HECK) %>%
  distinct() %>%
  mutate(
    pprn_incum = case_when(
      date_pprn %in% 2002:2008 & an == 2008 ~ 1,
      date_pprn %in% 2009:2014 & an == 2014 ~ 1,
      date_pprn %in% 2015:2020 & an == 2020 ~ 1,
      TRUE ~ 0
    ),
    tx_reco        = catnat_T / catnat,
    catnat_100TRUE = ifelse(is.na(tx_reco), 0, ifelse(tx_reco == 1, 1, 0)),
    catnat_categ   = case_when(
      catnat_cycle_u == 0                       ~ "aNo shock",
      catnat_cycle_u == 1 & catnat_100TRUE == 1 ~ "declared",
      catnat_cycle_u == 1 & catnat_100TRUE == 0 ~ "undeclared"
    ),
    couleur_maire = ifelse(is.na(couleur_pol), "SANSETIQUETTE", couleur_pol),
    couleur_maire2 = case_when(
      couleur_maire == "DIVERS" ~ "SANSETIQUETTE",
      TRUE ~ couleur_maire
    )
  ) %>%
  mutate(couleur_maire2 = relevel(factor(couleur_maire2), ref = "SANSETIQUETTE"))

# -----------------------------------------------------------------------------
# -- 3. HECKMAN ML — pool (with year FE) ----
# -----------------------------------------------------------------------------
heck_pool <- selection(
  selection = as.factor(candi_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    as.factor(gender) + age +  age2 +
    nb_candi_lists +
    as.factor(an),
  outcome = as.factor(reelect_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    gender + age +
    as.factor(an),
  data   = panel_heck,
  method = "ml"
)
summary(heck_pool)

# -----------------------------------------------------------------------------
# -- 4. HECKMAN ML — by wave (with exclusion restrictions: age2, nb_candi_lists) ----
# -----------------------------------------------------------------------------
sel_wave <- as.factor(candi_all) ~ catnat_categ +
  PPRN + asinh(fdepinv) + asinh(debt) +
  EPCI + asinh(MEDREV) + p_pop_65 +
  as.factor(gender) + age + age2 +
  nb_candi_lists

out_wave <- as.factor(reelect_all) ~ catnat_categ +
  PPRN + asinh(fdepinv) + asinh(debt) +
  EPCI + asinh(MEDREV) + p_pop_65 +
  gender + age

heck_2008 <- selection(
  selection = sel_wave, outcome = out_wave,
  data = panel_heck %>% filter(an == 2008), method = "ml"
)
summary(heck_2008)

heck_2014 <- selection(
  selection = sel_wave, outcome = out_wave,
  data = panel_heck %>% filter(an == 2014), method = "ml"
)
summary(heck_2014)

heck_2020 <- selection(
  selection = sel_wave, outcome = out_wave,
  data = panel_heck %>% filter(an == 2020), method = "ml"
)
summary(heck_2020)

texreg(
  list(heck_pool, heck_2008, heck_2014, heck_2020),
  custom.model.names = c("Pool", "2008", "2014", "2020"),
  digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
  include.loglik = TRUE, include.nobs = TRUE
)

# -----------------------------------------------------------------------------
# -- 5. HECKMAN 2-STEP — robustness ----
# -----------------------------------------------------------------------------
heck_pool_2s <- selection(
  selection = as.factor(candi_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    as.factor(gender) + age + as.factor(an),
  outcome = as.factor(reelect_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    gender + age + as.factor(an),
  data = panel_heck, method = "2step"
)

heck_2008_2s <- selection(selection = sel_wave, outcome = out_wave,
                          data = panel_heck %>% filter(an == 2008), method = "2step")
heck_2014_2s <- selection(selection = sel_wave, outcome = out_wave,
                          data = panel_heck %>% filter(an == 2014), method = "2step")
heck_2020_2s <- selection(selection = sel_wave, outcome = out_wave,
                          data = panel_heck %>% filter(an == 2020), method = "2step")

texreg(
  list(heck_pool, heck_pool_2s,
       heck_2008, heck_2008_2s,
       heck_2014, heck_2014_2s,
       heck_2020, heck_2020_2s),
  custom.model.names = c("Pool","Pool 2step","2008","2008 2step",
                         "2014","2014 2step","2020","2020 2step"),
  digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
  include.loglik = TRUE, include.nobs = TRUE
)

# -----------------------------------------------------------------------------
# -- 6. VALIDITY TESTS — placebo probit on exclusion restrictions ----
# -----------------------------------------------------------------------------
panel_selection <- panel_heck %>% filter(candi_all == 1)

# Pool
placebo_fe <- feglm(
  reelect_all ~ catnat_categ + PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 + gender + age + age2 + nb_candi_lists | an,
  data = panel_selection, family = binomial("probit"),
  cluster = c("cod_commune", "an")
)
summary(placebo_fe)


# Benchmark probit without exclusion restrictions
pooling_fe <- feglm(
  reelect_all ~ catnat_categ + PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 + gender + age | an,
  data = panel_selection, family = binomial("probit"),
  cluster = c("cod_commune", "an")
)
probit_2008 <- feglm(
  reelect_all ~ catnat_categ + PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 + gender + age | an,
  data = panel_selection %>% filter(an == 2008),
  family = binomial("probit"), cluster = "cod_commune"
)
probit_2014 <- feglm(
  reelect_all ~ catnat_categ + PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 + gender + age | an,
  data = panel_selection %>% filter(an == 2014),
  family = binomial("probit"), cluster = "cod_commune"
)
probit_2020 <- feglm(
  reelect_all ~ catnat_categ + PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 + gender + age | an,
  data = panel_selection %>% filter(an == 2020),
  family = binomial("probit"), cluster = "cod_commune"
)
etable(pooling_fe, placebo_fe, probit_2008, probit_2014, probit_2020)

# -----------------------------------------------------------------------------
# -- 7. ROBUSTNESS BY MUNICIPALITY SIZE ----
# -----------------------------------------------------------------------------
panel_heck <- panel_heck %>%
  group_by(cod_commune) %>%
  mutate(mean_pop = mean(total_pop, na.rm = TRUE)) %>%
  ungroup()

heck_small <- selection(
  selection = as.factor(candi_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    as.factor(gender) + age + age2 +
    nb_candi_lists + as.factor(an),
  outcome = as.factor(reelect_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    gender + age + as.factor(an),
  data   = panel_heck %>% filter(mean_pop < 3500),
  method = "ml"
)

heck_medium <- selection(
  selection = as.factor(candi_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    as.factor(gender) + age + age2 +
    nb_candi_lists + as.factor(an),
  outcome = as.factor(reelect_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    gender + age + as.factor(an),
  data   = panel_heck %>% filter(mean_pop>=3500 &
                                 mean_pop<=8000),
  method = "ml"
)

heck_large <- selection(
  selection = as.factor(candi_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    as.factor(gender) + age + age2 +
    nb_candi_lists + as.factor(an),
  outcome = as.factor(reelect_all) ~ catnat_categ +
    PPRN + asinh(fdepinv) + asinh(debt) +
    EPCI + asinh(MEDREV) + p_pop_65 +
    gender + age + as.factor(an),
  data   = panel_heck %>% filter(mean_pop >= 8000),
  method = "ml"
)

texreg(
  list(heck_small, heck_medium, heck_large),
  custom.model.names = c("< 3500 hab", "3500 -- 8000" ,">8000 hab"),
  digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
  include.loglik = TRUE, include.nobs = TRUE
)

# -----------------------------------------------------------------------------
# -- 8. HECKMAN WITH POLITICAL VARIABLES (couleur_maire2, voix_pres) ----
# -----------------------------------------------------------------------------
panel_heck <- panel_heck %>%
  mutate(
    couleur_maire2 = case_when(
      couleur_maire == "DIVERS" ~ "SANSETIQUETTE",
      TRUE ~ as.character(couleur_maire)
    ),
    couleur_maire2 = relevel(factor(couleur_maire2), ref = "SANSETIQUETTE")
  )

sel_pol <- as.factor(candi_all) ~ catnat_categ +
  PPRN + asinh(fdepinv) + asinh(debt) +
  EPCI + asinh(MEDREV) + p_pop_65 +
  as.factor(gender) + age + age2 +
  nb_candi_lists + couleur_maire2 + voix_pres

out_pol <- as.factor(reelect_all) ~ catnat_categ +
  PPRN + asinh(fdepinv) + asinh(debt) +
  EPCI + asinh(MEDREV) + p_pop_65 +
  gender + age + couleur_maire2 + voix_pres

heck_pool_pol <- selection(
  selection = update(sel_pol, ~ . + as.factor(an)),
  outcome   = update(out_pol, ~ . + as.factor(an)),
  data = panel_heck, method = "ml"
)
summary(heck_pool_pol)

heck_2008_pol <- selection(
  selection = sel_pol, outcome = out_pol,
  data = panel_heck %>% filter(an == 2008), method = "ml"
)
heck_2014_pol <- selection(
  selection = sel_pol, outcome = out_pol,
  data = panel_heck %>% filter(an == 2014), method = "ml"
)
heck_2020_pol <- selection(
  selection = sel_pol, outcome = out_pol,
  data = panel_heck %>% filter(an == 2020), method = "ml"
)

texreg(
  list(heck_pool_pol, heck_2008_pol, heck_2014_pol, heck_2020_pol),
  custom.model.names = c("Pool", "2008", "2014", "2020"),
  digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
  include.loglik = TRUE, include.nobs = TRUE
)
message("04_heckman.R done.")