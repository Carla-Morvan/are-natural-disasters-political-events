# =============================================================================
# 03_did_period2.R
# Political Alignment and Natural Disaster Recognition in France
# Period 2: Presidential election 2017 (2014-2020)
#
# Input:  panel_cat_elec_EMDAT.csv  (all municipalities, presidential alignment)
# Output: figures saved to PATH_FIGURES
# =============================================================================

# -----------------------------------------------------------------------------
# -- 0. PATHS -----
# -----------------------------------------------------------------------------
PATH_ROOT        <- "C:/sdrive/CATNAT/couleur_catnat/REPO"
PATH_PANEL_P2 <- file.path(PATH_ROOT, "output/panel_cat_elec_EMDAT.csv")
PATH_FIGURES  <- file.path(PATH_ROOT, "output/figures/")

# -----------------------------------------------------------------------------
# -- 1. PACKAGES ----
# -----------------------------------------------------------------------------
library(dplyr); library(tidyr); library(stringr)
library(fixest); library(MatchIt); library(ggplot2); library(broom)

# -----------------------------------------------------------------------------
# -- 2. LOAD AND PREPARE DATA ----
# -----------------------------------------------------------------------------
panel_p2 <- read.csv(PATH_PANEL_P2) %>%
  filter(an %in% 2014:2020) %>%
  filter(!is.na(cod_commune)) %>%
  mutate(dep = substr(cod_commune, 1, 2)) %>%
  filter(dep != "97") %>%
  distinct() %>%
  mutate(
    groupe_parlementaire = ifelse(
      groupe_parlementaire %in% c("S.R.C", "S.R.C."), "S.R.C", groupe_parlementaire
    ),
    tx_reco              = catnat_T / catnat * 100,
    tx_reco_noex2        = (Drought_T + Floods_T_noex) /
      (Drought_T + Drought_F + Floods_T_noex + Floods_F_noex) * 100,
    tx_reco_drought      = Drought_T / (Drought_T + Drought_F) * 100,
    tx_reco_drought_noex = Drought_T_noex / (Drought_T_noex + Drought_F_noex) * 100,
    tx_reco_floods       = Floods_T / (Floods_T + Floods_F) * 100,
    tx_reco_floods_noex  = Floods_T_noex / (Floods_T_noex + Floods_F_noex) * 100
  )

# Control group: municipalities never aligned to the left (Hollande) 2012-2017
control_communes_p2 <- panel_p2 %>%
  filter(an %in% 2012:2017) %>%
  group_by(cod_commune) %>%
  summarise(never_left = all(president_en_tete != "Hollande" | is.na(president_en_tete)),
            .groups = "drop") %>%
  filter(never_left) %>%
  pull(cod_commune)

# Treatment: Macron in first round 2017
treat_p2 <- panel_p2 %>%
  filter(cod_commune %in% control_communes_p2, an == 2018) %>%
  mutate(TREAT = as.integer(president_en_tete == "MACRON")) %>%
  select(cod_commune, TREAT)

# DiD panel — lag calculated before final year filter
did_p2 <- panel_p2 %>%
  filter(cod_commune %in% control_communes_p2) %>%
  left_join(treat_p2, by = "cod_commune") %>%
  mutate(POST = as.integer(an > 2017)) %>%
  filter(an %in% 2014:2019) %>%        # filtre AVANT le lag
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE) %>%
  group_by(cod_commune) %>%
  mutate(lag_catnat = lead(catnat)) %>% # lag sans 2020
  ungroup()                       # filter AFTER lag

# -----------------------------------------------------------------------------
# -- 3. BASELINE DiD ----
# -----------------------------------------------------------------------------
p2_base <- feols(
  tx_reco ~ TREAT * POST | cod_commune + an,
  data = did_p2, cluster = ~cod_commune
)
p2_ctrl <- feols(
  tx_reco ~ TREAT * POST + PPRN + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2, cluster = ~cod_commune
)
p2_nopprn <- feols(
  tx_reco ~ TREAT * POST + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2, cluster = ~cod_commune
)
etable(p2_base, p2_ctrl, p2_nopprn)

# -----------------------------------------------------------------------------
# -- 4. PARALLEL TRENDS — EVENT STUDY ----
# -----------------------------------------------------------------------------
did_p2_es <- did_p2 %>% filter(an > 2014)

es_p2 <- feols(
  tx_reco ~ i(an, TREAT, ref = 2015) + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex |
    cod_commune + an,
  data = did_p2_es, cluster = ~cod_commune
)

wald_res <- wald(es_p2, keep = c("an::2015", "an::2016"))
p_wald   <- round(wald_res$p, 3)

es_df <- tidy(es_p2, conf.int = TRUE) %>%
  filter(str_detect(term, "^an::"), str_detect(term, ":TREAT$")) %>%
  mutate(year = as.numeric(str_extract(term, "(?<=an::)-?\\d+"))) %>%
  filter(year > 2014) %>%
  arrange(year)

ggplot(es_df, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  geom_point(size = 2) +
  annotate("point", x = 2015, y = 0, size = 2, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(y = "Estimate and 95% Conf. Int.", x = "",
       caption = paste0("Joint Wald test of pre-treatment coefficients: p-value = ", p_wald)) +
  theme_minimal(base_size = 13) +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))
ggsave(file.path(PATH_FIGURES, "fig_eventstudy_period2.png"), dpi = 300, width = 7, height = 3)

# -----------------------------------------------------------------------------
# -- 5. DiD with lagged request ----
# -----------------------------------------------------------------------------
p2_ctrl_lag <- feols(
  tx_reco ~ TREAT * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2, cluster = ~cod_commune
)
p2_nopprn_lag <- feols(
  tx_reco ~ TREAT * POST + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2, cluster = ~cod_commune
)
etable(p2_base, p2_ctrl_lag, p2_nopprn_lag)

# -----------------------------------------------------------------------------
# -- 6. PROPENSITY SCORE MATCHING (PSM) ----
# -----------------------------------------------------------------------------
base_match_p2 <- did_p2 %>%
  filter(an %in% 2014:2017) %>%
  group_by(cod_commune, TREAT) %>%
  summarise(across(c(PPRN, pluie, max_pluie, MEDREV, total_pop, p_pop_65, totalex),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  filter(!is.na(TREAT),
         if_all(c(PPRN, pluie, MEDREV, total_pop, p_pop_65, totalex), ~ !is.na(.x)))

m_p2 <- matchit(
  TREAT ~ PPRN + pluie + max_pluie + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex,
  data = base_match_p2, method = "nearest", distance = "logit",
  caliper = 0.2, std.caliper = TRUE, ratio = 1, replace = FALSE
)
summary(m_p2, standardize = TRUE)

# Love plot
s_p2   <- summary(m_p2, standardize = TRUE)
smd_p2 <- data.frame(
  variable = rownames(s_p2$sum.all),
  avant    = s_p2$sum.all[, "Std. Mean Diff."],
  apres    = s_p2$sum.matched[, "Std. Mean Diff."]
) %>%
  filter(variable != "distance") %>%
  pivot_longer(c(avant, apres), names_to = "moment", values_to = "smd") %>%
  mutate(smd    = abs(smd),
         moment = factor(moment, levels = c("avant","apres"),
                         labels = c("Before matching","After matching")))
ggplot(smd_p2, aes(x = smd, y = reorder(variable, smd), color = moment, shape = moment)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "red", alpha = 0.6) +
  scale_color_manual(values = c("Before matching"="grey50","After matching"="steelblue")) +
  labs(x = "|SMD|", y = NULL, color = NULL, shape = NULL, caption = "Red line = threshold 0.1") +
  theme_minimal() + theme(legend.position = "bottom")

did_p2_m <- did_p2 %>%
  filter(cod_commune %in% (match.data(m_p2) %>% pull(cod_commune))) %>%
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE)

p2_m_base <- feols(tx_reco ~ TREAT * POST | cod_commune + an,
                   data = did_p2_m, cluster = ~cod_commune)
p2_m_ctrl <- feols(
  tx_reco ~ TREAT * POST + PPRN + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2_m, cluster = ~cod_commune)
p2_m_nopprn <- feols(
  tx_reco ~ TREAT * POST + max_pluie  +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2_m, cluster = ~cod_commune)

etable( p2_ctrl, p2_nopprn,
        p2_m_ctrl, p2_m_nopprn,
       headers = list("Without matching" = 2, "With matching" = 2))

# Event-study on matched sample
es_p2_m <- feols(
  tx_reco ~ i(an, TREAT, ref = 2015) + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2_m %>% filter(an > 2014), cluster = ~cod_commune)
wald_res_m <- wald(es_p2_m, keep = c("an::2015", "an::2016"))
p_wald_m   <- round(wald_res_m$p, 3)
es_df_m <- tidy(es_p2_m, conf.int = TRUE) %>%
  filter(str_detect(term, "^an::"), str_detect(term, ":TREAT$")) %>%
  mutate(year = as.numeric(str_extract(term, "(?<=an::)-?\\d+"))) %>%
  filter(year > 2014) %>% arrange(year)
ggplot(es_df_m, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  geom_point(size = 2) +
  annotate("point", x = 2015, y = 0, size = 2, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(y = "Estimate and 95% Conf. Int.", x = "",
       caption = paste0("Joint Wald test of pre-treatment coefficients: p-value = ", p_wald_m)) +
  theme_minimal(base_size = 13) +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))
ggsave(file.path(PATH_FIGURES, "fig_eventstudy_period2_matched.png"), dpi = 300, width = 7, height = 3)

# -----------------------------------------------------------------------------
# -- 7. EM-DAT ROBUSTNESS (excluding major events) ----
# -----------------------------------------------------------------------------
p2_noex        <- feols(tx_reco_noex2 ~ TREAT*POST | cod_commune + an,
                        data = did_p2, cluster = ~cod_commune)
p2_noex_ctrl   <- feols(tx_reco_noex2 ~ TREAT*POST + PPRN + max_pluie + lag_catnat +
                          EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                        data = did_p2, cluster = ~cod_commune)
p2_noex_nopprn <- feols(tx_reco_noex2 ~ TREAT*POST + max_pluie + lag_catnat +
                          EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                        data = did_p2, cluster = ~cod_commune)
etable(p2_noex, p2_noex_ctrl, p2_noex_nopprn,
       headers = list("Without extreme events" = 3))


# -----------------------------------------------------------------------------
# -- 8. LEGISLATIVE ALIGNMENT (deputy group) ----
# -----------------------------------------------------------------------------
control_communes_legis_p2 <- panel_p2 %>%
  filter(an %in% 2012:2017) %>%
  group_by(cod_commune) %>%
  summarise(never_left = all(groupe_parlementaire != "SER" | is.na(groupe_parlementaire)),
            .groups = "drop") %>%
  filter(never_left) %>%
  pull(cod_commune)

treat_legis_p2 <- panel_p2 %>%
  filter(cod_commune %in% control_communes_legis_p2, an == 2018) %>%
  mutate(TREAT = as.integer(groupe_parlementaire == "LREM")) %>%
  select(cod_commune, TREAT)

did_p2_legis <- panel_p2 %>%
  filter(cod_commune %in% control_communes_legis_p2) %>%
  left_join(treat_legis_p2, by = "cod_commune") %>%
  mutate(POST = as.integer(an > 2017)) %>%
  filter(!is.na(TREAT), an %in% 2014:2020) %>%   # keep 2020 for lag
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE) %>%
  group_by(cod_commune) %>%
  mutate(lag_catnat = lead(catnat)) %>%
  ungroup() %>%
  filter(an %in% 2014:2019) %>%                   # filter AFTER lag
  mutate(grand_groupe = factor(case_when(
    groupe_parlementaire %in% c("NI","NC")                              ~ "No group",
    groupe_parlementaire %in% c("SOC","S.R.C","SER","RRDP","LFI","GDR") ~ "Left",
    groupe_parlementaire %in% c("LR","UDI","LT","UMP","LES_REPUBLICAINS") ~ "Right",
    groupe_parlementaire %in% c("LREM","MODEM")                         ~ "Center"
  ), levels = c("No group", "Left", "Right", "Center"))) %>%
  distinct()

p2_legis1 <- feols(tx_reco ~ TREAT:POST | cod_commune + an,
                   data = did_p2_legis, cluster = ~cod_commune)
p2_legis2 <- feols(tx_reco ~ TREAT:POST + PPRN + max_pluie + lag_catnat +
                     EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                   data = did_p2_legis, cluster = ~cod_commune)
p2_legis3 <- feols(tx_reco ~ TREAT:POST + grand_groupe + PPRN + max_pluie + lag_catnat +
                     EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                   data = did_p2_legis, cluster = ~cod_commune)

etable(p2_legis1, p2_legis2, p2_legis3)

# -----------------------------------------------------------------------------
# -- 10. MECHANISM — NUMBER OF REQUESTS ----
# -----------------------------------------------------------------------------
did_p2 <- did_p2 %>%
  mutate(
    catnatD      = as.integer(catnat > 0)
  )

# Restrict to communes that submitted at least one request over the period
did_p2_demande <- did_p2 %>%
  group_by(cod_commune) %>%
  summarise(catnatD_mean = mean(catnatD, na.rm = TRUE), .groups = "drop") %>%
  filter(catnatD_mean > 0) %>%
  select(-catnatD_mean) %>%
  left_join(did_p2, by = "cod_commune")

p2_demande_ols <- feols(
  catnatD ~ TREAT * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2_demande, cluster = ~cod_commune
)
p2_demande_logit <- feglm(
  catnatD ~ TREAT * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  family = binomial("logit"),
  data = did_p2_demande, cluster = ~cod_commune
)
p2_demande_probit <- feglm(
  catnatD ~ TREAT * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  family = binomial("probit"),
  data = did_p2_demande, cluster = ~cod_commune
)
p2_demande_pois <- fepois(
  catnatD ~ TREAT * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p2, cluster = ~cod_commune  # full sample for Poisson
)
etable(p2_demande_ols, p2_demande_logit, p2_demande_probit, p2_demande_pois)


message("03_did_period2.R done.")
