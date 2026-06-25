# =============================================================================
# 02_did_period1.R
# Political Alignment and Natural Disaster Recognition in France
# Period 1: Presidential election 2012 (2008-2014)
#
# Input:  panel_couleur_cat_elec_EMDAT.csv  (large municipalities, mayor color)
#         panel_cat_elec_EMDAT.csv          (all municipalities, presidential)
# Output: figures saved to PATH_FIGURES
# =============================================================================

# -----------------------------------------------------------------------------
# -- 0. PATHS -----
# -----------------------------------------------------------------------------
PATH_ROOT        <- "C:/sdrive/CATNAT/couleur_catnat/REPO"

PATH_PANEL_P1      <- file.path(PATH_ROOT, "output/panel_couleur_cat_elec_EMDAT.csv") 
PATH_PANEL_P1_ALL  <- file.path(PATH_ROOT, "output/panel_cat_elec_EMDAT.csv")
PATH_FIGURES       <- file.path(PATH_ROOT, "output/figures/") 

# -----------------------------------------------------------------------------
# -- 1. PACKAGES ----
# -----------------------------------------------------------------------------
library(dplyr); library(tidyr); library(stringr)
library(fixest); library(MatchIt); library(ggplot2); library(broom)

# -----------------------------------------------------------------------------
# -- 2. LOAD AND PREPARE DATA — large municipalities (mayor color known) ----
# -----------------------------------------------------------------------------
panel_p1 <- read.csv(PATH_PANEL_P1) %>%
  filter(an %in% 2008:2014) %>%
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

# Control group: municipalities never aligned to the right before 2012
control_communes_p1 <- panel_p1 %>%
  filter(an %in% 2008:2011) %>%
  group_by(cod_commune) %>%
  summarise(never_right = all(couleur_pol != "Droite" | is.na(couleur_pol)),
            .groups = "drop") %>%
  filter(never_right) %>%
  pull(cod_commune)

# Treatment: socialist mayor elected in 2012
treat_p1 <- panel_p1 %>%
  filter(cod_commune %in% control_communes_p1, an == 2012) %>%
  mutate(treated = as.integer(nuance %in% "LSOC")) %>%
  select(cod_commune, treated)

# DiD panel
did_p1 <- panel_p1 %>%
  filter(cod_commune %in% control_communes_p1) %>%
  left_join(treat_p1, by = "cod_commune") %>%
  mutate(POST = as.integer(an %in% c(2012, 2013, 2014))) %>%
  filter(an %in% 2008:2014, !is.na(treated)) %>%   # garder 2014 pour le lag
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE) %>%
  group_by(cod_commune) %>%
  mutate(lag_catnat = lead(catnat)) %>%             # lag calculé ici, 2014 visible
  ungroup() %>%
  filter(an %in% 2008:2013)                         # filtre APRÈS le lag

# -----------------------------------------------------------------------------
# -- 3. BASELINE DiD ----
# -----------------------------------------------------------------------------
p1_base <- feols(
  tx_reco ~ treated * POST | cod_commune + an,
  data = did_p1, cluster = ~cod_commune
)
p1_ctrl <- feols(
  tx_reco ~ treated * POST + PPRN + max_pluie  +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1, cluster = ~cod_commune
)
p1_nopprn <- feols(
  tx_reco ~ treated * POST + max_pluie  +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1, cluster = ~cod_commune
)
etable(p1_base, p1_ctrl, p1_nopprn)


# -----------------------------------------------------------------------------
# -- 4. PARALLEL TRENDS — EVENT STUDY -----
# -----------------------------------------------------------------------------
es_p1 <- feols(
  tx_reco ~ i(an, treated, ref = 2008) + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex |
    cod_commune + an + scrutin + nb_candi_lists,
  data = did_p1, cluster = ~cod_commune
)

wald_res <- wald(es_p1, keep = c("an::2009", "an::2010", "an::2011"))
p_wald   <- round(wald_res$p, 3)

es_df <- tidy(es_p1, conf.int = TRUE) %>%
  filter(str_detect(term, "^an::"), str_detect(term, ":treated$")) %>%
  mutate(year = as.numeric(str_extract(term, "(?<=an::)-?\\d+"))) %>%
  arrange(year)

ggplot(es_df, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  geom_point(size = 2) +
  annotate("point", x = 2008, y = 0, size = 2, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(y = "Estimate and 95% Conf. Int.", x = "",
       caption = paste0("Joint Wald test of pre-treatment coefficients: p-value = ", p_wald)) +
  theme_minimal(base_size = 13) +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))
ggsave(file.path(PATH_FIGURES, "fig_eventstudy_period1.png"), dpi = 300, width = 7, height = 3)

# -----------------------------------------------------------------------------
# -- 5. DiD with lagged request ----
# -----------------------------------------------------------------------------

p1_ctrl_lag <- feols(
  tx_reco ~ treated * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1, cluster = ~cod_commune
)
p1_nopprn_lag <- feols(
  tx_reco ~ treated * POST + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1, cluster = ~cod_commune
)
etable(p1_base, p1_ctrl_lag, p1_nopprn_lag)


# -----------------------------------------------------------------------------
# -- 6. PROPENSITY SCORE MATCHING (PSM) -----
# -----------------------------------------------------------------------------
base_match_p1 <- did_p1 %>%
  filter(an %in% 2008:2011) %>%
  group_by(cod_commune, treated) %>%
  summarise(across(c(PPRN, pluie, max_pluie, MEDREV, total_pop, p_pop_65, totalex),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  filter(!is.na(treated),
         if_all(c(PPRN, pluie, MEDREV, total_pop, p_pop_65, totalex), ~ !is.na(.x)))

m_p1 <- matchit(
  treated ~ PPRN + pluie + max_pluie + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex,
  data = base_match_p1, method = "nearest", distance = "logit",
  caliper = 0.2, std.caliper = TRUE, ratio = 1, replace = FALSE
)
summary(m_p1, standardize = TRUE)

# Love plot
s_p1   <- summary(m_p1, standardize = TRUE)
smd_p1 <- data.frame(
  variable = rownames(s_p1$sum.all),
  avant    = s_p1$sum.all[, "Std. Mean Diff."],
  apres    = s_p1$sum.matched[, "Std. Mean Diff."]
) %>%
  filter(variable != "distance") %>%
  pivot_longer(c(avant, apres), names_to = "moment", values_to = "smd") %>%
  mutate(smd    = abs(smd),
         moment = factor(moment, levels = c("avant","apres"),
                         labels = c("Before matching","After matching")))
ggplot(smd_p1, aes(x = smd, y = reorder(variable, smd), color = moment, shape = moment)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "red", alpha = 0.6) +
  scale_color_manual(values = c("Before matching"="grey50","After matching"="steelblue")) +
  labs(x = "|SMD|", y = NULL, color = NULL, shape = NULL, caption = "Red line = threshold 0.1") +
  theme_minimal() + theme(legend.position = "bottom")

did_p1_m <- did_p1 %>%
  filter(cod_commune %in% (match.data(m_p1) %>% pull(cod_commune))) %>%
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE)

p1_m_base <- feols(tx_reco ~ treated * POST | cod_commune + an,
                   data = did_p1_m, cluster = ~cod_commune)
p1_m_ctrl <- feols(
  tx_reco ~ treated * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1_m, cluster = ~cod_commune)
p1_m_nopprn <- feols(
  tx_reco ~ treated * POST + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1_m, cluster = ~cod_commune)

etable(p1_base, p1_ctrl, p1_nopprn,
       p1_m_base, p1_m_ctrl, p1_m_nopprn,
       headers = list("Without matching" = 3, "With matching" = 3))

# Event-study on matched sample
es_p1_m <- feols(
  tx_reco ~ i(an, treated, ref = 2008) + max_pluie +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an + scrutin ,
  data = did_p1_m, cluster = ~cod_commune)
wald_res_m <- wald(es_p1_m, keep = c("an::2009", "an::2010", "an::2011"))
p_wald_m   <- round(wald_res_m$p, 3)
es_df_m <- tidy(es_p1_m, conf.int = TRUE) %>%
  filter(str_detect(term, "^an::"), str_detect(term, ":treated$")) %>%
  mutate(year = as.numeric(str_extract(term, "(?<=an::)-?\\d+"))) %>%
  arrange(year)
ggplot(es_df_m, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  geom_point(size = 2) +
  annotate("point", x = 2008, y = 0, size = 2, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(y = "Estimate and 95% Conf. Int.", x = "",
       caption = paste0("Joint Wald test of pre-treatment coefficients: p-value = ", p_wald_m)) +
  theme_minimal(base_size = 13) +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))
ggsave(file.path(PATH_FIGURES, "fig_eventstudy_period1_matched.png"), dpi = 300, width = 7, height = 3)

# -----------------------------------------------------------------------------
# -- 7. EM-DAT ROBUSTNESS (excluding major events) -----
# -----------------------------------------------------------------------------
p1_noex       <- feols(tx_reco_noex2 ~ treated*POST | cod_commune + an,
                       data = did_p1, cluster = ~cod_commune)
p1_noex_ctrl  <- feols(tx_reco_noex2 ~ treated*POST + PPRN + max_pluie + lag_catnat +
                         EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                       data = did_p1, cluster = ~cod_commune)
p1_noex_nopprn <- feols(tx_reco_noex2 ~ treated*POST + max_pluie + lag_catnat +
                          EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                        data = did_p1, cluster = ~cod_commune)
etable(p1_noex, p1_noex_ctrl, p1_noex_nopprn,
       headers = list("Without extreme events" = 3))



# -----------------------------------------------------------------------------
# -- 8. PLACEBO — left-wing mayors (not PS-only) as treatment -----
# -----------------------------------------------------------------------------
treat_placebo_left <- panel_p1 %>%
  filter(cod_commune %in% control_communes_p1, an == 2012) %>%
  mutate(treated = as.integer(couleur_pol %in% "Gauche")) %>%
  select(cod_commune, treated)

did_placebo_left <- panel_p1 %>%
  filter(cod_commune %in% control_communes_p1) %>%
  left_join(treat_placebo_left, by = "cod_commune") %>%
  mutate(POST = as.integer(an %in% c(2012, 2013, 2014))) %>%
  filter(an %in% 2008:2013, !is.na(treated)) %>%
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE) %>%
  group_by(cod_commune) %>%
  mutate(lag_catnat = lead(catnat)) %>%
  ungroup()
did_placebo_left_p <- panel(did_placebo_left, ~ cod_commune + an)

placebo_left1 <- feols(tx_reco ~ treated*POST | cod_commune + an,
                       data = did_placebo_left_p, cluster = ~cod_commune)
placebo_left2 <- feols(tx_reco ~ treated*POST + PPRN + max_pluie +
                         EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                       data = did_placebo_left_p, cluster = ~cod_commune)
placebo_left3 <- feols(tx_reco ~ treated*POST + max_pluie +
                         EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                       data = did_placebo_left_p, cluster = ~cod_commune)
placebo_left4 <- feols(tx_reco ~ treated*POST + max_pluie + lag_catnat +
                         EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                       data = did_placebo_left_p, cluster = ~cod_commune)
etable(placebo_left1, placebo_left2, placebo_left3, placebo_left4)

# -----------------------------------------------------------------------------
# -- 9. LEGISLATIVE ALIGNMENT (deputy group, all municipalities) -----
# -----------------------------------------------------------------------------
panel_p1_all <- read.csv(PATH_PANEL_P1_ALL) %>%
  filter(an %in% 2008:2014) %>%
  filter(!is.na(cod_commune)) %>%
  mutate(dep = substr(cod_commune, 1, 2)) %>%
  filter(dep != "97") %>%
  mutate(groupe_parlementaire = ifelse(
    groupe_parlementaire %in% c("S.R.C","S.R.C."), "S.R.C", groupe_parlementaire)) %>%
  distinct()

control_communes_legis <- panel_p1_all %>%
  filter(an %in% 2008:2011) %>%
  group_by(cod_commune) %>%
  summarise(never_right = all(!(groupe_parlementaire %in% c("UMP","LR","UDI")) |
                                is.na(groupe_parlementaire)), .groups = "drop") %>%
  filter(never_right) %>%
  pull(cod_commune)

treat_legis <- panel_p1_all %>%
  filter(cod_commune %in% control_communes_legis, an == 2013) %>%
  mutate(TREAT = as.integer(groupe_parlementaire %in% c("SER","S.R.C"))) %>%
  select(cod_commune, TREAT)

# lag_catnat calculé avant le filtre
did_p1_legis <- panel_p1_all %>%
  filter(cod_commune %in% control_communes_legis) %>%
  left_join(treat_legis, by = "cod_commune") %>%
  mutate(POST = as.integer(an >= 2012)) %>%
  filter(!is.na(TREAT), an %in% 2008:2014) %>%   # garder 2014 pour le lag
  arrange(cod_commune, an) %>%
  distinct(cod_commune, an, .keep_all = TRUE) %>%
  group_by(cod_commune) %>%
  mutate(lag_catnat = lead(catnat)) %>%
  ungroup() %>%
  filter(an %in% 2008:2013) %>%                   # filtre APRÈS le lag
  mutate(grand_groupe = factor(case_when(
    groupe_parlementaire %in% c("NI","NC")                         ~ "No group",
    groupe_parlementaire %in% c("SOC","S.R.C","SER","RRDP","LFI", "GDR") ~ "Left",
    groupe_parlementaire %in% c("LR","UDI","LT", "UMP", "LES_REPUBLICAINS")                   ~ "Right",
    groupe_parlementaire %in% c("LREM","MODEM")                    ~ "Center"
  ), levels = c("No group", "Left", "Right", "Center"))) %>%
  distinct()

p1_legis1 <- feols(tx_reco ~ TREAT:POST | cod_commune + an,
                   data = did_p1_legis, cluster = ~cod_commune)
p1_legis2 <- feols(tx_reco ~ TREAT:POST + PPRN + max_pluie +
                     EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                   data = did_p1_legis, cluster = ~cod_commune)
p1_legis3 <- feols(tx_reco ~ TREAT:POST + grand_groupe + PPRN + max_pluie +
                     EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
                   data = did_p1_legis, cluster = ~cod_commune)
etable(p1_legis1, p1_legis2,p1_legis3)

# -----------------------------------------------------------------------------
# -- 10. MECHANISM — NUMBER OF REQUESTS ----
# -----------------------------------------------------------------------------
did_p1 <- did_p1 %>% mutate(catnatD = as.integer(catnat > 0))

# Restrict to communes that submitted at least one request over the period
did_p1_demande <- did_p1 %>%
  group_by(cod_commune) %>%
  summarise(catnatD_mean = mean(catnatD, na.rm = TRUE), .groups = "drop") %>%
  filter(catnatD_mean > 0) %>%
  select(-catnatD_mean) %>%
  left_join(did_p1, by = "cod_commune")

p1_demande_ols <- feols(
  catnatD ~ treated * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1_demande, cluster = ~cod_commune
)
p1_demande_logit <- feglm(
  catnatD ~ treated * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  family = binomial("logit"),
  data = did_p1_demande, cluster = ~cod_commune
)
p1_demande_probit <- feglm(
  catnatD ~ treated * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  family = binomial("probit"),
  data = did_p1_demande, cluster = ~cod_commune
)
p1_demande_pois <- fepois(
  catnatD ~ treated * POST + PPRN + max_pluie + lag_catnat +
    EPCI + asinh(MEDREV) + asinh(total_pop) + p_pop_65 + totalex | cod_commune + an,
  data = did_p1, cluster = ~cod_commune  # full sample for Poisson
)
etable(p1_demande_ols, p1_demande_logit, p1_demande_probit, p1_demande_pois)

message("02_did_period1.R done.")