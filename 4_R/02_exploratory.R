# 02_exploratory.R

# Master's thesis analysis: Warm but Bounded?
# Empathic and Recognition-Oriented Communication in NGO Chatbots

# Author: Joël Sieber
# Code written with AI assistance (Claude, Anthropic); see README.

# Exploratory and descriptive analyses, main study.
#
# Preregistered as exploratory: the additional outcomes, balance checks,
# the moderators trust in NGOs and prior chatbot use, and the agency mediation.
# Not preregistered: the moderator prior NGO contact, the social-presence
# mediation, response times, and the item-level analysis.

# Requires data_clean from 01_confirmatory.R.

source(here::here("R", "01_confirmatory.R"))

# ---- Balance checks ----
# Only genuinely pre-treatment or dispositional variables are used. Scales
# measured after the chatbot interaction (IMM, SOC, DISC, RISK, AGE) are
# potential outcomes of the manipulation and cannot serve as balance checks:
# any imbalance there would be a treatment effect, not a randomisation failure.
balance_vars <- c("DEM_age")

balance_table <- dplyr::bind_rows(lapply(balance_vars, function(v) {
  m <- lm(as.formula(paste0("`", v, "` ~ E_num * R_num")), data = data_clean)
  broom::tidy(m) %>% mutate(variable = v, .before = 1)
}))

cat("--- Balance: continuous background variables ---\n")
print(as.data.frame(balance_table), digits = 4)

cat("\n--- Balance: gender by condition ---\n")
gender_tab <- table(data_clean$DEM_gender, interaction(data_clean$E_f, data_clean$R_f))
print(gender_tab)
print(suppressWarnings(chisq.test(gender_tab)))

cat("\n--- Balance: country by condition ---\n")
country_tab <- table(data_clean[[COUNTRY_VAR]], interaction(data_clean$E_f, data_clean$R_f))
print(country_tab)
print(suppressWarnings(chisq.test(country_tab)))

# Omnibus balance tests across the four cells
cat("\n--- Balance: age, omnibus F across four cells ---\n")
age_omnibus <- anova(lm(DEM_age ~ interaction(E_f, R_f), data = data_clean))
print(age_omnibus)

cat("\n--- Balance: gender, Fisher test with simulated p (sparse cells) ---\n")
gender_fisher <- fisher.test(gender_tab, simulate.p.value = TRUE, B = 10000)
print(gender_fisher)

# ---- Additional outcomes (preregistered as exploratory/descriptive) ----
# Immersion, social presence, disclosure comfort, privacy risk and perceived
# agency, each modelled with the preregistered factorial specification.
# These are outcomes, not moderators: all were measured after the interaction.
extra_outcomes <- c(IMM = "IMM_mean", SOC = "SOC_mean", DISC = "DISC_mean",
                    RISK = "RISK_mean", AGE = "AGE_mean")

extra_models <- lapply(extra_outcomes, function(dv) {
  lm(as.formula(paste(dv, "~ E_num * R_num")), data = data_clean)
})

for (nm in names(extra_models)) {
  cat("\n---", nm, "---\n")
  print(car::Anova(extra_models[[nm]], type = 3))
  print(round(summary(extra_models[[nm]])$coefficients, 4))
}

cat("\n--- Cell means, additional outcomes ---\n")
print(as.data.frame(
  data_clean %>%
    group_by(E_f, R_f) %>%
    summarise(n = n(),
              across(all_of(unname(extra_outcomes)),
                     list(M = ~mean(.x), SD = ~sd(.x))),
              .groups = "drop")
), digits = 3)

# ---- Dispositional moderators ----
# Trust in NGOs and prior chatbot use were preregistered as exploratory
# moderators; prior NGO contact was added. Trust in NGOs was measured after
# the interaction, so it is used as a moderator only if the manipulation did
# not shift it (tested below).
#
# Post-interaction scales (IMM, SOC, DISC, RISK, AGE) are NOT used as
# moderators: they are potential outcomes of the manipulation, and
# conditioning on them would induce collider bias.
data_clean <- data_clean %>%
  mutate(
    TRU_ngo_mean = rowMeans(across(c(A7.3_TRU_ngo_1, A7.3_TRU_ngo_2,
                                     A7.3_TRU_ngo_3))),
    TRU_ngo_c    = TRU_ngo_mean - mean(TRU_ngo_mean, na.rm = TRUE),
    EXP_chatbot_c = as.numeric(A7.1_EXP_chatbot) -
      mean(as.numeric(A7.1_EXP_chatbot), na.rm = TRUE),
    EXP_ngo_num   = as.numeric(A7.2_EXP_ngo_binary)
  )

# Trust in NGOs must be unaffected by the manipulations
cat("\n--- Trust in NGOs: unaffected by the manipulations? ---\n")
print(car::Anova(lm(TRU_ngo_mean ~ E_num * R_num, data = data_clean), type = 3))

# Restricted to the two outcomes that showed treatment effects, to limit
# multiplicity. Three moderators x two outcomes = six models.
mod_vars <- c("TRU_ngo_c", "EXP_chatbot_c", "EXP_ngo_num")
mod_outcomes <- c("PAT_mean", "ATRU_mean")

cat("=== Exploratory moderation ===\n")
cat("Six models: 3 moderators x 2 outcomes (2 preregistered, 1 added).\n")
cat("No correction for multiple testing; interpret with caution.\n")

for (mv in mod_vars) {
  for (dv in mod_outcomes) {
    cat("\n---", dv, "moderated by", mv, "---\n")
    m <- lm(as.formula(paste(dv, "~ E_num * R_num *", mv)), data = data_clean)
    print(car::Anova(m, type = 3))
  }
}


# ---- Preregistered exploratory mediation: R -> Agency -> Paternalism ----
# Preregistered as exploratory. Reported regardless of outcome.
med_m_pre <- lm(AGE_mean ~ R_num + E_num, data = data_clean)
med_y_pre <- lm(PAT_mean ~ R_num + AGE_mean + E_num, data = data_clean)

cat("\n=== Preregistered mediation: R -> Agency -> Paternalism ===\n")
print(round(summary(med_m_pre)$coefficients, 4))
print(round(summary(med_y_pre)$coefficients, 4))

med_pre <- mediation::mediate(med_m_pre, med_y_pre, treat = "R_num",
                              mediator = "AGE_mean",
                              control.value = -0.5, treat.value = 0.5,
                              boot = TRUE, sims = 5000)
print(summary(med_pre))


# ---- Exploratory mediation: E -> social presence -> affective trust ----
# Both paths are strong (E -> SOC: b = 0.98, p < .001; SOC -> ATRU tested below).
# NOT preregistered. Mediator and outcome were measured at the same time and
# share method variance, so this is descriptive rather than causal evidence.
med_m <- lm(SOC_mean  ~ E_num + R_num, data = data_clean)
med_y <- lm(ATRU_mean ~ E_num + SOC_mean + R_num, data = data_clean)

cat("\n=== Mediation: E -> Social Presence -> Affective Trust ===\n")
cat("--- a path (E -> SOC) ---\n")
print(round(summary(med_m)$coefficients, 4))
cat("--- b and c' paths (SOC, E -> ATRU) ---\n")
print(round(summary(med_y)$coefficients, 4))

med_out <- mediation::mediate(med_m, med_y, treat = "E_num",
                              mediator = "SOC_mean",
                              control.value = -0.5, treat.value = 0.5,
                              boot = TRUE, sims = 5000)
print(summary(med_out))

# ---- Response times (not preregistered) ----
# Response times are the only behavioural measure without a ceiling:
# uptake shows 92.4% agreement, whereas rt_8 varies continuously.
#
# Word counts of the bot messages preceding each decision point differ by
# condition, so raw comparisons confound reading time with deliberation.
# Word counts per decision point (from the scripts, S3):.
#
# Bot words shown immediately before each decision point, by condition
# (cond order: lowE/lowR, highE/lowR, lowE/highR, highE/highR):
words_before <- tibble::tribble(
  ~rt_var,  ~c1, ~c2, ~c3, ~c4,
  "rt_1",    39,  73,  51,  93,
  "rt_3",    78,  88,  82,  91,
  "rt_5",    51,  90,  49, 106,
  "rt_8",    46,  70,  87, 107,
  "rt_10",    9,  52,  25,  71
)

# Attach the word count that each participant actually saw.
data_clean <- data_clean %>%
  mutate(
    words_rt8 = case_when(
      E_num == -0.5 & R_num == -0.5 ~ 46,
      E_num ==  0.5 & R_num == -0.5 ~ 70,
      E_num == -0.5 & R_num ==  0.5 ~ 87,
      E_num ==  0.5 & R_num ==  0.5 ~ 107
    ),
    # Reading-speed baseline: rt_3 is the decision point with the most
    # comparable text length across conditions (78-91 words).
    read_speed = rt_3 / 82,
    # Log transform: response times are strongly right-skewed
    # (rt_8 median 17,960 ms, maximum 2,529,854 ms).
    log_rt8 = log(rt_8)
  )

cat("--- Response times by condition (seconds) ---\n")
print(as.data.frame(
  data_clean %>%
    group_by(E_f, R_f) %>%
    summarise(n = n(),
              words = first(words_rt8),
              rt8_median = median(rt_8) / 1000,
              rt8_mean   = mean(rt_8) / 1000,
              ms_per_word = median(rt_8) / first(words_rt8),
              .groups = "drop")
), digits = 4)

# Model 1: raw log response time. Confounded with text length.
cat("\n--- rt_8, unadjusted (confounded with text length) ---\n")
print(car::Anova(lm(log_rt8 ~ E_num * R_num, data = data_clean), type = 3))

# Text length is perfectly collinear with condition (each cell has exactly one
# word count), so it cannot be entered as a covariate. Instead, response time
# is normalised by words read: ms per word.
data_clean <- data_clean %>%
  mutate(log_rt8_per_word = log(rt_8 / words_rt8))

cat("\n--- rt_8 per word read ---\n")
m_rt8 <- lm(log_rt8_per_word ~ E_num * R_num, data = data_clean)
print(car::Anova(m_rt8, type = 3))
print(round(summary(m_rt8)$coefficients, 4))

cat("\n--- rt_8 per word, adjusted for individual reading speed ---\n")
m_rt8b <- lm(log_rt8_per_word ~ E_num * R_num + log(read_speed),
             data = data_clean)
print(car::Anova(m_rt8b, type = 3))
print(round(summary(m_rt8b)$coefficients, 4))

# Does deliberation time predict the disclosure decision?
cat("\n--- Does rt_8 predict disclosure? ---\n")
print(round(summary(glm(SDR_binary ~ log_rt8 + E_num * R_num,
                        family = binomial, data = data_clean))$coefficients, 4))

# Total interaction duration, for reference.
cat("\n--- Chat duration by condition (seconds) ---\n")
print(as.data.frame(
  data_clean %>%
    group_by(E_f, R_f) %>%
    summarise(n = n(),
              median = median(as.numeric(chat_duration)),
              mean   = mean(as.numeric(chat_duration)),
              .groups = "drop")
), digits = 4)

# Validity check for the per-word normalisation.
# rt_3 is the only decision point with comparable text length across
# conditions (78-91 words). A genuine recognition effect should appear there
# as well. It does not (b = -0.037, p = .706), nor at rt_5 (p = .999),
# which indicates that the recognition effect in the normalised rt_8 model
# is an artefact: reading time does not scale proportionally with word count,
# so dividing by words overcorrects where text was longest.
#
# Conclusion: response times in this design measure reading time rather than
# deliberation. They are reported as a null result.
cat("\n--- Normalisation validity check (comparable text length) ---\n")
print(round(summary(lm(log(rt_3) ~ E_num * R_num, data = data_clean))$coefficients, 4))
print(round(summary(lm(log(rt_5) ~ E_num * R_num, data = data_clean))$coefficients, 4))


# ---- Descriptives cited in the Discussion (not preregistered) ----

# (a) Disclosure choice by condition, with the national ID share on two
# bases (all participants, disclosers only); see Section 5.4.
cat("\n--- Disclosure choice by condition (counts) ---\n")
print(table(data_clean$choice_8, interaction(data_clean$E_f, data_clean$R_f)))

summarise_disclosure <- function(d) {
  d %>% dplyr::summarise(
    n                   = dplyr::n(),
    n_disclosed         = sum(choice_8 != "data_refused"),
    n_id                = sum(choice_8 == "data_id"),
    id_share_all        = n_id / n,
    id_share_disclosers = n_id / n_disclosed,
    .groups = "drop"
  )
}

disclosure_by_cell <- dplyr::bind_rows(
  data_clean %>%
    dplyr::group_by(E_f, R_f) %>%
    summarise_disclosure() %>%
    dplyr::mutate(E_f = as.character(E_f), R_f = as.character(R_f)),
  data_clean %>%
    summarise_disclosure() %>%
    dplyr::mutate(E_f = "all", R_f = "all")
)

cat("\n--- National ID share by condition ---\n")
print(as.data.frame(disclosure_by_cell), digits = 3)

# Exploratory test: national ID vs. all other choices across the four cells.
id_tab <- table(id_chosen = data_clean$choice_8 == "data_id",
                cell = interaction(data_clean$E_f, data_clean$R_f))
cat("\n--- National ID vs. other choices by condition ---\n")
print(id_tab)
print(suppressWarnings(chisq.test(id_tab)))
print(fisher.test(id_tab))   # exact test, given small cell counts


# (b) Correlations of the binary decisions with the evaluation outcomes
# (point-biserial = Pearson r with a 0/1 variable). Correlational and,
# for uptake, constrained by the ceiling (92.4%).
decision_cor <- dplyr::bind_rows(lapply(c("uptake_num", "SDR_binary"), function(dec) {
  dplyr::bind_rows(lapply(c(CT = "CTRU_mean", PAT = "PAT_mean", AT = "ATRU_mean"),
                          function(v) {
                            ct <- cor.test(data_clean[[dec]], data_clean[[v]])
                            tibble::tibble(decision = dec, variable = v,
                                           r = unname(ct$estimate),
                                           ci_low = ct$conf.int[1], ci_high = ct$conf.int[2],
                                           p = ct$p.value)
                          }))
}))

cat("\n--- Correlations of decisions with evaluation outcomes ---\n")
print(as.data.frame(decision_cor), digits = 3)

# (c) Frequency of prior chatbot use, plus value labels, which
# zap_labels() removed on import. Gender labels are printed as well
# for the sample description in Section 4.1.
chatbot_freq <- data_clean %>%
  dplyr::count(A7.1_EXP_chatbot) %>%
  dplyr::mutate(prop = n / sum(n))

cat("\n--- Prior chatbot use (frequency) ---\n")
print(as.data.frame(chatbot_freq), digits = 3)

label_source <- haven::read_sav(post_file)
if (!"A7.1_EXP_chatbot" %in% names(label_source)) {
  label_source <- haven::read_sav(pre_file)
}
cat("\n--- Value labels: prior chatbot use ---\n")
haven::print_labels(label_source$A7.1_EXP_chatbot)

label_source_dem <- if ("DEM_gender" %in% names(label_source)) {
  label_source
} else {
  haven::read_sav(pre_file)
}
cat("\n--- Value labels: gender ---\n")
haven::print_labels(label_source_dem$DEM_gender)




# ---- Exports ----
path_expl <- file.path(path_out, "exploratory")
if (!dir.exists(path_expl)) dir.create(path_expl, recursive = TRUE)

# Additional outcomes (preregistered as exploratory/descriptive)
extra_results <- dplyr::bind_rows(lapply(names(extra_models), function(nm) {
  broom::tidy(extra_models[[nm]], conf.int = TRUE) %>% mutate(Outcome = nm)
}))
readr::write_csv(extra_results, file.path(path_expl, "additional_outcomes.csv"))

extra_eta <- dplyr::bind_rows(lapply(names(extra_models), function(nm) {
  effectsize::eta_squared(car::Anova(extra_models[[nm]], type = 3),
                          partial = TRUE) %>%
    as.data.frame() %>% mutate(Outcome = nm)
}))
readr::write_csv(extra_eta, file.path(path_expl, "additional_outcomes_eta.csv"))

# Balance checks
readr::write_csv(balance_table, file.path(path_expl, "balance_age.csv"))

# Moderation: all six models, fully archived rather than selectively reported
mod_results <- dplyr::bind_rows(lapply(mod_vars, function(mv) {
  dplyr::bind_rows(lapply(mod_outcomes, function(dv) {
    m <- lm(as.formula(paste(dv, "~ E_num * R_num *", mv)), data = data_clean)
    as.data.frame(car::Anova(m, type = 3)) %>%
      tibble::rownames_to_column("effect") %>%
      mutate(Outcome = dv, Moderator = mv)
  }))
}))
readr::write_csv(mod_results, file.path(path_expl, "moderation_models.csv"))

# Mediation
med_summary <- summary(med_out)
mediation_table <- tibble::tibble(
  effect   = c("ACME (indirect)", "ADE (direct)", "Total", "Prop. mediated"),
  estimate = c(med_summary$d.avg, med_summary$z.avg,
               med_summary$tau.coef, med_summary$n.avg),
  ci_low   = c(med_summary$d.avg.ci[1], med_summary$z.avg.ci[1],
               med_summary$tau.ci[1], med_summary$n.avg.ci[1]),
  ci_high  = c(med_summary$d.avg.ci[2], med_summary$z.avg.ci[2],
               med_summary$tau.ci[2], med_summary$n.avg.ci[2]),
  p        = c(med_summary$d.avg.p, med_summary$z.avg.p,
               med_summary$tau.p, med_summary$n.avg.p)
)
readr::write_csv(mediation_table, file.path(path_expl, "mediation_soc_atru.csv"))

# Response times: reported as a null result (see validity check above)
rt_summary <- data_clean %>%
  group_by(E_f, R_f) %>%
  summarise(n = n(),
            words_before_rt8 = first(words_rt8),
            rt8_median_s = median(rt_8) / 1000,
            rt8_ms_per_word = median(rt_8) / first(words_rt8),
            chat_duration_median_s = median(as.numeric(chat_duration)),
            .groups = "drop")
readr::write_csv(rt_summary, file.path(path_expl, "response_times.csv"))

# Institutional trust: descriptive distribution
tru_desc <- tibble::tibble(
  M = mean(data_clean$TRU_ngo_mean),
  SD = sd(data_clean$TRU_ngo_mean),
  median = median(data_clean$TRU_ngo_mean),
  alpha = psych::alpha(data_clean[c("A7.3_TRU_ngo_1", "A7.3_TRU_ngo_2",
                                    "A7.3_TRU_ngo_3")],
                       check.keys = FALSE)$total$raw_alpha,
  prop_below_midpoint = mean(data_clean$TRU_ngo_mean < 4),
  prop_high = mean(data_clean$TRU_ngo_mean >= 6)
)
readr::write_csv(tru_desc, file.path(path_expl, "institutional_trust.csv"))

cat("\n=== Exploratory pipeline complete ===\n")
print(list.files(path_expl))

# ---- Item-level diagnostics ----
# Reported in the appendix. Two scales fell short of conventional thresholds
# (RISK a = .50, REC a = .65) and the item-level pattern is documented here.
# check.keys = FALSE: item keying is fixed a priori from the instrument.
scales_diag <- list(
  PAT  = c("PAT_01","PAT_02","PAT_03","PAT_04","PAT_05"),
  ATRU = c("ATRU_01_1","ATRU_02_1","ATRU_03_1","ATRU_04_1"),
  CTRU = c("CTRU_01_1","CTRU_02_1","CTRU_03_1","CTRU_04_1"),
  IMM  = c("IMM_1","IMM_2","IMM_3","IMM_4"),
  SOC  = c("SOC_1","SOC_2","SOC_4"),
  DISC = c("DISC_1","DISC_2","DISC_3"),
  AGE  = c("AGE_1","AGE_2","AGE_3","AGE_4"),
  RISK = c("RISK_1","RISK_2_rev","RISK_3"),
  EMP  = c("EMP_01","EMP_02","EMP_03"),
  REC  = c("REC_01","REC_02","REC_03")
)

alpha_drop <- dplyr::bind_rows(lapply(names(scales_diag), function(nm) {
  a <- suppressWarnings(psych::alpha(data_clean[, scales_diag[[nm]]],
                                     check.keys = FALSE))
  as.data.frame(a$alpha.drop) %>%
    tibble::rownames_to_column("item_dropped") %>%
    mutate(scale = nm, .before = 1)
}))

cat("--- Alpha if item dropped ---\n")
print(as.data.frame(alpha_drop), digits = 3)
readr::write_csv(alpha_drop, file.path(path_expl, "alpha_if_dropped.csv"))

# ---- H1 item-level decomposition ----
# The scale combines three ETRS items (PAT_01-03) with two role-appropriateness
# items (PAT_04-05). The interaction is reported per item to show which items
# carry it.
pat_items <- dplyr::bind_rows(lapply(scales_diag$PAT, function(i) {
  broom::tidy(lm(as.formula(paste(i, "~ E_num * R_num")), data = data_clean)) %>%
    mutate(item = i, .before = 1)
}))

cat("\n--- H1: item-level effects ---\n")
print(as.data.frame(pat_items), digits = 3)
readr::write_csv(pat_items, file.path(path_expl, "paternalism_item_level.csv"))

# Robustness: H1 restricted to the three items closest to the original
# ETRS control/dominance subscale.
data_clean <- data_clean %>%
  mutate(PAT_core = rowMeans(across(c(PAT_01, PAT_02, PAT_03))))

cat("\n--- H1 robustness: ETRS core items only (PAT_01-03) ---\n")
m_core <- lm(PAT_core ~ E_num * R_num, data = data_clean)
print(car::Anova(m_core, type = 3))
print(round(summary(m_core)$coefficients, 4))
cat("Cronbach's alpha, core items:",
    round(psych::alpha(data_clean[c("PAT_01","PAT_02","PAT_03")],
                       check.keys = FALSE)$total$raw_alpha, 3), "\n")

