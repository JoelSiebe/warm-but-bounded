# 03_manuscript_tables.R

# Master's thesis analysis: Warm but Bounded?
# Empathic and Recognition-Oriented Communication in NGO Chatbots

# Author: Joël Sieber
# Code written with AI assistance (Claude, Anthropic); see README.

# Export of every table reported in the manuscript.
#
# Purpose: this script does not format tables for publication. The manuscript
# tables are typeset in LaTeX. What this script does is export the underlying
# numbers, one file per manuscript table, so that every value in the thesis can
# be traced to a specific model and verified independently.
#
# Output: output/manuscript_tables/, one CSV per table. File names follow an
# earlier numbering; the README maps each file to its table in the thesis.
#
# Requires 02_exploratory.R, which in turn sources 01_confirmatory.R.

source(here::here("R", "02_exploratory.R"))

path_tab <- file.path(path_out, "manuscript_tables")
if (!dir.exists(path_tab)) dir.create(path_tab, recursive = TRUE)

wr <- function(x, name) {
  readr::write_csv(as.data.frame(x), file.path(path_tab, paste0(name, ".csv")))
  cat("  written:", name, "\n")
}

fmt_p <- function(p) ifelse(is.na(p), NA_character_,
                     ifelse(p < .001, "< .001", sprintf("%.3f", p)))

# Partial eta squared from an F value and its residual df.
eta_from_F <- function(f, df_res) f / (f + df_res)

cat("\n=== Exporting manuscript tables ===\n")

# ---- Table 2: Sample flow ----
wr(attrition, "table2_sample_flow")

# ---- Appendix B.2: Descriptive statistics by condition ----
cont_vars <- c(PAT = "PAT_mean", ATRU = "ATRU_mean", CTRU = "CTRU_mean",
               EMP = "EMP_mean", REC = "REC_mean", SOC = "SOC_mean",
               IMM = "IMM_mean", DISC = "DISC_mean", RISK = "RISK_mean",
               AGE = "AGE_mean", TrustDiff = "Trust_Diff")

desc_cont <- dplyr::bind_rows(lapply(names(cont_vars), function(lab) {
  v <- cont_vars[[lab]]
  data_clean %>%
    dplyr::group_by(E_f, R_f) %>%
    dplyr::summarise(n = dplyr::n(),
                     M = mean(.data[[v]], na.rm = TRUE),
                     SD = sd(.data[[v]], na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(variable = lab, .before = 1)
}))

desc_bin <- dplyr::bind_rows(lapply(c(SDR = "SDR_binary", WTE = "uptake_num"),
                                    function(v) {
  data_clean %>%
    dplyr::group_by(E_f, R_f) %>%
    dplyr::summarise(n = dplyr::n(),
                     proportion = mean(.data[[v]], na.rm = TRUE),
                     successes = sum(.data[[v]] == 1, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(variable = v, .before = 1)
}))

wr(desc_cont, "appB2_descriptives_continuous")
wr(desc_bin,  "appB2_descriptives_binary")

# ----Table 3: Confirmatory hypothesis tests ----
# hyp_table applies the decision rule (Section 3.6). Effect sizes are attached
# for the three linear models; they are undefined for logistic models.
eta_lookup <- eta_table %>%
  dplyr::select(Outcome, Parameter, Eta2_partial, CI_low)

table04 <- hyp_table %>%
  dplyr::left_join(eta_lookup,
                   by = c("Model" = "Outcome", "Effect" = "Parameter")) %>%
  dplyr::mutate(p_apa = fmt_p(p)) %>%
  dplyr::select(Hyp, Model, Effect, Expected, estimate, p, p_apa,
                eta2_partial = Eta2_partial, eta2_CI_low = CI_low, supported)

wr(table04, "table3_confirmatory_tests")

# ----Appendix G.1: Exploratory outcomes ----
table05 <- dplyr::bind_rows(lapply(names(extra_models), function(nm) {
  a <- car::Anova(extra_models[[nm]], type = 3)
  ad <- as.data.frame(a) %>% tibble::rownames_to_column("effect")
  broom::tidy(extra_models[[nm]], conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::left_join(ad, by = c("term" = "effect")) %>%
    dplyr::mutate(Outcome = nm,
                  eta2_partial = eta_from_F(`F value`, df.residual(extra_models[[nm]])),
                  p_apa = fmt_p(p.value), .before = 1)
}))

wr(table05, "tableG1_exploratory_outcomes")

# ---- Appendix C.1: Continuous outcome coefficients ----
appA1 <- dplyr::bind_rows(lapply(c("H1", "H2a", "H2b"), function(nm) {
  a <- as.data.frame(anovas[[nm]]) %>% tibble::rownames_to_column("term")
  broom::tidy(models[[nm]], conf.int = TRUE) %>%
    dplyr::left_join(a, by = "term") %>%
    dplyr::mutate(Model = nm,
                  eta2_partial = eta_from_F(`F value`, df.residual(models[[nm]])),
                  p_apa = fmt_p(p.value), .before = 1)
}))

wr(appA1, "appC1_continuous_coefficients")

# ---- Appendix C.2: Binary outcome coefficients with odds ratios ----
# Wald intervals are used for the odds ratios to avoid profile-likelihood
# failures; these match the intervals reported in the manuscript.
appA2 <- dplyr::bind_rows(lapply(c("H3", "H4"), function(nm) {
  a <- as.data.frame(anovas[[nm]]) %>% tibble::rownames_to_column("term")
  broom::tidy(models[[nm]]) %>%
    dplyr::mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      OR = exp(estimate), OR_low = exp(conf.low), OR_high = exp(conf.high)
    ) %>%
    dplyr::left_join(a, by = "term") %>%
    dplyr::mutate(Model = nm, p_apa = fmt_p(p.value), .before = 1)
}))

wr(appA2, "appC2_binary_coefficients")

# ---- Appendix C.3: Estimated marginal means and predicted probabilities ----
wr(emm_table, "appC3_marginal_means")
wr(simple_table, "appC4_simple_effects")

# ---- Appendix D.1: Reliabilities ----
appB1 <- reliability %>%
  dplyr::arrange(dplyr::desc(alpha)) %>%
  dplyr::mutate(below_threshold = alpha < .70)

wr(appB1, "appD1_reliabilities")
wr(alpha_drop, "extra_alpha_if_dropped")

# ---- Appendix D.2: Manipulation checks ----
appB3 <- dplyr::bind_rows(lapply(c(MC_Empathy = "EMP_mean",
                                   MC_Recognition = "REC_mean"), function(dv) {
  m <- lm(as.formula(paste(dv, "~ E_num * R_num")), data = data_clean)
  a <- as.data.frame(car::Anova(m, type = 3)) %>% tibble::rownames_to_column("term")
  broom::tidy(m, conf.int = TRUE) %>%
    dplyr::left_join(a, by = "term") %>%
    dplyr::mutate(Outcome = dv,
                  eta2_partial = eta_from_F(`F value`, df.residual(m)),
                  p_apa = fmt_p(p.value), .before = 1)
}))

wr(appB3, "appD2_manipulation_checks")

# ---- Appendix D.3: Item-level decomposition of the H1 interaction ----
appB4 <- pat_items %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(p_apa = fmt_p(p.value))

core_row <- broom::tidy(m_core) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(item = "PAT_core (items 01-03)", p_apa = fmt_p(p.value), .before = 1)

full_row <- broom::tidy(models$H1) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(item = "PAT_mean (full scale)", p_apa = fmt_p(p.value), .before = 1)

wr(dplyr::bind_rows(appB4, full_row, core_row), "appD3_item_level_h1")

# ---- Appendix D.4: Model diagnostics ----
diag_full <- diag_table

robust_rows <- tibble::tibble(
  effect = c("H1: E x R", "H1: E x R", "H1: E x R",
             "H2a: Empathy", "H2a: Empathy", "H2a: Empathy"),
  method = c("Type III F test", "HC3 robust SE", "Permutation (10,000 draws)",
             "Type III F test", "HC3 robust SE", "Welch t test"),
  p = c(
    get_p("H1", "E_num:R_num"),
    lmtest::coeftest(models$H1,
      vcov = sandwich::vcovHC(models$H1, type = "HC3"))["E_num:R_num", 4],
    p_perm,
    get_p("H2a", "E_num"),
    lmtest::coeftest(models$H2a,
      vcov = sandwich::vcovHC(models$H2a, type = "HC3"))["E_num", 4],
    t.test(ATRU_mean ~ E_f, data = data_clean)$p.value
  )
) %>% dplyr::mutate(p_apa = fmt_p(p))

wr(diag_full, "appD4_diagnostics")
wr(robust_rows, "appD4_robustness_checks")

# ---- Appendix F.1: Sensitivity, country and recruitment wave ----
extract_key <- function(model, label, spec) {
  a <- as.data.frame(car::Anova(model, type = 3)) %>%
    tibble::rownames_to_column("term")
  cf <- broom::tidy(model)
  keys <- c("E_num", "R_num", "E_num:R_num")
  cf %>%
    dplyr::filter(term %in% keys) %>%
    dplyr::left_join(a, by = "term") %>%
    dplyr::mutate(Model = label, Specification = spec, .before = 1)
}

appC1 <- dplyr::bind_rows(
  dplyr::bind_rows(lapply(names(models), function(nm)
    extract_key(models[[nm]], nm, "preregistered"))),
  dplyr::bind_rows(lapply(names(country_models), function(nm)
    extract_key(country_models[[nm]], nm, "country-adjusted"))),
  dplyr::bind_rows(lapply(names(wave_adjusted), function(nm)
    extract_key(wave_adjusted[[nm]], nm, "wave-adjusted")))
) %>% dplyr::mutate(p_apa = fmt_p(p.value))

wr(appC1, "appF1_sensitivity")

# Full Type III tables for the country models, including the country term
appC1b <- dplyr::bind_rows(lapply(names(country_models), function(nm) {
  as.data.frame(car::Anova(country_models[[nm]], type = 3)) %>%
    tibble::rownames_to_column("effect") %>%
    dplyr::mutate(Model = nm, .before = 1)
}))

wr(appC1b, "appF1_country_models_full")

# ---- Appendix F.2: Recruitment wave moderation ----
appC2 <- dplyr::bind_rows(lapply(names(wave_moderation), function(nm) {
  as.data.frame(car::Anova(wave_moderation[[nm]], type = 3)) %>%
    tibble::rownames_to_column("effect") %>%
    dplyr::mutate(Model = nm, .before = 1)
}))

wr(appC2, "appF2_wave_moderation")

# ---- Appendix G.2: Mediation analyses ----
med_row <- function(s, model_label) {
  tibble::tibble(
    model = model_label,
    effect = c("ACME (indirect)", "ADE (direct)", "Total effect",
               "Proportion mediated"),
    estimate = c(s$d.avg, s$z.avg, s$tau.coef, s$n.avg),
    ci_low  = c(s$d.avg.ci[1], s$z.avg.ci[1], s$tau.ci[1], s$n.avg.ci[1]),
    ci_high = c(s$d.avg.ci[2], s$z.avg.ci[2], s$tau.ci[2], s$n.avg.ci[2]),
    p = c(s$d.avg.p, s$z.avg.p, s$tau.p, s$n.avg.p)
  )
}

appD1 <- dplyr::bind_rows(
  med_row(summary(med_out), "Empathy -> social presence -> affective trust"),
  med_row(summary(med_pre), "Recognition -> agency -> paternalism")
) %>% dplyr::mutate(p_apa = fmt_p(p))

# Constituent paths, so that each mediation coefficient can be traced
appD1b <- dplyr::bind_rows(
  broom::tidy(med_m)     %>% dplyr::mutate(path = "a: E -> SOC", .before = 1),
  broom::tidy(med_y)     %>% dplyr::mutate(path = "b, c': SOC + E -> ATRU", .before = 1),
  broom::tidy(med_m_pre) %>% dplyr::mutate(path = "a: R -> AGE", .before = 1),
  broom::tidy(med_y_pre) %>% dplyr::mutate(path = "b, c': AGE + R -> PAT", .before = 1)
) %>% dplyr::mutate(p_apa = fmt_p(p.value))

wr(appD1,  "appG2_mediation")
wr(appD1b, "appG2_mediation_paths")

# ---- Appendix G.3: Moderation ----
appD2 <- dplyr::bind_rows(lapply(mod_vars, function(mv) {
  dplyr::bind_rows(lapply(mod_outcomes, function(dv) {
    m <- lm(as.formula(paste(dv, "~ E_num * R_num *", mv)), data = data_clean)
    as.data.frame(car::Anova(m, type = 3)) %>%
      tibble::rownames_to_column("effect") %>%
      dplyr::mutate(Outcome = dv, Moderator = mv, .before = 1)
  }))
})) %>% dplyr::mutate(p_apa = fmt_p(`Pr(>F)`))

wr(appD2, "appG3_moderation")

# ---- Appendix G.4: Ordinal disclosure model ----
ord <- as.data.frame(summary(h3_ordinal_model)$coefficients) %>%
  tibble::rownames_to_column("term")
ord$p <- 2 * pnorm(abs(ord[["t value"]]), lower.tail = FALSE)
ord$p_apa <- fmt_p(ord$p)

grid <- expand.grid(E_num = c(-0.5, 0.5), R_num = c(-0.5, 0.5))
ord_probs <- cbind(grid, predict(h3_ordinal_model, newdata = grid, type = "probs"))
names(ord_probs)[3:6] <- c("p_none", "p_name", "p_phone", "p_id")

wr(ord, "appG4_ordinal:coefficients")
wr(ord_probs, "appG4_ordinal_probabilities")
wr(as.data.frame(table(data_clean$SDR_ordinal)), "appG4_ordinal_frequencies")

# ---- Appendix G.6: Response times ----
wr(rt_summary, "appG6_response_times")

rt_models <- tibble::tibble(
  specification = c("raw log rt_8", "raw log rt_8",
                    "per word", "per word",
                    "validity check rt_3", "validity check rt_5",
                    "rt_8 predicting disclosure"),
  effect = c("Empathy", "Empathy x Recognition",
             "Empathy", "Recognition",
             "Recognition", "Recognition", "log rt_8"),
  p = c(
    summary(lm(log_rt8 ~ E_num * R_num, data = data_clean))$coefficients["E_num", 4],
    summary(lm(log_rt8 ~ E_num * R_num, data = data_clean))$coefficients["E_num:R_num", 4],
    summary(m_rt8)$coefficients["E_num", 4],
    summary(m_rt8)$coefficients["R_num", 4],
    summary(lm(log(rt_3) ~ E_num * R_num, data = data_clean))$coefficients["R_num", 4],
    summary(lm(log(rt_5) ~ E_num * R_num, data = data_clean))$coefficients["R_num", 4],
    summary(glm(SDR_binary ~ log_rt8 + E_num * R_num,
                family = binomial, data = data_clean))$coefficients["log_rt8", 4]
  )
) %>% dplyr::mutate(p_apa = fmt_p(p))

wr(rt_models, "appG6b_response_time_models")

# ---- Appendix: Institutional trust (not reported in thesis) ----
appD5 <- tibble::tibble(
  statistic = c("M", "SD", "Median", "Cronbach's alpha",
                "Proportion below scale midpoint", "Proportion >= 6",
                "10th percentile"),
  value = c(mean(data_clean$TRU_ngo_mean), sd(data_clean$TRU_ngo_mean),
            median(data_clean$TRU_ngo_mean),
            psych::alpha(data_clean[c("A7.3_TRU_ngo_1", "A7.3_TRU_ngo_2",
                                      "A7.3_TRU_ngo_3")],
                         check.keys = FALSE)$total$raw_alpha,
            mean(data_clean$TRU_ngo_mean < 4),
            mean(data_clean$TRU_ngo_mean >= 6),
            quantile(data_clean$TRU_ngo_mean, .10))
)

wr(appD5, "extra_institutional_trust")

# ---- Appendix E: Correlation matrix (not reported in thesis) ----
# Correlations among all scale means, for completeness (not reported)
cor_vars <- c("PAT_mean", "ATRU_mean", "CTRU_mean", "AGE_mean", "SOC_mean",
              "IMM_mean", "DISC_mean", "RISK_mean", "EMP_mean", "REC_mean",
              "TRU_ngo_mean")

cor_mat <- cor(data_clean[cor_vars], use = "pairwise.complete.obs")
cor_out <- as.data.frame(round(cor_mat, 3)) %>%
  tibble::rownames_to_column("variable")

# p-values for the same matrix
cor_p <- psych::corr.test(data_clean[cor_vars], adjust = "none")$p
cor_p_out <- as.data.frame(round(cor_p, 4)) %>%
  tibble::rownames_to_column("variable")

wr(cor_out,   "extra_correlations_r")
wr(cor_p_out, "extra_correlations_p")

cat("\n--- Correlation matrix ---\n")
print(round(cor_mat, 2))

# ---- Appendix B.6: Balance checks ----
wr(balance_table, "extra_balance_age_factorial")

balance_cat <- tibble::tibble(
  variable = c("Gender", "Country", "Recruitment wave"),
  test = "Pearson chi-squared",
  statistic = c(
    suppressWarnings(chisq.test(table(data_clean$DEM_gender,
      interaction(data_clean$E_f, data_clean$R_f))))$statistic,
    suppressWarnings(chisq.test(table(data_clean[[COUNTRY_VAR]],
      interaction(data_clean$E_f, data_clean$R_f))))$statistic,
    suppressWarnings(chisq.test(table(data_clean$recruitment_wave,
      interaction(data_clean$E_f, data_clean$R_f))))$statistic
  ),
  df = c(
    suppressWarnings(chisq.test(table(data_clean$DEM_gender,
      interaction(data_clean$E_f, data_clean$R_f))))$parameter,
    suppressWarnings(chisq.test(table(data_clean[[COUNTRY_VAR]],
      interaction(data_clean$E_f, data_clean$R_f))))$parameter,
    suppressWarnings(chisq.test(table(data_clean$recruitment_wave,
      interaction(data_clean$E_f, data_clean$R_f))))$parameter
  ),
  p = c(
    suppressWarnings(chisq.test(table(data_clean$DEM_gender,
      interaction(data_clean$E_f, data_clean$R_f))))$p.value,
    suppressWarnings(chisq.test(table(data_clean[[COUNTRY_VAR]],
      interaction(data_clean$E_f, data_clean$R_f))))$p.value,
    suppressWarnings(chisq.test(table(data_clean$recruitment_wave,
      interaction(data_clean$E_f, data_clean$R_f))))$p.value
  )
) %>% dplyr::mutate(p_apa = fmt_p(p))

wr(balance_cat, "appB6_balance_categorical")

# Values as reported in Appendix Table B.6 (computed once in 02_exploratory.R)
balance_b6 <- tibble::tibble(
  variable = c("Age", "Gender"),
  test     = c("One-way ANOVA across four cells",
               "Fisher's exact test (simulated, B = 10000)"),
  statistic = c(age_omnibus$`F value`[1], NA),
  df1       = c(age_omnibus$Df[1], NA),
  df2       = c(age_omnibus$Df[2], NA),
  p         = c(age_omnibus$`Pr(>F)`[1], gender_fisher$p.value)
) %>% dplyr::mutate(p_apa = fmt_p(p))

wr(balance_b6, "appB6_balance_table")

# ---- Sample characteristics ----
sample_desc <- tibble::tibble(
  statistic = c("N", "Age M", "Age SD",
                "Chat duration median (s)",
                "Prior NGO contact (proportion)",
                "Received NGO support (n)"),
  value = c(nrow(data_clean),
            mean(data_clean$DEM_age, na.rm = TRUE),
            sd(data_clean$DEM_age, na.rm = TRUE),
            median(as.numeric(data_clean$chat_duration), na.rm = TRUE),
            mean(as.numeric(data_clean$A7.2_EXP_ngo_binary) == 1, na.rm = TRUE),
            sum(as.numeric(data_clean$A7.2_EXP_ngo_type) == 1, na.rm = TRUE))
)

wr(sample_desc, "sec4-1_sample_characteristics")
wr(as.data.frame(table(data_clean$DEM_gender)), "sec4-1_gender")
wr(as.data.frame(table(data_clean[[COUNTRY_VAR]])), "appB3_country")
wr(disclosure_by_cell, "appG5_id_choice")
wr(decision_cor,       "sec4-3_decision_correlations")
wr(chatbot_freq,       "appB4_prior_chatbot_use")

# ---- Figure 3: Estimated marginal means, three continuous outcomes ----
# Monochrome (line type and shape) so that the figure prints in greyscale.
fig_dat <- emm_table %>%
  dplyr::filter(Outcome %in% c("H1", "H2a", "H2b")) %>%
  dplyr::mutate(
    panel = factor(Outcome, levels = c("H1", "H2a", "H2b"),
                   labels = c("Perceived paternalism (1 to 7)",
                              "Affective trust (-3 to +3)",
                              "Cognitive trust (-3 to +3)")),
    Empathy = factor(E_label, levels = c("lowE", "highE"),
                     labels = c("Low", "High")),
    Recognition = factor(R_label, levels = c("lowR", "highR"),
                         labels = c("Low recognition", "High recognition"))
  )

pd <- position_dodge(width = 0.15)

fig1 <- ggplot(fig_dat, aes(x = Empathy, y = emmean, group = Recognition,
                            linetype = Recognition, shape = Recognition)) +
  geom_line(position = pd) +
  geom_point(size = 2.4, position = pd) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.08, position = pd) +
  facet_wrap(~ panel, scales = "free_y") +
  labs(x = "Empathic communication",
       y = "Estimated marginal mean (95% CI)",
       linetype = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(path_out, "figure1_emm_panels.pdf"), fig1,
       width = 7, height = 3)
cat("  written: figure1_emm_panels.pdf\n")

cat("\n=== Manuscript tables complete ===\n")
cat("Output directory:", path_tab, "\n")
print(list.files(path_tab))
