# 04_additional_analyses.R

# Master's thesis analysis: Warm but Bounded? 
# Empathic and Recognition-Oriented Communication in NGO Chatbots

# Author: Joël Sieber
# Code written with AI assistance (Claude, Anthropic); see README.

# Additional analyses (not preregistered): equivalence tests, multiplicity
# adjustment, attrition, discriminant validity, text length, mediation
# sensitivity, and effect-size intervals.
#
# Run in the same session after 03_manuscript_tables.R (see README).
# -----------------------------------------------------------------------------


if (!exists("path_out")) source(here::here("R", "00_setup.R"))

# 01 and 02 must have run first: this script reuses their fitted models rather
# than refitting them, so the numbers cannot drift apart.
needed <- c("data_clean", "models", "anovas", "hyp_table", "reliability",
            "scales_def", "pre_unique", "post_unique",
            "ID_PRE_RESPONSE", "ID_POST_PID")
absent <- needed[!vapply(needed, exists, logical(1))]

if (length(absent) > 0) {
  stop("Missing objects: ", paste(absent, collapse = ", "),
       "\nRun in this order, in one session:\n",
       "  source(here::here('R', '01_confirmatory.R'))\n",
       "  source(here::here('R', '02_exploratory.R'))\n",
       "  source(here::here('R', '04_additional_analyses.R'))")
}

# med_out / med_pre come from 02_exploratory.R. Block 6 is skipped if absent,
# so this script still runs when only 01 has been executed.
if (!exists("med_out")) {
  message("Note: med_out not found - mediation sensitivity (block 6) will be skipped. ",
          "Run 02_exploratory.R first to include it.")
}

path_add <- file.path(path_out, "additional")
if (!dir.exists(path_add)) dir.create(path_add, recursive = TRUE)

N_total <- nrow(data_clean)

# ---- 1. EQUIVALENCE TESTS (Appendix E.1)--------------------------------------

SESOI_f  <- 0.15
SESOI_f2 <- SESOI_f^2
lambda_bound <- SESOI_f2 * N_total

cat("\n=== Equivalence bound ===\n")
cat("SESOI: f =", SESOI_f, "| f^2 =", round(SESOI_f2, 4),
    "| eta2p =", round(SESOI_f2 / (1 + SESOI_f2), 4),
    "| lambda =", round(lambda_bound, 2), "\n")

equiv_ftest <- function(F_obs, df1, df2, lambda = lambda_bound) {
  pf(F_obs, df1, df2, ncp = lambda, lower.tail = TRUE)
}

# Applied to the three linear confirmatory models.
equiv_linear <- dplyr::bind_rows(lapply(c("H1", "H2a", "H2b"), function(nm) {
  a      <- as.data.frame(anovas[[nm]])
  df_res <- df.residual(models[[nm]])
  terms  <- c("E_num", "R_num", "E_num:R_num")
  Fcol   <- grep("^F", names(a))[1]
  pcol   <- grep("^Pr", names(a))[1]
  
  tibble::tibble(
    Model   = nm,
    Effect  = terms,
    F_obs   = a[terms, Fcol],
    df1     = 1,
    df2     = df_res,
    p_nhst  = a[terms, pcol],
    p_equiv = equiv_ftest(a[terms, Fcol], 1, df_res)
  )
}))

equiv_linear <- equiv_linear %>%
  dplyr::mutate(
    conclusion = dplyr::case_when(
      p_nhst <  .05 & p_equiv <  .05 ~ "significant but smaller than SESOI",
      p_nhst <  .05 & p_equiv >= .05 ~ "significant, not equivalent",
      p_nhst >= .05 & p_equiv <  .05 ~ "equivalent (effects >= SESOI rejected)",
      TRUE                           ~ "inconclusive (underpowered)"
    )
  )

cat("\n=== Equivalence tests: linear models ===\n")
print(as.data.frame(equiv_linear), digits = 4)
readr::write_csv(equiv_linear, file.path(path_add, "equivalence_linear.csv"))

# ---- Binary outcomes -------------------------------------------------------
# The bound below is derived from the same threshold (Appendix E.2):
SESOI_d     <- 2 * SESOI_f                     # 0.30
SESOI_logOR <- SESOI_d * pi / sqrt(3)          # ~0.544
SESOI_OR    <- exp(SESOI_logOR)                # ~1.72

cat("\nDerived binary bound: d =", SESOI_d,
    "| log OR =", round(SESOI_logOR, 3),
    "| OR =", round(SESOI_OR, 3), "\n")

# TOST at alpha = .05 corresponds to checking whether the 90% CI lies
# inside the equivalence band. Wald intervals are used for consistency
# with Appendix Table A2.
EQUIV_CONF <- 0.90
z_equiv    <- qnorm(1 - (1 - EQUIV_CONF) / 2)   # 1.645

equiv_binary <- dplyr::bind_rows(lapply(c("H3", "H4"), function(nm) {
  broom::tidy(models[[nm]]) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::transmute(
      Model      = nm, 
      Effect     = term,
      logOR      = estimate,
      ci_low     = estimate - z_equiv * std.error,
      ci_high    = estimate + z_equiv * std.error,
      OR         = exp(logOR), 
      OR_low     = exp(ci_low), 
      OR_high    = exp(ci_high),
      conf_level = EQUIV_CONF,
      equivalent = ci_low > -SESOI_logOR & ci_high < SESOI_logOR
    )
}))

cat("\n=== Equivalence by 90% CI inclusion (TOST, alpha = .05): binary models ===\n")
print(as.data.frame(equiv_binary), digits = 4)
readr::write_csv(equiv_binary, file.path(path_add, "equivalence_binary.csv"))

cat("\nNote: H4 has too little variance (92.4% uptake) for an equivalence claim.\n")

# ---- 2. MULTIPLICITY (Appendix E.1)-------------------------------------------

multiplicity <- hyp_table %>%
  dplyr::select(Hyp, Model, Effect, Expected, estimate, p, supported) %>%
  dplyr::mutate(
    p_holm = p.adjust(p, method = "holm"),
    p_BH   = p.adjust(p, method = "BH"),
    supported_holm = !is.na(p_holm) & p_holm < .05 &
      dplyr::if_else(Expected == "positive", estimate > 0, estimate < 0)
  )

cat("\n=== Multiplicity-adjusted hypothesis tests ===\n")
print(as.data.frame(multiplicity), digits = 4)
readr::write_csv(multiplicity, file.path(path_add, "multiplicity.csv"))


# ---- 3. DIFFERENTIAL ATTRITION BY CONDITION (Appendix B.5)--------------------
# Condition was assigned before the chatbot, so it is known for participants
# who dropped out. Tests whether dropout differs by condition.

attrition_by_cond <- pre_unique %>%
  dplyr::mutate(
    completed = .data[[ID_PRE_RESPONSE]] %in% post_unique[[ID_POST_PID]],
    cond_num  = as.numeric(as.character(cond))
  ) %>%
  dplyr::filter(!is.na(cond_num))

attr_tab <- table(attrition_by_cond$cond_num, attrition_by_cond$completed)
attr_chi <- suppressWarnings(chisq.test(attr_tab))

cat("\n=== Attrition by condition ===\n")
print(attr_tab)
cat("\nDropout rate per condition:\n")
print(round(prop.table(attr_tab, 1)[, "FALSE"], 4))
print(attr_chi)

# Same test on the E and R factors separately (more interpretable than 4 cells).
attrition_by_cond <- attrition_by_cond %>%
  dplyr::mutate(
    E_hi = cond_num %in% c(2, 4),
    R_hi = cond_num %in% c(3, 4)
  )
cat("\n--- Dropout by empathy factor ---\n")
print(suppressWarnings(chisq.test(table(attrition_by_cond$E_hi,
                                        attrition_by_cond$completed))))
cat("\n--- Dropout by recognition factor ---\n")
print(suppressWarnings(chisq.test(table(attrition_by_cond$R_hi,
                                        attrition_by_cond$completed))))

readr::write_csv(as.data.frame(attr_tab),
                 file.path(path_add, "attrition_by_condition.csv"))

# 4. DISCRIMINANT VALIDITY: affective trust vs. perceived empathy (Appendix D.5)
# Checks whether affective trust (amiability) is distinguishable from the
# empathy manipulation check.

r_emp_atru <- cor(data_clean$EMP_mean, data_clean$ATRU_mean,
                  use = "pairwise.complete.obs")
a_emp  <- reliability$alpha[reliability$scale == "EMP"]
a_atru <- reliability$alpha[reliability$scale == "ATRU"]
r_disatt <- r_emp_atru / sqrt(a_emp * a_atru)

cat("\n=== Discriminant validity: EMP vs ATRU ===\n")
cat("r =", round(r_emp_atru, 3),
    "| disattenuated r =", round(r_disatt, 3),
    "| adequate if < .85\n")

# Comparison anchor: how much lower is the EMP-CTRU correlation?
cat("r(EMP, CTRU) =",
    round(cor(data_clean$EMP_mean, data_clean$CTRU_mean,
              use = "pairwise.complete.obs"), 3), "\n")

# ---- Confirmatory factor analysis: two factors vs. one ----------------------
if (requireNamespace("lavaan", quietly = TRUE)) {
  emp_items  <- scales_def$EMP
  atru_items <- scales_def$ATRU
  
  m2 <- paste0(
    "EMP  =~ ", paste(emp_items,  collapse = " + "), "\n",
    "ATRU =~ ", paste(atru_items, collapse = " + ")
  )
  m1 <- paste0("G =~ ", paste(c(emp_items, atru_items), collapse = " + "))
  
  fit2 <- lavaan::cfa(m2, data = data_clean, estimator = "MLR")
  fit1 <- lavaan::cfa(m1, data = data_clean, estimator = "MLR")
  
  cat("\n--- CFA: two-factor model ---\n")
  print(lavaan::fitMeasures(fit2, c("cfi.robust", "tli.robust",
                                    "rmsea.robust", "srmr")))
  cat("\n--- CFA: one-factor model ---\n")
  print(lavaan::fitMeasures(fit1, c("cfi.robust", "tli.robust",
                                    "rmsea.robust", "srmr")))
  cat("\n--- Model comparison ---\n")
  print(lavaan::anova(fit1, fit2))
  cat("\nFactor correlation (two-factor model):\n")
  print(lavaan::lavInspect(fit2, "cor.lv"))
} else {
  cat("\nlavaan not installed - CFA skipped. install.packages('lavaan')\n")
}

# ---- Effect on affective trust controlling for perceived empathy ------------
# CAUTION: EMP_mean is measured post-treatment. Controlling for it is
# overcontrol / partial mediation, not a clean covariate adjustment. Report as
# a descriptive upper-bound check only, never as the primary estimate.
m_atru_ctrl <- lm(ATRU_mean ~ E_num * R_num + EMP_mean, data = data_clean)
cat("\n--- ATRU model controlling for perceived empathy (descriptive) ---\n")
print(round(summary(m_atru_ctrl)$coefficients, 4))
cat("Unadjusted E coefficient:", round(coef(models$H2a)["E_num"], 4), "\n")

# 5. TEXT LENGTH (not reported in the thesis; supports Section 5.6)

word_counts <- c(lowE_lowR = 231, highE_lowR = 381, lowE_highR = 302,
                 highE_highR = 476)
cat("\n=== Word counts per condition ===\n"); print(word_counts)

matched <- data_clean %>%
  dplyr::filter((E_f == "highE" & R_f == "lowR") |
                  (E_f == "lowE"  & R_f == "highR")) %>%
  dplyr::mutate(cell = paste0(E_f, "_", R_f))

cat("\n--- Length-matched contrast on affective trust ---\n")
print(matched %>% dplyr::group_by(cell) %>%
        dplyr::summarise(n = dplyr::n(), M = mean(ATRU_mean),
                         SD = sd(ATRU_mean), .groups = "drop"))
print(t.test(ATRU_mean ~ cell, data = matched))

# Non-monotonicity check: cell means ordered by word count.
cat("\n--- Cell means for the non-monotonicity argument ---\n")
print(data_clean %>% dplyr::group_by(E_f, R_f) %>%
        dplyr::summarise(n = dplyr::n(),
                         ATRU = mean(ATRU_mean), PAT = mean(PAT_mean),
                         .groups = "drop"))

# ---- 6. MEDIATION SENSITIVITY-------------------------------------------------
# The social-presence mediation assumes no unmeasured mediator-outcome
# confounding. Both variables were measured concurrently, so that assumption is
# not credible. medsens reports the correlation rho at which the indirect
# effect would vanish.

if (exists("med_out")) {
  cat("\n=== Sensitivity: E -> social presence -> affective trust ===\n")
  s_soc <- mediation::medsens(med_out, rho.by = 0.05, effect.type = "indirect")
  print(summary(s_soc))
  png(file.path(path_add, "medsens_social_presence.png"),
      width = 1600, height = 1200, res = 200)
  plot(s_soc, main = "Sensitivity: social presence mediation")
  dev.off()
}

if (exists("med_pre")) {
  cat("\n=== Sensitivity: R -> agency -> paternalism ===\n")
  s_age <- mediation::medsens(med_pre, rho.by = 0.05, effect.type = "indirect")
  print(summary(s_age))
}

# ---- 7. EFFECT SIZES: full confidence intervals-------------------------------
# Partial eta squared with full confidence intervals (Table 3 note).
eta_full <- dplyr::bind_rows(lapply(c("H1", "H2a", "H2b"), function(nm) {
  effectsize::eta_squared(anovas[[nm]], partial = TRUE, ci = 0.95) %>%
    as.data.frame() %>% dplyr::mutate(Outcome = nm)
}))

cat("\n=== Partial eta squared with full CIs ===\n")
print(as.data.frame(eta_full), digits = 4)
readr::write_csv(eta_full, file.path(path_add, "effect_sizes_full_ci.csv"))

# Cohen's d for the empathy main effect, for magnitude reporting in the text.
cat("\n--- Standardised empathy effect on affective trust ---\n")
print(effectsize::cohens_d(ATRU_mean ~ E_f, data = data_clean))
cat("Scale range: -3 to +3 (6 points). A raw effect of",
    round(coef(models$H2a)["E_num"], 3), "equals",
    round(100 * coef(models$H2a)["E_num"] / 6, 1), "% of the scale range.\n")


cat("\n=== 04_additional_analyses.R complete ===\n")
cat("Output written to:", path_add, "\n")