# 01_confirmatory.R

# Master's thesis analysis: Warm but Bounded? 
# Empathic and Recognition-Oriented Communication in NGO Chatbot

# Author: Joël Sieber
# Code written with AI assistance (Claude, Anthropic); see README.

# Confirmatory and sensitivity analysis, main study.

# Preregistered primary models:
#   outcome ~ empathic communication * recognition-oriented communication
#
# Input: Qualtrics pre- and post-exposure exports (SPSS .sav).

source(here::here("R", "00_setup.R"))

# ---- Configuration ----
pre_file <- file.path(path_raw, "Pre_Exposure.sav")
post_file <- file.path(path_raw, "Post_Exposure.sav")

# Preregistered target N and attention check rule.
# ATTCH is embedded in the "social presence" block and exported as SOC_3.
# The instructed response is "Somewhat agree", i.e. value 5 on the 7-point scale.
N_TARGET        <- 360
ATTENTION_VAR   <- "SOC_3"
ATTENTION_VALUE <- 5

# Pre-post linkage: the pre-survey ResponseId is passed via the external
# chatbot-interface website to the post-survey as pid.
ID_PRE_RESPONSE <- "ResponseId"
ID_POST_PID <- "pid"

# Prolific IDs are used for (secondary) identify verification, not as the merge key.
ID_PRE_PROLIFIC <- "Prolific_1"
ID_POST_PROLIFIC <- "Prolific_2"

COUNTRY_VAR <- "DEM_country"

# Main study field period. Earlier records stem from pilot runs & pretests
# that used the same Qualtrics links
MAIN_START <- as.POSIXct("2026-04-28 00:00:00", tz = "Europe/Zurich")
MAIN_END   <- as.POSIXct("2026-04-30 23:00:00", tz = "Europe/Zurich")

# Point at which the Prolific student prescreener was removed.
# The exact wall-clock time is uncertain (Prolific dashboard time zone vs.
# Qualtrics UTC export), but no responses were recorded between 17:15 and
# 19:15 CEST, so any cutoff in that window yields identical wave assignment
# (223 / 146). Verified explicitly.
WAVE_CUTOFF <- as.POSIXct("2026-04-29 17:15:00", tz = "Europe/Zurich")

# ---- Import ----
# read_sav() keeps SPSS metadata; zap_labels() strips value labels
# and retains the underlying numeric recode values.
pre_raw  <- haven::read_sav(pre_file)  %>% haven::zap_labels()
post_raw <- haven::read_sav(post_file) %>% haven::zap_labels()

cat("Pre rows imported: ",  nrow(pre_raw),  "\n")
cat("Post rows imported:",  nrow(post_raw), "\n")

# ---- Completed responses only ----
# Only finished responses are analysed. Later exports also contain partial
# records that Qualtrics closed automatically; these are excluded.
pre_raw  <- pre_raw  %>% filter(Finished == 1)
post_raw <- post_raw %>% filter(Finished == 1)

# ---- Field period and ID standardisation ----
# Drop rows without a Qualtrics ResponseId; these are not valid records.
pre_raw  <- pre_raw  %>% filter(!is.na(ResponseId), ResponseId != "")
post_raw <- post_raw %>% filter(!is.na(ResponseId), ResponseId != "")

# Restrict to the main study field period.
pre_raw <- pre_raw %>%
  mutate(StartDate_dt = as.POSIXct(StartDate, tz = "Europe/Zurich")) %>%
  filter(StartDate_dt >= MAIN_START, StartDate_dt <= MAIN_END)

post_raw <- post_raw %>%
  mutate(StartDate_dt = as.POSIXct(StartDate, tz = "Europe/Zurich")) %>%
  filter(StartDate_dt >= MAIN_START, StartDate_dt <= MAIN_END)

# Standardise the technical linkage IDs.
pre_raw[[ID_PRE_RESPONSE]] <- str_trim(as.character(pre_raw[[ID_PRE_RESPONSE]]))
post_raw[[ID_POST_PID]]    <- str_trim(as.character(post_raw[[ID_POST_PID]]))

# Standardise Prolific IDs. Exports may contain whitespace, upper case,
# or an appended Prolific email suffix.
clean_prolific <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_lower() %>%
    str_remove("@email\\.prolific\\.com$")
}

pre_raw[[ID_PRE_PROLIFIC]]   <- clean_prolific(pre_raw[[ID_PRE_PROLIFIC]])
post_raw[[ID_POST_PROLIFIC]] <- clean_prolific(post_raw[[ID_POST_PROLIFIC]])

cat("Pre rows in field period: ",  nrow(pre_raw),  "\n")
cat("Post rows in field period:",  nrow(post_raw), "\n")

# ---- Pre-post merge ----
# The pre-survey ResponseID was passed into the post-survey as pid.
# This technical link preserves the actual experimental session and is
# therefore the primary merge key. Prolific IDs serve as an identity check.


cat("Technical matches (ResponseId -> pid):",
    length(intersect(pre_raw[[ID_PRE_RESPONSE]], post_raw[[ID_POST_PID]])), "\n")
cat("Prolific ID matches:",
    length(intersect(pre_raw[[ID_PRE_PROLIFIC]], post_raw[[ID_POST_PROLIFIC]])), "\n")

# Keep one row per technical ID in each file to prevent many-to-many joins.
pre_unique <- pre_raw %>%
  filter(!is.na(.data[[ID_PRE_RESPONSE]]), .data[[ID_PRE_RESPONSE]] != "") %>%
  distinct(.data[[ID_PRE_RESPONSE]], .keep_all = TRUE)

post_unique <- post_raw %>%
  filter(!is.na(.data[[ID_POST_PID]]), .data[[ID_POST_PID]] != "") %>%
  distinct(.data[[ID_POST_PID]], .keep_all = TRUE)

# Inner join at session level. Suffixes distinguish variables that occur
# in both files (e.g. cond, Finished, StartDate).
data_merged <- merge(
  pre_unique, post_unique,
  by.x = ID_PRE_RESPONSE,
  by.y = ID_POST_PID,
  all = FALSE,
  suffixes = c(".pre", ".post")
)

# Store flow counts for the attrition table.
n_pre_raw     <- nrow(pre_raw)
n_post_raw    <- nrow(post_raw)
n_pre_unique  <- nrow(pre_unique)
n_post_unique <- nrow(post_unique)
n_matched     <- nrow(data_merged)

cat("Matched sessions:", n_matched, "\n")

# ---- Audit variables ----
# Created before any exclusion so that the checks can be reported.
data_merged <- data_merged %>%
  mutate(
    prolific_match  = .data[[ID_PRE_PROLIFIC]] == .data[[ID_POST_PROLIFIC]],
    cond_pre_num    = as.numeric(as.character(cond.pre)),
    cond_post_num   = as.numeric(as.character(cond.post)),
    condition_match = cond_pre_num == cond_post_num,
    Finished_post_num = as.numeric(as.character(Finished.post)),
    Progress_post_num = as.numeric(as.character(Progress.post)),
    EndDate_post_dt   = as.POSIXct(EndDate.post,   tz = "Europe/Zurich"),
    StartDate_post_dt = as.POSIXct(StartDate.post, tz = "Europe/Zurich")
  )

cat("--- Prolific ID consistency ---\n")
print(table(data_merged$prolific_match, useNA = "ifany"))

cat("--- Condition consistency (pre x post) ---\n")
print(table(data_merged$cond_pre_num, data_merged$cond_post_num, useNA = "ifany"))

# ---- Duplicate session resolution ----
# Participants could reopen the Qualtrics link, so one Prolific ID may map to
# several technically valid sessions. Retain the best session per participant.
# Priority: matching Prolific ID > matching condition > completed post-survey >
#           higher progress > later end time > later start time.
n_prolific_ids <- n_distinct(data_merged[[ID_PRE_PROLIFIC]], na.rm = TRUE)

data_merged <- data_merged %>%
  arrange(
    .data[[ID_PRE_PROLIFIC]],
    desc(prolific_match), desc(condition_match),
    desc(Finished_post_num), desc(Progress_post_num),
    desc(EndDate_post_dt), desc(StartDate_post_dt)
  ) %>%
  distinct(.data[[ID_PRE_PROLIFIC]], .keep_all = TRUE)

n_after_duplicates <- nrow(data_merged)

# ---- Condition consistency exclusion ----
# Treatment assignment must be unambiguous across pre and post.
n_condition_mismatch <- sum(!data_merged$condition_match |
                              is.na(data_merged$condition_match))

data_merged <- data_merged %>% filter(condition_match == TRUE)

# Randomisation occurred before the chatbot interaction, so the pre-survey
# condition is the final condition variable.
data_merged$cond_final <- data_merged$cond_pre_num

cat("Unique Prolific IDs:      ", n_prolific_ids, "\n")
cat("After duplicate resolution:", n_after_duplicates, "\n")
cat("Condition mismatches:     ", n_condition_mismatch, "\n")

# ---- Data validity screening ----
# Two researcher test runs (placeholder Prolific IDs "000"/"0000", implausible
# ages) are removed. Both would also fail the attention check..
EXCLUDE_TEST_RUNS <- TRUE   

data_merged <- data_merged %>%
  mutate(
    excl_invalid = if_else(
      .data[[ID_PRE_PROLIFIC]] %in% c("000", "0000") | DEM_age > 99,
      1, 0
    )
  )

n_invalid <- sum(data_merged$excl_invalid)
cat("Invalid records identified:", n_invalid, "\n")

if (EXCLUDE_TEST_RUNS) {
  data_merged <- data_merged %>% filter(excl_invalid == 0)
}

# ---- Preregistered exclusions ----
# Age below 18 and failure of the instructed-response attention check.
# Forced response prevented missing values (SOC_3: 0 missing); the NA branch
# is defensive only.
data_merged <- data_merged %>%
  mutate(
    excl_age = if_else(!is.na(DEM_age) & DEM_age < 18, 1, 0),
    excl_att = if_else(is.na(.data[[ATTENTION_VAR]]) |
                         .data[[ATTENTION_VAR]] != ATTENTION_VALUE, 1, 0)
  )

n_age_excl <- sum(data_merged$excl_age)
n_att_excl <- sum(data_merged$excl_att)
n_both     <- sum(data_merged$excl_age == 1 & data_merged$excl_att == 1)

data_clean <- data_merged %>% filter(excl_age == 0, excl_att == 0)

cat("Excluded, age < 18:        ", n_age_excl, "\n")
cat("Excluded, attention check: ", n_att_excl, "\n")
cat("Excluded on both criteria: ", n_both, "\n")
cat("Analytic sample:           ", nrow(data_clean), "\n")

# ---- Recruitment wave ----
# The Prolific student prescreener was removed during fieldwork after the
# filtered pool stopped producing responses. Coded as a recruitment wave,
# not as individual student status.
data_clean <- data_clean %>%
  mutate(
    recruitment_wave = factor(
      if_else(StartDate_dt.pre < WAVE_CUTOFF,
              "student_filter", "filter_relaxed"),
      levels = c("student_filter", "filter_relaxed")
    )
  )

cat("--- Recruitment wave ---\n")
print(table(data_clean$recruitment_wave, useNA = "ifany"))

# ---- Country ----
# DEM_country replaced the preregistered eligibility item (DEM_medellin) when
# recruitment moved to Prolific. It was collected as free text, so cleaning
# involves explicit classification decisions. Raw values are printed first so
# that every decision can be verified.
cat("--- Country, raw free-text entries ---\n")
print(sort(table(data_clean[[COUNTRY_VAR]], useNA = "ifany"), decreasing = TRUE))

# Normalise: trim, lower case, strip accents and punctuation, collapse spaces.
# Accents are removed BEFORE matching, so that variants such as "México",
# "Mexico" and "Méxicow" all map to the same country.
data_clean[[COUNTRY_VAR]] <- data_clean[[COUNTRY_VAR]] %>%
  as.character() %>%
  str_trim() %>%
  str_to_lower() %>%
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
  str_replace_all("[^a-z0-9]", "") %>%
  str_squish()

# Map spelling variants to canonical country names.
# "35" is an age entered in the country field and is treated as invalid.
data_clean[[COUNTRY_VAR]] <- case_when(
  str_detect(data_clean[[COUNTRY_VAR]], "^mexico") ~ "mexico",
  str_detect(data_clean[[COUNTRY_VAR]], "^chile")  ~ "chile",
  str_detect(data_clean[[COUNTRY_VAR]], "^argentina") ~ "argentina",
  str_detect(data_clean[[COUNTRY_VAR]], "^colombia") ~ "colombia",
  str_detect(data_clean[[COUNTRY_VAR]], "^peru")   ~ "peru",
  str_detect(data_clean[[COUNTRY_VAR]], "^suiza")  ~ "switzerland",
  data_clean[[COUNTRY_VAR]] == "" ~ NA_character_,
  TRUE ~ "unclassified"
)

cat("--- Country, after normalisation ---\n")
print(sort(table(data_clean[[COUNTRY_VAR]], useNA = "ifany"), decreasing = TRUE))

# Collapse categories with fewer than 10 cases to avoid unstable dummy
# coefficients in the country-adjusted sensitivity models.
country_counts  <- table(data_clean[[COUNTRY_VAR]])
rare_countries  <- names(country_counts[country_counts < 10])

data_clean[[COUNTRY_VAR]] <- factor(if_else(
  data_clean[[COUNTRY_VAR]] %in% rare_countries,
  "other", data_clean[[COUNTRY_VAR]]
))

cat("--- Country, analysis categories ---\n")
print(table(data_clean[[COUNTRY_VAR]], useNA = "ifany"))
cat("Collapsed into 'other':", paste(rare_countries, collapse = ", "), "\n")

# ---- Experimental coding ----
# Condition mapping from Qualtrics randomisation:
#   1 = low empathy  / low recognition
#   2 = high empathy / low recognition
#   3 = low empathy  / high recognition
#   4 = high empathy / high recognition
#
# Effect coding as preregistered: -0.5 = low, +0.5 = high.
# Main effects are therefore average effects across the other factor.
data_clean <- data_clean %>%
  mutate(
    cond_val = as.numeric(as.character(cond_final)),
    E_num = if_else(cond_val %in% c(2, 4),  0.5, -0.5),
    R_num = if_else(cond_val %in% c(3, 4),  0.5, -0.5),
    E_f   = factor(E_num, levels = c(-0.5, 0.5), labels = c("lowE", "highE")),
    R_f   = factor(R_num, levels = c(-0.5, 0.5), labels = c("lowR", "highR"))
  )

condition_counts <- data_clean %>% count(E_f, R_f, name = "n")

cat("--- Realised condition balance ---\n")
print(condition_counts)

# Verify that the recruitment wave change is balanced across conditions.
# Diagnostic only; it does not enter the primary models.
wave_by_condition <- table(data_clean$recruitment_wave,
                           interaction(data_clean$E_f, data_clean$R_f))
wave_chisq <- suppressWarnings(chisq.test(wave_by_condition))

cat("--- Recruitment wave by condition ---\n")
print(wave_by_condition)
print(wave_chisq)

# ---- Behavioural outcomes ----
# Both are in-interaction choices, recorded as text by Qualtrics.
# Raw values are printed before recoding so the mapping can be verified.
cat("--- SDR raw choice values ---\n")
print(table(data_clean$choice_8, useNA = "ifany"))
cat("--- WTE raw values ---\n")
print(table(data_clean$uptake, useNA = "ifany"))

# H3: Sensitive Disclosure Readiness.
# Preregistered primary coding is binary: 0 = refused, 1 = any identifying
# information. NA is preserved rather than silently coded as refusal.
data_clean <- data_clean %>%
  mutate(
    SDR_binary = case_when(
      choice_8 == "data_refused" ~ 0,
      is.na(choice_8) ~ NA_real_,
      TRUE ~ 1
    ),
    # Ordered version for the descriptive robustness model:
    # 0 = none, 1 = name, 2 = phone number, 3 = ID number.
    SDR_ordinal = ordered(case_when(
      choice_8 == "data_refused" ~ 0,
      choice_8 == "data_name"    ~ 1,
      choice_8 == "data_phone"   ~ 2,
      choice_8 == "data_id"      ~ 3,
      TRUE ~ NA_real_
    ), levels = 0:3),
    # H4: Willingness to Engage. Exported as "0"/"1" character values.
    uptake_num = case_when(
      as.character(uptake) == "1" ~ 1,
      as.character(uptake) == "0" ~ 0,
      TRUE ~ NA_real_
    )
  )
# Response times at the decision points, exported by Qualtrics as text.
# Converted to numeric here; analysed in 02_exploratory.R.
data_clean <- data_clean %>%
  mutate(across(c(rt_1, rt_3, rt_5, rt_8, rt_10), as.numeric))

cat("--- SDR coding check ---\n")
print(table(data_clean$choice_8, data_clean$SDR_binary, useNA = "ifany"))
cat("--- SDR ordinal ---\n")
print(table(data_clean$SDR_ordinal, useNA = "ifany"))
cat("--- WTE coding check ---\n")
print(table(data_clean$uptake, data_clean$uptake_num, useNA = "ifany"))

# ---- Scale scoring and reliability ----
# Item name mapping: the survey instrument documents items as PAT_01, SOC_01
# etc.; the Qualtrics export uses single-digit suffixes for some blocks
# (SOC_1, IMM_1, AGE_1, RISK_1, DISC_1). Names below follow the export.
#
# ATTCH is exported as SOC_3 and is therefore excluded from the SOC scale.

# RISK_02 is the only reverse-keyed item (higher agreement = lower perceived
# risk). Scale runs 1-7, so 8 - x restores the common direction.
data_clean <- data_clean %>% mutate(RISK_2_rev = 8 - as.numeric(RISK_2))

scales_def <- list(
  PAT  = c("PAT_01", "PAT_02", "PAT_03", "PAT_04", "PAT_05"),
  ATRU = c("ATRU_01_1", "ATRU_02_1", "ATRU_03_1", "ATRU_04_1"),
  CTRU = c("CTRU_01_1", "CTRU_02_1", "CTRU_03_1", "CTRU_04_1"),
  IMM  = c("IMM_1", "IMM_2", "IMM_3", "IMM_4"),
  SOC  = c("SOC_1", "SOC_2", "SOC_4"),
  DISC = c("DISC_1", "DISC_2", "DISC_3"),
  AGE  = c("AGE_1", "AGE_2", "AGE_3", "AGE_4"),
  RISK = c("RISK_1", "RISK_2_rev", "RISK_3"),
  EMP  = c("EMP_01", "EMP_02", "EMP_03"),
  REC  = c("REC_01", "REC_02", "REC_03")
)

# Verify all items exist before scoring.
missing_items <- setdiff(unlist(scales_def, use.names = FALSE), names(data_clean))
if (length(missing_items) > 0) {
  stop("Missing items: ", paste(missing_items, collapse = ", "))
}

# For each scale: Cronbach's alpha, then a row-wise mean index.
# check.keys = FALSE: item keying is fixed a priori from the instrument,
# so alpha must describe the scale that is actually analysed.
reliability <- tibble::tibble()

for (nm in names(scales_def)) {
  items <- scales_def[[nm]]
  a <- suppressWarnings(psych::alpha(data_clean[, items], check.keys = FALSE))
  
  data_clean[[paste0(nm, "_mean")]] <- rowMeans(data_clean[, items], na.rm = TRUE)
  
  reliability <- dplyr::bind_rows(reliability, tibble::tibble(
    scale = nm, n_items = length(items), alpha = a$total$raw_alpha
  ))
}

# Supplementary H2 robustness measure: positive values indicate that
# affective trust exceeds cognitive trust.
data_clean <- data_clean %>% mutate(Trust_Diff = ATRU_mean - CTRU_mean)

cat("--- Reliability ---\n")
print(as.data.frame(reliability), digits = 3)

# ---- Confirmatory models ----
# Preregistered specification for every outcome:
#   DV ~ E_num * R_num
# Linear models for continuous outcomes, logistic for binary ones.
# Type III tests are valid here because E and R are effect-coded and
# contr.sum is set in 00_setup.R.

fit_primary <- function(dv, family = "gaussian") {
  form <- as.formula(paste(dv, "~ E_num * R_num"))
  if (family == "gaussian") {
    lm(form, data = data_clean)
  } else {
    glm(form, family = binomial, data = data_clean)
  }
}

models <- list(
  H1  = fit_primary("PAT_mean"),
  H2a = fit_primary("ATRU_mean"),
  H2b = fit_primary("CTRU_mean"),
  H3  = fit_primary("SDR_binary", family = "binomial"),
  H4  = fit_primary("uptake_num", family = "binomial")
)

anovas <- lapply(models, car::Anova, type = 3)

for (nm in names(models)) {
  cat("\n---", nm, "---\n")
  print(anovas[[nm]])
  print(round(summary(models[[nm]])$coefficients, 4))
}

# ---- Estimated marginal means ----
# Preregistered: report EMMs (or predicted probabilities) for all four cells.
get_emm <- function(model, label) {
  emmeans::emmeans(model, ~ E_num * R_num,
                   at = list(E_num = c(-0.5, 0.5), R_num = c(-0.5, 0.5)),
                   type = "response") %>%
    as.data.frame() %>%
    mutate(Outcome = label,
           E_label = if_else(E_num == -0.5, "lowE", "highE"),
           R_label = if_else(R_num == -0.5, "lowR", "highR"))
}

emm_table <- dplyr::bind_rows(
  lapply(names(models), function(nm) get_emm(models[[nm]], nm))
)

cat("--- Estimated marginal means / predicted probabilities ---\n")
print(emm_table, digits = 4)

# ---- Simple effects ----
# Preregistered: reported only if the E x R interaction is significant.
simple_effects <- function(model, label) {
  p_int <- car::Anova(model, type = 3) %>%
    as.data.frame() %>%
    { .[grepl("E_num:R_num", rownames(.)), grep("^Pr", names(.))[1]] }
  
  if (length(p_int) == 0 || is.na(p_int) || p_int >= .05) return(NULL)
  
  emmeans::emmeans(model, ~ E_num | R_num,
                   at = list(E_num = c(-0.5, 0.5), R_num = c(-0.5, 0.5)),
                   type = "response") %>%
    emmeans::contrast(method = "pairwise") %>%
    as.data.frame() %>%
    mutate(Outcome = label)
}

simple_table <- dplyr::bind_rows(
  lapply(names(models), function(nm) simple_effects(models[[nm]], nm))
)

cat("--- Simple effects (only where the interaction is significant) ---\n")
print(simple_table, digits = 4)

# ---- Effect sizes ----
# Preregistered: partial eta-squared for the linear models.
eta_table <- dplyr::bind_rows(lapply(c("H1", "H2a", "H2b"), function(nm) {
  effectsize::eta_squared(anovas[[nm]], partial = TRUE) %>%
    as.data.frame() %>%
    mutate(Outcome = nm)
}))

cat("--- Partial eta squared ---\n")
print(eta_table, digits = 4)

# ---- Hypothesis support summary ----
# Mapping follows the preregistration exactly:
#   H1a/b/c: E, R, E x R on perceived paternalism
#   H2a:     E on affective trust
#   H2b:     R on cognitive trust
#   H2c:     E x R in the affective trust model
#   H3a/b/c: E, R, E x R on sensitive disclosure readiness
#   H4a/b/c: E, R, E x R on willingness to engage
#
# Decision rule (Section 3.6): supported if p < .05 and the estimate has the
# preregistered sign. 
hypotheses <- tibble::tribble(
  ~Hyp,  ~Model, ~Effect,       ~Expected,
  "H1a", "H1",   "E_num",       "positive",
  "H1b", "H1",   "R_num",       "negative",
  "H1c", "H1",   "E_num:R_num", "negative",
  "H2a", "H2a",  "E_num",       "positive",
  "H2b", "H2b",  "R_num",       "positive",
  "H2c", "H2a",  "E_num:R_num", "negative",
  "H3a", "H3",   "E_num",       "positive",
  "H3b", "H3",   "R_num",       "negative",
  "H3c", "H3",   "E_num:R_num", "negative",
  "H4a", "H4",   "E_num",       "negative",
  "H4b", "H4",   "R_num",       "positive",
  "H4c", "H4",   "E_num:R_num", "positive"
)

get_p <- function(nm, effect) {
  d <- as.data.frame(anovas[[nm]])
  d[effect, grep("^Pr", names(d))[1]]
}

hyp_table <- hypotheses %>%
  rowwise() %>%
  mutate(
    estimate  = coef(models[[Model]])[Effect],
    p         = get_p(Model, Effect),
    supported = !is.na(p) & p < .05 &
      if_else(Expected == "positive", estimate > 0, estimate < 0)
  ) %>%
  ungroup()

cat("--- Hypothesis support ---\n")
print(as.data.frame(hyp_table), digits = 4)

# ---- Robustness and secondary models ----
# H2 robustness: difference score (AT - CT). Preregistered as a supplementary
# analysis, not used for confirmatory inference.
h2_diff_model <- lm(Trust_Diff ~ E_num * R_num, data = data_clean)

# H3 robustness: ordered logistic regression on disclosure intensity.
# Preregistered as a descriptive robustness analysis.
h3_ordinal_model <- MASS::polr(SDR_ordinal ~ E_num * R_num,
                               data = data_clean, Hess = TRUE)

# Manipulation checks. Not used for exclusion, not entered as mediators.
mc_e_model <- lm(EMP_mean ~ E_num * R_num, data = data_clean)
mc_r_model <- lm(REC_mean ~ E_num * R_num, data = data_clean)

# Secondary descriptive outcome: perceived user agency.
agency_model <- lm(AGE_mean ~ E_num * R_num, data = data_clean)

secondary <- list(
  Trust_Diff = h2_diff_model,
  MC_Empathy = mc_e_model,
  MC_Recognition = mc_r_model,
  Agency = agency_model
)

for (nm in names(secondary)) {
  cat("\n---", nm, "---\n")
  print(car::Anova(secondary[[nm]], type = 3))
  print(round(summary(secondary[[nm]])$coefficients, 4))
}

# Ordinal model: coefficients plus p-values (polr reports t-values only).
cat("\n--- H3 ordinal (polr) ---\n")
ord_coef <- as.data.frame(summary(h3_ordinal_model)$coefficients)
ord_coef$p <- 2 * pnorm(abs(ord_coef[, "t value"]), lower.tail = FALSE)
print(round(ord_coef, 4))

# Predicted probability of each disclosure level in each cell.
# This is what the binary coding discards.
cat("\n--- H3 ordinal: predicted probabilities per cell ---\n")
ord_probs <- cbind(
  expand.grid(E_num = c(-0.5, 0.5), R_num = c(-0.5, 0.5)),
  predict(h3_ordinal_model,
          newdata = expand.grid(E_num = c(-0.5, 0.5), R_num = c(-0.5, 0.5)),
          type = "probs")
)
print(round(ord_probs, 4))

# ---- Sensitivity: country adjustment ----
# The realised sample spans several LATAM countries. Country adjustment is a
# post hoc sensitivity check, NOT preregistered. Primary models remain unadjusted.
fit_country <- function(dv, family = "gaussian") {
  form <- as.formula(paste(dv, "~ E_num * R_num +", COUNTRY_VAR))
  if (family == "gaussian") lm(form, data = data_clean)
  else glm(form, family = binomial, data = data_clean)
}

country_models <- list(
  H1  = fit_country("PAT_mean"),
  H2a = fit_country("ATRU_mean"),
  H2b = fit_country("CTRU_mean"),
  H3  = fit_country("SDR_binary", family = "binomial"),
  H4  = fit_country("uptake_num", family = "binomial")
)

cat("=== Country-adjusted sensitivity models ===\n")
for (nm in names(country_models)) {
  cat("\n---", nm, "---\n")
  print(car::Anova(country_models[[nm]], type = 3))
  
  key <- c("E_num", "R_num", "E_num:R_num")
  cmp <- data.frame(
    term        = key,
    primary_est = round(unname(coef(models[[nm]])[key]), 4),
    country_est = round(unname(coef(country_models[[nm]])[key]), 4)
  )
  print(cmp)
}

# ---- Sensitivity: recruitment wave ----
# The student prescreener was relaxed mid-fieldwork. Two checks:
#   (a) adjustment  - do the primary effects survive controlling for wave?
#   (b) moderation  - do treatment effects differ between waves?
# Neither is preregistered.
wave_adjusted <- lapply(
  c(H1 = "PAT_mean", H2a = "ATRU_mean", H2b = "CTRU_mean",
    H3 = "SDR_binary", H4 = "uptake_num"),
  function(dv) {
    form <- as.formula(paste(dv, "~ E_num * R_num + recruitment_wave"))
    if (dv %in% c("SDR_binary", "uptake_num")) {
      glm(form, family = binomial, data = data_clean)
    } else {
      lm(form, data = data_clean)
    }
  }
)

wave_moderation <- lapply(
  c(H1 = "PAT_mean", H2a = "ATRU_mean", H2b = "CTRU_mean",
    H3 = "SDR_binary", H4 = "uptake_num"),
  function(dv) {
    form <- as.formula(paste(dv, "~ E_num * R_num * recruitment_wave"))
    if (dv %in% c("SDR_binary", "uptake_num")) {
      glm(form, family = binomial, data = data_clean)
    } else {
      lm(form, data = data_clean)
    }
  }
)

cat("\n=== Recruitment wave: adjusted models ===\n")
for (nm in names(wave_adjusted)) {
  cat("\n---", nm, "---\n")
  print(car::Anova(wave_adjusted[[nm]], type = 3))
}

cat("\n=== Recruitment wave: moderation ===\n")
for (nm in names(wave_moderation)) {
  cat("\n---", nm, "---\n")
  print(car::Anova(wave_moderation[[nm]], type = 3))
}

# ---- Model diagnostics ----
# Residual and variance checks for the linear models (not preregistered).

cat("=== Residual diagnostics, linear models ===\n")
diag_table <- dplyr::bind_rows(lapply(c("H1", "H2a", "H2b"), function(nm) {
  r <- residuals(models[[nm]])
  sw <- shapiro.test(r)
  dv <- all.vars(formula(models[[nm]]))[1]
  lv <- car::leveneTest(as.formula(paste(dv, "~ E_f * R_f")), data = data_clean)
  tibble::tibble(
    Model = nm,
    shapiro_W = sw$statistic,
    shapiro_p = sw$p.value,
    levene_F  = lv$`F value`[1],
    levene_p  = lv$`Pr(>F)`[1],
    floor_prop = mean(data_clean[[dv]] == min(data_clean[[dv]], na.rm = TRUE),
                      na.rm = TRUE)
  )
}))
print(as.data.frame(diag_table), digits = 4)

# ---- H1: robustness of the interaction ----
# (a) Heteroskedasticity-consistent standard errors (HC3).
cat("\n=== H1: HC3 robust standard errors ===\n")
print(lmtest::coeftest(models$H1,
                       vcov = sandwich::vcovHC(models$H1, type = "HC3")))

# (b) Permutation test. The outcome is shuffled 10,000 times to build the
# null distribution of the interaction coefficient under random assignment.
# This makes no distributional assumptions.
cat("\n=== H1: permutation test for E x R ===\n")
obs_int <- coef(models$H1)["E_num:R_num"]

perm_int <- replicate(10000, {
  d <- data_clean
  d$PAT_mean <- sample(d$PAT_mean)
  coef(lm(PAT_mean ~ E_num * R_num, data = d))["E_num:R_num"]
})

p_perm <- mean(abs(perm_int) >= abs(obs_int))

cat("Observed interaction coefficient:", round(obs_int, 4), "\n")
cat("Permutation p-value (two-sided, 10,000 draws):", round(p_perm, 4), "\n")
cat("Parametric p-value for comparison: 0.0137\n")

# ---- H2a: variance homogeneity ----
# Levene's test indicates unequal variances across cells, so the effect is
# re-estimated with HC3 standard errors and a Welch test.
cat("\n=== H2a: variance homogeneity check ===\n")
print(data_clean %>%
        group_by(E_f, R_f) %>%
        summarise(n = n(), mean = mean(ATRU_mean), sd = sd(ATRU_mean), .groups = "drop"))

cat("\n--- HC3 robust standard errors ---\n")
print(lmtest::coeftest(models$H2a,
                       vcov = sandwich::vcovHC(models$H2a, type = "HC3")))

cat("\n--- Welch test on the E main effect ---\n")
print(t.test(ATRU_mean ~ E_f, data = data_clean))

# ---- Firth check ----
# Preregistered: Firth logistic regression if separation occurs in a binary
# outcome. Checked explicitly here; no separation was present.
for (nm in c("H3", "H4")) {
  probs <- fitted(models[[nm]])
  sep <- any(probs < 1e-06 | probs > 1 - 1e-06)
  cat(nm, "- fitted probability range:", round(range(probs), 4),
      "| separation:", sep, "\n")
}

# ---- Descriptive cell summaries ----
desc_continuous <- data_clean %>%
  group_by(E_f, R_f) %>%
  summarise(n = n(),
            across(c(PAT_mean, ATRU_mean, CTRU_mean, Trust_Diff,
                     AGE_mean, EMP_mean, REC_mean),
                   list(M = ~mean(.x), SD = ~sd(.x))),
            .groups = "drop")

desc_binary <- data_clean %>%
  group_by(E_f, R_f) %>%
  summarise(n = n(),
            SDR_prop = mean(SDR_binary, na.rm = TRUE),
            WTE_prop = mean(uptake_num, na.rm = TRUE),
            .groups = "drop")

cat("\n--- Continuous outcomes by cell ---\n")
print(as.data.frame(desc_continuous), digits = 3)
cat("\n--- Binary outcomes by cell ---\n")
print(as.data.frame(desc_binary), digits = 3)

# ---- Plots ----
plot_cells <- function(dv, xvar, colvar, title, ylab) {
  ggplot(data_clean, aes(x = .data[[xvar]], y = .data[[dv]],
                         colour = .data[[colvar]], group = .data[[colvar]])) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .1) +
    labs(title = title, x = xvar, y = ylab, colour = colvar) +
    theme_minimal()
}

plots <- list(
  h1_paternalism    = plot_cells("PAT_mean",  "E_f", "R_f",
                                 "H1: Perceived paternalism", "Paternalism"),
  h2a_affective     = plot_cells("ATRU_mean", "E_f", "R_f",
                                 "H2a: Affective trust", "Affective trust"),
  h2b_cognitive     = plot_cells("CTRU_mean", "R_f", "E_f",
                                 "H2b: Cognitive trust", "Cognitive trust"),
  mc_empathy        = plot_cells("EMP_mean",  "E_f", "R_f",
                                 "Manipulation check: empathy", "Perceived empathy"),
  mc_recognition    = plot_cells("REC_mean",  "R_f", "E_f",
                                 "Manipulation check: recognition", "Perceived recognition")
)

for (nm in names(plots)) {
  ggsave(file.path(path_out, paste0("plot_", nm, ".png")),
         plots[[nm]], width = 7, height = 5, dpi = 320)
}

# ---- Exports ----
readr::write_csv(hyp_table,        file.path(path_out, "hypothesis_support.csv"))
readr::write_csv(emm_table,        file.path(path_out, "marginal_means.csv"))
readr::write_csv(simple_table,     file.path(path_out, "simple_effects.csv"))
readr::write_csv(eta_table,        file.path(path_out, "effect_sizes.csv"))
readr::write_csv(reliability,      file.path(path_out, "reliability.csv"))
readr::write_csv(diag_table,       file.path(path_out, "model_diagnostics.csv"))
readr::write_csv(condition_counts, file.path(path_out, "condition_balance.csv"))

writexl::write_xlsx(
  list(continuous = desc_continuous, binary = desc_binary),
  file.path(path_out, "descriptive_cells.xlsx")
)

readr::write_csv(data_clean, file.path(path_out, "analytic_sample_scored.csv"))

# ---- Anonymised dataset for public sharing ----
# Removes direct identifiers, free-text responses and Qualtrics metadata.
# Timestamps are retained in full because recruitment_wave derives from them
# and the field period is documented in the manuscript.
# All variables required to reproduce the reported models
# are kept. Raw data available from the author on reasonable request.
drop_direct_id <- c("ResponseId", "ResponseId.pre", "ResponseId.post",
                    "pid", "Prolific_1", "Prolific_2", "MAIL",
                    "IPAddress.pre", "IPAddress.post",
                    "LocationLatitude.pre", "LocationLatitude.post",
                    "LocationLongitude.pre", "LocationLongitude.post",
                    "RecipientEmail", "RecipientFirstName", "RecipientLastName",
                    "ExternalReference",
                    "PROLIFIC_PID", "STUDY_ID", "SESSION_ID") 
# Also drop the .pre/.post versions of Qualtrics contact fields
drop_direct_id <- c(drop_direct_id,
                    grep("^(Recipient|ExternalReference|IPAddress|Location)",
                         names(data_clean), value = TRUE))

drop_freetext  <- c("P1", "P2", "A7.2_EXP_ngo_type")   # open-ended responses

# any "other, please specify" text fields exported by Qualtrics
drop_text_cols <- grep("_TEXT$", names(data_clean), value = TRUE)

drop_redundant <- c("cond.pre", "cond.post", "cond_pre_num", "cond_post_num",
                    "prolific_match", "condition_match",
                    "excl_invalid", "excl_age", "excl_att",
                    "Finished_post_num", "Progress_post_num")

drop_metadata  <- c("Status.pre", "Status.post", "Progress.pre", "Progress.post",
                    "Finished.pre", "Finished.post", "Q_RecaptchaScore",
                    "DistributionChannel.pre", "DistributionChannel.post",
                    "UserLanguage.pre", "UserLanguage.post", "Consent")

data_public <- data_clean %>%
  select(-any_of(c(drop_direct_id, drop_freetext, drop_text_cols,       
                   drop_redundant, drop_metadata))) %>%
  mutate(participant_id = seq_len(n())) %>%
  relocate(participant_id)

# Safety check: no variable may have a near-unique value per row.
n_unique <- sapply(data_public, function(x) length(unique(x)))
risky <- names(n_unique)[n_unique > nrow(data_public) * 0.5 &
                           sapply(data_public, is.character)]
if (length(risky) > 0) {
  stop("Potential identifiers remain: ", paste(risky, collapse = ", "))
}

# List every remaining character column for a manual check
cat("Character columns in public data (check manually):\n")
print(names(data_public)[sapply(data_public, is.character)])

cat("Public dataset:", nrow(data_public), "rows,", ncol(data_public), "vars\n")
print(names(data_public))

readr::write_csv(data_public,
                 file.path(path_out, "analytic_sample_public.csv"))

# ---- Attrition flow ----
attrition <- tibble::tibble(
  stage = c("Pre raw (completed, field period)",
            "Post raw (completed, field period)",
            "Pre-post dropout (no matching post record)",
            "Matched sessions",
            "Excluded: invalid records (test runs)",
            "Excluded: condition mismatch",
            "Excluded: age < 18",
            "Excluded: failed attention check",
            "Final analytic sample"),
  n = c(n_pre_raw, n_post_raw,
        n_pre_raw - n_matched,
        n_matched,
        if (EXCLUDE_TEST_RUNS) n_invalid else 0,
        n_condition_mismatch, n_age_excl, n_att_excl,
        nrow(data_clean))
)
print(attrition)
readr::write_csv(attrition, file.path(path_out, "attrition_flow.csv"))

# ---- Sensitivity model exports ----
tidy_all <- function(model_list, label) {
  dplyr::bind_rows(lapply(names(model_list), function(nm) {
    as.data.frame(car::Anova(model_list[[nm]], type = 3)) %>%
      tibble::rownames_to_column("effect") %>%
      mutate(Model = nm, Analysis = label)
  }))
}

sensitivity <- dplyr::bind_rows(
  tidy_all(country_models,   "country_adjusted"),
  tidy_all(wave_adjusted,    "wave_adjusted"),
  tidy_all(wave_moderation,  "wave_moderation")
)
readr::write_csv(sensitivity, file.path(path_out, "sensitivity_models.csv"))

# Primary model coefficients, with odds ratios for the logistic models.
primary_coefs <- dplyr::bind_rows(lapply(names(models), function(nm) {
  broom::tidy(models[[nm]], conf.int = TRUE) %>%
    mutate(Model = nm,
           OR = if (inherits(models[[nm]], "glm")) exp(estimate) else NA_real_)
}))
readr::write_csv(primary_coefs, file.path(path_out, "primary_coefficients.csv"))

# ---- Session info ----
# Package versions for the manuscript appendix and OSF documentation.
writeLines(capture.output(sessionInfo()),
           file.path(path_out, "sessionInfo.txt"))

cat("\n=== Pipeline complete ===\n")
cat("Analytic N:", nrow(data_clean), "\n")
cat("Test runs excluded:", EXCLUDE_TEST_RUNS, "\n")
cat("Output written to:", path_out, "\n")
print(list.files(path_out))
