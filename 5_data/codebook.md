# Codebook: analytic_sample_public.csv

369 participants (one row each), 92 variables. Item wording for all scales is
documented in S8 (English) and S9 (Spanish). Unless stated otherwise, items use
a fully labelled 7-point scale (1 = strongly disagree, 7 = strongly agree).

## Identifiers and timing

| Variable | Description |
|---|---|
| `participant_id` | Sequential ID (1–369); not linked to any external identifier |
| `StartDate.pre`, `EndDate.pre`, `RecordedDate.pre` | Timestamps of the pre-interaction survey, as exported by Qualtrics |
| `Duration__in_seconds_.pre` | Duration of the pre-interaction survey (seconds) |
| `StartDate.post`, `EndDate.post`, `RecordedDate.post` | Timestamps of the post-interaction survey |
| `Duration__in_seconds_.post` | Duration of the post-interaction survey (seconds) |
| `StartDate_dt.pre`, `StartDate_dt.post`, `StartDate_post_dt`, `EndDate_post_dt` | Parsed timestamps (Europe/Zurich) used for the field period, recruitment wave, and data processing |
| `recruitment_wave` | `student_filter` = Prolific student prescreen active; `filter_relaxed` = prescreen removed |

## Experimental conditions

| Variable | Description |
|---|---|
| `cond_final`, `cond_val` | Condition: 1 = low EC / low ROC (neutral), 2 = high EC / low ROC (empathic-only), 3 = low EC / high ROC (recognition-only), 4 = high EC / high ROC (combined) |
| `E_num` | Empathic communication, effect-coded (−0.5 = low, +0.5 = high) |
| `R_num` | Recognition-oriented communication, effect-coded (−0.5 = low, +0.5 = high) |
| `E_f`, `R_f` | Same as factors (`lowE`/`highE`, `lowR`/`highR`) |

EC = empathic communication; ROC = recognition-oriented communication.

## Confirmatory outcomes

| Variable | Description |
|---|---|
| `PAT_01`–`PAT_05` | Perceived paternalism (H1). PAT_01–03 adapted from the Emotional Tone Rating Scale; PAT_04–05 on role appropriateness |
| `PAT_mean` | Mean of PAT_01–05 |
| `ATRU_01_1`–`ATRU_04_1` | Affective trust (H2), semantic differential, −3 to +3 |
| `ATRU_mean` | Mean of the four affective-trust items |
| `CTRU_01_1`–`CTRU_04_1` | Cognitive trust (H2), semantic differential, −3 to +3 |
| `CTRU_mean` | Mean of the four cognitive-trust items |
| `Trust_Diff` | `ATRU_mean` − `CTRU_mean` (preregistered robustness analysis) |
| `choice_8` | Disclosure choice at Turn 8: `data_name`, `data_phone`, `data_id` (national identification number), `data_refused` |
| `SDR_binary` | Sensitive disclosure readiness (H3): 0 = no disclosure, 1 = any disclosure |
| `SDR_ordinal` | Disclosure intensity: 0 = none, 1 = name, 2 = phone, 3 = ID number |
| `uptake` | Contact decision at Turn 10, as exported (`"1"` = yes, `"0"` = no) |
| `uptake_num` | Willingness to engage (H4): 0 = no, 1 = yes |

## Manipulation checks

| Variable | Description |
|---|---|
| `EMP_01`–`EMP_03`, `EMP_mean` | Perceived empathy |
| `REC_01`–`REC_03`, `REC_mean` | Perceived recognition |

## Exploratory measures

| Variable | Description |
|---|---|
| `AGE_1`–`AGE_4`, `AGE_mean` | Perceived user agency |
| `SOC_1`, `SOC_2`, `SOC_4`, `SOC_mean` | Social presence |
| `SOC_3` | Attention check (instructed response: 5 = somewhat agree); not part of the social-presence scale |
| `IMM_1`–`IMM_4`, `IMM_mean` | Scenario immersion |
| `DISC_1`–`DISC_3`, `DISC_mean` | Self-reported disclosure willingness |
| `RISK_1`–`RISK_3` | Perceived privacy risk; `RISK_2` is reverse-keyed |
| `RISK_2_rev` | `8 − RISK_2` |
| `RISK_mean` | Mean of RISK_1, RISK_2_rev, RISK_3 |

## Interaction data

| Variable | Description |
|---|---|
| `choice_1` | Turn 1: `info_general` or `info_eligibility` |
| `choice_3` | Turn 3: `expression_uncertainty` (single scripted option) |
| `choice_5` | Turn 5: `detail_process` or `detail_support` |
| `choice_10` | Turn 10: `"1"` or `"0"` (same as `uptake`) |
| `rt_1`, `rt_3`, `rt_5`, `rt_8`, `rt_10` | Response time at each decision node (milliseconds) |
| `chat_duration` | Duration of the chatbot interaction (seconds) |

## Background variables

| Variable | Description |
|---|---|
| `DEM_age` | Age in years |
| `DEM_gender` | Gender: 1 = male, 2 = female, 3 = non-binary, 4 = prefer not to say |
| `DEM_country` | Country of residence, cleaned from open text: `mexico`, `chile`, `argentina`, `colombia`, `other` |
| `A7.1_EXP_chatbot` | Prior chatbot use, 7-point frequency scale: 1 = never … 7 = daily or almost daily |
| `A7.2_EXP_ngo_binary` | Prior NGO contact: 1 = yes, 2 = no |
| `A7.3_TRU_ngo_1`–`A7.3_TRU_ngo_3` | General trust in NGOs (mean computed in `02_exploratory.R`) |