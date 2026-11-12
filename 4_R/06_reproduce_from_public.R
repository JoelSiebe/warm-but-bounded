# 06_reproduce_from_public.R

# Master's thesis analysis: Warm but Bounded? Empathic and Recognition-Oriented Communication in NGO Chatbots

# Author: Joël Sieber
# Code written with AI assistance (Claude, Anthropic); see README.

# Re-estimates the confirmatory models from the public dataset (S11),
# without access to the raw data.

source(here::here("R", "00_setup.R"))

d <- readr::read_csv(here::here("data", "analytic_sample_public.csv"))

models_public <- list(
  H1  = lm(PAT_mean  ~ E_num * R_num, data = d),
  H2a = lm(ATRU_mean ~ E_num * R_num, data = d),
  H2b = lm(CTRU_mean ~ E_num * R_num, data = d),
  H3  = glm(SDR_binary ~ E_num * R_num, family = binomial, data = d),
  H4  = glm(uptake_num ~ E_num * R_num, family = binomial, data = d)
)

for (nm in names(models_public)) {
  cat("\n---", nm, "---\n")
  print(car::Anova(models_public[[nm]], type = 3))
  print(round(summary(models_public[[nm]])$coefficients, 3))
}