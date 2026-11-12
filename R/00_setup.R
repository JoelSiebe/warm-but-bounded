# 00_setup.R

# Master's thesis analysis: Warm but Bounded? 
# Empathic and Recognition-Oriented Communication in NGO Chatbots

# Author: Joël Sieber
# Code written with AI assistance (Claude, Anthropic); see README.

#Purpose: load packages, define paths, ensure reproducibility.
# Sourced by all subsequent scripts.


# ---- Packages -----
library(here)
library(haven)
library(dplyr)
library(stringr)
library(ggplot2)

# The following are called via :: only, to avoid masking conflicts
# (notably MASS::select vs. dplyr::select):

# ---- Reproducibility -----
# Fixes the random number generator, which the permutation test and the
# bootstrapped mediation require.
set.seed(20260428)

# ---- Paths -----
# here() resolves paths from the project root, so the code runs
# unchanged on any machine.
path_raw <- here("data", "raw")
path_out <- here("output")

if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)

# ---- Contrasts ----
# Sum-to-zero contrasts are required for meaningful Type III tests.
options(contrasts = c("contr.sum", "contr.poly"))

# ---- Check ----
cat("Project root:", here(), "\n")
cat("Raw data:    ", path_raw, "\n")
cat("Output:      ", path_out, "\n\n")
print(list.files(path_raw))