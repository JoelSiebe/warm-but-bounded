\# Analysis code: Warm but Bounded? Empathic and Recognition-Oriented Communication in NGO Chatbots



Master's thesis, University of St.Gallen, 2026. Author: Joël Sieber.



Code development: The analysis scripts were written with the assistance of an

AI assistant (Claude, Anthropic). The analyses were specified by the author

following the preregistration (S1); all outputs were checked against the

results reported in the thesis.



\## Contents



| Script | Purpose |

|---|---|

| `00\_setup.R` | Packages, paths, seed, contrasts. Sourced by all other scripts. |

| `01\_confirmatory.R` | Data import, merging, exclusions, scale scoring, confirmatory models, sensitivity analyses, anonymised dataset. |

| `02\_exploratory.R` | Balance checks, exploratory outcomes, moderators, mediation, response times, disclosure choices, item-level analysis. |

| `03\_manuscript\_tables.R` | Exports the values behind every table in the thesis. |

| `04\_additional\_analyses.R` | Equivalence tests, multiplicity adjustment, attrition, discriminant validity, mediation sensitivity. Not preregistered. |

| `05_pretest.R` | Manipulation checks and reliabilities in the pretest. ||

| `06\_reproduce\_from\_public.R` | Re-estimates the confirmatory models from the public dataset. |





\## How to run



1\. R \[4.5.0]; package versions are listed in `sessionInfo.txt`.

2\. Open the RStudio project (`.Rproj`). Paths are resolved with `here()`.

3\. Place the raw Qualtrics exports (`Pre\_Exposure.sav`, `Post\_Exposure.sav`) in `data/raw/`.

3.1 Without the raw data: copy `analytic\\\\\\\_sample\\\\\\\_public.csv` (S11) into `data/`

&#x20;  and run `06\\\\\\\_reproduce\\\\\\\_from\\\\\\\_public.R` to re-estimate the confirmatory models.

4\. Run `03\_manuscript\_tables.R`. It runs `00` to `02` automatically.

5\. In the same session, run `04\_additional\_analyses.R`.




The seed is fixed in `00\_setup.R` (`set.seed(20260428)`), so permutation and

bootstrap results are reproducible.



\## Data



## Data



The raw exports are not shared because they contain Prolific IDs. The

anonymised analytic dataset (`data/analytic_sample_public.csv`) contains all

variables needed to re-estimate the reported models, `06_reproduce_from_public.R`

does this without the raw data. Direct identifiers, open-ended responses, and

Qualtrics metadata were removed. Variables are described in `data/codebook.md`.



\## Output and thesis tables



Files in `output/manuscript\_tables/` are named after the table they belong to:



\- `table2\_…`, `table3\_…`: tables in the main text

\- `appB2\_…` to `appG6\_…`: appendix tables (e.g., `appC1` = Appendix Table C.1)

\- `sec4-1\_…`, `sec4-3\_…`: values reported in the text of Sections 4.1 and 4.3

\- `extra\_…`: additional output not reported in the thesis



Appendix Tables B.5 (attrition), D.5 (discriminant validity), E.1

(multiplicity and equivalence), and E.2 (binary equivalence) are written by

`04\_additional\_analyses.R` to `output/additional/`. Figure 3 of the thesis is

`output/figure1\_emm\_panels.pdf`.



## Materials

| No. | Material | Location |
|---|---|---|
| S1 | Preregistration | `materials/` and [OSF](https://osf.io/fjptc) |
| S2 | Stimulus-construction protocol | `materials/` |
| S3–S4 | Chatbot scripts and scenario (Spanish, English) | `materials/` |
| S5 | Chatbot implementation | `materials/` |
| S6 | Chatbot source code | [GitHub](https://github.com/JoelSiebe/ai-experiment) |
| S7 | Live demonstration of all four conditions | [1](https://ai-experiment-five.vercel.app/?pid=test&cond=1) · [2](https://ai-experiment-five.vercel.app/?pid=test&cond=2) · [3](https://ai-experiment-five.vercel.app/?pid=test&cond=3) · [4](https://ai-experiment-five.vercel.app/?pid=test&cond=4) |
| S8–S9 | Survey instrument (English) and interface (Spanish) | `materials/` |
| S10 | Analysis code | `R/` |
| S11 | Anonymised dataset and codebook | `data/` |


The preregistration and the materials registered with it remain unchanged on

OSF, where they use an earlier numbering (01 = S5, 02 = S2, 03 = S4, 04 = S3,

05 = S8, 06 = S9). The versions in this repository reflect the materials as

used in the main study. They differ from the registered files only by the

changes reported in the thesis (Section 3.8), including the removal of the

university logos and banners that had been added to increase trust during

campus recruitment.



\## License



CC-BY 4.0

