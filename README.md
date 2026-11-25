# statarbor

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24534800.svg)](https://doi.org/10.5281/zenodo.24534800)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

**Assumption-Guided Statistical Engine, Special-Character-Safe CLD, and Bilingual (EN/TR) Reporting for R**

`statarbor` is an open-source R package (GNU GPL >= 3) and bundled interactive Shiny web application designed for experimental researchers in agriculture, plant biotechnology, and life sciences.

## Key Capabilities

- **Independent Multi-Group Engine (`auto_compare()`):** Evaluates zero-variance groups, group-level Shapiro-Wilk normality, and Levene's variance homogeneity, routing automatically to One-Way ANOVA + Tukey HSD, native Welch ANOVA + Games-Howell, or Kruskal-Wallis + Dunn (Bonferroni / Holm / BH).
- **Special-Character-Safe CLD (`generate_cld()`):** Constructs a symmetric significance matrix directly, preventing label corruption on treatment names containing hyphens, degree signs, or spaces (e.g., `"-20°C - 1. Week"`).
- **Experimental Designs (`v0.2.0`):**
  - **Paired & Repeated Measures (`compare_paired()`, `auto_repeated()`):** Routes between paired *t*-test / Wilcoxon signed-rank ($k = 2$) and Greenhouse-Geisser corrected RM-ANOVA / Friedman ($k \ge 3$) with CLD.
  - **Count Data Regression (`analyze_count()`):** Evaluates Pearson overdispersion and routes automatically from Poisson to Negative Binomial (`MASS::glm.nb`) or Quasi-Poisson GLM, reporting Incidence Rate Ratios (IRR) with Hauck-Donner likelihood-ratio safeguards for zero-count groups.
  - **Analysis of Covariance (`analyze_ancova()`):** Evaluates regression slope homogeneity, computes Type II sums of squares, partial $\eta_p^2$, covariate-adjusted marginal means, and CLD.
  - **Randomized Complete Block Designs (`analyze_rcbd()`):** Supports fixed blocks (Type II SS, Cochran-Cox relative efficiency of blocking, Tukey-Kramer HSD, Friedman fallback) and optional random blocks (`lme4` REML/LRT mixed models with block ICC).
- **Spreadsheet Cleaning & Advanced Models:** Replicate prefix merging (`merge_group_prefix()`), composite factor builders (`combine_factors()`), two-group routing (`compare_two_groups()`), separation-aware Firth logistic regression (`analyze_binary()`), ordinal cumulative link models (`analyze_ordinal()`), and Gaussian/Gamma-log factorial GLMs (`analyze_interaction()`).
- **Visualization & Bilingual Reporting:** Publication-ready CLD plots (`plot()` S3 methods across all core and v0.2.0 classes), correlation heatmaps (`plot_correlation()`), PCA biplots (`plot_pca()`), bilingual EN/TR methods paragraphs (`generate_methods_text()`), executable verification scripts (`export_r_script()`), and page-fitted Word reports (`export_word_report()`).
- **No-Code Bilingual Interface (`run_app()`):** Real-time English/Turkish Shiny GUI featuring automatic CSV/TXT delimiter detection and the dedicated **6. Experimental Designs** module.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("tahabastug-arch/statarbor", build_vignettes = TRUE)
```

## Quick Start

```r
library(statarbor)
data(thermal_stress)

# 1. Automated assumption-guided multi-group comparison
res_fw <- auto_compare(thermal_stress, response = "fresh_weight_g", group = "treatment")
print(res_fw)
plot(res_fw, type = "boxplot")

# 2. Experimental design example (RCBD with relative efficiency & CLD)
set.seed(42)
rcbd_df <- expand.grid(treatment = c("Control", "Bio-A", "Bio-B"), block = paste0("B", 1:5))
rcbd_df$yield <- rep(c(18, 24, 30), 5) + rep(c(-2, -1, 0, 1, 2), each = 3) + rnorm(15, 0, 0.8)
res_rcbd <- analyze_rcbd(rcbd_df, response = "yield", treatment = "treatment", block = "block")
print(res_rcbd)
plot(res_rcbd)

# 3. Publication-ready methods paragraph (English or Turkish) & reproducible R script
cat(generate_methods_text(res_rcbd, lang = "en"), "\n")
cat(generate_methods_text(res_rcbd, lang = "tr"), "\n")

# 4. Launch the interactive bilingual Shiny app
run_app()
```

## Citation

If you use `statarbor` in published research, please cite:

> Bastug, T. (2026). *statarbor: Assumption-Guided Statistical Engine and Bilingual Reporting* (Version 0.2.0). Zenodo. [https://doi.org/10.5281/zenodo.24534800](https://doi.org/10.5281/zenodo.24534800)

You can also retrieve the BibTeX entry directly in R via `citation("statarbor")`.

## Validation & License

`statarbor` v0.2.0 passes `R CMD check --as-cran` with 0 errors and 0 warnings across **136 automated unit tests** verifying numerical agreement (`< 1e-6`) against base R (`stats`), `car`, and `MASS` reference implementations. Released under the **GNU General Public License v3.0 (GPL >= 3)**.
