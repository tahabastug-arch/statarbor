# statarbor

**Assumption-Guided Statistical Decision Engine, Special-Character-Safe CLD, and Bilingual (EN/TR) Reporting for R**

`statarbor` is an open-source R package (GNU GPL >= 3) and bundled interactive Shiny web application designed for experimental researchers in agriculture, plant biotechnology, and life sciences.

## Key Capabilities

- **Automated Decision Tree (`auto_compare()`):** Evaluates zero-variance groups, group-level Shapiro-Wilk normality, and Levene's variance homogeneity, routing automatically to One-Way ANOVA + Tukey HSD, native Welch ANOVA + Games-Howell, or Kruskal-Wallis + Dunn (Bonferroni / Holm / BH).
- **Special-Character-Safe CLD (`generate_cld()`):** Constructs a symmetric significance matrix directly, preventing label corruption on treatment names containing hyphens, degree signs, or spaces (e.g., `"-20°C - 1. Week"`).
- **Spreadsheet Cleaning (`merge_group_prefix()`, `combine_factors()`):** Collapses numbered replicate labels (e.g., `"Control 1..63"` -> `"Control"`) and builds composite factorial columns.
- **Advanced Modeling:** Two-group auto-routing (`compare_two_groups()`), separation-aware Firth logistic regression (`analyze_binary()`), proportional-odds cumulative link models (`analyze_ordinal()`), and Gaussian/Gamma-log factorial GLMs (`analyze_interaction()`).
- **Visualization & Reproducible Reporting:** Publication-ready CLD plots (`plot()`), correlation heatmaps (`plot_correlation()`), PCA biplots (`plot_pca()`), bilingual EN/TR methods text (`generate_methods_text()`), executable verification scripts (`export_r_script()`), and page-fitted Word reports (`export_word_report()`).
- **No-Code Bilingual Interface (`run_app()`):** Real-time English/Turkish Shiny GUI with automatic CSV/TXT delimiter detection.

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

# Automated assumption-guided comparisons
res_fw <- auto_compare(thermal_stress, response = "fresh_weight_g", group = "treatment")
print(res_fw)
plot(res_fw, type = "boxplot")

# Publication-ready methods paragraph (English or Turkish)
cat(generate_methods_text(res_fw, lang = "en"), "\n")

# Launch the interactive bilingual Shiny app
run_app()
```

## Validation & License

`statarbor` v0.1.0 passes `R CMD check --as-cran` with 0 errors, 0 warnings, and 0 notes across 60 unit tests verifying numerical agreement (`< 1e-6`) against base R reference implementations. Released under the **GNU General Public License v3.0 (GPL >= 3)**.
