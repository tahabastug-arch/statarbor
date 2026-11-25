# statarbor 0.2.0

## New Experimental Design Modules
* Added `compare_paired()` for assumption-guided 2-condition paired comparisons (paired Student's t-test with Cohen's dz vs. Wilcoxon signed-rank test with rank-biserial r).
* Added `auto_repeated()` for repeated-measures designs (k >= 3) with Greenhouse-Geisser sphericity-corrected RM-ANOVA, Friedman fallback, and special-character-safe CLD.
* Added `analyze_count()` for count data regression with Pearson overdispersion screening, automatic Poisson-to-Negative Binomial (`MASS::glm.nb`) / Quasi-Poisson routing, incidence rate ratios (IRR), and Hauck-Donner likelihood-ratio safeguards for zero-count groups.
* Added `analyze_ancova()` for single-covariate ANCOVA with regression slope homogeneity screening, Type II sums of squares, adjusted marginal means, and CLD.
* Added `analyze_rcbd()` for Randomized Complete Block Designs supporting fixed blocks (Type II SS, Cochran-Cox relative efficiency, Tukey-Kramer HSD, Friedman fallback) and optional random blocks (`lme4` mixed model with ICC).

## Reporting, Visualization & Shiny Integration
* Added `plot()` S3 methods for all five new analysis classes.
* Extended `generate_methods_text()` (EN/TR) and `export_r_script()` to support all v0.2.0 design classes.
* Added the bilingual **6. Experimental Designs** module (`inst/shiny/mod_designs_v02.R`) to `run_app()`.

# statarbor 0.1.0

* Initial archived release (`10.5281/zenodo.24534800`) featuring `auto_compare()`, special-character-safe `generate_cld()`, native Welch/Games-Howell routines, `compare_two_groups()`, `analyze_binary()`, `analyze_ordinal()`, `analyze_interaction()`, bilingual reporting, and the interactive Shiny application.
