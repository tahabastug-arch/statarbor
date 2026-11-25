# Internal helper for p-value formatting in reporting
.fmt_p_rep <- function(p) {
  if (is.null(p) || is.na(p)) return("= NA")
  if (p < 0.001) "< 0.001" else sprintf("= %.4f", p)
}

# Internal helper to extract p_adjust from method label if present
.extract_padj <- function(method_str, default = "holm") {
  if (grepl("Tukey-Kramer", method_str, fixed = TRUE)) return("tukey")
  if (grepl("\\([^()]+\\)$", method_str)) {
    cand <- sub(".*\\(([^()]+)\\)$", "\\1", method_str)
    if (cand %in% c("holm", "bonferroni", "BH", "BY", "fdr", "hochberg", "hommel", "none", "tukey")) {
      return(cand)
    }
  }
  default
}

# Internal helper for appending citation sentence
.append_citation <- function(txt, lang, include_citation) {
  if (!isTRUE(include_citation)) return(txt)
  if (lang == "en") {
    paste0(txt, " All analyses were performed in R (R Core Team) using the statarbor package.")
  } else {
    paste0(
      txt,
      " T\u00fcm istatistiksel analizler R ortam\u0131nda statarbor paketi ",
      "kullan\u0131larak ger\u00e7ekle\u015ftirildi."
    )
  }
}

#' Generate Publication-Ready Statistical Methods Paragraph
#'
#' Produces a manuscript-ready statistical methods paragraph (in English or
#' Turkish) summarizing the assumption checks, routing decisions, post-hoc
#' procedures, compact letter display (CLD) logic, and effect size metrics
#' applied to `statarbor` analysis objects (`"statarbor_result"`,
#' `"statarbor_paired"`, `"statarbor_repeated"`, `"statarbor_count"`,
#' `"statarbor_ancova"`, or `"statarbor_rcbd"`).
#'
#' @param x A `statarbor` result object returned by [auto_compare()],
#'   [compare_paired()], [auto_repeated()], [analyze_count()],
#'   [analyze_ancova()], or [analyze_rcbd()].
#' @param lang Output language: `"en"` for English (default) or `"tr"` for
#'   Turkish.
#' @param include_citation Logical; if `TRUE` (default), appends a ready-to-use
#'   software citation sentence for `statarbor`.
#'
#' @return A character scalar containing the formatted methods paragraph.
#' @export
#'
#' @examples
#' res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
#' cat(generate_methods_text(res, lang = "en"))
generate_methods_text <- function(x,
                                  lang = c("en", "tr"),
                                  include_citation = TRUE) {
  lang <- match.arg(lang)
  alpha_str <- format(x$alpha, trim = TRUE)

  # --- 1. Independent multi-group comparison (v0.1.0 core) ---
  if (inherits(x, "statarbor_result")) {
    p_str <- .fmt_p_rep(x$p_value)
    eff_txt <- format_effect_compact(x$effect_size)

    if (lang == "en") {
      method_desc <- switch(
        x$method_code,
        anova = paste0(
          "Because normality and homogeneity of variances were satisfied, ",
          "group differences were evaluated using one-way ANOVA followed by ",
          "Tukey's HSD post-hoc test (", x$statistic_name, " = ",
          sprintf("%.3f", x$statistic), ", p ", p_str, "; ", eff_txt, ")."
        ),
        welch = paste0(
          "Because group variances were heterogeneous despite acceptable ",
          "normality, group differences were evaluated using Welch's ANOVA ",
          "followed by the Games-Howell post-hoc test (", x$statistic_name,
          " = ", sprintf("%.3f", x$statistic), ", p ", p_str, "; ",
          eff_txt, ")."
        ),
        kruskal = paste0(
          "Because parametric assumptions were not met",
          if (isTRUE(x$assumptions$zero_variance)) " (including zero-variance group(s))" else "",
          ", group differences were evaluated using the non-parametric ",
          "Kruskal-Wallis test followed by Dunn's pairwise post-hoc test (",
          x$method, "; ", x$statistic_name, " = ",
          sprintf("%.3f", x$statistic), ", p ", p_str, "; ", eff_txt, ")."
        )
      )

      body_txt <- paste0(
        "For ", x$response, " across ", x$group, " groups, parametric assumptions ",
        "were systematically evaluated prior to hypothesis testing using the ",
        "Shapiro-Wilk test for group-level normality, Levene's test for ",
        "homogeneity of variances, and zero-variance screening (significance ",
        "threshold \u03b1 = ", alpha_str, "). ",
        method_desc,
        " Statistically homogeneous subsets were summarized via compact letter ",
        "display (CLD) derived from a symmetric pairwise significance matrix."
      )
      return(.append_citation(body_txt, "en", include_citation))
    }

    method_desc_tr <- switch(
      x$method_code,
      anova = paste0(
        "Normallik ve varyans homojenli\u011fi varsay\u0131mlar\u0131 sa\u011fland\u0131\u011f\u0131 ",
        "i\u00e7in gruplar aras\u0131 farklar Tek Y\u00f6nl\u00fc ANOVA ve ard\u0131ndan Tukey HSD ",
        "\u00e7oklu kar\u015f\u0131la\u015ft\u0131rma testi ile de\u011ferlendirildi (",
        x$statistic_name, " = ", sprintf("%.3f", x$statistic),
        ", p ", p_str, "; ", eff_txt, ")."
      ),
      welch = paste0(
        "Varyans homojenli\u011fi ihlal edildi\u011fi i\u00e7in gruplar aras\u0131 farklar ",
        "Welch ANOVA ve ard\u0131ndan Games-Howell \u00e7oklu kar\u015f\u0131la\u015ft\u0131rma ",
        "testi ile de\u011ferlendirildi (", x$statistic_name, " = ",
        sprintf("%.3f", x$statistic), ", p ", p_str, "; ", eff_txt, ")."
      ),
      kruskal = paste0(
        "Parametrik varsay\u0131mlar sa\u011flanmad\u0131\u011f\u0131 i\u00e7in",
        if (isTRUE(x$assumptions$zero_variance)) " (s\u0131f\u0131r varyansl\u0131 grup i\u00e7erdi\u011finden)" else "",
        " gruplar aras\u0131 farklar parametrik olmayan Kruskal-Wallis testi ve ",
        "ard\u0131ndan Dunn \u00e7oklu kar\u015f\u0131la\u015ft\u0131rma testi ile de\u011ferlendirildi (",
        x$method, "; ", x$statistic_name, " = ",
        sprintf("%.3f", x$statistic), ", p ", p_str, "; ", eff_txt, ")."
      )
    )

    body_tr <- paste0(
      x$response, " de\u011fi\u015fkeninin ", x$group, " gruplar\u0131na g\u00f6re ",
      "kar\u015f\u0131la\u015ft\u0131r\u0131lmas\u0131nda hipotez testi \u00f6ncesinde ",
      "grup bazl\u0131 Shapiro-Wilk normallik testi, Levene varyans homojenli\u011fi ",
      "testi ve s\u0131f\u0131r varyans kontrol\u00fc uyguland\u0131 (anlaml\u0131l\u0131k ",
      "d\u00fczeyi \u03b1 = ", alpha_str, "). ",
      method_desc_tr,
      " \u0130statistiksel olarak homojen alt gruplar simetrik ikili anlaml\u0131l\u0131k ",
      "matrisi \u00fczerinden harflendirme (CLD) y\u00f6ntemiyle belirlendi."
    )
    return(.append_citation(body_tr, "tr", include_citation))
  }

  # --- 2. Paired two-condition comparison ---
  if (inherits(x, "statarbor_paired")) {
    p_str <- .fmt_p_rep(x$p_value)
    is_param <- identical(x$method, "Paired t-test")
    if (lang == "en") {
      stat_txt <- if (is_param) {
        sprintf("a paired Student's t-test (t(%.0f) = %.3f, p %s; Cohen's dz = %.3f)",
                x$df, x$statistic, p_str, x$effect_size["cohens_dz"])
      } else {
        sprintf("a paired Wilcoxon signed-rank test (V = %.3f, p %s; r = %.3f)",
                x$statistic, p_str, x$effect_size["r"])
      }
      reason <- if (is_param) {
        "Because paired differences met normality assumptions"
      } else {
        "Because paired differences deviated from normality or had zero variance"
      }
      txt <- sprintf(
        "Paired measurements of %s across %s (%s vs. %s; n = %d complete pairs) were screened for normality of within-subject differences via the Shapiro-Wilk test (\u03b1 = %s). %s, conditions were compared using %s.",
        x$response, x$condition, x$levels[1L], x$levels[2L], x$n_pairs,
        alpha_str, reason, stat_txt
      )
      return(.append_citation(txt, "en", include_citation))
    }

    stat_tr <- if (is_param) {
      sprintf("e\u015fle\u015ftirilmi\u015f t-testi ile de\u011ferlendirildi (t(%.0f) = %.3f, p %s; Cohen's dz = %.3f)",
              x$df, x$statistic, p_str, x$effect_size["cohens_dz"])
    } else {
      sprintf("parametrik olmayan Wilcoxon i\u015faretli s\u0131ralar testi ile de\u011ferlendirildi (V = %.3f, p %s; r = %.3f)",
              x$statistic, p_str, x$effect_size["r"])
    }
    reason_tr <- if (is_param) {
      "E\u015fle\u015ftirilmi\u015f farklar normal da\u011f\u0131l\u0131m varsay\u0131m\u0131n\u0131 sa\u011flad\u0131\u011f\u0131 i\u00e7in"
    } else {
      "E\u015fle\u015ftirilmi\u015f farklar normallik varsay\u0131m\u0131n\u0131 sa\u011flamad\u0131\u011f\u0131 i\u00e7in"
    }
    txt_tr <- sprintf(
      "%s de\u011fi\u015fkeninin %s (%s ve %s; n = %d tam \u00e7ift) ko\u015fullar\u0131 aras\u0131ndaki e\u015fle\u015ftirilmi\u015f farklar\u0131 Shapiro-Wilk normallik testi ile incelendi (\u03b1 = %s). %s ko\u015fullar aras\u0131 fark %s.",
      x$response, x$condition, x$levels[1L], x$levels[2L], x$n_pairs,
      alpha_str, reason_tr, stat_tr
    )
    return(.append_citation(txt_tr, "tr", include_citation))
  }

  # --- 3. Repeated-measures comparison (k >= 3) ---
  if (inherits(x, "statarbor_repeated")) {
    p_str <- .fmt_p_rep(x$p_value)
    is_param <- grepl("^RM-ANOVA", x$method)
    if (lang == "en") {
      res_txt <- if (is_param) {
        sprintf(
          "Because within-subject residuals satisfied normality, repeated measurements were analyzed via Greenhouse-Geisser corrected repeated-measures ANOVA (F(%.2f, %.2f) = %.3f, p %s; \u03b7p\u00b2 = %.3f, \u03b7g\u00b2 = %.3f, GG \u03b5 = %.3f) followed by %s.",
          x$df[1L], x$df[2L], x$statistic, p_str,
          x$effect_size["eta_p2"], x$effect_size["eta_g2"], x$effect_size["gg_epsilon"],
          x$method
        )
      } else {
        sprintf(
          "Because parametric repeated-measures assumptions were not met, conditions were compared using the non-parametric Friedman test (\u03c7\u00b2(%.0f) = %.3f, p %s; Kendall's W = %.3f) followed by %s.",
          x$df[1L], x$statistic, p_str, x$effect_size["kendalls_w"], x$method
        )
      }
      txt <- sprintf(
        "Repeated measurements of %s across %d %s levels (n = %d complete subjects) were assessed for within-subject residual normality (Shapiro-Wilk) and sphericity (\u03b1 = %s). %s Homogeneous subsets were summarized using compact letter display (CLD).",
        x$response, x$k_conditions, x$condition, x$n_subjects, alpha_str, res_txt
      )
      return(.append_citation(txt, "en", include_citation))
    }

    res_tr <- if (is_param) {
      sprintf(
        "Birey i\u00e7i art\u0131klar normallik varsay\u0131m\u0131n\u0131 sa\u011flad\u0131\u011f\u0131 i\u00e7in tekrarl\u0131 \u00f6l\u00e7\u00fcmler Greenhouse-Geisser d\u00fczeltmeli tekrarl\u0131 \u00f6l\u00e7\u00fcmler ANOVA ile de\u011ferlendirildi (F(%.2f, %.2f) = %.3f, p %s; \u03b7p\u00b2 = %.3f, \u03b7g\u00b2 = %.3f, GG \u03b5 = %.3f; %s).",
        x$df[1L], x$df[2L], x$statistic, p_str,
        x$effect_size["eta_p2"], x$effect_size["eta_g2"], x$effect_size["gg_epsilon"],
        x$method
      )
    } else {
      sprintf(
        "Parametrik tekrarl\u0131 \u00f6l\u00e7\u00fcm varsay\u0131mlar\u0131 sa\u011flanmad\u0131\u011f\u0131 i\u00e7in ko\u015fullar parametrik olmayan Friedman testi ile kar\u015f\u0131la\u015ft\u0131r\u0131ld\u0131 (\u03c7\u00b2(%.0f) = %.3f, p %s; Kendall W = %.3f; %s).",
        x$df[1L], x$statistic, p_str, x$effect_size["kendalls_w"], x$method
      )
    }
    txt_tr <- sprintf(
      "%s de\u011fi\u015fkeninin %d %s d\u00fczeyindeki tekrarl\u0131 \u00f6l\u00e7\u00fcmleri (n = %d tam denek) art\u0131k normalli\u011fi (Shapiro-Wilk) ve k\u00fcresellik a\u00e7\u0131s\u0131ndan de\u011ferlendirildi (\u03b1 = %s). %s Homojen alt gruplar harflendirme (CLD) y\u00f6ntemiyle \u00f6zetlendi.",
      x$response, x$k_conditions, x$condition, x$n_subjects, alpha_str, res_tr
    )
    return(.append_citation(txt_tr, "tr", include_citation))
  }

  # --- 4. Count data GLM ---
  if (inherits(x, "statarbor_count")) {
    p_str <- .fmt_p_rep(x$p_value)
    disp_val <- x$assumptions$dispersion
    stat_str <- if (length(x$df) == 2L) {
      sprintf("F(%.0f, %.0f) = %.3f, p %s", x$df[1L], x$df[2L], x$statistic, p_str)
    } else {
      sprintf("LR \u03c7\u00b2(%.0f) = %.3f, p %s", x$df[1L], x$statistic, p_str)
    }
    fam_en <- switch(x$family, poisson = "Poisson GLM", negbin = "Negative Binomial GLM",
                     quasipoisson = "Quasi-Poisson GLM")
    fam_tr <- switch(x$family, poisson = "Poisson GLM", negbin = "Negatif Binom GLM",
                     quasipoisson = "Quasi-Poisson GLM")
    zero_en <- if (isTRUE(x$assumptions$any_zero_group)) {
      " Pairwise contrasts involving zero-count groups were evaluated via two-group likelihood-ratio tests to prevent Hauck-Donner Wald distortion."
    } else ""
    zero_tr <- if (isTRUE(x$assumptions$any_zero_group)) {
      " S\u0131f\u0131r say\u0131ml\u0131 gruplar\u0131 i\u00e7eren ikili kar\u015f\u0131la\u015ft\u0131rmalarda Hauck-Donner Wald bozulmas\u0131n\u0131 \u00f6nlemek amac\u0131yla olabilirlik oran\u0131 testi uyguland\u0131."
    } else ""

    if (lang == "en") {
      txt <- sprintf(
        "Count observations of %s across %s groups were first screened for overdispersion using a log-linear Poisson GLM (Pearson dispersion = %.2f, \u03b1 = %s). Group effects were modeled via %s (%s; deviance R\u00b2 = %.3f) with incidence rate ratios (IRR) and adjusted pairwise comparisons (%s).%s Homogeneous subsets were assigned via compact letter display (CLD).",
        x$response, x$group, disp_val, alpha_str, fam_en, stat_str,
        x$effect_size["deviance_r2"], x$method, zero_en
      )
      return(.append_citation(txt, "en", include_citation))
    }

    txt_tr <- sprintf(
      "%s say\u0131m verileri %s gruplar\u0131na g\u00f6re \u00f6nce log-do\u011frusal Poisson GLM ile a\u015f\u0131r\u0131 yay\u0131l\u0131m a\u00e7\u0131s\u0131ndan de\u011ferlendirildi (Pearson dispersiyon = %.2f, \u03b1 = %s). Grup etkileri %s ile modellendi (%s; devians R\u00b2 = %.3f) ve insidans oran oranlar\u0131 (IRR) ile d\u00fczeltilmi\u015f ikili kar\u015f\u0131la\u015ft\u0131rmalar hesapland\u0131 (%s).%s Homojen gruplar harflendirme (CLD) ile \u00f6zetlendi.",
      x$response, x$group, disp_val, alpha_str, fam_tr, stat_str,
      x$effect_size["deviance_r2"], x$method, zero_tr
    )
    return(.append_citation(txt_tr, "tr", include_citation))
  }

  # --- 5. ANCOVA ---
  if (inherits(x, "statarbor_ancova")) {
    p_grp <- .fmt_p_rep(x$group_test$p_value)
    p_cov <- .fmt_p_rep(x$covariate_test$p_value)
    p_slp <- .fmt_p_rep(x$assumptions$slopes_p)
    if (lang == "en") {
      slp_txt <- if (isTRUE(x$assumptions$slopes_homogeneous)) {
        sprintf("Homogeneity of regression slopes was supported (F = %.3f, p %s), permitting a standard Type II SS ANCOVA.",
                x$assumptions$slopes_f, p_slp)
      } else {
        sprintf("Because regression slopes differed significantly across groups (F = %.3f, p %s), an interaction ANCOVA was fitted and group means were evaluated at the grand mean of %s (%.3f).",
                x$assumptions$slopes_f, p_slp, x$covariate, x$covariate_mean)
      }
      txt <- sprintf(
        "To compare %s across %s while controlling for %s (\u03b1 = %s), slope homogeneity, residual normality (Shapiro-Wilk), and homoscedasticity (Levene) were evaluated. %s After covariate adjustment (covariate F(%.0f, %.0f) = %.3f, p %s, \u03b7p\u00b2 = %.3f), the group effect yielded F(%.0f, %.0f) = %.3f, p %s (\u03b7p\u00b2 = %.3f), with adjusted marginal means compared via %s and summarized by CLD.",
        x$response, x$group, x$covariate, alpha_str, slp_txt,
        x$covariate_test$df1, x$covariate_test$df2, x$covariate_test$statistic,
        p_cov, x$effect_size["eta_p2_covariate"],
        x$group_test$df1, x$group_test$df2, x$group_test$statistic,
        p_grp, x$effect_size["eta_p2_group"], x$method
      )
      return(.append_citation(txt, "en", include_citation))
    }

    slp_tr <- if (isTRUE(x$assumptions$slopes_homogeneous)) {
      sprintf("Regresyon e\u011fimlerinin homojenli\u011fi sa\u011fland\u0131 (F = %.3f, p %s) ve Tip II kareler toplaml\u0131 ANCOVA uyguland\u0131.",
              x$assumptions$slopes_f, p_slp)
    } else {
      sprintf("Regresyon e\u011fimleri gruplar aras\u0131nda anlaml\u0131 d\u00fczeyde farkl\u0131 bulundu\u011fu i\u00e7in (F = %.3f, p %s) etkile\u015fimli ANCOVA kuruldu ve d\u00fczeltilmi\u015f ortalamalar %s genel ortalamas\u0131nda (%.3f) de\u011ferlendirildi.",
              x$assumptions$slopes_f, p_slp, x$covariate, x$covariate_mean)
    }
    txt_tr <- sprintf(
      "%s de\u011fi\u015fkeninin %s kontrol edilerek %s gruplar\u0131na g\u00f6re kar\u015f\u0131la\u015ft\u0131r\u0131lmas\u0131nda (\u03b1 = %s) e\u011fim homojenli\u011fi, art\u0131k normalli\u011fi (Shapiro-Wilk) ve varyans homojenli\u011fi (Levene) incelendi. %s Kovaryat etkisi (F(%.0f, %.0f) = %.3f, p %s, \u03b7p\u00b2 = %.3f) kontrol edildikten sonra grup etkisi F(%.0f, %.0f) = %.3f, p %s (\u03b7p\u00b2 = %.3f) olarak hesapland\u0131; d\u00fczeltilmi\u015f ortalamalar %s ile kar\u015f\u0131la\u015ft\u0131r\u0131l\u0131p CLD harfleriyle \u00f6zetlendi.",
      x$response, x$covariate, x$group, alpha_str, slp_tr,
      x$covariate_test$df1, x$covariate_test$df2, x$covariate_test$statistic,
      p_cov, x$effect_size["eta_p2_covariate"],
      x$group_test$df1, x$group_test$df2, x$group_test$statistic,
      p_grp, x$effect_size["eta_p2_group"], x$method
    )
    return(.append_citation(txt_tr, "tr", include_citation))
  }

  # --- 6. RCBD ---
  if (inherits(x, "statarbor_rcbd")) {
    p_trt <- .fmt_p_rep(x$treatment_test$p_value)
    is_friedman <- grepl("^Friedman", x$method)
    if (lang == "en") {
      if (is_friedman) {
        txt <- sprintf(
          "Randomized complete block data for %s (%d %s treatments across %d %s blocks; \u03b1 = %s) violated parametric residual assumptions and were analyzed via the non-parametric Friedman test (\u03c7\u00b2(%.0f) = %.3f, p %s; Kendall's W = %.3f; %s) with CLD grouping.",
          x$response, x$k_treatments, x$treatment, x$n_blocks, x$block, alpha_str,
          x$treatment_test$df1, x$treatment_test$statistic, p_trt,
          x$effect_size["kendalls_w"], x$method
        )
      } else if (x$block_type == "random") {
        txt <- sprintf(
          "Randomized complete block data for %s (%d %s treatments across %d random %s blocks; \u03b1 = %s) were analyzed using a linear mixed-effects model (REML estimation, ML likelihood-ratio test: \u03c7\u00b2(%.0f) = %.3f, p %s; block ICC = %.3f; %s) with CLD grouping.",
          x$response, x$k_treatments, x$treatment, x$n_blocks, x$block, alpha_str,
          x$treatment_test$df1, x$treatment_test$statistic, p_trt, x$icc, x$method
        )
      } else {
        txt <- sprintf(
          "Randomized complete block data for %s (%d %s treatments across %d fixed %s blocks; \u03b1 = %s) were screened for residual normality (Shapiro-Wilk) and homoscedasticity (Levene) and analyzed via Type II SS RCBD ANOVA (treatment F(%.0f, %.0f) = %.3f, p %s, \u03b7p\u00b2 = %.3f; block F(%.0f, %.0f) = %.3f, p %s; relative efficiency of blocking = %.2f). Marginal treatment means were compared using %s and summarized via CLD.",
          x$response, x$k_treatments, x$treatment, x$n_blocks, x$block, alpha_str,
          x$treatment_test$df1, x$treatment_test$df2, x$treatment_test$statistic,
          p_trt, x$effect_size["eta_p2_treatment"],
          x$block_test$df1, x$block_test$df2, x$block_test$statistic,
          .fmt_p_rep(x$block_test$p_value), x$relative_efficiency, x$method
        )
      }
      return(.append_citation(txt, "en", include_citation))
    }

    if (is_friedman) {
      txt_tr <- sprintf(
        "%s de\u011fi\u015fkenine ait Tesad\u00fcf Bloklar\u0131 (RCBD) verileri (%d %s muamelesi, %d %s bloku; \u03b1 = %s) parametrik art\u0131k varsay\u0131mlar\u0131n\u0131 sa\u011flamad\u0131\u011f\u0131 i\u00e7in parametrik olmayan Friedman testi ile de\u011ferlendirildi (\u03c7\u00b2(%.0f) = %.3f, p %s; Kendall W = %.3f; %s) ve CLD harfleriyle grupland\u0131r\u0131ld\u0131.",
        x$response, x$k_treatments, x$treatment, x$n_blocks, x$block, alpha_str,
        x$treatment_test$df1, x$treatment_test$statistic, p_trt,
        x$effect_size["kendalls_w"], x$method
      )
    } else if (x$block_type == "random") {
      txt_tr <- sprintf(
        "%s de\u011fi\u015fkenine ait Tesad\u00fcf Bloklar\u0131 (RCBD) verileri (%d %s muamelesi, %d rastgele %s bloku; \u03b1 = %s) do\u011frusal karma model ile analiz edildi (REML tahmini, ML olabilirlik oran\u0131 testi: \u03c7\u00b2(%.0f) = %.3f, p %s; blok ICC = %.3f; %s) ve CLD harfleriyle \u00f6zetlendi.",
        x$response, x$k_treatments, x$treatment, x$n_blocks, x$block, alpha_str,
        x$treatment_test$df1, x$treatment_test$statistic, p_trt, x$icc, x$method
      )
    } else {
      txt_tr <- sprintf(
        "%s de\u011fi\u015fkenine ait Tesad\u00fcf Bloklar\u0131 (RCBD) verileri (%d %s muamelesi, %d sabit %s bloku; \u03b1 = %s) art\u0131k normalli\u011fi (Shapiro-Wilk) ve varyans homojenli\u011fi (Levene) a\u00e7\u0131s\u0131ndan incelenerek Tip II kareler toplaml\u0131 RCBD ANOVA ile analiz edildi (muamele F(%.0f, %.0f) = %.3f, p %s, \u03b7p\u00b2 = %.3f; blok F(%.0f, %.0f) = %.3f, p %s; bloklama g\u00f6reli verimlili\u011fi = %.2f). Marjinal ortalamalar %s ile kar\u015f\u0131la\u015ft\u0131r\u0131l\u0131p CLD ile \u00f6zetlendi.",
        x$response, x$k_treatments, x$treatment, x$n_blocks, x$block, alpha_str,
        x$treatment_test$df1, x$treatment_test$df2, x$treatment_test$statistic,
        p_trt, x$effect_size["eta_p2_treatment"],
        x$block_test$df1, x$block_test$df2, x$block_test$statistic,
        .fmt_p_rep(x$block_test$p_value), x$relative_efficiency, x$method
      )
    }
    return(.append_citation(txt_tr, "tr", include_citation))
  }

  stop(
    "`x` must be an object of class 'statarbor_result', 'statarbor_paired', ",
    "'statarbor_repeated', 'statarbor_count', 'statarbor_ancova', or 'statarbor_rcbd'.",
    call. = FALSE
  )
}

#' Export a Self-Contained Reproducible R Script
#'
#' Generates an executable, human-readable R script that reproduces both the
#' high-level `statarbor` workflow and the underlying base R / reference package
#' verification calls for complete analytical transparency.
#'
#' @param x A `statarbor` result object returned by [auto_compare()],
#'   [compare_paired()], [auto_repeated()], [analyze_count()],
#'   [analyze_ancova()], or [analyze_rcbd()].
#' @param data_name Character scalar naming the data frame object to reference
#'   in the generated script (default `"my_data"`).
#' @param file Optional file path. If provided, writes the script to disk and
#'   returns the path invisibly; otherwise returns the script lines as a
#'   character vector.
#'
#' @return Character vector of R code lines (or `file` path invisibly if
#'   written to disk).
#' @export
#'
#' @examples
#' res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
#' code_lines <- export_r_script(res, data_name = "iris")
#' cat(code_lines, sep = "\n")
export_r_script <- function(x, data_name = "my_data", file = NULL) {
  alpha_str <- format(x$alpha, trim = TRUE)
  date_str <- format(Sys.Date(), "%Y-%m-%d")

  if (inherits(x, "statarbor_result")) {
    p_adj <- if (grepl("holm", x$method, ignore.case = TRUE)) {
      "holm"
    } else if (grepl("BH", x$method, ignore.case = FALSE)) {
      "BH"
    } else {
      "bonferroni"
    }

    fml_str <- sprintf("`%s` ~ `%s`", x$response, x$group)

    low_level_block <- switch(
      x$method_code,
      anova = c(
        "# Direct verification via base R & rstatix:",
        sprintf("mod <- stats::aov(%s, data = %s)", fml_str, data_name),
        "summary(mod)",
        sprintf("rstatix::tukey_hsd(%s, %s)", data_name, fml_str)
      ),
      welch = c(
        "# Direct verification via base R (Welch ANOVA) & rstatix (Games-Howell):",
        sprintf("stats::oneway.test(%s, data = %s, var.equal = FALSE)", fml_str, data_name),
        sprintf("rstatix::games_howell_test(%s, %s)", data_name, fml_str)
      ),
      kruskal = c(
        "# Direct verification via base R (Kruskal-Wallis) & rstatix (Dunn):",
        sprintf("stats::kruskal.test(%s, data = %s)", fml_str, data_name),
        sprintf("rstatix::dunn_test(%s, %s, p.adjust.method = \"%s\")",
                data_name, fml_str, p_adj)
      )
    )

    lines <- c(
      "# =========================================================================",
      "# Reproducible R Script Generated by statarbor",
      sprintf("# Date    : %s", date_str),
      sprintf("# Response: %s | Group: %s", x$response, x$group),
      sprintf("# Routed  : %s", x$method),
      "# =========================================================================",
      "",
      "library(statarbor)",
      "library(car)",
      "library(rstatix)",
      "",
      sprintf("# 1. Ensure `%s` is loaded in your environment (e.g., read.csv(...))", data_name),
      sprintf("# %s <- read.csv(\"your_dataset.csv\")", data_name),
      "",
      "# 2. Assumption diagnostics",
      sprintf("car::leveneTest(%s[[%s]], as.factor(%s[[%s]]))",
              data_name, deparse(x$response), data_name, deparse(x$group)),
      "",
      "# 3. Automated statarbor decision workflow",
      sprintf(
        "res <- statarbor::auto_compare(data = %s, response = %s, group = %s, mode = \"auto\", p_adjust = \"%s\", alpha = %s)",
        data_name, deparse(x$response), deparse(x$group), p_adj, alpha_str
      ),
      "print(res)",
      "res$descriptives",
      "res$posthoc",
      "",
      "# 4. Transparent verification calls",
      low_level_block,
      "",
      "# 5. Publication-ready methods paragraph",
      "cat(statarbor::generate_methods_text(res, lang = \"en\"), \"\\n\")"
    )
  } else if (inherits(x, "statarbor_paired")) {
    lines <- c(
      "# =========================================================================",
      "# Reproducible Paired Comparison Script Generated by statarbor",
      sprintf("# Date    : %s", date_str),
      sprintf("# Response: %s | Condition: %s | Subject ID: %s", x$response, x$condition, x$id),
      sprintf("# Routed  : %s", x$method),
      "# =========================================================================",
      "",
      "library(statarbor)",
      "",
      sprintf("res <- statarbor::compare_paired(data = %s, response = %s, condition = %s, id = %s, alpha = %s)",
              data_name, deparse(x$response), deparse(x$condition), deparse(x$id), alpha_str),
      "print(res)",
      "plot(res)",
      "cat(statarbor::generate_methods_text(res, lang = \"en\"), \"\\n\")"
    )
  } else if (inherits(x, "statarbor_repeated")) {
    p_adj <- .extract_padj(x$method, default = "holm")
    lines <- c(
      "# =========================================================================",
      "# Reproducible Repeated-Measures Script Generated by statarbor",
      sprintf("# Date    : %s", date_str),
      sprintf("# Response: %s | Condition: %s | Subject ID: %s", x$response, x$condition, x$id),
      sprintf("# Routed  : %s", x$method),
      "# =========================================================================",
      "",
      "library(statarbor)",
      "",
      sprintf("res <- statarbor::auto_repeated(data = %s, response = %s, condition = %s, id = %s, alpha = %s, p_adjust = \"%s\")",
              data_name, deparse(x$response), deparse(x$condition), deparse(x$id), alpha_str, p_adj),
      "print(res)",
      "res$pairwise",
      "plot(res)",
      "",
      "# Base R reference check",
      sprintf("summary(stats::aov(`%s` ~ factor(`%s`) + Error(factor(`%s`) / factor(`%s`)), data = %s))",
              x$response, x$condition, x$id, x$condition, data_name),
      "cat(statarbor::generate_methods_text(res, lang = \"en\"), \"\\n\")"
    )
  } else if (inherits(x, "statarbor_count")) {
    p_adj <- .extract_padj(x$method, default = "holm")
    off_arg <- if (is.null(x$offset)) "" else sprintf(", offset = %s", deparse(x$offset))
    fml_str <- if (is.null(x$offset)) {
      sprintf("`%s` ~ factor(`%s`)", x$response, x$group)
    } else {
      sprintf("`%s` ~ factor(`%s`) + offset(log(`%s`))", x$response, x$group, x$offset)
    }
    ref_call <- if (x$family == "negbin") {
      sprintf("MASS::glm.nb(%s, data = %s)", fml_str, data_name)
    } else if (x$family == "quasipoisson") {
      sprintf("stats::glm(%s, data = %s, family = stats::quasipoisson(link = \"log\"))", fml_str, data_name)
    } else {
      sprintf("stats::glm(%s, data = %s, family = stats::poisson(link = \"log\"))", fml_str, data_name)
    }
    lines <- c(
      "# =========================================================================",
      "# Reproducible Count Data GLM Script Generated by statarbor",
      sprintf("# Date    : %s", date_str),
      sprintf("# Response: %s | Group: %s | Family: %s", x$response, x$group, x$family),
      sprintf("# Routed  : %s", x$method),
      "# =========================================================================",
      "",
      "library(statarbor)",
      "",
      sprintf("res <- statarbor::analyze_count(data = %s, response = %s, group = %s%s, alpha = %s, p_adjust = \"%s\")",
              data_name, deparse(x$response), deparse(x$group), off_arg, alpha_str, p_adj),
      "print(res)",
      "res$pairwise",
      "plot(res)",
      "",
      "# Base R / MASS reference fit",
      sprintf("summary(%s)", ref_call),
      "cat(statarbor::generate_methods_text(res, lang = \"en\"), \"\\n\")"
    )
  } else if (inherits(x, "statarbor_ancova")) {
    p_adj <- .extract_padj(x$method, default = "holm")
    lines <- c(
      "# =========================================================================",
      "# Reproducible ANCOVA Script Generated by statarbor",
      sprintf("# Date     : %s", date_str),
      sprintf("# Response : %s | Group: %s | Covariate: %s", x$response, x$group, x$covariate),
      sprintf("# Routed   : %s", x$method),
      "# =========================================================================",
      "",
      "library(statarbor)",
      "library(car)",
      "",
      sprintf("res <- statarbor::analyze_ancova(data = %s, response = %s, group = %s, covariate = %s, alpha = %s, p_adjust = \"%s\")",
              data_name, deparse(x$response), deparse(x$group), deparse(x$covariate), alpha_str, p_adj),
      "print(res)",
      "res$pairwise",
      "plot(res)",
      "",
      "# Reference slope homogeneity and Type II SS check",
      sprintf("fit_add <- stats::lm(`%s` ~ factor(`%s`) + `%s`, data = %s)",
              x$response, x$group, x$covariate, data_name),
      sprintf("fit_int <- stats::lm(`%s` ~ factor(`%s`) * `%s`, data = %s)",
              x$response, x$group, x$covariate, data_name),
      "stats::anova(fit_add, fit_int)",
      "car::Anova(fit_add, type = 2)",
      "cat(statarbor::generate_methods_text(res, lang = \"en\"), \"\\n\")"
    )
  } else if (inherits(x, "statarbor_rcbd")) {
    p_adj <- .extract_padj(x$method, default = "tukey")
    ref_line <- if (x$block_type == "random") {
      sprintf("lme4::lmer(`%s` ~ factor(`%s`) + (1 | factor(`%s`)), data = %s)",
              x$response, x$treatment, x$block, data_name)
    } else {
      sprintf("summary(stats::aov(`%s` ~ factor(`%s`) + factor(`%s`), data = %s))",
              x$response, x$treatment, x$block, data_name)
    }
    lines <- c(
      "# =========================================================================",
      "# Reproducible RCBD Script Generated by statarbor",
      sprintf("# Date      : %s", date_str),
      sprintf("# Response  : %s | Treatment: %s | Block: %s (%s)",
              x$response, x$treatment, x$block, x$block_type),
      sprintf("# Routed    : %s", x$method),
      "# =========================================================================",
      "",
      "library(statarbor)",
      "",
      sprintf("res <- statarbor::analyze_rcbd(data = %s, response = %s, treatment = %s, block = %s, block_type = \"%s\", alpha = %s, p_adjust = \"%s\")",
              data_name, deparse(x$response), deparse(x$treatment), deparse(x$block),
              x$block_type, alpha_str, p_adj),
      "print(res)",
      "res$pairwise",
      "plot(res)",
      "",
      "# Reference fit",
      ref_line,
      "cat(statarbor::generate_methods_text(res, lang = \"en\"), \"\\n\")"
    )
  } else {
    stop(
      "`x` must be an object of class 'statarbor_result', 'statarbor_paired', ",
      "'statarbor_repeated', 'statarbor_count', 'statarbor_ancova', or 'statarbor_rcbd'.",
      call. = FALSE
    )
  }

  if (!is.null(file)) {
    writeLines(lines, con = file, useBytes = TRUE)
    return(invisible(file))
  }
  lines
}
