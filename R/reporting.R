#' Generate Publication-Ready Statistical Methods Paragraph
#'
#' Produces a manuscript-ready statistical methods paragraph (in English or
#' Turkish) summarizing the assumption checks, routing decisions, post-hoc
#' procedures, compact letter display (CLD) logic, and effect size metrics
#' applied to a `"statarbor_result"` object.
#'
#' @param x An object of class `"statarbor_result"` returned by
#'   [auto_compare()].
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
  if (!inherits(x, "statarbor_result")) {
    stop("`x` must be an object of class 'statarbor_result'.", call. = FALSE)
  }
  lang <- match.arg(lang)

  alpha_str <- format(x$alpha, trim = TRUE)
  p_str <- if (x$p_value < 0.001) "< 0.001" else sprintf("= %.4f", x$p_value)
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

    if (isTRUE(include_citation)) {
      body_txt <- paste0(
        body_txt,
        " All analyses were performed in R (R Core Team) using the statarbor ",
        "package."
      )
    }
    return(body_txt)
  }

  # Turkish output using CRAN-safe ASCII Unicode escapes
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

  if (isTRUE(include_citation)) {
    body_tr <- paste0(
      body_tr,
      " T\u00fcm istatistiksel analizler R ortam\u0131nda statarbor paketi ",
      "kullan\u0131larak ger\u00e7ekle\u015ftirildi."
    )
  }
  body_tr
}

#' Export a Self-Contained Reproducible R Script
#'
#' Generates an executable, human-readable R script that reproduces both the
#' high-level `statarbor::auto_compare()` workflow and the underlying base R /
#' `car` / `rstatix` calls for complete analytical transparency.
#'
#' @param x An object of class `"statarbor_result"` returned by
#'   [auto_compare()].
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
  if (!inherits(x, "statarbor_result")) {
    stop("`x` must be an object of class 'statarbor_result'.", call. = FALSE)
  }

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
    sprintf("# Date    : %s", format(Sys.Date(), "%Y-%m-%d")),
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
      data_name, deparse(x$response), deparse(x$group), p_adj, format(x$alpha, trim = TRUE)
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

  if (!is.null(file)) {
    writeLines(lines, con = file, useBytes = TRUE)
    return(invisible(file))
  }
  lines
}
