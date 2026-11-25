# Internal helper for compact p-value formatting in plot subtitles
.fmt_p_v02 <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("= %.3f", p)
}

# Internal helper to build a publication-grade CLD point/bar plot
.plot_cld_summary_v02 <- function(df, x_col, y_col, lo_col, hi_col,
                                  cld_col = NULL, title = NULL,
                                  subtitle = NULL, xlab = "", ylab = "",
                                  style = c("pointrange", "bar")) {
  style <- match.arg(style)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  plot_df <- data.frame(
    x_val = factor(df[[x_col]], levels = df[[x_col]]),
    y_val = as.numeric(df[[y_col]]),
    lo_val = as.numeric(df[[lo_col]]),
    hi_val = as.numeric(df[[hi_col]]),
    stringsAsFactors = FALSE
  )
  # Safeguard against NA CI bounds (e.g., zero-count groups in Poisson/NB)
  plot_df$lo_val <- ifelse(is.na(plot_df$lo_val), plot_df$y_val, plot_df$lo_val)
  plot_df$hi_val <- ifelse(is.na(plot_df$hi_val), plot_df$y_val, plot_df$hi_val)

  y_span <- max(plot_df$hi_val, plot_df$y_val, na.rm = TRUE) -
    min(plot_df$lo_val, plot_df$y_val, 0, na.rm = TRUE)
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1
  plot_df$lbl_y <- pmax(plot_df$hi_val, plot_df$y_val, na.rm = TRUE) + 0.08 * y_span

  if (!is.null(cld_col) && cld_col %in% names(df)) {
    plot_df$cld_lbl <- as.character(df[[cld_col]])
  } else {
    plot_df$cld_lbl <- NULL
  }

  # Local bindings to avoid R CMD check global variable notes
  x_val <- y_val <- lo_val <- hi_val <- lbl_y <- cld_lbl <- NULL

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x_val, y = y_val))
  if (style == "bar") {
    p <- p +
      ggplot2::geom_col(fill = "#2C5F8A", color = "#1A3A54", width = 0.58, alpha = 0.88) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lo_val, ymax = hi_val),
        width = 0.18, linewidth = 0.65, color = "#1A1A1A"
      )
  } else {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lo_val, ymax = hi_val),
        width = 0.18, linewidth = 0.7, color = "#2C5F8A"
      ) +
      ggplot2::geom_point(size = 3.2, color = "#1A3A54", fill = "#2C5F8A", shape = 21)
  }

  if (!is.null(plot_df$cld_lbl)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(y = lbl_y, label = cld_lbl),
        fontface = "bold", size = 4.2, color = "#1A1A1A"
      )
  }

  y_top <- max(plot_df$lbl_y, na.rm = TRUE) + 0.08 * y_span
  p +
    ggplot2::expand_limits(y = y_top) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    theme_statarbor() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}

#' Plot Method for Paired Two-Condition Comparisons
#'
#' @param x An object of class `"statarbor_paired"`.
#' @param style Character. Either `"pointrange"` (default) or `"bar"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `ggplot` object.
#' @export
plot.statarbor_paired <- function(x, style = c("pointrange", "bar"), ...) {
  style <- match.arg(style)
  df <- x$descriptives
  is_param <- identical(x$method, "Paired t-test")
  if (is_param) {
    df$center <- df$mean
    df$lo <- df$mean - df$sd
    df$hi <- df$mean + df$sd
    eff_txt <- sprintf("Cohen's dz = %.3f", x$effect_size["cohens_dz"])
    ylab <- paste0(x$response, " (Mean \u00b1 SD)")
  } else {
    df$center <- df$median
    df$lo <- df$median - df$iqr / 2
    df$hi <- df$median + df$iqr / 2
    eff_txt <- sprintf("r = %.3f", x$effect_size["r"])
    ylab <- paste0(x$response, " (Median \u00b1 IQR/2)")
  }
  df$cld <- if (!is.na(x$p_value) && x$p_value < x$alpha) c("a", "b") else c("a", "a")

  sub_txt <- sprintf("%s | p %s | %s (n = %d pairs)",
                     x$method, .fmt_p_v02(x$p_value), eff_txt, x$n_pairs)
  .plot_cld_summary_v02(
    df, x_col = "condition", y_col = "center", lo_col = "lo", hi_col = "hi",
    cld_col = "cld", title = paste("Paired Comparison:", x$response),
    subtitle = sub_txt, xlab = x$condition, ylab = ylab, style = style
  )
}

#' Plot Method for Repeated-Measures Comparisons
#'
#' @param x An object of class `"statarbor_repeated"`.
#' @param style Character. Either `"pointrange"` (default) or `"bar"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `ggplot` object.
#' @export
plot.statarbor_repeated <- function(x, style = c("pointrange", "bar"), ...) {
  style <- match.arg(style)
  df <- x$descriptives
  is_param <- grepl("^RM-ANOVA", x$method)
  if (is_param) {
    df$center <- df$mean
    df$lo <- df$mean - df$sd
    df$hi <- df$mean + df$sd
    eff_txt <- sprintf("\u03b7p\u00b2 = %.3f, GG \u03b5 = %.2f",
                       x$effect_size["eta_p2"], x$effect_size["gg_epsilon"])
    ylab <- paste0(x$response, " (Mean \u00b1 SD)")
  } else {
    df$center <- df$median
    df$lo <- df$median - df$iqr / 2
    df$hi <- df$median + df$iqr / 2
    eff_txt <- sprintf("Kendall's W = %.3f", x$effect_size["kendalls_w"])
    ylab <- paste0(x$response, " (Median \u00b1 IQR/2)")
  }

  sub_txt <- sprintf("%s | p %s | %s (n = %d)",
                     x$method, .fmt_p_v02(x$p_value), eff_txt, x$n_subjects)
  .plot_cld_summary_v02(
    df, x_col = "condition", y_col = "center", lo_col = "lo", hi_col = "hi",
    cld_col = "cld", title = paste("Repeated-Measures:", x$response),
    subtitle = sub_txt, xlab = x$condition, ylab = ylab, style = style
  )
}

#' Plot Method for Count Data GLM Analyses
#'
#' @param x An object of class `"statarbor_count"`.
#' @param style Character. Either `"bar"` (default) or `"pointrange"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `ggplot` object.
#' @export
plot.statarbor_count <- function(x, style = c("bar", "pointrange"), ...) {
  style <- match.arg(style)
  df <- x$descriptives
  sub_txt <- sprintf("%s | p %s | Dev. R\u00b2 = %.3f, Disp. = %.2f",
                     x$method, .fmt_p_v02(x$p_value),
                     x$effect_size["deviance_r2"], x$effect_size["dispersion"])
  ylab <- if (is.null(x$offset)) {
    paste0(x$response, " (Estimated Mean Count, 95% CI)")
  } else {
    paste0(x$response, " / ", x$offset, " (Estimated Rate, 95% CI)")
  }
  .plot_cld_summary_v02(
    df, x_col = "group", y_col = "est_rate", lo_col = "ci_low", hi_col = "ci_high",
    cld_col = "cld", title = paste("Count GLM:", x$response),
    subtitle = sub_txt, xlab = x$group, ylab = ylab, style = style
  )
}

#' Plot Method for ANCOVA Adjusted Means
#'
#' @param x An object of class `"statarbor_ancova"`.
#' @param style Character. Either `"pointrange"` (default) or `"bar"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `ggplot` object.
#' @export
plot.statarbor_ancova <- function(x, style = c("pointrange", "bar"), ...) {
  style <- match.arg(style)
  df <- x$descriptives
  sub_txt <- sprintf("%s | Group p %s, \u03b7p\u00b2 = %.3f (at %s = %.2f)",
                     x$method, .fmt_p_v02(x$group_test$p_value),
                     x$effect_size["eta_p2_group"], x$covariate, x$covariate_mean)
  .plot_cld_summary_v02(
    df, x_col = "group", y_col = "adj_mean", lo_col = "ci_low", hi_col = "ci_high",
    cld_col = "cld", title = paste("ANCOVA Adjusted Means:", x$response),
    subtitle = sub_txt, xlab = x$group,
    ylab = paste0(x$response, " (Adjusted Mean, 95% CI)"), style = style
  )
}

#' Plot Method for Randomized Complete Block Designs (RCBD)
#'
#' @param x An object of class `"statarbor_rcbd"`.
#' @param style Character. Either `"pointrange"` (default) or `"bar"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `ggplot` object.
#' @export
plot.statarbor_rcbd <- function(x, style = c("pointrange", "bar"), ...) {
  style <- match.arg(style)
  df <- x$descriptives
  is_friedman <- grepl("^Friedman", x$method)

  if (is_friedman) {
    df$center <- df$median
    df$lo <- df$median - df$iqr / 2
    df$hi <- df$median + df$iqr / 2
    sub_txt <- sprintf("%s | p %s | Kendall's W = %.3f (b = %d blocks)",
                       x$method, .fmt_p_v02(x$treatment_test$p_value),
                       x$effect_size["kendalls_w"], x$n_blocks)
    ylab <- paste0(x$response, " (Median \u00b1 IQR/2)")
  } else {
    df$center <- df$emm
    df$lo <- df$ci_low
    df$hi <- df$ci_high
    if (x$block_type == "random") {
      sub_txt <- sprintf("%s | p %s | Block ICC = %.3f (b = %d)",
                         x$method, .fmt_p_v02(x$treatment_test$p_value),
                         x$icc, x$n_blocks)
    } else {
      sub_txt <- sprintf("%s | p %s | \u03b7p\u00b2 = %.3f, Block RE = %.2f",
                         x$method, .fmt_p_v02(x$treatment_test$p_value),
                         x$effect_size["eta_p2_treatment"], x$relative_efficiency)
    }
    ylab <- paste0(x$response, " (Estimated Marginal Mean, 95% CI)")
  }

  .plot_cld_summary_v02(
    df, x_col = "treatment", y_col = "center", lo_col = "lo", hi_col = "hi",
    cld_col = "cld", title = paste("RCBD Treatment Means:", x$response),
    subtitle = sub_txt, xlab = x$treatment, ylab = ylab, style = style
  )
}
