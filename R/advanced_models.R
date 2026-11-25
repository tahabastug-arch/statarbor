#' Assumption-Guided Two-Group Comparison
#'
#' Automatically checks group-level normality (Shapiro-Wilk) and zero-variance
#' conditions for a two-level grouping factor, routing to either Welch's
#' two-sample t-test (with Cohen's d) or the non-parametric Mann-Whitney U test
#' (with rank-biserial-like effect size r = |Z| / sqrt(N)).
#'
#' @param data A data frame.
#' @param response Character scalar naming the numeric response column.
#' @param group Character scalar naming a two-level grouping column.
#' @param mode Routing strategy: `"auto"` (default), `"t.test"`, or
#'   `"mann_whitney"`.
#' @param alpha Numeric significance level (default `0.05`).
#'
#' @return An object of S3 class `"statarbor_two_group"`.
#' @export
#'
#' @examples
#' sub_iris <- iris[iris$Species != "setosa", ]
#' compare_two_groups(sub_iris, response = "Sepal.Length", group = "Species")
compare_two_groups <- function(data,
                               response,
                               group,
                               mode = c("auto", "t.test", "mann_whitney"),
                               alpha = 0.05) {
  mode <- match.arg(mode)

  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!response %in% names(data)) {
    stop(sprintf("Response column '%s' not found.", response), call. = FALSE)
  }
  if (!group %in% names(data)) {
    stop(sprintf("Group column '%s' not found.", group), call. = FALSE)
  }

  y_raw <- suppressWarnings(as.numeric(as.character(data[[response]])))
  g_raw <- as.character(data[[group]])
  keep <- !is.na(y_raw) & !is.na(g_raw) & nzchar(trimws(g_raw))

  sub_df <- data.frame(
    y_val = y_raw[keep],
    g_val = factor(g_raw[keep], levels = unique(g_raw[keep])),
    stringsAsFactors = FALSE
  )

  if (nlevels(sub_df$g_val) != 2L || nrow(sub_df) < 4L) {
    stop("`group` must have exactly 2 non-empty levels with at least 4 total observations.",
         call. = FALSE)
  }

  lvls <- levels(sub_df$g_val)
  g1 <- sub_df$y_val[sub_df$g_val == lvls[1L]]
  g2 <- sub_df$y_val[sub_df$g_val == lvls[2L]]

  desc_df <- data.frame(
    group = lvls,
    n = c(length(g1), length(g2)),
    mean = c(mean(g1), mean(g2)),
    sd = c(stats::sd(g1), stats::sd(g2)),
    median = c(stats::median(g1), stats::median(g2)),
    iqr = c(stats::IQR(g1), stats::IQR(g2)),
    shapiro_p = c(safe_shapiro(g1), safe_shapiro(g2)),
    stringsAsFactors = FALSE
  )

  zero_var <- any(is.na(desc_df$sd) | desc_df$sd == 0)
  norm_violated <- any(!is.na(desc_df$shapiro_p) & desc_df$shapiro_p < alpha)

  chosen <- mode
  if (mode == "auto") {
    chosen <- if (zero_var || norm_violated) "mann_whitney" else "t.test"
  } else if (chosen == "t.test" && zero_var) {
    chosen <- "mann_whitney"
  }

  if (chosen == "t.test") {
    tt <- stats::t.test(y_val ~ g_val, data = sub_df, var.equal = FALSE)
    pooled_sd <- sqrt(((length(g1) - 1) * stats::var(g1) +
                         (length(g2) - 1) * stats::var(g2)) /
                        (length(g1) + length(g2) - 2))
    cohens_d <- if (pooled_sd > 0) abs(mean(g1) - mean(g2)) / pooled_sd else NA_real_

    out <- list(
      response = response,
      group = group,
      method_code = "t.test",
      method = "Welch Two-Sample t-test",
      statistic_name = sprintf("t(df=%.2f)", as.numeric(tt$parameter)),
      statistic = as.numeric(tt$statistic),
      p_value = as.numeric(tt$p.value),
      effect_size = list(cohens_d = cohens_d),
      descriptives = desc_df,
      test_object = tt
    )
  } else {
    wt <- stats::wilcox.test(y_val ~ g_val, data = sub_df, exact = FALSE)
    p_safe <- max(min(wt$p.value, 1 - 1e-15), 1e-15)
    z_val <- abs(stats::qnorm(p_safe / 2))
    r_eff <- z_val / sqrt(nrow(sub_df))

    out <- list(
      response = response,
      group = group,
      method_code = "mann_whitney",
      method = "Mann-Whitney U Test",
      statistic_name = "W",
      statistic = as.numeric(wt$statistic),
      p_value = as.numeric(wt$p.value),
      effect_size = list(r = r_eff),
      descriptives = desc_df,
      test_object = wt
    )
  }

  class(out) <- "statarbor_two_group"
  out
}

#' Binary Outcome Analysis with Separation-Aware Firth Logistic Regression
#'
#' Evaluates a binary (`0`/`1` or two-level) outcome across groups using a
#' Chi-square test of independence (with Cram\'er's V) and logistic regression.
#' Automatically detects complete separation (groups with 0% or 100% event
#' rates) and routes to Firth's penalized-likelihood logistic regression via
#' `logistf` when available.
#'
#' @param data A data frame.
#' @param response Character scalar naming the binary outcome column.
#' @param group Character scalar naming the grouping column.
#' @param use_firth Character string: `"auto"` (default; uses Firth when
#'   complete separation is detected), `"always"`, or `"never"`.
#'
#' @return An object of S3 class `"statarbor_binary"`.
#' @export
#'
#' @examples
#' df_bin <- data.frame(
#'   grp = rep(c("Control", "Stress_A", "Stress_B"), each = 10),
#'   germ = c(rep(1, 10), c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), rep(0, 10))
#' )
#' analyze_binary(df_bin, response = "germ", group = "grp")
analyze_binary <- function(data,
                           response,
                           group,
                           use_firth = c("auto", "always", "never")) {
  use_firth <- match.arg(use_firth)

  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!all(c(response, group) %in% names(data))) {
    stop("`response` and `group` must exist in `data`.", call. = FALSE)
  }

  y_raw <- data[[response]]
  g_raw <- as.character(data[[group]])
  keep <- !is.na(y_raw) & !is.na(g_raw) & nzchar(trimws(g_raw))

  y_chr <- as.character(y_raw[keep])
  u_vals <- unique(y_chr)
  if (length(u_vals) != 2L) {
    stop("`response` must contain exactly 2 distinct non-missing values.",
         call. = FALSE)
  }

  if (all(u_vals %in% c("0", "1"))) {
    y_num <- as.numeric(y_chr)
    event_label <- "1"
  } else {
    y_fac <- factor(y_chr)
    y_num <- as.numeric(y_fac) - 1
    event_label <- levels(y_fac)[2L]
  }

  sub_df <- data.frame(
    y_val = y_num,
    g_val = factor(g_raw[keep], levels = unique(g_raw[keep])),
    stringsAsFactors = FALSE
  )

  if (nlevels(sub_df$g_val) < 2L) {
    stop("`group` must have at least 2 levels.", call. = FALSE)
  }

  # Group success rates and complete separation check
  lvls <- levels(sub_df$g_val)
  rates_df <- do.call(rbind, lapply(lvls, function(g) {
    yv <- sub_df$y_val[sub_df$g_val == g]
    data.frame(
      group = g,
      n = length(yv),
      events = sum(yv == 1),
      event_rate = mean(yv == 1),
      stringsAsFactors = FALSE
    )
  }))

  complete_sep <- any(rates_df$event_rate %in% c(0, 1))

  # Contingency table, Chi-square, and Cramer's V
  tbl <- table(Outcome = sub_df$y_val, Group = sub_df$g_val)
  chi <- suppressWarnings(stats::chisq.test(tbl))
  n_tot <- sum(tbl)
  cramers_v <- sqrt(as.numeric(chi$statistic) / (n_tot * (min(dim(tbl)) - 1L)))
  low_expected <- any(chi$expected < 5)

  # Route between Firth penalized logistic regression and standard GLM
  want_firth <- (use_firth == "always") || (use_firth == "auto" && complete_sep)
  has_logistf <- requireNamespace("logistf", quietly = TRUE)

  if (want_firth && has_logistf) {
    fit <- logistf::logistf(y_val ~ g_val, data = sub_df, pl = TRUE)
    coef_tbl <- data.frame(
      term = names(fit$coefficients),
      estimate = as.numeric(fit$coefficients),
      odds_ratio = exp(as.numeric(fit$coefficients)),
      ci_low = exp(as.numeric(fit$ci.lower)),
      ci_high = exp(as.numeric(fit$ci.upper)),
      p_value = as.numeric(fit$prob),
      stringsAsFactors = FALSE
    )
    reg_method <- "Firth Penalized Logistic Regression"
  } else {
    fit <- suppressWarnings(
      stats::glm(y_val ~ g_val, data = sub_df, family = stats::binomial())
    )
    sm <- summary(fit)$coefficients
    est <- sm[, "Estimate"]
    se <- sm[, "Std. Error"]
    coef_tbl <- data.frame(
      term = rownames(sm),
      estimate = as.numeric(est),
      odds_ratio = exp(as.numeric(est)),
      ci_low = exp(as.numeric(est - 1.96 * se)),
      ci_high = exp(as.numeric(est + 1.96 * se)),
      p_value = as.numeric(sm[, "Pr(>|z|)"]),
      stringsAsFactors = FALSE
    )
    reg_method <- if (complete_sep) {
      "Standard Logistic Regression (Warning: complete separation detected)"
    } else {
      "Standard Logistic Regression"
    }
  }

  out <- list(
    response = response,
    event_label = event_label,
    group = group,
    complete_separation = complete_sep,
    method = reg_method,
    group_rates = rates_df,
    contingency_table = tbl,
    chisq = list(
      statistic = as.numeric(chi$statistic),
      df = as.numeric(chi$parameter),
      p_value = as.numeric(chi$p.value),
      cramers_v = cramers_v,
      low_expected_counts = low_expected
    ),
    coefficients = coef_tbl,
    model = fit
  )
  class(out) <- "statarbor_binary"
  out
}

#' Ordinal Logistic Regression (Cumulative Link Model)
#'
#' Fits a proportional-odds cumulative link model using `ordinal::clm()` for
#' ordered categorical outcomes (e.g., viability scores or regeneration stages)
#' and evaluates overall group significance via a likelihood-ratio test against
#' the intercept-only model.
#'
#' @param data A data frame.
#' @param response Character scalar naming the ordered outcome column.
#' @param group Character scalar naming the grouping factor column.
#' @param levels_order Optional character vector specifying the ordered levels
#'   of `response` from lowest to highest.
#'
#' @return An object of S3 class `"statarbor_ordinal"`.
#' @export
#'
#' @examples
#' if (requireNamespace("ordinal", quietly = TRUE)) {
#'   df_ord <- data.frame(
#'     grp = rep(c("Low", "Mid", "High"), each = 15),
#'     score = c(rep(1:2, length.out = 15),
#'               rep(2:3, length.out = 15),
#'               rep(3:4, length.out = 15))
#'   )
#'   analyze_ordinal(df_ord, response = "score", group = "grp")
#' }
analyze_ordinal <- function(data, response, group, levels_order = NULL) {
  if (!requireNamespace("ordinal", quietly = TRUE)) {
    stop("Package 'ordinal' is required for analyze_ordinal(). Please install it.",
         call. = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!all(c(response, group) %in% names(data))) {
    stop("`response` and `group` must exist in `data`.", call. = FALSE)
  }

  y_raw <- data[[response]]
  g_raw <- as.character(data[[group]])
  keep <- !is.na(y_raw) & !is.na(g_raw) & nzchar(trimws(g_raw))

  y_sub <- y_raw[keep]
  if (!is.null(levels_order)) {
    y_ord <- factor(as.character(y_sub), levels = levels_order, ordered = TRUE)
  } else if (is.ordered(y_sub)) {
    y_ord <- droplevels(y_sub)
  } else if (is.numeric(y_sub)) {
    y_ord <- factor(y_sub, levels = sort(unique(y_sub)), ordered = TRUE)
  } else {
    y_ord <- factor(as.character(y_sub), ordered = TRUE)
  }

  sub_df <- data.frame(
    y_val = y_ord,
    g_val = factor(g_raw[keep], levels = unique(g_raw[keep])),
    stringsAsFactors = FALSE
  )

  if (nlevels(sub_df$y_val) < 2L || nlevels(sub_df$g_val) < 2L) {
    stop("Both `response` and `group` must have at least 2 levels.", call. = FALSE)
  }

  fit <- ordinal::clm(y_val ~ g_val, data = sub_df)
  null_fit <- ordinal::clm(y_val ~ 1, data = sub_df)

  lrt <- stats::anova(null_fit, fit)
  ll_null <- as.numeric(stats::logLik(null_fit))
  ll_full <- as.numeric(stats::logLik(fit))
  n_obs <- nrow(sub_df)
  cox_snell <- 1 - exp(-2 * (ll_full - ll_null) / n_obs)
  nagelkerke_r2 <- cox_snell / (1 - exp(2 * ll_null / n_obs))

  sm_coef <- summary(fit)$coefficients
  coef_df <- data.frame(
    term = rownames(sm_coef),
    estimate = as.numeric(sm_coef[, "Estimate"]),
    std_error = as.numeric(sm_coef[, "Std. Error"]),
    odds_ratio = exp(as.numeric(sm_coef[, "Estimate"])),
    z_value = as.numeric(sm_coef[, "z value"]),
    p_value = as.numeric(sm_coef[, "Pr(>|z|)"]),
    stringsAsFactors = FALSE
  )

  out <- list(
    response = response,
    group = group,
    method = "Ordinal Logistic Regression (Cumulative Link Model)",
    n_obs = n_obs,
    lrt = as.data.frame(lrt),
    nagelkerke_r2 = as.numeric(nagelkerke_r2),
    coefficients = coef_df,
    model = fit
  )
  class(out) <- "statarbor_ordinal"
  out
}

#' Multi-Factor Interaction Generalized Linear Model
#'
#' Fits a full-factorial interaction model (`response ~ f1 * f2 * ...`) using
#' either a Gaussian (identity link) or Gamma (log link) generalized linear
#' model, returning an F-based deviance ANOVA table and model summary.
#'
#' @param data A data frame.
#' @param response Character scalar naming the numeric response column.
#' @param factors Character vector of length >= 2 naming the factorial
#'   predictor columns.
#' @param family Model family: `"gaussian"` (default) or `"gamma_log"` (for
#'   strictly positive right-skewed continuous measurements such as fresh
#'   weight).
#'
#' @return An object of S3 class `"statarbor_interaction"`.
#' @export
#'
#' @examples
#' analyze_interaction(ToothGrowth, response = "len",
#'                     factors = c("supp", "dose"), family = "gamma_log")
analyze_interaction <- function(data,
                                response,
                                factors,
                                family = c("gaussian", "gamma_log")) {
  family <- match.arg(family)

  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (length(factors) < 2L) {
    stop("`factors` must include at least 2 column names.", call. = FALSE)
  }
  if (!all(c(response, factors) %in% names(data))) {
    stop("All specified `response` and `factors` columns must exist in `data`.",
         call. = FALSE)
  }

  sub_df <- data[, c(response, factors), drop = FALSE]
  sub_df[[response]] <- suppressWarnings(as.numeric(as.character(sub_df[[response]])))
  for (f in factors) {
    sub_df[[f]] <- as.factor(as.character(sub_df[[f]]))
  }
  sub_df <- stats::na.omit(sub_df)

  if (nrow(sub_df) < 6L) {
    stop("Insufficient complete observations for interaction modeling.", call. = FALSE)
  }

  if (family == "gamma_log") {
    if (any(sub_df[[response]] <= 0)) {
      stop("`family = 'gamma_log'` requires strictly positive response values (> 0).",
           call. = FALSE)
    }
    fam_obj <- stats::Gamma(link = "log")
    fam_label <- "Gamma GLM (log link)"
  } else {
    fam_obj <- stats::gaussian(link = "identity")
    fam_label <- "Gaussian GLM (identity link)"
  }

  rhs <- paste(sprintf("`%s`", factors), collapse = " * ")
  fml <- stats::as.formula(sprintf("`%s` ~ %s", response, rhs))

  fit <- stats::glm(fml, data = sub_df, family = fam_obj)
  anv <- as.data.frame(stats::anova(fit, test = "F"))
  anv$term <- rownames(anv)
  rownames(anv) <- NULL
  anv <- anv[, c("term", setdiff(names(anv), "term")), drop = FALSE]

  out <- list(
    response = response,
    factors = factors,
    method = paste("Factorial Interaction", fam_label),
    n_obs = nrow(sub_df),
    aic = stats::AIC(fit),
    anova_table = anv,
    model = fit
  )
  class(out) <- "statarbor_interaction"
  out
}

#' @export
print.statarbor_two_group <- function(x, ...) {
  cat("-- statarbor Two-Group Comparison ------------------------\n")
  cat(sprintf("Response : %s  |  Group: %s\n", x$response, x$group))
  cat(sprintf("Method   : %s\n", x$method))
  p_fmt <- if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)
  cat(sprintf("Test Stat: %s = %.3f, p = %s\n", x$statistic_name, x$statistic, p_fmt))
  eff_txt <- paste(sprintf("%s = %.3f", names(x$effect_size), unlist(x$effect_size)),
                   collapse = ", ")
  cat(sprintf("Effect   : %s\n", eff_txt))
  cat("----------------------------------------------------------\n")
  print(x$descriptives, row.names = FALSE)
  invisible(x)
}

#' @export
print.statarbor_binary <- function(x, ...) {
  cat("-- statarbor Binary Outcome Analysis ---------------------\n")
  cat(sprintf("Response : %s (event = '%s')  |  Group: %s\n",
              x$response, x$event_label, x$group))
  cat(sprintf("Method   : %s\n", x$method))
  p_fmt <- if (x$chisq$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$chisq$p_value)
  cat(sprintf("Chi-sq   : X2(df=%d) = %.3f, p = %s, Cramer's V = %.3f\n",
              x$chisq$df, x$chisq$statistic, p_fmt, x$chisq$cramers_v))
  cat(sprintf("Complete Separation: %s\n", ifelse(x$complete_separation, "Yes", "No")))
  cat("----------------------------------------------------------\n")
  print(x$group_rates, row.names = FALSE)
  invisible(x)
}

#' @export
print.statarbor_ordinal <- function(x, ...) {
  cat("-- statarbor Ordinal Logistic Regression -----------------\n")
  cat(sprintf("Response : %s  |  Group: %s  |  n = %d\n",
              x$response, x$group, x$n_obs))
  cat(sprintf("Nagelkerke pseudo-R2: %.3f\n", x$nagelkerke_r2))
  cat("----------------------------------------------------------\n")
  print(x$lrt)
  invisible(x)
}

#' @export
print.statarbor_interaction <- function(x, ...) {
  cat("-- statarbor Factorial Interaction GLM -------------------\n")
  cat(sprintf("Response : %s  |  Factors: %s\n",
              x$response, paste(x$factors, collapse = " * ")))
  cat(sprintf("Method   : %s  |  n = %d  |  AIC = %.2f\n",
              x$method, x$n_obs, x$aic))
  cat("----------------------------------------------------------\n")
  print(x$anova_table, row.names = FALSE)
  invisible(x)
}
