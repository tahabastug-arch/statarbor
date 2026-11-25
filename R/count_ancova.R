#' Assumption-Guided Count Data Regression (Poisson / Negative Binomial)
#'
#' Fits a log-linear Poisson GLM, evaluates Pearson overdispersion, and
#' automatically routes overdispersed count data to a Negative Binomial GLM
#' (`MASS::glm.nb`, with a Quasi-Poisson fallback if `MASS` is unavailable or
#' fails to converge). Produces Incidence Rate Ratios (IRR), likelihood-ratio
#' omnibus inference, Hauck-Donner-safe pairwise comparisons for zero-count
#' groups, and special-character-safe compact letter displays (CLD).
#'
#' @param data A data.frame.
#' @param response Character. Name of the non-negative integer count column.
#' @param group Character. Name of the grouping factor column (k >= 2).
#' @param offset Optional character. Name of a strictly positive numeric exposure
#'   column included as `offset(log(offset))`.
#' @param alpha Numeric. Significance threshold (default `0.05`).
#' @param disp_threshold Numeric. Pearson dispersion cutoff above which
#'   overdispersion triggers routing when `p < alpha` (default `1.25`).
#' @param p_adjust Character. P-value adjustment method for pairwise comparisons
#'   (default `"holm"`).
#' @param force_family Character. One of `"auto"`, `"poisson"`, `"negbin"`, or
#'   `"quasipoisson"`.
#'
#' @return An object of class `"statarbor_count"`.
#' @export
analyze_count <- function(data, response, group,
                          offset = NULL,
                          alpha = 0.05,
                          disp_threshold = 1.25,
                          p_adjust = "holm",
                          force_family = c("auto", "poisson", "negbin", "quasipoisson")) {
  force_family <- match.arg(force_family)
  stopifnot(is.data.frame(data))
  for (col in c(response, group, offset)) {
    if (!is.null(col) && !col %in% names(data)) {
      stop("Column '", col, "' not found in data.", call. = FALSE)
    }
  }

  cols <- c(group, response, offset)
  df <- data[, cols, drop = FALSE]
  if (is.null(offset)) {
    names(df) <- c(".grp", ".y")
    df$.off <- 1
  } else {
    names(df) <- c(".grp", ".y", ".off")
    df$.off <- as.numeric(df$.off)
  }
  df$.y <- as.numeric(df$.y)
  df <- df[!is.na(df$.y) & !is.na(df$.grp) & !is.na(df$.off), , drop = FALSE]
  df$.grp <- factor(df$.grp)

  if (any(df$.y < 0) || any(abs(df$.y - round(df$.y)) > 1e-8)) {
    stop("`response` must contain non-negative integer counts.", call. = FALSE)
  }
  if (!is.null(offset) && any(df$.off <= 0)) {
    stop("`offset` values must be strictly positive.", call. = FALSE)
  }

  lvls <- levels(df$.grp)
  k <- length(lvls)
  if (k < 2L) {
    stop("`group` must have at least 2 levels.", call. = FALSE)
  }

  has_off <- !is.null(offset)
  f_full <- if (has_off) .y ~ .grp + offset(log(.off)) else .y ~ .grp
  f_null <- if (has_off) .y ~ 1 + offset(log(.off)) else .y ~ 1
  f_cell <- if (has_off) .y ~ 0 + .grp + offset(log(.off)) else .y ~ 0 + .grp

  # Step 1: Fit baseline Poisson GLM and assess overdispersion
  fit_pois <- stats::glm(f_full, data = df, family = stats::poisson(link = "log"))
  df_resid_p <- fit_pois$df.residual
  pearson_chi2 <- sum(stats::residuals(fit_pois, type = "pearson")^2)
  dispersion <- if (df_resid_p > 0L) pearson_chi2 / df_resid_p else NA_real_
  disp_p <- if (df_resid_p > 0L) {
    stats::pchisq(pearson_chi2, df = df_resid_p, lower.tail = FALSE)
  } else {
    NA_real_
  }
  overdispersed <- isTRUE(df_resid_p > 0L && dispersion > disp_threshold && disp_p < alpha)

  target_family <- if (force_family == "auto") {
    if (overdispersed) "negbin" else "poisson"
  } else {
    force_family
  }

  # Step 2: Fit selected model
  theta_est <- NA_real_
  used_family <- target_family

  if (target_family == "poisson") {
    fit_full <- fit_pois
    fit_null <- stats::glm(f_null, data = df, family = stats::poisson(link = "log"))
    fit_cell <- stats::glm(f_cell, data = df, family = stats::poisson(link = "log"))
    stat_val <- max(0, fit_null$deviance - fit_full$deviance)
    df_out <- c(df = k - 1L)
    p_val <- stats::pchisq(stat_val, df = k - 1L, lower.tail = FALSE)
    method_name <- sprintf("Poisson GLM + Pairwise Wald/LRT (%s)", p_adjust)
  } else if (target_family == "negbin") {
    fit_nb <- NULL
    if (requireNamespace("MASS", quietly = TRUE)) {
      fit_nb <- suppressWarnings(
        tryCatch(MASS::glm.nb(f_full, data = df), error = function(e) NULL)
      )
    }
    if (!is.null(fit_nb) && is.finite(fit_nb$theta) && fit_nb$theta > 0) {
      theta_est <- fit_nb$theta
      nb_fam <- MASS::negative.binomial(theta = theta_est)
      fit_full <- fit_nb
      fit_null <- stats::glm(f_null, data = df, family = nb_fam)
      fit_cell <- stats::glm(f_cell, data = df, family = nb_fam)
      stat_val <- max(0, fit_null$deviance - fit_full$deviance)
      df_out <- c(df = k - 1L)
      p_val <- stats::pchisq(stat_val, df = k - 1L, lower.tail = FALSE)
      method_name <- sprintf("Negative Binomial GLM + Pairwise Wald/LRT (%s)", p_adjust)
    } else {
      used_family <- "quasipoisson"
    }
  }

  if (used_family == "quasipoisson") {
    fit_full <- stats::glm(f_full, data = df, family = stats::quasipoisson(link = "log"))
    fit_null <- stats::glm(f_null, data = df, family = stats::quasipoisson(link = "log"))
    fit_cell <- stats::glm(f_cell, data = df, family = stats::quasipoisson(link = "log"))
    phi_q <- summary(fit_full)$dispersion
    stat_val <- max(0, ((fit_null$deviance - fit_full$deviance) / (k - 1L)) / phi_q)
    df_out <- c(df1 = k - 1L, df2 = fit_full$df.residual)
    p_val <- stats::pf(stat_val, df1 = k - 1L, df2 = fit_full$df.residual, lower.tail = FALSE)
    method_name <- sprintf("Quasi-Poisson GLM + Pairwise t/F (%s)", p_adjust)
  }

  # Deviance-based McFadden pseudo-R2
  dev_r2 <- if (fit_null$deviance > 0) {
    max(0, 1 - (fit_full$deviance / fit_null$deviance))
  } else {
    0
  }
  eff <- c(deviance_r2 = dev_r2, dispersion = dispersion)
  if (!is.na(theta_est)) {
    eff <- c(eff, theta = theta_est)
  }

  # Group-level estimated rates and CIs
  coef_names <- paste0(".grp", lvls)
  beta_cell <- stats::coef(fit_cell)[coef_names]
  V_cell <- stats::vcov(fit_cell)[coef_names, coef_names, drop = FALSE]
  se_cell <- sqrt(diag(V_cell))
  crit <- if (used_family == "quasipoisson") {
    stats::qt(1 - alpha / 2, df = fit_full$df.residual)
  } else {
    stats::qnorm(1 - alpha / 2)
  }

  grp_sums <- tapply(df$.y, df$.grp, sum)[lvls]
  grp_zero <- (grp_sums == 0)

  # Pairwise comparisons with Hauck-Donner LRT safeguard for zero-count groups
  pw_rows <- list()
  idx <- 1L
  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      g1 <- lvls[i]
      g2 <- lvls[j]
      diff_log <- unname(beta_cell[i] - beta_cell[j])
      se_diff <- sqrt(max(0, V_cell[i, i] + V_cell[j, j] - 2 * V_cell[i, j]))

      if (grp_zero[i] && grp_zero[j]) {
        irr_val <- 1
        ci_lo <- NA_real_
        ci_hi <- NA_real_
        p_raw <- 1
      } else if (grp_zero[i] || grp_zero[j]) {
        # Hauck-Donner safeguard: 2-group likelihood ratio / F test
        sub_df <- df[df$.grp %in% c(g1, g2), , drop = FALSE]
        sub_df$.grp <- droplevels(sub_df$.grp)
        sub_fam <- if (used_family == "poisson") {
          stats::poisson(link = "log")
        } else if (used_family == "negbin") {
          MASS::negative.binomial(theta = theta_est)
        } else {
          stats::quasipoisson(link = "log")
        }
        s_full <- stats::glm(f_full, data = sub_df, family = sub_fam)
        s_null <- stats::glm(f_null, data = sub_df, family = sub_fam)
        if (used_family == "quasipoisson") {
          phi_s <- max(1e-8, summary(s_full)$dispersion)
          f_s <- max(0, (s_null$deviance - s_full$deviance) / phi_s)
          p_raw <- stats::pf(f_s, 1, s_full$df.residual, lower.tail = FALSE)
        } else {
          lr_s <- max(0, s_null$deviance - s_full$deviance)
          p_raw <- stats::pchisq(lr_s, df = 1, lower.tail = FALSE)
        }
        irr_val <- if (grp_zero[i]) 0 else Inf
        ci_lo <- NA_real_
        ci_hi <- NA_real_
      } else {
        irr_val <- exp(diff_log)
        ci_lo <- exp(diff_log - crit * se_diff)
        ci_hi <- exp(diff_log + crit * se_diff)
        z_stat <- diff_log / se_diff
        p_raw <- if (used_family == "quasipoisson") {
          2 * stats::pt(abs(z_stat), df = fit_full$df.residual, lower.tail = FALSE)
        } else {
          2 * stats::pnorm(abs(z_stat), lower.tail = FALSE)
        }
      }

      pw_rows[[idx]] <- data.frame(
        group1 = g1,
        group2 = g2,
        irr = irr_val,
        ci_low = ci_lo,
        ci_high = ci_hi,
        p_raw = p_raw,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  pairwise_df <- do.call(rbind, pw_rows)
  pairwise_df$p.adj <- stats::p.adjust(pairwise_df$p_raw, method = p_adjust)

  # Special-character-safe CLD
  if (k == 2L) {
    cld_vec <- if (pairwise_df$p.adj[1L] < alpha) c("a", "b") else c("a", "a")
    names(cld_vec) <- lvls
  } else {
    p_mat <- matrix(1, nrow = k, ncol = k, dimnames = list(lvls, lvls))
    for (r in seq_len(nrow(pairwise_df))) {
      g1 <- pairwise_df$group1[r]
      g2 <- pairwise_df$group2[r]
      pv <- pairwise_df$p.adj[r]
      if (is.na(pv)) pv <- 1
      p_mat[g1, g2] <- pv
      p_mat[g2, g1] <- pv
    }
    cld_vec <- multcompView::multcompLetters(p_mat, threshold = alpha)$Letters[lvls]
  }

  rate_est <- ifelse(grp_zero, 0, exp(beta_cell))
  rate_lo <- ifelse(grp_zero, 0, exp(beta_cell - crit * se_cell))
  rate_hi <- ifelse(grp_zero, NA_real_, exp(beta_cell + crit * se_cell))

  desc <- data.frame(
    group = lvls,
    n = as.integer(table(df$.grp)[lvls]),
    mean_count = as.numeric(tapply(df$.y, df$.grp, mean)[lvls]),
    sd_count = as.numeric(tapply(df$.y, df$.grp, stats::sd)[lvls]),
    median_count = as.numeric(tapply(df$.y, df$.grp, stats::median)[lvls]),
    est_rate = unname(rate_est),
    ci_low = unname(rate_lo),
    ci_high = unname(rate_hi),
    cld = unname(cld_vec),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  structure(
    list(
      response = response,
      group = group,
      offset = offset,
      family = used_family,
      assumptions = list(
        pearson_chi2 = pearson_chi2,
        df_resid = df_resid_p,
        dispersion = dispersion,
        dispersion_p = disp_p,
        overdispersed = overdispersed,
        any_zero_group = any(grp_zero)
      ),
      method = method_name,
      statistic = stat_val,
      df = df_out,
      p_value = p_val,
      effect_size = eff,
      descriptives = desc,
      pairwise = pairwise_df,
      cld = cld_vec,
      model = fit_full,
      alpha = alpha
    ),
    class = "statarbor_count"
  )
}

#' @export
print.statarbor_count <- function(x, ...) {
  cat("statarbor Count Data Analysis\n")
  cat("-----------------------------\n")
  cat(sprintf("Response : %s | Group: %s | Family: %s\n",
              x$response, x$group, x$family))
  cat(sprintf("Method   : %s\n", x$method))
  if (length(x$df) == 2L) {
    cat(sprintf("Result   : F(%.0f, %.0f) = %.3f, p = %.4g\n",
                x$df[1L], x$df[2L], x$statistic, x$p_value))
  } else {
    cat(sprintf("Result   : LR Chi2(%.0f) = %.3f, p = %.4g\n",
                x$df[1L], x$statistic, x$p_value))
  }
  eff_str <- paste(sprintf("%s = %.3f", names(x$effect_size), x$effect_size), collapse = ", ")
  cat(sprintf("Effect   : %s\n\n", eff_str))
  print(x$descriptives[, c("group", "n", "mean_count", "est_rate", "cld")], row.names = FALSE)
  invisible(x)
}

#' Assumption-Guided Analysis of Covariance (ANCOVA)
#'
#' Evaluates homogeneity of regression slopes (`group:covariate` interaction),
#' residual normality (Shapiro-Wilk), and residual homoscedasticity (Levene's
#' test). Computes Type II sums of squares, partial eta-squared effect sizes,
#' covariate-adjusted group means (evaluated at the grand mean of the
#' covariate), exact adjusted pairwise t-tests, and special-character-safe CLD.
#'
#' @param data A data.frame.
#' @param response Character. Name of the numeric response column.
#' @param group Character. Name of the grouping factor column (k >= 2).
#' @param covariate Character. Name of the continuous numeric covariate column.
#' @param alpha Numeric. Significance threshold (default `0.05`).
#' @param p_adjust Character. P-value adjustment method for adjusted pairwise
#'   comparisons (default `"holm"`).
#' @param check_homogeneity Logical. If `TRUE` (default) and slopes are
#'   significantly heterogeneous (`p < alpha`), routes to an interaction ANCOVA
#'   evaluating group means at the covariate grand mean with a diagnostic flag.
#'
#' @return An object of class `"statarbor_ancova"`.
#' @export
analyze_ancova <- function(data, response, group, covariate,
                           alpha = 0.05,
                           p_adjust = "holm",
                           check_homogeneity = TRUE) {
  stopifnot(is.data.frame(data))
  for (col in c(response, group, covariate)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in data.", call. = FALSE)
    }
  }

  df <- data[, c(group, response, covariate), drop = FALSE]
  names(df) <- c(".grp", ".y", ".cov")
  df$.y <- as.numeric(df$.y)
  df$.cov <- as.numeric(df$.cov)
  df <- df[!is.na(df$.y) & !is.na(df$.grp) & !is.na(df$.cov), , drop = FALSE]
  df$.grp <- factor(df$.grp)

  lvls <- levels(df$.grp)
  k <- length(lvls)
  if (k < 2L) {
    stop("`group` must have at least 2 levels.", call. = FALSE)
  }
  if (stats::sd(df$.cov) == 0) {
    stop("`covariate` has zero variance.", call. = FALSE)
  }

  cov_mean <- mean(df$.cov)
  df$.cov_c <- df$.cov - cov_mean

  # Step 1: Homogeneity of regression slopes test
  fit_add <- stats::lm(.y ~ .grp + .cov_c, data = df)
  fit_int <- stats::lm(.y ~ .grp * .cov_c, data = df)
  slope_aov <- stats::anova(fit_add, fit_int)
  slopes_f <- slope_aov$F[2L]
  slopes_p <- slope_aov[["Pr(>F)"]][2L]
  slopes_homogeneous <- isTRUE(is.na(slopes_p) || slopes_p >= alpha)

  use_interaction <- isTRUE(check_homogeneity && !slopes_homogeneous)

  # Step 2: Type II omnibus inference and adjusted means model
  if (!use_interaction) {
    fit_primary <- fit_add
    fit_no_grp <- stats::lm(.y ~ .cov_c, data = df)
    fit_no_cov <- stats::lm(.y ~ .grp, data = df)
    fit_means <- stats::lm(.y ~ 0 + .grp + .cov_c, data = df)
    method_name <- sprintf("ANCOVA (Type II SS) + Adjusted Pairwise t (%s)", p_adjust)
  } else {
    fit_primary <- fit_int
    fit_no_grp <- stats::lm(.y ~ 1 + .grp:.cov_c, data = df)
    fit_no_cov <- stats::lm(.y ~ .grp, data = df)
    fit_means <- stats::lm(.y ~ 0 + .grp + .grp:.cov_c, data = df)
    method_name <- sprintf("Interaction ANCOVA (at mean covariate) + Pairwise t (%s)", p_adjust)
  }

  aov_grp <- stats::anova(fit_no_grp, fit_primary)
  ss_grp <- aov_grp[["Sum of Sq"]][2L]
  df_grp <- aov_grp$Df[2L]
  f_grp <- aov_grp$F[2L]
  p_grp <- aov_grp[["Pr(>F)"]][2L]
  ss_err <- aov_grp$RSS[2L]
  df_err <- aov_grp$Res.Df[2L]

  aov_cov <- stats::anova(fit_no_cov, fit_primary)
  ss_cov <- aov_cov[["Sum of Sq"]][2L]
  df_cov <- aov_cov$Df[2L]
  f_cov <- aov_cov$F[2L]
  p_cov <- aov_cov[["Pr(>F)"]][2L]

  eta_p2_grp <- ss_grp / (ss_grp + ss_err)
  eta_p2_cov <- ss_cov / (ss_cov + ss_err)

  # Step 3: Residual diagnostics
  resids <- stats::residuals(fit_primary)
  shapiro_p <- if (length(resids) < 3L || length(resids) > 5000L || stats::sd(resids) == 0) {
    NA_real_
  } else {
    stats::shapiro.test(resids)$p.value
  }
  lev_df <- data.frame(r = resids, g = df$.grp)
  levene_p <- tryCatch(
    car::leveneTest(r ~ g, data = lev_df)[1L, "Pr(>F)"],
    error = function(e) NA_real_
  )

  # Step 4: Adjusted means at mean(covariate) and exact pairwise contrasts
  coef_names <- paste0(".grp", lvls)
  adj_means <- stats::coef(fit_means)[coef_names]
  V <- stats::vcov(fit_means)[coef_names, coef_names, drop = FALSE]
  adj_se <- sqrt(diag(V))
  t_crit <- stats::qt(1 - alpha / 2, df = df_err)

  pw_rows <- list()
  idx <- 1L
  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      g1 <- lvls[i]
      g2 <- lvls[j]
      diff_val <- unname(adj_means[i] - adj_means[j])
      se_val <- sqrt(max(0, V[i, i] + V[j, j] - 2 * V[i, j]))
      t_val <- diff_val / se_val
      p_raw <- 2 * stats::pt(abs(t_val), df = df_err, lower.tail = FALSE)
      pw_rows[[idx]] <- data.frame(
        group1 = g1,
        group2 = g2,
        diff_adj = diff_val,
        se = se_val,
        t_stat = t_val,
        ci_low = diff_val - t_crit * se_val,
        ci_high = diff_val + t_crit * se_val,
        p_raw = p_raw,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  pairwise_df <- do.call(rbind, pw_rows)
  pairwise_df$p.adj <- stats::p.adjust(pairwise_df$p_raw, method = p_adjust)

  # Special-character-safe CLD
  if (k == 2L) {
    cld_vec <- if (pairwise_df$p.adj[1L] < alpha) c("a", "b") else c("a", "a")
    names(cld_vec) <- lvls
  } else {
    p_mat <- matrix(1, nrow = k, ncol = k, dimnames = list(lvls, lvls))
    for (r in seq_len(nrow(pairwise_df))) {
      g1 <- pairwise_df$group1[r]
      g2 <- pairwise_df$group2[r]
      pv <- pairwise_df$p.adj[r]
      if (is.na(pv)) pv <- 1
      p_mat[g1, g2] <- pv
      p_mat[g2, g1] <- pv
    }
    cld_vec <- multcompView::multcompLetters(p_mat, threshold = alpha)$Letters[lvls]
  }

  desc <- data.frame(
    group = lvls,
    n = as.integer(table(df$.grp)[lvls]),
    raw_mean = as.numeric(tapply(df$.y, df$.grp, mean)[lvls]),
    adj_mean = unname(adj_means),
    adj_se = unname(adj_se),
    ci_low = unname(adj_means - t_crit * adj_se),
    ci_high = unname(adj_means + t_crit * adj_se),
    cld = unname(cld_vec),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  structure(
    list(
      response = response,
      group = group,
      covariate = covariate,
      covariate_mean = cov_mean,
      assumptions = list(
        slopes_f = slopes_f,
        slopes_p = slopes_p,
        slopes_homogeneous = slopes_homogeneous,
        shapiro_resid_p = shapiro_p,
        levene_resid_p = levene_p
      ),
      method = method_name,
      group_test = list(statistic = f_grp, df1 = df_grp, df2 = df_err, p_value = p_grp),
      covariate_test = list(statistic = f_cov, df1 = df_cov, df2 = df_err, p_value = p_cov),
      effect_size = c(eta_p2_group = eta_p2_grp, eta_p2_covariate = eta_p2_cov),
      descriptives = desc,
      pairwise = pairwise_df,
      cld = cld_vec,
      model = fit_primary,
      alpha = alpha
    ),
    class = "statarbor_ancova"
  )
}

#' @export
print.statarbor_ancova <- function(x, ...) {
  cat("statarbor ANCOVA Analysis\n")
  cat("-------------------------\n")
  cat(sprintf("Response : %s | Group: %s | Covariate: %s (mean = %.3f)\n",
              x$response, x$group, x$covariate, x$covariate_mean))
  cat(sprintf("Method   : %s\n", x$method))
  cat(sprintf("Slopes   : F = %.3f, p = %.4g (%s)\n",
              x$assumptions$slopes_f, x$assumptions$slopes_p,
              if (x$assumptions$slopes_homogeneous) "homogeneous" else "HETEROGENEOUS"))
  cat(sprintf("Group    : F(%.0f, %.0f) = %.3f, p = %.4g, partial eta2 = %.3f\n",
              x$group_test$df1, x$group_test$df2, x$group_test$statistic,
              x$group_test$p_value, x$effect_size["eta_p2_group"]))
  cat(sprintf("Covariate: F(%.0f, %.0f) = %.3f, p = %.4g, partial eta2 = %.3f\n\n",
              x$covariate_test$df1, x$covariate_test$df2, x$covariate_test$statistic,
              x$covariate_test$p_value, x$effect_size["eta_p2_covariate"]))
  print(x$descriptives[, c("group", "n", "raw_mean", "adj_mean", "adj_se", "cld")],
        row.names = FALSE)
  invisible(x)
}
