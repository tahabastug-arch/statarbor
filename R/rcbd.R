#' Assumption-Guided Randomized Complete Block Design (RCBD) Analysis
#'
#' Analyzes Randomized Complete Block Designs (RCBD) with fixed or optional
#' random (`lme4`) block effects. Evaluates residual normality (Shapiro-Wilk)
#' and homoscedasticity (Levene's test), computes Type II sums of squares,
#' partial eta-squared, Relative Efficiency (RE) of blocking, exact marginal
#' treatment means, Tukey-Kramer or Holm pairwise comparisons, and
#' special-character-safe compact letter displays (CLD). On balanced single-rep
#' designs with violated normality, `force_method = "auto"` routes to the
#' non-parametric Friedman test.
#'
#' @param data A data.frame.
#' @param response Character. Name of the numeric response column.
#' @param treatment Character. Name of the treatment factor column (k >= 2).
#' @param block Character. Name of the blocking factor column (b >= 2).
#' @param block_type Character. Either `"fixed"` (default, base R) or `"random"`
#'   (linear mixed model via `lme4::lmer`, if installed).
#' @param alpha Numeric. Significance threshold (default `0.05`).
#' @param p_adjust Character. Pairwise p-value adjustment: `"tukey"` (default,
#'   exact Studentized range / Tukey-Kramer) or any method supported by
#'   `stats::p.adjust()` (e.g., `"holm"`, `"bonferroni"`).
#' @param force_method Character. One of `"auto"`, `"parametric"`, or
#'   `"nonparametric"`.
#'
#' @return An object of class `"statarbor_rcbd"`.
#' @export
analyze_rcbd <- function(data, response, treatment, block,
                         block_type = c("fixed", "random"),
                         alpha = 0.05,
                         p_adjust = "tukey",
                         force_method = c("auto", "parametric", "nonparametric")) {
  block_type <- match.arg(block_type)
  force_method <- match.arg(force_method)
  stopifnot(is.data.frame(data))
  for (col in c(response, treatment, block)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in data.", call. = FALSE)
    }
  }

  df <- data[, c(treatment, block, response), drop = FALSE]
  names(df) <- c(".trt", ".blk", ".y")
  df$.y <- as.numeric(df$.y)
  df <- df[!is.na(df$.y) & !is.na(df$.trt) & !is.na(df$.blk), , drop = FALSE]
  df$.trt <- factor(df$.trt)
  df$.blk <- factor(df$.blk)

  lvls <- levels(df$.trt)
  blks <- levels(df$.blk)
  k <- length(lvls)
  b <- length(blks)
  if (k < 2L) stop("`treatment` must have at least 2 levels.", call. = FALSE)
  if (b < 2L) stop("`block` must have at least 2 levels.", call. = FALSE)

  cell_tab <- table(df$.trt, df$.blk)
  is_balanced_single <- all(cell_tab == 1L)

  # Baseline additive fixed model for diagnostics and RE
  fit_add <- stats::lm(.y ~ .trt + .blk, data = df)
  resids <- stats::residuals(fit_add)
  trt_sds <- tapply(df$.y, df$.trt, stats::sd)
  zero_var <- any(is.na(trt_sds) | trt_sds == 0)

  shapiro_p <- if (length(resids) < 3L || length(resids) > 5000L || stats::sd(resids) == 0) {
    NA_real_
  } else {
    stats::shapiro.test(resids)$p.value
  }
  norm_violated <- isTRUE(!is.na(shapiro_p) && shapiro_p < alpha)

  lev_df <- data.frame(r = resids, g = df$.trt)
  levene_p <- tryCatch(
    car::leveneTest(r ~ g, data = lev_df)[1L, "Pr(>F)"],
    error = function(e) NA_real_
  )
  var_violated <- isTRUE(!is.na(levene_p) && levene_p < alpha)

  # Route decision
  can_friedman <- is_balanced_single && (k >= 3L)
  use_nonparam <- switch(
    force_method,
    nonparametric = if (can_friedman) TRUE else {
      stop("Non-parametric Friedman RCBD requires a complete balanced design (1 obs/cell, k >= 3).",
           call. = FALSE)
    },
    parametric = FALSE,
    auto = (block_type == "fixed" && can_friedman && (zero_var || norm_violated))
  )

  # --- Branch 1: Non-parametric Friedman RCBD ---
  if (use_nonparam) {
    wide <- stats::reshape(df, idvar = ".blk", timevar = ".trt", direction = "wide")
    val_cols <- paste0(".y.", lvls)
    mat <- as.matrix(wide[, val_cols, drop = FALSE])
    colnames(mat) <- lvls

    fr <- stats::friedman.test(mat)
    chi2_val <- unname(fr$statistic)
    df_fr <- unname(fr$parameter)
    p_fr <- fr$p.value
    kendall_w <- chi2_val / (b * (k - 1))

    adj_np <- if (tolower(p_adjust) == "tukey") "holm" else p_adjust
    pw_raw <- suppressWarnings(
      stats::pairwise.wilcox.test(df$.y, df$.trt, paired = TRUE,
                                  p.adjust.method = adj_np, exact = FALSE)$p.value
    )
    p_mat <- matrix(1, nrow = k, ncol = k, dimnames = list(lvls, lvls))
    pw_rows <- list()
    idx <- 1L
    for (i in seq_len(k - 1L)) {
      for (j in (i + 1L):k) {
        g1 <- lvls[i]
        g2 <- lvls[j]
        pv <- pw_raw[g2, g1]
        if (is.na(pv)) pv <- 1
        p_mat[g1, g2] <- pv
        p_mat[g2, g1] <- pv
        pw_rows[[idx]] <- data.frame(
          group1 = g1, group2 = g2, p.adj = pv, stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
    pairwise_df <- do.call(rbind, pw_rows)
    cld_vec <- multcompView::multcompLetters(p_mat, threshold = alpha)$Letters[lvls]

    desc <- data.frame(
      treatment = lvls,
      n = as.integer(table(df$.trt)[lvls]),
      mean = as.numeric(tapply(df$.y, df$.trt, mean)[lvls]),
      sd = as.numeric(tapply(df$.y, df$.trt, stats::sd)[lvls]),
      median = as.numeric(tapply(df$.y, df$.trt, stats::median)[lvls]),
      iqr = as.numeric(tapply(df$.y, df$.trt, stats::IQR)[lvls]),
      cld = unname(cld_vec),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    return(structure(
      list(
        response = response,
        treatment = treatment,
        block = block,
        block_type = "fixed",
        n_blocks = b,
        k_treatments = k,
        balanced_single = is_balanced_single,
        assumptions = list(
          zero_var = zero_var,
          shapiro_resid_p = shapiro_p,
          norm_violated = norm_violated,
          levene_resid_p = levene_p,
          var_violated = var_violated
        ),
        method = sprintf("Friedman RCBD + Paired Wilcoxon (%s)", adj_np),
        treatment_test = list(statistic = chi2_val, df1 = df_fr, df2 = NA_real_, p_value = p_fr),
        block_test = NULL,
        relative_efficiency = NA_real_,
        icc = NA_real_,
        effect_size = c(kendalls_w = kendall_w),
        descriptives = desc,
        pairwise = pairwise_df,
        cld = cld_vec,
        model = fr,
        alpha = alpha
      ),
      class = "statarbor_rcbd"
    ))
  }

  # --- Branch 2: Random-block Linear Mixed Model (lme4) ---
  if (block_type == "random") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' is required for `block_type = 'random'`. ",
           "Install 'lme4' or use `block_type = 'fixed'`.", call. = FALSE)
    }
    fit_reml <- lme4::lmer(.y ~ 0 + .trt + (1 | .blk), data = df, REML = TRUE)
    fit_ml1 <- lme4::lmer(.y ~ .trt + (1 | .blk), data = df, REML = FALSE)
    fit_ml0 <- lme4::lmer(.y ~ 1 + (1 | .blk), data = df, REML = FALSE)
    lrt <- stats::anova(fit_ml0, fit_ml1)
    chi2_val <- lrt$Chisq[2L]
    df_trt <- lrt$Df[2L]
    p_trt <- lrt[["Pr(>Chisq)"]][2L]

    vc <- as.data.frame(lme4::VarCorr(fit_reml))
    var_blk <- vc$vcov[vc$grp == ".blk"][1L]
    var_res <- vc$vcov[vc$grp == "Residual"][1L]
    icc_val <- if ((var_blk + var_res) > 0) var_blk / (var_blk + var_res) else 0

    coef_names <- paste0(".trt", lvls)
    emms <- lme4::fixef(fit_reml)[coef_names]
    V <- as.matrix(stats::vcov(fit_reml))[coef_names, coef_names, drop = FALSE]
    emm_se <- sqrt(diag(V))
    z_crit <- stats::qnorm(1 - alpha / 2)

    adj_mm <- if (tolower(p_adjust) == "tukey") "holm" else p_adjust
    pw_rows <- list()
    idx <- 1L
    for (i in seq_len(k - 1L)) {
      for (j in (i + 1L):k) {
        g1 <- lvls[i]
        g2 <- lvls[j]
        diff_val <- unname(emms[i] - emms[j])
        se_val <- sqrt(max(0, V[i, i] + V[j, j] - 2 * V[i, j]))
        z_val <- diff_val / se_val
        p_raw <- 2 * stats::pnorm(abs(z_val), lower.tail = FALSE)
        pw_rows[[idx]] <- data.frame(
          group1 = g1, group2 = g2, diff = diff_val, se = se_val,
          z_stat = z_val, p_raw = p_raw, stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
    pairwise_df <- do.call(rbind, pw_rows)
    pairwise_df$p.adj <- stats::p.adjust(pairwise_df$p_raw, method = adj_mm)

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
      treatment = lvls,
      n = as.integer(table(df$.trt)[lvls]),
      raw_mean = as.numeric(tapply(df$.y, df$.trt, mean)[lvls]),
      emm = unname(emms),
      se = unname(emm_se),
      ci_low = unname(emms - z_crit * emm_se),
      ci_high = unname(emms + z_crit * emm_se),
      cld = unname(cld_vec),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    return(structure(
      list(
        response = response,
        treatment = treatment,
        block = block,
        block_type = "random",
        n_blocks = b,
        k_treatments = k,
        balanced_single = is_balanced_single,
        assumptions = list(
          zero_var = zero_var,
          shapiro_resid_p = shapiro_p,
          norm_violated = norm_violated,
          levene_resid_p = levene_p,
          var_violated = var_violated
        ),
        method = sprintf("Mixed RCBD (REML/LRT) + Pairwise Wald (%s)", adj_mm),
        treatment_test = list(statistic = chi2_val, df1 = df_trt, df2 = NA_real_, p_value = p_trt),
        block_test = list(var_block = var_blk, var_resid = var_res, icc = icc_val),
        relative_efficiency = NA_real_,
        icc = icc_val,
        effect_size = c(icc = icc_val),
        descriptives = desc,
        pairwise = pairwise_df,
        cld = cld_vec,
        model = fit_reml,
        alpha = alpha
      ),
      class = "statarbor_rcbd"
    ))
  }

  # --- Branch 3: Fixed-block RCBD ANOVA (Type II SS + exact EMMs) ---
  fit_no_trt <- stats::lm(.y ~ .blk, data = df)
  fit_no_blk <- stats::lm(.y ~ .trt, data = df)

  aov_trt <- stats::anova(fit_no_trt, fit_add)
  ss_trt <- aov_trt[["Sum of Sq"]][2L]
  df_trt <- aov_trt$Df[2L]
  f_trt <- aov_trt$F[2L]
  p_trt <- aov_trt[["Pr(>F)"]][2L]
  ss_err <- aov_trt$RSS[2L]
  df_err <- aov_trt$Res.Df[2L]

  aov_blk <- stats::anova(fit_no_blk, fit_add)
  ss_blk <- aov_blk[["Sum of Sq"]][2L]
  df_blk <- aov_blk$Df[2L]
  f_blk <- aov_blk$F[2L]
  p_blk <- aov_blk[["Pr(>F)"]][2L]

  ms_blk <- ss_blk / df_blk
  ms_err <- ss_err / df_err

  # Cochran & Cox Relative Efficiency of Blocking
  re_val <- if (ms_err > 0) {
    if (is_balanced_single) {
      ((b - 1) * ms_blk + b * (k - 1) * ms_err) / ((b * k - 1) * ms_err)
    } else {
      (df_blk * ms_blk + (nrow(df) - k - df_blk) * ms_err) / ((nrow(df) - k) * ms_err)
    }
  } else {
    NA_real_
  }

  eta_p2_trt <- ss_trt / (ss_trt + ss_err)
  eta_p2_blk <- ss_blk / (ss_blk + ss_err)

  # Exact EMMs averaged equally over blocks using sum-to-zero block contrasts
  fit_emm <- stats::lm(.y ~ 0 + .trt + .blk, data = df,
                       contrasts = list(.blk = stats::contr.sum))
  coef_names <- paste0(".trt", lvls)
  emms <- stats::coef(fit_emm)[coef_names]
  V <- stats::vcov(fit_emm)[coef_names, coef_names, drop = FALSE]
  emm_se <- sqrt(diag(V))
  t_crit <- stats::qt(1 - alpha / 2, df = df_err)

  use_tukey <- (tolower(p_adjust) == "tukey")
  pw_rows <- list()
  idx <- 1L
  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      g1 <- lvls[i]
      g2 <- lvls[j]
      diff_val <- unname(emms[i] - emms[j])
      se_val <- sqrt(max(0, V[i, i] + V[j, j] - 2 * V[i, j]))
      t_val <- diff_val / se_val
      p_raw <- 2 * stats::pt(abs(t_val), df = df_err, lower.tail = FALSE)
      p_tuk <- stats::ptukey(abs(t_val) * sqrt(2), nmeans = k, df = df_err, lower.tail = FALSE)
      pw_rows[[idx]] <- data.frame(
        group1 = g1,
        group2 = g2,
        diff = diff_val,
        se = se_val,
        t_stat = t_val,
        p_raw = p_raw,
        p_tukey = p_tuk,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  pairwise_df <- do.call(rbind, pw_rows)
  pairwise_df$p.adj <- if (use_tukey) {
    pairwise_df$p_tukey
  } else {
    stats::p.adjust(pairwise_df$p_raw, method = p_adjust)
  }
  pairwise_df$p_tukey <- NULL

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
    treatment = lvls,
    n = as.integer(table(df$.trt)[lvls]),
    raw_mean = as.numeric(tapply(df$.y, df$.trt, mean)[lvls]),
    emm = unname(emms),
    se = unname(emm_se),
    ci_low = unname(emms - t_crit * emm_se),
    ci_high = unname(emms + t_crit * emm_se),
    cld = unname(cld_vec),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  method_lbl <- if (use_tukey) {
    "RCBD ANOVA (Type II SS) + Tukey-Kramer HSD"
  } else {
    sprintf("RCBD ANOVA (Type II SS) + Pairwise t (%s)", p_adjust)
  }

  structure(
    list(
      response = response,
      treatment = treatment,
      block = block,
      block_type = "fixed",
      n_blocks = b,
      k_treatments = k,
      balanced_single = is_balanced_single,
      assumptions = list(
        zero_var = zero_var,
        shapiro_resid_p = shapiro_p,
        norm_violated = norm_violated,
        levene_resid_p = levene_p,
        var_violated = var_violated
      ),
      method = method_lbl,
      treatment_test = list(statistic = f_trt, df1 = df_trt, df2 = df_err, p_value = p_trt),
      block_test = list(statistic = f_blk, df1 = df_blk, df2 = df_err, p_value = p_blk),
      relative_efficiency = re_val,
      icc = NA_real_,
      effect_size = c(eta_p2_treatment = eta_p2_trt, eta_p2_block = eta_p2_blk,
                      relative_efficiency = re_val),
      descriptives = desc,
      pairwise = pairwise_df,
      cld = cld_vec,
      model = fit_add,
      alpha = alpha
    ),
    class = "statarbor_rcbd"
  )
}

#' @export
print.statarbor_rcbd <- function(x, ...) {
  cat("statarbor RCBD Analysis\n")
  cat("-----------------------\n")
  cat(sprintf("Response : %s | Treatment: %s (k = %d) | Block: %s (b = %d, %s)\n",
              x$response, x$treatment, x$k_treatments, x$block, x$n_blocks, x$block_type))
  cat(sprintf("Method   : %s\n", x$method))
  if (!is.na(x$treatment_test$df2)) {
    cat(sprintf("Treatment: F(%.0f, %.0f) = %.3f, p = %.4g, partial eta2 = %.3f\n",
                x$treatment_test$df1, x$treatment_test$df2, x$treatment_test$statistic,
                x$treatment_test$p_value, x$effect_size["eta_p2_treatment"]))
    cat(sprintf("Block    : F(%.0f, %.0f) = %.3f, p = %.4g | Rel. Efficiency = %.2f\n\n",
                x$block_test$df1, x$block_test$df2, x$block_test$statistic,
                x$block_test$p_value, x$relative_efficiency))
    print(x$descriptives[, c("treatment", "n", "emm", "se", "cld")], row.names = FALSE)
  } else if (x$block_type == "random") {
    cat(sprintf("Treatment: LR Chi2(%.0f) = %.3f, p = %.4g | Block ICC = %.3f\n\n",
                x$treatment_test$df1, x$treatment_test$statistic,
                x$treatment_test$p_value, x$icc))
    print(x$descriptives[, c("treatment", "n", "emm", "se", "cld")], row.names = FALSE)
  } else {
    cat(sprintf("Treatment: Chi2(%.0f) = %.3f, p = %.4g, Kendall's W = %.3f\n\n",
                x$treatment_test$df1, x$treatment_test$statistic,
                x$treatment_test$p_value, x$effect_size["kendalls_w"]))
    print(x$descriptives[, c("treatment", "n", "median", "iqr", "cld")], row.names = FALSE)
  }
  invisible(x)
}
