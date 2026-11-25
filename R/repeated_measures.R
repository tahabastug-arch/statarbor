#' Assumption-Guided Paired Two-Condition Comparison
#'
#' Automatically evaluates the normality and variance of within-subject
#' differences to route between a paired Student's t-test (with Cohen's dz)
#' and a paired Wilcoxon signed-rank test (with rank-biserial r).
#'
#' @param data A data.frame in long format.
#' @param response Character. Name of the numeric response column.
#' @param condition Character. Name of the 2-level within-subject factor column.
#' @param id Character. Name of the subject/replicate identifier column.
#' @param alpha Numeric. Significance threshold (default `0.05`).
#' @param force_method Character. One of `"auto"`, `"parametric"`, or
#'   `"nonparametric"`.
#'
#' @return An object of class `"statarbor_paired"`.
#' @export
compare_paired <- function(data, response, condition, id,
                           alpha = 0.05,
                           force_method = c("auto", "parametric", "nonparametric")) {
  force_method <- match.arg(force_method)
  stopifnot(is.data.frame(data))
  for (col in c(response, condition, id)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in data.", call. = FALSE)
    }
  }

  df <- data[, c(id, condition, response), drop = FALSE]
  names(df) <- c(".id", ".cond", ".y")
  df$.y <- as.numeric(df$.y)
  df <- df[!is.na(df$.y) & !is.na(df$.cond) & !is.na(df$.id), , drop = FALSE]
  df$.cond <- factor(df$.cond)

  lvls <- levels(df$.cond)
  if (length(lvls) != 2L) {
    stop("`condition` must have exactly 2 levels; found ", length(lvls), ".", call. = FALSE)
  }

  agg <- stats::aggregate(.y ~ .id + .cond, data = df, FUN = mean)
  wide <- stats::reshape(agg, idvar = ".id", timevar = ".cond", direction = "wide")
  y1_col <- paste0(".y.", lvls[1L])
  y2_col <- paste0(".y.", lvls[2L])

  complete_idx <- stats::complete.cases(wide[, c(y1_col, y2_col), drop = FALSE])
  n_dropped <- sum(!complete_idx)
  if (n_dropped > 0L) {
    warning(n_dropped, " subject(s) with incomplete pairs were excluded.", call. = FALSE)
  }
  wide <- wide[complete_idx, , drop = FALSE]
  n_pairs <- nrow(wide)
  if (n_pairs < 3L) {
    stop("At least 3 complete subject pairs are required.", call. = FALSE)
  }

  y1 <- wide[[y1_col]]
  y2 <- wide[[y2_col]]
  diffs <- y2 - y1

  zero_var_diff <- (stats::sd(diffs) == 0)
  shapiro_p <- if (zero_var_diff || n_pairs > 5000L) NA_real_ else stats::shapiro.test(diffs)$p.value
  norm_violated <- isTRUE(!is.na(shapiro_p) && shapiro_p < alpha)

  use_parametric <- switch(
    force_method,
    parametric = !zero_var_diff,
    nonparametric = FALSE,
    auto = (!zero_var_diff && !norm_violated)
  )

  if (use_parametric) {
    tst <- stats::t.test(y2, y1, paired = TRUE)
    dz <- mean(diffs) / stats::sd(diffs)
    method_name <- "Paired t-test"
    stat_val <- unname(tst$statistic)
    df_val <- unname(tst$parameter)
    p_val <- tst$p.value
    eff <- c(cohens_dz = unname(dz))
  } else {
    tst <- suppressWarnings(stats::wilcox.test(y2, y1, paired = TRUE, exact = FALSE))
    p_val <- tst$p.value
    z_val <- if (is.na(p_val) || p_val >= 1) 0 else abs(stats::qnorm(p_val / 2))
    r_val <- z_val / sqrt(n_pairs)
    method_name <- "Wilcoxon signed-rank test"
    stat_val <- unname(tst$statistic)
    df_val <- NA_real_
    eff <- c(r = unname(r_val))
  }

  desc <- data.frame(
    condition = lvls,
    n = c(n_pairs, n_pairs),
    mean = c(mean(y1), mean(y2)),
    sd = c(stats::sd(y1), stats::sd(y2)),
    median = c(stats::median(y1), stats::median(y2)),
    iqr = c(stats::IQR(y1), stats::IQR(y2)),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      response = response,
      condition = condition,
      id = id,
      levels = lvls,
      n_pairs = n_pairs,
      assumptions = list(
        zero_var_diff = zero_var_diff,
        shapiro_diff_p = shapiro_p,
        norm_violated = norm_violated
      ),
      method = method_name,
      statistic = stat_val,
      df = df_val,
      p_value = p_val,
      effect_size = eff,
      descriptives = desc,
      alpha = alpha
    ),
    class = "statarbor_paired"
  )
}

#' @export
print.statarbor_paired <- function(x, ...) {
  cat("statarbor Paired Comparison\n")
  cat("---------------------------\n")
  cat(sprintf("Response : %s | Condition: %s (%s vs %s) | Pairs: %d\n",
              x$response, x$condition, x$levels[1L], x$levels[2L], x$n_pairs))
  cat(sprintf("Method   : %s\n", x$method))
  if (!is.na(x$df)) {
    cat(sprintf("Result   : t(%.0f) = %.3f, p = %.4g\n", x$df, x$statistic, x$p_value))
  } else {
    cat(sprintf("Result   : V = %.3f, p = %.4g\n", x$statistic, x$p_value))
  }
  eff_str <- paste(sprintf("%s = %.3f", names(x$effect_size), x$effect_size), collapse = ", ")
  cat(sprintf("Effect   : %s\n", eff_str))
  invisible(x)
}

#' Assumption-Guided Repeated-Measures Comparison (k >= 3 Conditions)
#'
#' Automatically evaluates zero-variance conditions, within-subject residual
#' normality, and Greenhouse-Geisser sphericity to route between repeated-measures
#' ANOVA (with Greenhouse-Geisser correction and paired t post-hoc tests) and
#' the Friedman test (with paired Wilcoxon post-hoc tests), producing
#' special-character-safe compact letter displays (CLD).
#'
#' @param data A data.frame in long format.
#' @param response Character. Name of the numeric response column.
#' @param condition Character. Name of the within-subject factor column (k >= 3).
#' @param id Character. Name of the subject/block identifier column.
#' @param alpha Numeric. Significance threshold (default `0.05`).
#' @param p_adjust Character. P-value adjustment method for pairwise post-hoc
#'   tests (default `"holm"`).
#' @param force_method Character. One of `"auto"`, `"parametric"`, or
#'   `"nonparametric"`.
#'
#' @return An object of class `"statarbor_repeated"`.
#' @export
auto_repeated <- function(data, response, condition, id,
                          alpha = 0.05,
                          p_adjust = "holm",
                          force_method = c("auto", "parametric", "nonparametric")) {
  force_method <- match.arg(force_method)
  stopifnot(is.data.frame(data))
  for (col in c(response, condition, id)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in data.", call. = FALSE)
    }
  }

  df <- data[, c(id, condition, response), drop = FALSE]
  names(df) <- c(".id", ".cond", ".y")
  df$.y <- as.numeric(df$.y)
  df <- df[!is.na(df$.y) & !is.na(df$.cond) & !is.na(df$.id), , drop = FALSE]
  df$.cond <- factor(df$.cond)
  df$.id <- factor(df$.id)

  lvls <- levels(df$.cond)
  k <- length(lvls)
  if (k < 3L) {
    stop("`auto_repeated()` requires k >= 3 conditions; use `compare_paired()` for k = 2.",
         call. = FALSE)
  }

  agg <- stats::aggregate(.y ~ .id + .cond, data = df, FUN = mean)
  wide <- stats::reshape(agg, idvar = ".id", timevar = ".cond", direction = "wide")
  val_cols <- paste0(".y.", lvls)
  complete_idx <- stats::complete.cases(wide[, val_cols, drop = FALSE])
  n_dropped <- sum(!complete_idx)
  if (n_dropped > 0L) {
    warning(n_dropped, " subject(s) with incomplete repeated measurements were excluded.",
            call. = FALSE)
  }
  wide <- wide[complete_idx, , drop = FALSE]
  n_subj <- nrow(wide)
  if (n_subj < 3L) {
    stop("At least 3 complete subjects are required.", call. = FALSE)
  }

  mat <- as.matrix(wide[, val_cols, drop = FALSE])
  colnames(mat) <- lvls
  rownames(mat) <- as.character(wide$.id)

  col_sds <- apply(mat, 2L, stats::sd)
  zero_var <- any(col_sds == 0)

  long_clean <- data.frame(
    .id = factor(rep(rownames(mat), times = k)),
    .cond = factor(rep(lvls, each = n_subj), levels = lvls),
    .y = as.numeric(mat)
  )
  fit_lm <- stats::lm(.y ~ .cond + .id, data = long_clean)
  resids <- stats::residuals(fit_lm)
  shapiro_p <- if (stats::sd(resids) == 0 || length(resids) > 5000L) {
    NA_real_
  } else {
    stats::shapiro.test(resids)$p.value
  }
  norm_violated <- isTRUE(!is.na(shapiro_p) && shapiro_p < alpha)

  S <- stats::cov(mat)
  C_mat <- diag(k) - matrix(1 / k, nrow = k, ncol = k)
  S_star <- C_mat %*% S %*% C_mat
  tr_s <- sum(diag(S_star))
  tr_s2 <- sum(S_star * S_star)
  gg_eps <- if (tr_s2 > 0) (tr_s^2) / ((k - 1) * tr_s2) else 1
  gg_eps <- max(1 / (k - 1), min(1, gg_eps))

  use_parametric <- switch(
    force_method,
    parametric = !zero_var,
    nonparametric = FALSE,
    auto = (!zero_var && !norm_violated)
  )

  if (use_parametric) {
    aov_tab <- stats::anova(fit_lm)
    ss_cond <- aov_tab[".cond", "Sum Sq"]
    ss_subj <- aov_tab[".id", "Sum Sq"]
    ss_err <- aov_tab["Residuals", "Sum Sq"]
    df1 <- k - 1
    df2 <- (n_subj - 1) * (k - 1)
    f_val <- (ss_cond / df1) / (ss_err / df2)

    df1_gg <- df1 * gg_eps
    df2_gg <- df2 * gg_eps
    p_val <- stats::pf(f_val, df1_gg, df2_gg, lower.tail = FALSE)

    eta_p2 <- ss_cond / (ss_cond + ss_err)
    eta_g2 <- ss_cond / (ss_cond + ss_subj + ss_err)

    method_name <- sprintf("RM-ANOVA (GG-corrected) + Paired t (%s)", p_adjust)
    stat_val <- f_val
    df_out <- c(df1 = df1_gg, df2 = df2_gg)
    eff <- c(eta_p2 = eta_p2, eta_g2 = eta_g2, gg_epsilon = gg_eps)

    pw_raw <- stats::pairwise.t.test(long_clean$.y, long_clean$.cond,
                                     paired = TRUE, p.adjust.method = p_adjust)$p.value
  } else {
    fr <- stats::friedman.test(mat)
    chi2_val <- unname(fr$statistic)
    df_out <- c(df = unname(fr$parameter))
    p_val <- fr$p.value
    kendall_w <- chi2_val / (n_subj * (k - 1))

    method_name <- sprintf("Friedman + Paired Wilcoxon (%s)", p_adjust)
    stat_val <- chi2_val
    eff <- c(kendalls_w = kendall_w)

    pw_raw <- suppressWarnings(
      stats::pairwise.wilcox.test(long_clean$.y, long_clean$.cond,
                                  paired = TRUE, p.adjust.method = p_adjust,
                                  exact = FALSE)$p.value
    )
  }

  p_mat <- matrix(1, nrow = k, ncol = k, dimnames = list(lvls, lvls))
  pw_list <- list()
  idx <- 1L
  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      g1 <- lvls[i]
      g2 <- lvls[j]
      pv <- pw_raw[g2, g1]
      if (is.na(pv)) pv <- 1
      p_mat[g1, g2] <- pv
      p_mat[g2, g1] <- pv
      pw_list[[idx]] <- data.frame(
        group1 = g1, group2 = g2, p.adj = pv, stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  pairwise_df <- do.call(rbind, pw_list)

  cld_vec <- multcompView::multcompLetters(p_mat, threshold = alpha)$Letters
  cld_vec <- cld_vec[lvls]

  desc <- data.frame(
    condition = lvls,
    n = rep(n_subj, k),
    mean = apply(mat, 2L, mean),
    sd = apply(mat, 2L, stats::sd),
    median = apply(mat, 2L, stats::median),
    iqr = apply(mat, 2L, stats::IQR),
    cld = unname(cld_vec),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  structure(
    list(
      response = response,
      condition = condition,
      id = id,
      n_subjects = n_subj,
      k_conditions = k,
      assumptions = list(
        zero_var = zero_var,
        shapiro_resid_p = shapiro_p,
        norm_violated = norm_violated,
        gg_epsilon = gg_eps
      ),
      method = method_name,
      statistic = stat_val,
      df = df_out,
      p_value = p_val,
      effect_size = eff,
      descriptives = desc,
      pairwise = pairwise_df,
      cld = cld_vec,
      alpha = alpha
    ),
    class = "statarbor_repeated"
  )
}

#' @export
print.statarbor_repeated <- function(x, ...) {
  cat("statarbor Repeated-Measures Comparison\n")
  cat("--------------------------------------\n")
  cat(sprintf("Response : %s | Condition: %s (k = %d) | Subjects: %d\n",
              x$response, x$condition, x$k_conditions, x$n_subjects))
  cat(sprintf("Method   : %s\n", x$method))
  if (length(x$df) == 2L) {
    cat(sprintf("Result   : F(%.2f, %.2f) = %.3f, p = %.4g\n",
                x$df[1L], x$df[2L], x$statistic, x$p_value))
  } else {
    cat(sprintf("Result   : Chi2(%.0f) = %.3f, p = %.4g\n",
                x$df[1L], x$statistic, x$p_value))
  }
  eff_str <- paste(sprintf("%s = %.3f", names(x$effect_size), x$effect_size), collapse = ", ")
  cat(sprintf("Effect   : %s\n\n", eff_str))
  print(x$descriptives, row.names = FALSE)
  invisible(x)
}
