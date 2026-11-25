#' Safe Shapiro-Wilk Normality Test
#'
#' @param x Numeric vector.
#' @return Numeric p-value or `NA_real_` if sample size or uniqueness is insufficient.
#' @keywords internal
safe_shapiro <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) < 3L || length(x) > 5000L || length(unique(x)) < 2L) {
    return(NA_real_)
  }
  tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
}

#' Native Welch One-Way ANOVA (1951)
#'
#' Computes Welch's heteroscedastic one-way F-test directly from group
#' summaries, matching `stats::oneway.test(..., var.equal = FALSE)` while
#' avoiding `parent.frame()` evaluation issues inside `tryCatch()`.
#'
#' @param sub_df Data frame with numeric `y_val` and factor `g_val`.
#' @return A list with `statistic`, `df1`, `df2`, and `p.value`, or `NULL`.
#' @keywords internal
welch_anova_native <- function(sub_df) {
  lvls <- levels(sub_df$g_val)
  k <- length(lvls)
  if (k < 2L) return(NULL)

  n_vec <- vapply(lvls, function(g) sum(sub_df$g_val == g), numeric(1))
  m_vec <- vapply(lvls, function(g) mean(sub_df$y_val[sub_df$g_val == g]), numeric(1))
  v_vec <- vapply(lvls, function(g) stats::var(sub_df$y_val[sub_df$g_val == g]), numeric(1))

  if (any(n_vec < 2) || any(is.na(v_vec)) || any(v_vec <= 0)) {
    return(NULL)
  }

  w <- n_vec / v_vec
  w_sum <- sum(w)
  xbar <- sum(w * m_vec) / w_sum
  tmp <- sum((1 - w / w_sum)^2 / (n_vec - 1)) / (k^2 - 1)
  if (!is.finite(tmp) || tmp <= 0) return(NULL)

  f_val <- sum(w * (m_vec - xbar)^2) / ((k - 1) * (1 + 2 * (k - 2) * tmp))
  df1 <- k - 1
  df2 <- 1 / (3 * tmp)
  p_val <- stats::pf(f_val, df1 = df1, df2 = df2, lower.tail = FALSE)

  list(
    statistic = as.numeric(f_val),
    df1 = as.numeric(df1),
    df2 = as.numeric(df2),
    p.value = as.numeric(p_val)
  )
}

#' Native Games-Howell Pairwise Post-Hoc Test
#'
#' Computes pairwise Games-Howell comparisons using Welch-Satterthwaite
#' degrees of freedom and the Studentized range distribution (`stats::ptukey`),
#' avoiding namespace/S3 conflicts from third-party formula utilities.
#'
#' @param sub_df Data frame with numeric `y_val` and factor `g_val`.
#' @param alpha Significance level for confidence intervals (default `0.05`).
#' @return Data frame with `group1`, `group2`, `estimate`, `conf.low`,
#'   `conf.high`, `df`, `p.adj`, and `p.adj.signif`.
#' @keywords internal
games_howell_native <- function(sub_df, alpha = 0.05) {
  lvls <- levels(sub_df$g_val)
  k <- length(lvls)

  n_vec <- vapply(lvls, function(g) sum(sub_df$g_val == g), numeric(1))
  m_vec <- vapply(lvls, function(g) mean(sub_df$y_val[sub_df$g_val == g]), numeric(1))
  v_vec <- vapply(lvls, function(g) stats::var(sub_df$y_val[sub_df$g_val == g]), numeric(1))

  rows <- list()
  idx <- 1L
  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      diff_val <- m_vec[j] - m_vec[i]
      se_sq <- (v_vec[i] / n_vec[i]) + (v_vec[j] / n_vec[j])
      se_gh <- sqrt(0.5 * se_sq)

      if (is.finite(se_gh) && se_gh > 0) {
        q_stat <- abs(diff_val) / se_gh
        denom <- ((v_vec[i] / n_vec[i])^2) / (n_vec[i] - 1) +
          ((v_vec[j] / n_vec[j])^2) / (n_vec[j] - 1)
        df_ws <- if (is.finite(denom) && denom > 0) {
          (se_sq^2) / denom
        } else {
          n_vec[i] + n_vec[j] - 2
        }
        p_adj <- stats::ptukey(q_stat, nmeans = k, df = df_ws, lower.tail = FALSE)
        q_crit <- stats::qtukey(1 - alpha, nmeans = k, df = df_ws)
        ci_low <- diff_val - q_crit * se_gh
        ci_high <- diff_val + q_crit * se_gh
      } else {
        df_ws <- n_vec[i] + n_vec[j] - 2
        p_adj <- if (diff_val == 0) 1 else 0
        ci_low <- diff_val
        ci_high <- diff_val
      }

      signif_lbl <- if (is.na(p_adj)) {
        "ns"
      } else if (p_adj < 0.0001) {
        "****"
      } else if (p_adj < 0.001) {
        "***"
      } else if (p_adj < 0.01) {
        "**"
      } else if (p_adj < 0.05) {
        "*"
      } else {
        "ns"
      }

      rows[[idx]] <- data.frame(
        group1 = lvls[i],
        group2 = lvls[j],
        estimate = as.numeric(diff_val),
        conf.low = as.numeric(ci_low),
        conf.high = as.numeric(ci_high),
        df = as.numeric(df_ws),
        p.adj = as.numeric(p_adj),
        p.adj.signif = signif_lbl,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  do.call(rbind, rows)
}

#' Assumption-Guided Group Comparison with Automated CLD
#'
#' Evaluates group-level normality (Shapiro-Wilk), variance homogeneity (Levene),
#' and zero-variance conditions to automatically select and execute the most
#' appropriate statistical test among one-way ANOVA (Tukey HSD), Welch ANOVA
#' (Games-Howell), and Kruskal-Wallis (Dunn). Computes standardized effect
#' sizes and compact letter displays (CLD).
#'
#' @param data A data frame containing the response and grouping variables.
#' @param response Character scalar naming the numeric response column.
#' @param group Character scalar naming the categorical grouping column.
#' @param mode Character string specifying the decision strategy: `"auto"`
#'   (default), `"anova"`, `"welch"`, or `"kruskal"`.
#' @param p_adjust P-value adjustment method for Dunn's post-hoc test:
#'   `"bonferroni"` (default), `"holm"`, or `"BH"`.
#' @param alpha Numeric significance level (default `0.05`).
#'
#' @return An object of S3 class `"statarbor_result"` containing test statistics,
#'   assumption diagnostics, effect sizes, group descriptives with CLD letters,
#'   and pairwise post-hoc comparisons.
#' @export
#'
#' @examples
#' res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
#' print(res)
#' res$descriptives
auto_compare <- function(data,
                         response,
                         group,
                         mode = c("auto", "anova", "welch", "kruskal"),
                         p_adjust = c("bonferroni", "holm", "BH"),
                         alpha = 0.05) {
  mode <- match.arg(mode)
  p_adjust <- match.arg(p_adjust)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!response %in% names(data)) {
    stop(sprintf("Response column '%s' not found in `data`.", response), call. = FALSE)
  }
  if (!group %in% names(data)) {
    stop(sprintf("Group column '%s' not found in `data`.", group), call. = FALSE)
  }

  y_raw <- suppressWarnings(as.numeric(as.character(data[[response]])))
  g_raw <- as.character(data[[group]])
  keep <- !is.na(y_raw) & !is.na(g_raw) & nzchar(trimws(g_raw))

  sub_df <- data.frame(
    y_val = y_raw[keep],
    g_val = factor(g_raw[keep], levels = unique(g_raw[keep])),
    stringsAsFactors = FALSE
  )

  if (nrow(sub_df) < 4L || nlevels(sub_df$g_val) < 2L) {
    stop("At least 2 groups and 4 valid observations are required.", call. = FALSE)
  }
  if (length(unique(sub_df$y_val)) <= 1L) {
    stop("Response variable is constant across all observations.", call. = FALSE)
  }

  # 1. Group-level descriptive statistics and assumption diagnostics
  grp_levels <- levels(sub_df$g_val)
  desc_list <- lapply(grp_levels, function(g) {
    vals <- sub_df$y_val[sub_df$g_val == g]
    data.frame(
      group = g,
      n = length(vals),
      mean = mean(vals),
      sd = if (length(vals) > 1L) stats::sd(vals) else NA_real_,
      median = stats::median(vals),
      iqr = stats::IQR(vals),
      min = min(vals),
      max = max(vals),
      shapiro_p = safe_shapiro(vals),
      variance = if (length(vals) > 1L) stats::var(vals) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  desc_df <- do.call(rbind, desc_list)
  rownames(desc_df) <- NULL

  zero_var_any <- any(is.na(desc_df$variance) | desc_df$variance == 0)
  normality_violated <- any(!is.na(desc_df$shapiro_p) & desc_df$shapiro_p < alpha)

  levene_p <- tryCatch({
    car::leveneTest(y = sub_df$y_val, group = sub_df$g_val)$`Pr(>F)`[1]
  }, error = function(e) NA_real_)
  variance_violated <- !is.na(levene_p) && levene_p < alpha

  # 2. Decision tree routing
  chosen <- mode
  decision_reason <- "User-selected method"

  if (mode == "auto") {
    if (zero_var_any) {
      chosen <- "kruskal"
      decision_reason <- "Zero-variance group detected -> routed to Kruskal-Wallis + Dunn"
    } else if (normality_violated) {
      chosen <- "kruskal"
      decision_reason <- "Shapiro-Wilk normality violated -> routed to Kruskal-Wallis + Dunn"
    } else if (variance_violated) {
      chosen <- "welch"
      decision_reason <- "Normality met but Levene homogeneity violated -> routed to Welch ANOVA + Games-Howell"
    } else {
      chosen <- "anova"
      decision_reason <- "Normality and homogeneity of variances met -> routed to One-way ANOVA + Tukey HSD"
    }
  } else if (chosen == "welch" && zero_var_any) {
    chosen <- "kruskal"
    decision_reason <- "Welch ANOVA fallback due to zero-variance group -> Kruskal-Wallis + Dunn"
  }

  fml <- stats::as.formula("y_val ~ g_val")

  # 3. Execute selected statistical pipeline
  if (chosen == "anova") {
    mod <- stats::aov(fml, data = sub_df)
    anv <- stats::anova(mod)
    f_val <- anv$`F value`[1]
    df1 <- anv$Df[1]
    df2 <- anv$Df[2]
    p_val <- anv$`Pr(>F)`[1]

    ss_tot <- sum(anv$`Sum Sq`)
    eta_sq <- anv$`Sum Sq`[1] / ss_tot
    omega_sq <- max(0, (anv$`Sum Sq`[1] - df1 * anv$`Mean Sq`[2]) /
                      (ss_tot + anv$`Mean Sq`[2]))

    ph <- rstatix::tukey_hsd(sub_df, fml)
    ord <- desc_df$group[order(-desc_df$mean)]
    cld_df <- generate_cld(ph, ord, alpha = alpha)

    method_label <- "One-way ANOVA + Tukey HSD"
    stat_name <- sprintf("F(%d, %d)", df1, df2)
    stat_val <- f_val
    eff_list <- list(eta_squared = eta_sq, omega_squared = omega_sq)

  } else if (chosen == "welch") {
    w_res <- tryCatch(welch_anova_native(sub_df), error = function(e) NULL)
    gh_res <- tryCatch(games_howell_native(sub_df, alpha = alpha), error = function(e) NULL)

    if (!is.null(w_res) && !is.null(gh_res) && !is.na(w_res$p.value)) {
      f_val <- w_res$statistic
      df1 <- w_res$df1
      df2 <- w_res$df2
      p_val <- w_res$p.value
      est_omega_sq <- max(0, (df1 * (f_val - 1)) / (df1 * (f_val - 1) + nrow(sub_df)))

      ph <- gh_res
      ord <- desc_df$group[order(-desc_df$mean)]
      cld_df <- generate_cld(ph, ord, alpha = alpha)

      method_label <- "Welch ANOVA + Games-Howell"
      stat_name <- sprintf("Welch F(%d, %.2f)", round(df1), df2)
      stat_val <- f_val
      eff_list <- list(est_omega_squared = est_omega_sq)
    } else {
      chosen <- "kruskal"
      decision_reason <- "Welch ANOVA numerical fallback -> Kruskal-Wallis + Dunn"
    }
  }

  if (chosen == "kruskal") {
    kw_res <- rstatix::kruskal_test(sub_df, fml)
    ph <- rstatix::dunn_test(sub_df, fml, p.adjust.method = p_adjust)

    h_val <- kw_res$statistic[1]
    df_val <- kw_res$df[1]
    p_val <- kw_res$p[1]
    n_tot <- nrow(sub_df)
    k_grp <- nlevels(sub_df$g_val)
    eps_sq <- max(0, (h_val - k_grp + 1) / (n_tot - k_grp))

    ord <- desc_df$group[order(-desc_df$median, -desc_df$mean)]
    cld_df <- generate_cld(ph, ord, alpha = alpha)

    method_label <- sprintf("Kruskal-Wallis + Dunn (%s)", p_adjust)
    stat_name <- sprintf("H(df=%d)", df_val)
    stat_val <- h_val
    eff_list <- list(epsilon_squared = eps_sq)
  }

  # Attach CLD letters while preserving original group ordering
  desc_df$cld <- cld_df$cld[match(desc_df$group, cld_df$group)]

  out <- list(
    response = response,
    group = group,
    method_code = chosen,
    method = method_label,
    decision_reason = decision_reason,
    alpha = alpha,
    assumptions = list(
      zero_variance = zero_var_any,
      normality_violated = normality_violated,
      variance_violated = variance_violated,
      levene_p = levene_p
    ),
    statistic_name = stat_name,
    statistic = as.numeric(stat_val),
    p_value = as.numeric(p_val),
    effect_size = eff_list,
    descriptives = desc_df,
    posthoc = as.data.frame(ph),
    cld = cld_df,
    data = sub_df
  )
  class(out) <- "statarbor_result"
  out
}

#' Print Method for statarbor_result Objects
#'
#' @param x An object of class `"statarbor_result"`.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns `x`.
#' @export
print.statarbor_result <- function(x, ...) {
  cat("-- statarbor Result --------------------------------------\n")
  cat(sprintf("Response : %s  |  Group: %s\n", x$response, x$group))
  cat(sprintf("Method   : %s\n", x$method))
  cat(sprintf("Decision : %s\n", x$decision_reason))
  p_fmt <- if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)
  cat(sprintf("Test Stat: %s = %.3f, p = %s\n", x$statistic_name, x$statistic, p_fmt))
  eff_txt <- paste(
    sprintf("%s = %.3f", names(x$effect_size), unlist(x$effect_size)),
    collapse = ", "
  )
  cat(sprintf("Effect   : %s\n", eff_txt))
  cat("----------------------------------------------------------\n")
  tbl <- x$descriptives[, c("group", "n", "mean", "sd", "median", "iqr", "cld")]
  tbl$mean <- round(tbl$mean, 3)
  tbl$sd <- round(tbl$sd, 3)
  tbl$median <- round(tbl$median, 3)
  tbl$iqr <- round(tbl$iqr, 3)
  print(tbl, row.names = FALSE)
  invisible(x)
}
