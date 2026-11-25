test_that("statarbor matches base R reference statistics on thermal_stress", {
  e <- new.env(parent = emptyenv())
  utils::data("thermal_stress", package = "statarbor", envir = e)
  ts_df <- e$thermal_stress

  # 1. One-way ANOVA pathway (embryo_mm) vs stats::aov
  res_aov <- auto_compare(ts_df, "embryo_mm", "treatment")
  ref_aov <- stats::anova(stats::aov(embryo_mm ~ treatment, data = ts_df))
  expect_equal(res_aov$method_code, "anova")
  expect_equal(res_aov$statistic, ref_aov$`F value`[1], tolerance = 1e-6)
  expect_equal(res_aov$p_value, ref_aov$`Pr(>F)`[1], tolerance = 1e-6)

  # 2. Welch ANOVA pathway (fresh_weight_g)
  # (a) Exact mathematical verification of Welch (1951) across all 6 groups
  res_welch <- auto_compare(ts_df, "fresh_weight_g", "treatment")
  expect_equal(res_welch$method_code, "welch")

  ns <- tapply(ts_df$fresh_weight_g, ts_df$treatment, length)
  ms <- tapply(ts_df$fresh_weight_g, ts_df$treatment, mean)
  vs <- tapply(ts_df$fresh_weight_g, ts_df$treatment, stats::var)
  k <- length(ns)
  w <- ns / vs
  xbar <- sum(w * ms) / sum(w)
  lam <- sum((1 - w / sum(w))^2 / (ns - 1)) / (k^2 - 1)
  ref_f <- sum(w * (ms - xbar)^2) / ((k - 1) * (1 + 2 * (k - 2) * lam))
  ref_p <- stats::pf(ref_f, df1 = k - 1, df2 = 1 / (3 * lam), lower.tail = FALSE)

  expect_equal(res_welch$statistic, as.numeric(ref_f), tolerance = 1e-6)
  expect_equal(res_welch$p_value, as.numeric(ref_p), tolerance = 1e-6)

  # (b) Exact equivalence with formula-free stats::t.test (F == t^2 for k = 2)
  sub2 <- ts_df[ts_df$treatment %in% c("Control", "-20C_1W"), ]
  res_w2 <- auto_compare(sub2, "fresh_weight_g", "treatment", mode = "welch")
  ref_t2 <- stats::t.test(
    sub2$fresh_weight_g[sub2$treatment == "Control"],
    sub2$fresh_weight_g[sub2$treatment == "-20C_1W"],
    var.equal = FALSE
  )
  expect_equal(res_w2$statistic, as.numeric(ref_t2$statistic)^2, tolerance = 1e-6)
  expect_equal(res_w2$p_value, as.numeric(ref_t2$p.value), tolerance = 1e-6)

  # 3. Kruskal-Wallis pathway (shoot_cm) vs stats::kruskal.test
  res_kw <- auto_compare(ts_df, "shoot_cm", "treatment")
  ref_kw <- stats::kruskal.test(shoot_cm ~ treatment, data = ts_df)
  expect_equal(res_kw$method_code, "kruskal")
  expect_equal(res_kw$statistic, as.numeric(ref_kw$statistic), tolerance = 1e-6)
  expect_equal(res_kw$p_value, as.numeric(ref_kw$p.value), tolerance = 1e-6)

  # 4. Compact scientific symbols appear in methods text (no raw snake_case)
  txt_en <- generate_methods_text(res_kw, lang = "en")
  expect_false(grepl("epsilon_squared", txt_en))
  expect_true(grepl("\u03b5\u00b2", txt_en))
})
