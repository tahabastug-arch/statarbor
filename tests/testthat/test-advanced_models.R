test_that("compare_two_groups routes properly and computes effect size", {
  z <- stats::qnorm(stats::ppoints(20))
  df_norm <- data.frame(
    grp = rep(c("Diploid", "Haploid"), each = 20),
    val = c(12 + 1.5 * z, 8 + 1.5 * z)
  )
  res_t <- compare_two_groups(df_norm, "val", "grp")
  expect_s3_class(res_t, "statarbor_two_group")
  expect_equal(res_t$method_code, "t.test")
  expect_true(res_t$p_value < 0.05)

  df_zero <- data.frame(
    grp = rep(c("A", "B"), each = 10),
    val = c(rep(0, 10), seq(1, 10))
  )
  res_mw <- compare_two_groups(df_zero, "val", "grp")
  expect_equal(res_mw$method_code, "mann_whitney")
  expect_true(res_mw$effect_size$r > 0)
})

test_that("analyze_binary detects complete separation and uses Firth regression", {
  skip_if_not_installed("logistf")
  df_sep <- data.frame(
    grp = rep(c("Control", "Cold", "Lethal"), each = 10),
    germ = c(rep(1, 10), c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), rep(0, 10))
  )
  res_bin <- analyze_binary(df_sep, "germ", "grp")
  expect_s3_class(res_bin, "statarbor_binary")
  expect_true(res_bin$complete_separation)
  expect_true(grepl("Firth", res_bin$method))
  expect_true(res_bin$chisq$cramers_v > 0.5)
})

test_that("analyze_ordinal fits cumulative link model cleanly", {
  skip_if_not_installed("ordinal")
  df_ord <- data.frame(
    grp = rep(c("Low", "Mid", "High"), each = 20),
    score = c(
      c(rep(1, 10), rep(2, 6), rep(3, 3), rep(4, 1)),
      c(rep(1, 3),  rep(2, 7), rep(3, 7), rep(4, 3)),
      c(rep(1, 1),  rep(2, 3), rep(3, 6), rep(4, 10))
    )
  )
  res_ord <- analyze_ordinal(df_ord, "score", "grp")
  expect_s3_class(res_ord, "statarbor_ordinal")
  expect_true(res_ord$nagelkerke_r2 > 0.2)
})

test_that("analyze_interaction supports Gamma-log GLM and catches non-positive values", {
  res_glm <- analyze_interaction(
    ToothGrowth,
    response = "len",
    factors = c("supp", "dose"),
    family = "gamma_log"
  )
  expect_s3_class(res_glm, "statarbor_interaction")
  expect_true("supp:dose" %in% res_glm$anova_table$term)

  df_bad <- ToothGrowth
  df_bad$len[1] <- 0
  expect_error(
    analyze_interaction(df_bad, "len", c("supp", "dose"), family = "gamma_log"),
    "strictly positive"
  )
})
