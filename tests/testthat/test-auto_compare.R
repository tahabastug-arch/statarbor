test_that("auto_compare routes parametric data to ANOVA + Tukey", {
  z <- stats::qnorm(stats::ppoints(25))
  df_param <- data.frame(
    grp = rep(c("A", "B", "C"), each = 25),
    val = c(10 + z, 15 + z, 20 + z)
  )
  res <- auto_compare(df_param, "val", "grp")
  expect_s3_class(res, "statarbor_result")
  expect_equal(res$method_code, "anova")
  expect_equal(res$cld$cld[res$cld$group == "C"], "a")
})

test_that("auto_compare handles zero-variance lethal groups and special characters", {
  df_stress <- data.frame(
    grp = rep(c("Control", "+4\u00b0C - 1. Week", "-196\u00b0C - 1. Week"), each = 10),
    val = c(c(10, 12, 11, 13, 9, 10, 14, 11, 12, 10),
            c(4, 5, 3, 6, 4, 5, 4, 3, 5, 4),
            rep(0, 10))
  )
  res <- auto_compare(df_stress, "val", "grp")
  expect_equal(res$method_code, "kruskal")
  expect_true(res$assumptions$zero_variance)
  expect_equal(nrow(res$cld), 3L)
  expect_false(any(is.na(res$descriptives$cld)))
})

test_that("auto_compare routes heteroscedastic normal data to Welch ANOVA", {
  z <- stats::qnorm(stats::ppoints(30))
  df_het <- data.frame(
    grp = rep(c("G1", "G2", "G3"), each = 30),
    val = c(10 + 0.5 * z, 12 + 4.0 * z, 18 + 1.5 * z)
  )
  res <- auto_compare(df_het, "val", "grp")
  expect_equal(res$method_code, "welch")
})
