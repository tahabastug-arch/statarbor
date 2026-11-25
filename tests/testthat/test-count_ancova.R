test_that("analyze_count stays on Poisson for equidispersed counts and matches base glm", {
  set.seed(101)
  n <- 30
  lvls <- c("Control", "+4\u00b0C - W1", "-20\u00b0C - W1")
  df <- data.frame(
    grp = factor(rep(lvls, each = n), levels = lvls),
    cnt = c(rpois(n, 2), rpois(n, 6), rpois(n, 14))
  )

  res <- analyze_count(df, response = "cnt", group = "grp")
  ref_full <- stats::glm(cnt ~ grp, data = df, family = stats::poisson())
  ref_null <- stats::glm(cnt ~ 1, data = df, family = stats::poisson())
  ref_lr <- ref_null$deviance - ref_full$deviance

  expect_s3_class(res, "statarbor_count")
  expect_equal(res$family, "poisson")
  expect_lt(abs(res$statistic - ref_lr), 1e-6)
  expect_equal(names(res$cld), lvls)
  expect_equal(length(unique(res$cld)), 3L)
})

test_that("analyze_count routes overdispersed data to Negative Binomial and handles zero groups", {
  skip_if_not_installed("MASS")
  set.seed(202)
  n <- 25
  df <- data.frame(
    grp = factor(rep(c("Control", "Stress_A", "Lethal_0"), each = n),
                 levels = c("Control", "Stress_A", "Lethal_0")),
    cnt = c(rnbinom(n, mu = 12, size = 1.5),
            rnbinom(n, mu = 4, size = 1.5),
            rep(0L, n))
  )

  res <- analyze_count(df, response = "cnt", group = "grp")

  expect_true(res$assumptions$overdispersed)
  expect_equal(res$family, "negbin")
  expect_true(res$assumptions$any_zero_group)
  # Hauck-Donner safeguard: Lethal_0 vs Control must be highly significant, not p = 1
  p_ctrl_zero <- res$pairwise$p.adj[res$pairwise$group1 == "Control" &
                                      res$pairwise$group2 == "Lethal_0"]
  expect_lt(p_ctrl_zero, 0.001)
})

test_that("analyze_ancova matches car::Anova Type II SS within 1e-6 and detects slope heterogeneity", {
  set.seed(303)
  n <- 25
  cov_x <- rnorm(3 * n, mean = 5, sd = 1.5)
  lvls <- c("Ctrl", "Low - 0\u00b0C", "High - 4\u00b0C")
  grp <- factor(rep(lvls, each = n), levels = lvls)
  y <- 10 + 1.8 * cov_x + rep(c(0, 3, 7), each = n) + rnorm(3 * n, 0, 1.2)
  df <- data.frame(grp = grp, cov = cov_x, y = y)

  res <- analyze_ancova(df, response = "y", group = "grp", covariate = "cov")
  ref_lm <- stats::lm(y ~ grp + cov, data = df)
  ref_car <- suppressWarnings(car::Anova(ref_lm, type = 2))

  expect_s3_class(res, "statarbor_ancova")
  expect_true(res$assumptions$slopes_homogeneous)
  expect_lt(abs(res$group_test$statistic - ref_car["grp", "F value"]), 1e-6)
  expect_lt(abs(res$group_test$p_value - ref_car["grp", "Pr(>F)"]), 1e-6)
  expect_lt(abs(res$covariate_test$statistic - ref_car["cov", "F value"]), 1e-6)
  expect_equal(names(res$cld), lvls)

  # Heterogeneous slopes scenario
  y_het <- 10 + rep(c(0.5, 2.0, 5.0), each = n) * cov_x + rnorm(3 * n, 0, 1.0)
  df_het <- data.frame(grp = grp, cov = cov_x, y = y_het)
  res_het <- analyze_ancova(df_het, response = "y", group = "grp", covariate = "cov")

  expect_false(res_het$assumptions$slopes_homogeneous)
  expect_match(res_het$method, "Interaction ANCOVA")
})
