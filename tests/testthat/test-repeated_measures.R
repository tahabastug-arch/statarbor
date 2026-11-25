test_that("compare_paired matches base R paired t-test within 1e-6", {
  set.seed(42)
  n <- 20
  y1 <- rnorm(n, mean = 10, sd = 2)
  y2 <- y1 + rnorm(n, mean = 2.5, sd = 1)
  df <- data.frame(
    id = rep(paste0("S", seq_len(n)), 2),
    cond = rep(c("Before", "After"), each = n),
    val = c(y1, y2)
  )

  res <- compare_paired(df, response = "val", condition = "cond", id = "id",
                        force_method = "parametric")
  ref <- stats::t.test(y2, y1, paired = TRUE)

  expect_s3_class(res, "statarbor_paired")
  expect_equal(res$method, "Paired t-test")
  expect_lt(abs(abs(res$statistic) - abs(unname(ref$statistic))), 1e-6)
  expect_lt(abs(res$p_value - ref$p.value), 1e-6)
})

test_that("compare_paired routes to Wilcoxon signed-rank on skewed diffs", {
  set.seed(123)
  n <- 25
  y1 <- rep(5, n)
  y2 <- y1 + rexp(n, rate = 0.5)^3
  df <- data.frame(
    id = rep(paste0("S", seq_len(n)), 2),
    cond = rep(c("T0", "T1"), each = n),
    val = c(y1, y2)
  )

  res <- compare_paired(df, response = "val", condition = "cond", id = "id")
  ref <- suppressWarnings(stats::wilcox.test(y2, y1, paired = TRUE, exact = FALSE))

  expect_equal(res$method, "Wilcoxon signed-rank test")
  expect_lt(abs(res$p_value - ref$p.value), 1e-6)
})

test_that("auto_repeated matches RM-ANOVA F and Friedman Chi2 within 1e-6", {
  set.seed(7)
  n <- 15
  subj_eff <- rnorm(n, 0, 2)
  t1 <- 10 + subj_eff + rnorm(n, 0, 0.8)
  t2 <- 13 + subj_eff + rnorm(n, 0, 0.8)
  t3 <- 18 + subj_eff + rnorm(n, 0, 0.8)

  lvls <- c("-20\u00b0C - Week 1", "-20\u00b0C - Week 2", "-20\u00b0C - Week 3")
  df <- data.frame(
    id = factor(rep(paste0("P", seq_len(n)), 3)),
    cond = factor(rep(lvls, each = n), levels = lvls),
    val = c(t1, t2, t3)
  )

  res_p <- auto_repeated(df, "val", "cond", "id", force_method = "parametric")
  ref_aov <- summary(stats::aov(val ~ cond + Error(id / cond), data = df))
  ref_f <- ref_aov[["Error: id:cond"]][[1]]["cond", "F value"]

  expect_s3_class(res_p, "statarbor_repeated")
  expect_lt(abs(res_p$statistic - ref_f), 1e-6)
  expect_equal(names(res_p$cld), lvls)
  expect_equal(length(unique(res_p$cld)), 3L)

  res_np <- auto_repeated(df, "val", "cond", "id", force_method = "nonparametric")
  ref_fr <- stats::friedman.test(cbind(t1, t2, t3))

  expect_lt(abs(res_np$statistic - unname(ref_fr$statistic)), 1e-6)
  expect_lt(abs(res_np$p_value - ref_fr$p.value), 1e-6)
})
