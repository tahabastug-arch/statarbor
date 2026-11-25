test_that("analyze_rcbd fixed matches base aov and TukeyHSD within 1e-6", {
  set.seed(404)
  lvls <- c("Control", "Dose - 10%", "Dose - 20%", "Dose - 30%")
  blks <- paste0("Block_", 1:6)
  grid <- expand.grid(trt = lvls, blk = blks, stringsAsFactors = FALSE)
  grid$trt <- factor(grid$trt, levels = lvls)
  grid$blk <- factor(grid$blk, levels = blks)

  blk_eff <- setNames(c(-3, -1.5, 0, 1, 2, 3.5), blks)
  trt_eff <- setNames(c(10, 13, 18, 22), lvls)
  grid$yield <- trt_eff[as.character(grid$trt)] + blk_eff[as.character(grid$blk)] +
    rnorm(nrow(grid), 0, 0.9)

  res <- analyze_rcbd(grid, response = "yield", treatment = "trt", block = "blk",
                      force_method = "parametric")
  ref_aov <- stats::aov(yield ~ trt + blk, data = grid)
  ref_tab <- summary(ref_aov)[[1]]
  ref_tuk <- stats::TukeyHSD(ref_aov, "trt")$trt

  expect_s3_class(res, "statarbor_rcbd")
  expect_lt(abs(res$treatment_test$statistic - ref_tab["trt", "F value"]), 1e-6)
  expect_lt(abs(res$treatment_test$p_value - ref_tab["trt", "Pr(>F)"]), 1e-6)
  expect_lt(abs(res$block_test$statistic - ref_tab["blk", "F value"]), 1e-6)
  expect_gt(res$relative_efficiency, 1.5)
  expect_equal(names(res$cld), lvls)

  # Check Tukey p-value agreement on a specific pair
  p_stat <- res$pairwise$p.adj[res$pairwise$group1 == "Control" &
                                 res$pairwise$group2 == "Dose - 10%"]
  p_ref <- ref_tuk["Dose - 10%-Control", "p adj"]
  expect_lt(abs(p_stat - p_ref), 1e-6)
})

test_that("analyze_rcbd routes balanced skewed data to Friedman", {
  set.seed(505)
  lvls <- c("T1", "T2", "T3")
  blks <- paste0("B", 1:8)
  grid <- expand.grid(trt = lvls, blk = blks)
  # Extreme outlier in one cell to violate residual normality
  grid$y <- rep(c(1, 5, 10), times = 8) + rep(seq_len(8), each = 3)
  grid$y[1] <- 80

  res <- analyze_rcbd(grid, response = "y", treatment = "trt", block = "blk")
  expect_true(res$assumptions$norm_violated)
  expect_match(res$method, "Friedman RCBD")
})

test_that("analyze_rcbd random block works when lme4 is available", {
  skip_if_not_installed("lme4")
  set.seed(606)
  grid <- expand.grid(trt = c("A", "B", "C"), blk = paste0("B", 1:6))
  grid$y <- rep(c(5, 10, 16), times = 6) + rep(c(-3, -2, -1, 1, 2, 3), each = 3) + rnorm(18, 0, 0.7)

  res <- analyze_rcbd(grid, response = "y", treatment = "trt", block = "blk",
                      block_type = "random")
  expect_equal(res$block_type, "random")
  expect_true(res$treatment_test$p_value < 0.001)
  expect_true(is.finite(res$icc) && res$icc >= 0 && res$icc <= 1)
})
