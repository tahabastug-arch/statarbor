test_that("v0.2.0 plot methods return valid ggplot objects across all 5 classes", {
  skip_if_not_installed("ggplot2")
  set.seed(707)

  # 1. Paired
  df_p <- data.frame(
    id = rep(paste0("S", 1:10), 2),
    cond = rep(c("Pre", "Post"), each = 10),
    y = c(rnorm(10, 5), rnorm(10, 8))
  )
  p1 <- plot(compare_paired(df_p, "y", "cond", "id"))
  expect_s3_class(p1, "ggplot")

  # 2. Repeated
  df_r <- data.frame(
    id = rep(paste0("S", 1:10), 3),
    cond = rep(c("W1", "W2", "W3"), each = 10),
    y = c(rnorm(10, 5), rnorm(10, 8), rnorm(10, 12))
  )
  p2 <- plot(auto_repeated(df_r, "y", "cond", "id"))
  expect_s3_class(p2, "ggplot")

  # 3. Count (including a zero-count group to verify NA-safe errorbars)
  df_c <- data.frame(
    grp = factor(rep(c("Ctrl", "Mid", "Zero"), each = 12)),
    cnt = c(rpois(12, 8), rpois(12, 3), rep(0L, 12))
  )
  p3 <- plot(analyze_count(df_c, "cnt", "grp"))
  expect_s3_class(p3, "ggplot")

  # 4. ANCOVA
  cov_x <- rnorm(30, 5, 1)
  df_a <- data.frame(
    grp = factor(rep(c("A", "B", "C"), each = 10)),
    cov = cov_x,
    y = 2 + 1.5 * cov_x + rep(c(0, 3, 6), each = 10) + rnorm(30, 0, 0.8)
  )
  p4 <- plot(analyze_ancova(df_a, "y", "grp", "cov"))
  expect_s3_class(p4, "ggplot")

  # 5. RCBD
  df_b <- expand.grid(trt = c("T1", "T2", "T3"), blk = paste0("B", 1:5))
  df_b$y <- rep(c(10, 15, 22), 5) + rep(c(-2, -1, 0, 1, 2), each = 3) + rnorm(15, 0, 0.6)
  p5 <- plot(analyze_rcbd(df_b, "y", "trt", "blk"))
  expect_s3_class(p5, "ggplot")
})
