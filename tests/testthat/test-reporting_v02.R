test_that("generate_methods_text (EN/TR) and export_r_script work across all 5 v0.2.0 classes", {
  set.seed(808)

  df_p <- data.frame(
    id = rep(paste0("S", 1:10), 2),
    cond = rep(c("Pre", "Post"), each = 10),
    y = c(rnorm(10, 5), rnorm(10, 8))
  )
  df_r <- data.frame(
    id = rep(paste0("S", 1:10), 3),
    cond = rep(c("W1", "W2", "W3"), each = 10),
    y = c(rnorm(10, 5), rnorm(10, 8), rnorm(10, 12))
  )
  df_c <- data.frame(
    grp = factor(rep(c("Ctrl", "Low", "Zero"), each = 12)),
    cnt = c(rpois(12, 10), rpois(12, 4), rep(0L, 12))
  )
  cov_x <- rnorm(30, 5, 1)
  df_a <- data.frame(
    grp = factor(rep(c("A", "B", "C"), each = 10)),
    cov = cov_x,
    y = 2 + 1.5 * cov_x + rep(c(0, 3, 6), each = 10) + rnorm(30, 0, 0.8)
  )
  df_b <- expand.grid(trt = c("T1", "T2", "T3"), blk = paste0("B", 1:5))
  df_b$y <- rep(c(10, 15, 22), 5) + rep(c(-2, -1, 0, 1, 2), each = 3) + rnorm(15, 0, 0.6)

  objs <- list(
    compare_paired(df_p, "y", "cond", "id"),
    auto_repeated(df_r, "y", "cond", "id"),
    analyze_count(df_c, "cnt", "grp"),
    analyze_ancova(df_a, "y", "grp", "cov"),
    analyze_rcbd(df_b, "y", "trt", "blk")
  )

  for (obj in objs) {
    txt_en <- generate_methods_text(obj, lang = "en")
    txt_tr <- generate_methods_text(obj, lang = "tr")
    expect_true(is.character(txt_en) && nchar(txt_en) > 80)
    expect_true(is.character(txt_tr) && nchar(txt_tr) > 80)
    expect_match(txt_en, "statarbor")
    expect_match(txt_tr, "statarbor")

    code <- export_r_script(obj, data_name = "demo_df")
    expect_true(is.character(code) && length(code) >= 10)
    expect_silent(parse(text = code))
  }
})
