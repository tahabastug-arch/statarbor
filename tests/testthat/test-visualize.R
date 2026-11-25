test_that("plot.statarbor_result returns ggplot objects for boxplot and violin", {
  skip_if_not_installed("ggplot2")
  res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
  p_box <- plot(res, type = "boxplot")
  p_vio <- plot(res, type = "violin")

  expect_s3_class(p_box, "ggplot")
  expect_s3_class(p_vio, "ggplot")
})

test_that("plot_correlation and plot_pca generate valid ggplot objects", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggcorrplot")
  skip_if_not_installed("factoextra")

  p_cor <- plot_correlation(iris, vars = names(iris)[1:4], method = "spearman")
  p_pca <- plot_pca(iris, vars = names(iris)[1:4], group = "Species")

  expect_s3_class(p_cor, "ggplot")
  expect_s3_class(p_pca, "ggplot")
})

test_that("export_word_report writes a non-empty docx file in EN and TR", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  res1 <- auto_compare(iris, response = "Sepal.Length", group = "Species")
  res2 <- auto_compare(iris, response = "Petal.Length", group = "Species")

  out_en <- tempfile(fileext = ".docx")
  out_tr <- tempfile(fileext = ".docx")
  on.exit(unlink(c(out_en, out_tr)), add = TRUE)

  export_word_report(list(res1, res2), file = out_en, lang = "en")
  export_word_report(res1, file = out_tr, lang = "tr")

  expect_true(file.exists(out_en))
  expect_gt(file.info(out_en)$size, 5000)
  expect_true(file.exists(out_tr))
  expect_gt(file.info(out_tr)$size, 5000)
})
