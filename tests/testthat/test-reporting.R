test_that("generate_methods_text produces valid EN and TR paragraphs", {
  res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
  txt_en <- generate_methods_text(res, lang = "en")
  txt_tr <- generate_methods_text(res, lang = "tr")

  expect_type(txt_en, "character")
  expect_true(grepl("Sepal.Length", txt_en))
  expect_true(grepl("statarbor", txt_en))
  expect_true(grepl("Shapiro-Wilk", txt_tr))
})

test_that("export_r_script generates parseable R code", {
  res <- auto_compare(iris, response = "Sepal.Length", group = "Species")
  code_lines <- export_r_script(res, data_name = "iris")

  expect_type(code_lines, "character")
  expect_no_error(parse(text = paste(code_lines, collapse = "\n")))

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  export_r_script(res, data_name = "iris", file = tmp)
  expect_true(file.exists(tmp))
})
