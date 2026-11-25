test_that("bundled Shiny app file exists and parses without syntax errors", {
  app_file <- system.file("shiny", "app.R", package = "statarbor")
  expect_true(nzchar(app_file))
  expect_true(file.exists(app_file))
  expect_no_error(parse(file = app_file))
})
