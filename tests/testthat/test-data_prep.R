test_that("merge_group_prefix collapses numbered replicate labels cleanly", {
  df <- data.frame(
    grup = c("Kontrol 1", "Kontrol 2", "Kontrol 3", "+4C - 1. Hafta", "+4C - 1. Hafta"),
    val = c(10, 12, 11, 5, 6),
    stringsAsFactors = FALSE
  )
  out <- merge_group_prefix(df, column = "grup", prefix = "Kontrol")
  expect_true("grup_clean" %in% names(out))
  expect_equal(nlevels(out$grup_clean), 2L)
  expect_equal(sum(out$grup_clean == "Kontrol"), 3L)

  out_ci <- merge_group_prefix(df, column = "grup", prefix = "kontrol",
                               new_label = "Control", ignore_case = TRUE)
  expect_equal(sum(out_ci$grup_clean == "Control"), 3L)
})

test_that("combine_factors creates composite interaction factor and preserves NAs", {
  df <- data.frame(
    temp = c("4", "4", "0", NA),
    week = c("1", "2", "1", "2"),
    stringsAsFactors = FALSE
  )
  out <- combine_factors(df, cols = c("temp", "week"), sep = " - ")
  expect_true("temp_x_week" %in% names(out))
  expect_equal(as.character(out$temp_x_week[1]), "4 - 1")
  expect_true(is.na(out$temp_x_week[4]))
})
