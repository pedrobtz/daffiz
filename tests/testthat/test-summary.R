test_that("diff_summary counts each status per measure, in x's column order", {
  x <- data.table(id = c("a", "b", "c"), w = c(1, 2, 3), v = c(1, 2, 3))
  y <- data.table(id = c("a", "b", "d"), w = c(1, 2, 3), v = c(1, 2.5, 4))
  s <- diff_summary(diff_table(x, y))
  expect_identical(s$metric, c("W", "V"))
  expect_identical(s$n, c(4L, 4L))
  expect_identical(s$n_same, c(2L, 1L))
  expect_identical(s$n_changed, c(0L, 1L))
  expect_identical(s$n_only_x, c(1L, 1L))
  expect_identical(s$n_only_y, c(1L, 1L))
  expect_identical(s$max_abs_diff, c(0, 0.5))
})

test_that("magnitudes keep a difference that overflows to Inf", {
  # Both source values are finite; only their difference is not. Filtering on
  # the difference would report 0 for the largest difference in the table.
  x <- data.table(id = c("a", "b"), v = c(-1e308, 1))
  y <- data.table(id = c("a", "b"), v = c(1e308, 2))
  s <- diff_summary(diff_table(x, y))
  expect_identical(s$max_abs_diff, Inf)
  expect_identical(s$rmse, Inf)
})

test_that("rmse does not overflow on large finite differences", {
  x <- data.table(id = c("a", "b"), v = c(0, 0))
  y <- data.table(id = c("a", "b"), v = c(1e200, 1e200))
  expect_equal(diff_summary(diff_table(x, y))$rmse, 1e200)
})

test_that("a measure with no finite pair has NA magnitudes", {
  x <- data.table(id = "a", v = NA_real_)
  s <- diff_summary(diff_table(x, x))
  expect_true(is.na(s$max_abs_diff) && is.na(s$mean_abs_diff) && is.na(s$rmse))
})

test_that("diff_summary works on the groups and rows shapes", {
  x <- data.table(id = c("a", "a", "b"), v = c(1, 2, 3))
  g <- diff_summary(diff_table(x, x, duplicates = "aggregate"))
  expect_identical(names(g), c("metric", "n", "n_same", "n_changed", "n_only_x", "n_only_y"))
  expect_identical(g$n_same, 2L)

  r <- suppressMessages(diff_table(data.table(k = c("a", "b")), data.table(k = c("a", "c"))))
  s <- diff_summary(r)
  expect_identical(nrow(s), 1L)
  expect_identical(c(s$n_same, s$n_only_x, s$n_only_y), c(1L, 1L, 1L))
})

test_that("diff_summary refuses anything but a diff_table() result", {
  expect_error(diff_summary(data.table(a = 1)), class = "daffiz_error_input")
})
