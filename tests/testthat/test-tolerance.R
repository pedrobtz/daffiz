test_that("rel_tol is relative to the larger magnitude of the two values", {
  x <- data.table(id = 1:2, v = c(100, 1))
  y <- data.table(id = 1:2, v = c(100.5, 1.5))
  d <- diff_table(x, y, rel_tol = 0.01)
  expect_identical(as.character(d$status), c("same", "changed"))
  expect_equal(d$rel_diff, c(0.5 / 100.5, 0.5 / 1.5))
  expect_identical(as.character(diff_table(x, y, rel_tol = 0.001)$status), c("changed", "changed"))
})

test_that("a value is same within either tolerance", {
  # Near zero only the absolute tolerance helps: 0 and 1e-12 differ by 100%.
  x <- data.table(id = 1:2, v = c(0, 1000))
  y <- data.table(id = 1:2, v = c(1e-12, 1001))
  d <- diff_table(x, y, tolerance = 1e-9, rel_tol = 0.01)
  expect_identical(as.character(d$status), c("same", "same"))
  expect_identical(d$rel_diff[1], 1)
  expect_identical(
    as.character(diff_table(x, y, rel_tol = 0.01)$status), c("changed", "same")
  )
  expect_identical(
    as.character(diff_table(x, y, tolerance = 1e-9)$status), c("same", "changed")
  )
})

test_that("the tolerances apply to finite pairs only", {
  # rel_tol times an infinite magnitude is infinite, which would call any
  # finite value the same as Inf.
  x <- data.table(id = 1:3, v = c(1, Inf, -Inf))
  y <- data.table(id = 1:3, v = c(Inf, Inf, -Inf))
  d <- diff_table(x, y, tolerance = 1e6, rel_tol = 10)
  expect_identical(as.character(d$status), c("changed", "same", "same"))
  expect_identical(d$rel_diff, c(NA_real_, NA_real_, NA_real_))
})

test_that("abs_diff and rel_diff of edge values", {
  x <- data.table(id = 1:4, v = c(-1e308, 0, NA, 2))
  y <- data.table(id = 1:4, v = c(1e308, 0, 1, -2))
  d <- diff_table(x, y)
  # The difference overflows; the relative one is scaled first, so it does not.
  expect_identical(d$abs_diff, c(Inf, 0, NA, 4))
  expect_identical(d$rel_diff, c(2, 0, NA, 2))
})

test_that("abs_diff and rel_diff are symmetric in equal mode", {
  x <- data.table(id = 1:3, v = c(1, 5, -3))
  y <- data.table(id = 1:3, v = c(2, 5, 4))
  xy <- diff_table(x, y, mode = "equal")
  yx <- diff_table(y, x, mode = "equal")
  expect_identical(yx$abs_diff, xy$abs_diff)
  expect_identical(yx$rel_diff, xy$rel_diff)
})

test_that("the tolerances are recorded and validated", {
  x <- data.table(id = "a", v = 1)
  expect_identical(attr(diff_table(x, x, tolerance = 0.5, rel_tol = 1e-6), "tolerance"), list(abs = 0.5, rel = 1e-6))
  expect_error(diff_table(x, x, rel_tol = -1), class = "daffiz_error_input")
  expect_error(diff_table(x, x, rel_tol = c(0, 1)), class = "daffiz_error_input")
  expect_error(diff_table(x, x, rel_tol = Inf), class = "daffiz_error_input")
  expect_error(diff_table(x, x, duplicates = "aggregate", rel_tol = 0.1), "rel_tol", class = "daffiz_error_input")
})

test_that("diff_summary and diff_report show the largest relative difference", {
  x <- data.table(id = 1:2, v = c(10, 4))
  y <- data.table(id = 1:2, v = c(11, 2))
  expect_identical(diff_summary(diff_table(x, y))$max_rel_diff, 0.5)
  r <- format(diff_report(diff_table(x, y, rel_tol = 0.2)), unicode = FALSE)
  expect_match(r, "Tolerance: 0 absolute, 0.2 relative.", fixed = TRUE, all = FALSE)
  expect_match(r, "Max rel diff", fixed = TRUE, all = FALSE)
})

test_that("expect_table_equal passes rel_tol on", {
  expected <- data.frame(id = 1:2, v = c(1e6, 1))
  actual <- data.frame(id = 1:2, v = c(1e6 + 1, 1))
  expect_success(expect_table_equal(actual, expected, rel_tol = 1e-5))
  expect_failure(expect_table_equal(actual, expected))
})

test_that("expect_table_equal allows floating-point noise by default, not more", {
  expected <- data.frame(id = 1:2, v = c(0.3, 1e6))
  expect_success(expect_table_equal(data.frame(id = 1:2, v = c(0.1 + 0.2, 1e6)), expected))
  expect_failure(expect_table_equal(data.frame(id = 1:2, v = c(0.1 + 0.2, 1e6)), expected, rel_tol = 0))
  # 1e-6 relative: real, and caught.
  expect_failure(expect_table_equal(data.frame(id = 1:2, v = c(0.3, 1e6 + 1)), expected))
  d <- expect_table_equal(expected, expected)
  expect_identical(attr(d, "tolerance")$rel, sqrt(.Machine$double.eps))
})

test_that("expect_table_equal's default steps aside for duplicates = \"aggregate\"", {
  expected <- data.table(id = c("a", "a"), v = c(1, 2))
  d <- expect_table_equal(expected[2:1], expected, duplicates = "aggregate")
  expect_identical(attr(d, "tolerance")$rel, 0)
  expect_error(
    expect_table_equal(expected, expected, duplicates = "aggregate", rel_tol = 0.1),
    class = "daffiz_error_input"
  )
})
