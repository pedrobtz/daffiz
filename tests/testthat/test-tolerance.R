test_that("scalar tolerance applies to every measure", {
  x <- data.frame(id = 1:2, a = c(1, 1), b = c(1, 1))
  y <- data.frame(id = 1:2, a = c(1.05, 1), b = c(1.05, 1))
  expect_true(is_matching(compare_dt(x, y, abs_tol = 0.1)))
})

test_that("named tolerance applies only to its own column", {
  x <- data.frame(id = 1L, a = 1, b = 1)
  y <- data.frame(id = 1L, a = 1.05, b = 1.05)
  cells <- all_cells(compare_dt(x, y, abs_tol = c(.default = 0, a = 0.1)))
  expect_equal(as.character(cells$.match_type[cells$.metric == "a"]), "equal")
  expect_equal(as.character(cells$.match_type[cells$.metric == "b"]), "diff")
})

test_that(".default covers unspecified columns", {
  x <- data.frame(id = 1L, a = 1, b = 1)
  y <- data.frame(id = 1L, a = 1.05, b = 1.05)
  expect_true(is_matching(compare_dt(x, y, abs_tol = c(.default = 0.1))))
  expect_true(is_matching(
    compare_dt(x, y, abs_tol = c(.default = 0.1, a = 0.2))
  ))
})

test_that("named tolerance without .default falls back to zero", {
  x <- data.frame(id = 1L, a = 1, b = 1)
  y <- data.frame(id = 1L, a = 1.05, b = 1.05)
  cells <- all_cells(compare_dt(x, y, abs_tol = c(a = 0.1)))
  expect_equal(as.character(cells$.match_type[cells$.metric == "b"]), "diff")
})

test_that("tolerance naming a non-measure is an error", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  err <- tryCatch(compare_dt(x, x, abs_tol = c(.default = 0, id = 1)),
                  daffiz_error_tolerance = identity)
  expect_s3_class(err, "daffiz_error_tolerance")
  expect_equal(err$columns, "id")
  expect_equal(err$argument, "abs_tol")
})

test_that("negative, non-finite and multi-valued unnamed tolerances are errors", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  expect_error(compare_dt(x, x, abs_tol = -1), class = "daffiz_error_tolerance")
  expect_error(compare_dt(x, x, abs_tol = NA_real_), class = "daffiz_error_tolerance")
  expect_error(compare_dt(x, x, abs_tol = Inf), class = "daffiz_error_tolerance")
  expect_error(compare_dt(x, x, rel_tol = c(1, 2)), class = "daffiz_error_tolerance")
})

test_that("resolved tolerances are recorded per measure", {
  x <- data.frame(id = 1L, a = 1, b = 1)
  cmp <- compare_dt(x, x, abs_tol = c(.default = 0.5, a = 0.1))
  expect_equal(cmp$settings$abs_tol, c(a = 0.1, b = 0.5))
})
