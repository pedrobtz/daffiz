test_that("reported duplicate groups make the result incomplete and non-matching", {
  x <- data.frame(k = c("a", "a"), value = c(1, 2))
  cmp <- suppressWarnings(compare_dt(x, x, duplicate_keys = "report"))
  expect_false(is_matching(cmp))
  expect_match(paste(format(cmp), collapse = "\n"), "incomplete")
  expect_match(paste(capture.output(print(cmp)), collapse = "\n"), "were not compared")
})

test_that("summary counts are backed by public accessors", {
  m <- mixed_comparison()
  out <- summary(m$cmp, max_columns = 1L, max_rows = 2L, max_cells = 3L)
  expect_s3_class(out, "summary.daffiz_comparison")
  expect_equal(out$n_non_equal_cells, nrow(diff_cells(m$cmp)))
  expect_equal(out$n_affected_rows, nrow(diff_rows(m$cmp)))
  expect_equal(out$n_affected_columns, nrow(diff_columns(m$cmp)))
  expect_equal(out$n_x_only_rows, nrow(x_only(m$cmp)))
  expect_equal(out$n_y_only_rows, nrow(y_only(m$cmp)))
  expect_equal(nrow(out$columns), 1L)
  expect_equal(nrow(out$rows), 2L)
  expect_equal(nrow(out$cells), 3L)
})

test_that("summary validates diagnostic limits", {
  m <- mixed_comparison()
  expect_error(summary(m$cmp, max_rows = -1), class = "daffiz_error_report")
  expect_error(summary(m$cmp, max_cells = 1.5), class = "daffiz_error_report")
  expect_equal(nrow(summary(m$cmp, max_cells = 0)$cells), 0L)
})

test_that("printed report names roles tolerances and unmatched profiles", {
  m <- mixed_comparison()
  text <- paste(capture.output(print(m$cmp)), collapse = "\n")
  expect_match(text, "Identity \\(inferred\\): id")
  expect_match(text, "Measures \\(inferred\\): a, b")
  expect_match(text, "Tolerance: abs 0 \\| rel 0")
  expect_match(text, "Unmatched rows: 1 only in x \\| 1 only in y")
  expect_match(text, "Leading x key profile: id")
  expect_match(text, "Measures with differences")
  expect_match(text, "Rows with differences")
  expect_match(text, "Example cells")
})

test_that("printed report distinguishes explicit role selection", {
  x <- data.frame(id = 1:2, value = c(1, 2), ignored = c(3, 4))
  cmp <- compare_dt(x, x, by = "id", compare = "value")
  text <- paste(capture.output(print(cmp)), collapse = "\n")
  expect_match(text, "Identity \\(explicit\\): id")
  expect_match(text, "Measures \\(explicit\\): value")
  expect_match(text, "Ignored after role resolution: ignored")
})

test_that("expect_dt_equal succeeds invisibly for matching inputs", {
  x <- data.frame(id = 1:2, value = c(1, 2))
  expect_invisible(cmp <- expect_dt_equal(x, x))
  expect_s3_class(cmp, "daffiz_comparison")
})

test_that("expect_dt_equal emits deterministic bounded diagnostics", {
  m <- mixed_comparison()
  actual <- m$x
  expected <- m$y
  failure <- testthat::capture_expectation(
    expect_dt_equal(
      actual,
      expected,
      max_columns = 1L,
      max_rows = 1L,
      max_cells = 1L
    )
  )
  expect_s3_class(failure, "expectation_failure")
  expect_s3_class(attr(failure, "comparison"), "daffiz_comparison")
  expect_match(failure$message, "Tables do not match: actual -> expected")
  expect_match(failure$message, "Columns:")
  expect_match(failure$message, "Rows:")
  expect_match(failure$message, "Cells:")
  lines <- strsplit(failure$message, "\n", fixed = TRUE)[[1L]]
  expect_equal(sum(grepl("^  [ab]:", lines)), 1L)
  expect_equal(sum(grepl("^  \\.row_x=", lines)), 1L)
})

test_that("expectation failures carry duplicate policy", {
  x <- data.frame(k = c("a", "a"), value = c(1, 2))
  failure <- testthat::capture_expectation(
    suppressWarnings(expect_dt_equal(x, x, duplicate_keys = "report"))
  )
  expect_s3_class(failure, "expectation_failure")
  expect_match(failure$message, "incomplete")
  expect_match(failure$message, "policy=report")
})
