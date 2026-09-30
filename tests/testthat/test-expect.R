test_that("expect_table_equal passes on matching tables, casting to the benchmark", {
  expected <- data.frame(id = 1:3, amount = c(10, 20, 30))
  actual <- data.frame(ID = c("1", "2", "3"), Amount = c(10, 20, 30 + 1e-9))
  expect_success(expect_table_equal(actual, expected, tolerance = 1e-6))
  d <- expect_table_equal(actual, expected, tolerance = 1e-6)
  expect_identical(attr(d, "shape"), "cells")
})

test_that("expect_table_equal fails with a summary and the records that differ", {
  expected <- data.frame(id = 1:3, amount = c(10, 20, 30))
  actual <- data.frame(id = 1:3, amount = c(10, 21, 30))
  expect_failure(expect_table_equal(actual, expected), "`actual` does not match `expected`: 1 of 3")
  expect_failure(expect_table_equal(actual, expected), "changed")
})

test_that("expect_table_equal treats expected as the benchmark", {
  expected <- data.frame(id = 1:2, v = c(1, 2))
  actual <- data.frame(id = 1:2, v = c(1, 2), extra = "e")
  expect_success(suppressMessages(expect_table_equal(actual, expected)))
  # Reversed, the benchmark has a column the tested table lacks.
  expect_error(expect_table_equal(expected, actual), class = "daffiz_error_columns")
  # As peers, the extra column is a difference.
  expect_error(expect_table_equal(actual, expected, mode = "equal"), class = "daffiz_error_columns")
})

test_that("expect_table_equal does not pair duplicate keys unless asked", {
  expected <- data.table(id = c("a", "a"), v = c(1, 2))
  actual <- data.table(id = c("a", "a"), v = c(2, 1))
  expect_error(expect_table_equal(actual, expected), "does not pair duplicate keys", class = "daffiz_error_duplicates")
  expect_success(expect_table_equal(actual, expected, duplicates = "aggregate"))
  expect_failure(suppressWarnings(expect_table_equal(actual, expected, duplicates = "disambiguate")))
})

test_that("without measures, identical duplicate rows need no permission", {
  expected <- data.table(k = c("a", "a", "b"))
  expect_success(suppressMessages(expect_table_equal(expected[c(2, 3, 1)], expected)))
  expect_failure(suppressMessages(expect_table_equal(expected[c(1, 3)], expected)))
})

test_that("the failure carries the comparison", {
  expected <- data.frame(id = 1:2, v = c(1, 2))
  actual <- data.frame(id = 1:2, v = c(1, 3))
  cnd <- tryCatch(expect_table_equal(actual, expected), expectation = function(e) e)
  expect_s3_class(attr(cnd, "diff"), "data.table")
  expect_identical(as.character(attr(cnd, "diff")$status), c("same", "changed"))
})
