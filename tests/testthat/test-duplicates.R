dup_pair <- function() {
  x <- data.frame(k = c("a", "a", "b"), v = c(1, 2, 3))
  y <- data.frame(k = c("a", "a", "b"), v = c(1, 2, 3))
  list(x = x, y = y)
}

test_that("duplicate identities warn and pair by default", {
  p <- dup_pair()
  expect_warning(cmp <- compare_dt(p$x, p$y), class = "daffiz_warning_duplicates")
  expect_true(is_matching(cmp))
  expect_true(".occurrence" %in% names(all_cells(cmp)))
  expect_equal(nrow(duplicate_info(cmp)), 1L)
})

test_that("duplicate_keys = 'error' stops before comparison", {
  p <- dup_pair()
  err <- tryCatch(compare_dt(p$x, p$y, duplicate_keys = "error"),
                  daffiz_error_duplicates = identity)
  expect_s3_class(err, "daffiz_error_duplicates")
  expect_equal(err$by, "k")
})

test_that("duplicate_keys = 'report' excludes ambiguous groups", {
  p <- dup_pair()
  expect_warning(
    cmp <- compare_dt(p$x, p$y, duplicate_keys = "report"),
    class = "daffiz_warning_duplicates"
  )
  cells <- all_cells(cmp)
  expect_equal(nrow(cells), 1L)
  expect_equal(cells$k, "b")
  expect_false(".occurrence" %in% names(cells))
  expect_equal(nrow(duplicate_info(cmp)), 1L)
})

test_that("no .occurrence column when identities are unique", {
  x <- data.frame(id = 1:3, v = c(1, 2, 3))
  expect_false(".occurrence" %in% names(all_cells(compare_dt(x, x))))
})

test_that("pairing is deterministic and does not reorder row indices", {
  x <- data.frame(k = c("a", "a"), v = c(2, 1))
  y <- data.frame(k = c("a", "a"), v = c(1, 2))
  suppressWarnings(cmp <- compare_dt(x, y))
  cells <- all_cells(cmp)
  # Sorted pairing makes both sides match, but the recorded source rows differ.
  expect_true(is_matching(cmp))
  expect_setequal(cells$.row_x, c(1L, 2L))
  expect_setequal(cells$.row_y, c(1L, 2L))
  expect_false(identical(cells$.row_x, cells$.row_y))
})

test_that("duplicate counts are recorded per side", {
  x <- data.frame(k = c("a", "a", "a"), v = c(1, 2, 3))
  y <- data.frame(k = c("a", "a"), v = c(1, 2))
  suppressWarnings(cmp <- compare_dt(x, y))
  info <- duplicate_info(cmp)
  expect_equal(info$n_x, 3L)
  expect_equal(info$n_y, 2L)
})
