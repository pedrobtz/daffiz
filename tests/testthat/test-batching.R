batch_pair <- function() {
  x <- data.frame(
    id = 1:4,
    a = c(1, 2, NA, Inf),
    b = c(10, 20, 30, 40),
    c = c(100, 200, 300, 400),
    d = c(0.1, 0.2, 0.3, 0.4)
  )
  y <- data.frame(
    id = 2:5,
    a = c(2, NA, -Inf, 5),
    b = c(20, 31, 40, 50),
    c = c(200, 300, 405, 500),
    d = c(0.2, 0.3, 0.4, 0.5)
  )
  list(x = x, y = y)
}

test_that("batching preserves the complete canonical cell table", {
  pair <- batch_pair()
  unbatched <- compare_dt(pair$x, pair$y)
  for (size in c(1L, 2L, 3L, 4L, 20L)) {
    batched <- compare_dt(pair$x, pair$y, batch = size)
    expect_identical(all_cells(batched), all_cells(unbatched))
  }
})

test_that("batching preserves all Phase 2 summaries", {
  pair <- batch_pair()
  unbatched <- compare_dt(pair$x, pair$y)
  batched <- compare_dt(pair$x, pair$y, batch = 1L)
  expect_identical(column_summary(batched), column_summary(unbatched))
  expect_identical(row_summary(batched), row_summary(unbatched))
  expect_identical(diff_columns(batched), diff_columns(unbatched))
  expect_identical(diff_rows(batched), diff_rows(unbatched))
  expect_identical(x_only(batched), x_only(unbatched))
  expect_identical(y_only(batched), y_only(unbatched))
})

test_that("batching preserves named tolerance classification", {
  pair <- batch_pair()
  tolerance <- c(.default = 0, b = 2, c = 10)
  unbatched <- compare_dt(pair$x, pair$y, abs_tol = tolerance)
  batched <- compare_dt(pair$x, pair$y, abs_tol = tolerance, batch = 1L)
  expect_identical(all_cells(batched), all_cells(unbatched))
  expect_identical(is_matching(batched), is_matching(unbatched))
})

test_that("batching preserves duplicate pairing", {
  x <- data.frame(
    key = c("a", "a", "b"),
    a = c(2, 1, 3),
    b = c(20, 10, 30),
    c = c(200, 100, 300)
  )
  y <- data.frame(
    key = c("a", "a", "b"),
    a = c(1, 4, 3),
    b = c(10, 40, 30),
    c = c(100, 400, 300)
  )
  unbatched <- suppressWarnings(compare_dt(x, y))
  batched <- suppressWarnings(compare_dt(x, y, batch = 1L))
  expect_identical(all_cells(batched), all_cells(unbatched))
  expect_identical(duplicate_info(batched), duplicate_info(unbatched))
})

test_that("batch validation is strict and records the resolved size", {
  x <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  expect_equal(compare_dt(x, x)$settings$batch, 2L)
  expect_equal(compare_dt(x, x, batch = 1)$settings$batch, 1L)
  expect_equal(compare_dt(x, x, batch = 100)$settings$batch, 2L)

  invalid <- list(0, -1, 1.5, NA_real_, Inf, "1", 1 + 0i, as.Date("2020-01-01"))
  for (value in invalid) {
    expect_error(compare_dt(x, x, batch = value), class = "daffiz_error_batch")
  }
})

test_that("batching handles empty inputs and does not mutate callers", {
  empty <- data.frame(id = integer(), a = numeric(), b = numeric())
  expect_identical(
    all_cells(compare_dt(empty, empty, batch = 1L)),
    all_cells(compare_dt(empty, empty))
  )

  pair <- batch_pair()
  x <- data.table::as.data.table(pair$x)
  y <- data.table::as.data.table(pair$y)
  before_x <- copy(x)
  before_y <- copy(y)
  compare_dt(x, y, batch = 1L)
  expect_identical(x, before_x)
  expect_identical(y, before_y)
})

test_that("reports disclose non-default batching", {
  x <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  text <- paste(capture.output(print(compare_dt(x, x, batch = 1L))), collapse = "\n")
  expect_match(text, "Measure batch size: 1")
})
