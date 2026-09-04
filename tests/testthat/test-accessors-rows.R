test_that("row_summary returns one record per aligned or unmatched row", {
  m <- mixed_comparison()
  out <- row_summary(m$cmp)
  expect_equal(nrow(out), 4L)
  expect_equal(as.character(out$row_type), c("x_only", "equal", "diff", "y_only"))
  expect_equal(out$n_columns, rep(2L, 4L))
  expect_equal(out$n_diff, c(2L, 0L, 1L, 2L))
  expect_equal(out$diff_columns[[1L]], c("a", "b"))
  expect_equal(out$diff_columns[[2L]], character())
  expect_equal(out$diff_columns[[3L]], "a")
})

test_that("row summaries reconcile with diff cells", {
  m <- mixed_comparison()
  rows <- row_summary(m$cmp)
  expected <- diff_cells(m$cmp)[, list(n_diff = .N), by = c(".row_x", ".row_y")]
  actual <- rows[row_type != "equal", c(".row_x", ".row_y", "n_diff")]
  setorderv(expected, c(".row_x", ".row_y"), na.last = TRUE)
  setorderv(actual, c(".row_x", ".row_y"), na.last = TRUE)
  expect_equal(actual, expected)
})

test_that("diff_rows summary is filtered and ranked", {
  m <- mixed_comparison()
  out <- diff_rows(m$cmp)
  expect_false(any(out$row_type == "equal"))
  expect_equal(out$n_diff, c(2L, 2L, 1L))
  expect_equal(as.character(out$row_type), c("x_only", "y_only", "diff"))
})

test_that("diff indices are sorted unique original positions", {
  m <- mixed_comparison()
  expect_equal(diff_indices(m$cmp, "x"), c(1L, 3L))
  expect_equal(diff_indices(m$cmp, "y"), c(2L, 3L))
  expect_error(diff_indices(m$cmp, "z"), "arg")
})

test_that("original_rows preserves source column order classes and values", {
  x <- data.table::data.table(
    id = 1:3,
    group = factor(c("a", "b", "c")),
    event_date = as.Date("2020-01-01") + 0:2,
    amount = c(10, 20, 30)
  )
  y <- copy(x)
  y[2L, amount := 25]
  cmp <- compare_dt(x, y)

  out <- original_rows(cmp, "x")
  expected <- copy(x[2L])
  expected[, .row_x := 2L]
  expect_equal(out, expected)
  expect_equal(names(out), c(names(x), ".row_x"))
  expect_s3_class(out$group, "factor")
  expect_s3_class(out$event_date, "Date")
})

test_that("original_rows supports changed only and all", {
  m <- mixed_comparison()
  expect_equal(original_rows(m$cmp, "x", "changed")$.row_x, 3L)
  expect_equal(original_rows(m$cmp, "x", "only")$.row_x, 1L)
  expect_equal(original_rows(m$cmp, "y", "only")$.row_y, 3L)
  expect_equal(original_rows(m$cmp, "x", "all")$.row_x, 1:3)
})

test_that("side-only accessors deduplicate melted measures", {
  m <- mixed_comparison()
  expect_equal(nrow(x_only(m$cmp)), 1L)
  expect_equal(nrow(y_only(m$cmp)), 1L)
  expect_equal(x_only(m$cmp)$id, 1L)
  expect_equal(y_only(m$cmp)$id, 4L)
})

test_that("side views recover affected original rows", {
  m <- mixed_comparison()
  expect_equal(diff_rows(m$cmp, "x"), original_rows(m$cmp, "x"))
  expect_equal(diff_rows(m$cmp, "y"), original_rows(m$cmp, "y"))
})

test_that("paired view includes both source values", {
  m <- mixed_comparison()
  out <- diff_rows(m$cmp, "paired")
  expect_true(all(c("a_x", "a_y", "b_x", "b_y") %in% names(out)))
  changed <- out[row_type == "diff"]
  expect_equal(changed$.row_x, 3L)
  expect_equal(changed$.row_y, 2L)
  expect_equal(changed$a_x, 3)
  expect_equal(changed$a_y, 4)
})

test_that("row accessors preserve duplicate occurrence pairing", {
  x <- data.frame(k = c("a", "a"), value = c(2, 1))
  y <- data.frame(k = c("a", "a"), value = c(1, 3))
  cmp <- suppressWarnings(compare_dt(x, y))
  out <- row_summary(cmp)
  expect_true(".occurrence" %in% names(out))
  expect_equal(nrow(out), 2L)
  expect_equal(sum(out$row_type == "diff"), 1L)
})

test_that("row_summary covers empty comparisons", {
  x <- data.frame(id = integer(), value = numeric())
  out <- row_summary(compare_dt(x, x))
  expect_equal(nrow(out), 0L)
  expect_named(out, c(
    "id", ".row_x", ".row_y", "row_type", "n_columns", "n_diff",
    "diff_columns", "mean_abs_diff", "max_abs_diff"
  ))
})

test_that("key_profile ranks identity columns on unmatched rows", {
  x <- data.frame(
    group = c("a", "a", "b"),
    id = 1:3,
    value = c(1, 2, 3)
  )
  y <- data.frame(
    group = c("c", "c", "b"),
    id = 1:3,
    value = c(1, 2, 3)
  )
  cmp <- compare_dt(x, y)
  profile <- key_profile(cmp, "x")
  expect_equal(profile$column, c("group", "id"))
  expect_equal(profile$n_distinct, c(1L, 2L))
  expect_equal(profile$values[[1L]], "a")
  expect_equal(key_profile(cmp, "y")$values[[1L]], "c")
})

test_that("key_profile is empty without side-only rows and returns copies", {
  x <- data.frame(id = 1:2, value = c(1, 2))
  cmp <- compare_dt(x, x)
  expect_equal(nrow(key_profile(cmp, "x")), 0L)

  m <- mixed_comparison()
  profile <- key_profile(m$cmp, "x")
  profile[, n_distinct := 999L]
  expect_false(any(key_profile(m$cmp, "x")$n_distinct == 999L))
})
