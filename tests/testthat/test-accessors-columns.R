test_that("diff_cells returns every non-equal classification", {
  m <- mixed_comparison()
  cells <- diff_cells(m$cmp)
  expect_false(any(cells$.match_type == "equal"))
  expect_equal(nrow(cells), 5L)
  expect_equal(sum(cells$.match_type == "diff"), 1L)
  expect_equal(sum(cells$.match_type == "x_only"), 2L)
  expect_equal(sum(cells$.match_type == "y_only"), 2L)
})

test_that("diff_cells filters measures and validates names", {
  m <- mixed_comparison()
  cells <- diff_cells(m$cmp, "a")
  expect_equal(unique(cells$.metric), "a")
  expect_equal(nrow(cells), 3L)
  expect_error(
    diff_cells(m$cmp, "missing"),
    class = "daffiz_error_columns"
  )
  expect_error(diff_cells(m$cmp, NA_character_), class = "daffiz_error_columns")
})

test_that("diff_cells returns a copy", {
  m <- mixed_comparison()
  cells <- diff_cells(m$cmp)
  cells[, .value_x := 999]
  expect_false(any(diff_cells(m$cmp)$.value_x == 999, na.rm = TRUE))
})

test_that("column_summary contains exact counts and statistics", {
  m <- mixed_comparison()
  out <- column_summary(m$cmp)

  expect_named(out, c(
    "column", "n_cells", "n_compared", "n_equal", "n_diff", "n_x_only",
    "n_y_only", "fraction_diff", "mean_diff", "mean_abs_diff", "rmse",
    "max_abs_diff", "p95_abs_diff"
  ))
  expect_equal(out$column, c("a", "b"))

  a <- out[column == "a"]
  expect_equal(a$n_cells, 4L)
  expect_equal(a$n_compared, 2L)
  expect_equal(a$n_equal, 1L)
  expect_equal(a$n_diff, 1L)
  expect_equal(a$n_x_only, 1L)
  expect_equal(a$n_y_only, 1L)
  expect_equal(a$fraction_diff, 0.5)
  expect_equal(a$mean_diff, -0.5)
  expect_equal(a$mean_abs_diff, 0.5)
  expect_equal(a$rmse, sqrt(0.5))
  expect_equal(a$max_abs_diff, 1)
  expect_equal(a$p95_abs_diff, 0.95)

  b <- out[column == "b"]
  expect_equal(b$n_equal, 2L)
  expect_equal(b$n_diff, 0L)
  expect_equal(b$fraction_diff, 0)
  expect_equal(b$mean_diff, 0)
})

test_that("column summaries reconcile with canonical cells", {
  m <- mixed_comparison()
  summary <- column_summary(m$cmp)
  cells <- all_cells(m$cmp)
  expect_equal(
    sum(summary$n_diff),
    sum(cells$.match_type == "diff")
  )

  rebuilt <- diff_cells(m$cmp)[, list(
    n_diff = sum(.match_type == "diff"),
    n_x_only = sum(.match_type == "x_only"),
    n_y_only = sum(.match_type == "y_only")
  ), by = .metric]
  for (nm in rebuilt$.metric) {
    expected <- rebuilt[.metric == nm]
    actual <- summary[column == nm]
    expect_equal(actual$n_diff, expected$n_diff)
    expect_equal(actual$n_x_only, expected$n_x_only)
    expect_equal(actual$n_y_only, expected$n_y_only)
  }
})

test_that("column_summary covers measures for empty inputs", {
  x <- data.frame(id = integer(), a = numeric(), b = numeric())
  out <- column_summary(compare_dt(x, x))
  expect_equal(out$column, c("a", "b"))
  expect_true(all(out$n_cells == 0L))
  expect_true(all(is.na(out$fraction_diff)))
  expect_true(all(is.na(out$max_abs_diff)))
})

test_that("non-finite differences are excluded from magnitude statistics", {
  x <- data.frame(id = 1:3, a = c(Inf, NA, 2))
  y <- data.frame(id = 1:3, a = c(-Inf, NA, 3))
  out <- column_summary(compare_dt(x, y))
  expect_equal(out$n_compared, 3L)
  expect_equal(out$n_equal, 1L)
  expect_equal(out$n_diff, 2L)
  expect_equal(out$mean_diff, -1)
  expect_equal(out$mean_abs_diff, 1)
  expect_equal(out$max_abs_diff, 1)
})

test_that("diff_columns filters and ranks affected measures", {
  m <- mixed_comparison()
  out <- diff_columns(m$cmp)
  expect_equal(out$column, c("a", "b"))

  equal <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  expect_equal(nrow(diff_columns(compare_dt(equal, equal))), 0L)
})

test_that("cached column summaries are protected by copies", {
  m <- mixed_comparison()
  out <- column_summary(m$cmp)
  out[, n_diff := 999L]
  expect_false(any(column_summary(m$cmp)$n_diff == 999L))
})
