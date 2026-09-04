test_that("the map has one tile per source row and measure", {
  m <- mixed_comparison()
  data <- plot_data(m$cmp)

  # 4 aligned-or-side-only records x 2 measures.
  expect_equal(nrow(data), 8L)
  expect_false(any(data$binned))
  expect_setequal(unique(data$column), c("a", "b"))
  expect_equal(uniqueN(data$row_label), 4L)
  expect_true(all(data$n_cells == 1L))
})

test_that("tiles agree with the canonical cell table", {
  m <- mixed_comparison()
  data <- plot_data(m$cmp)
  cells <- all_cells(m$cmp)

  expect_equal(sum(!data$matched), sum(cells$.match_type != "equal"))
  expect_setequal(as.character(data$match_type), as.character(cells$.match_type))
  # `fraction_mismatched` is the fill; unbinned it must be exactly 0 or 1.
  expect_setequal(unique(data$fraction_mismatched), c(0, 1))
  expect_equal(data$matched, data$fraction_mismatched == 0)
})

test_that("rows keep original source order rather than being ranked", {
  # Row 4 is the only affected record. A ranked map would put it first; a
  # difference map must leave it where it is, or the position pattern is lost.
  x <- data.frame(id = 1:4, v = c(1, 2, 3, 4))
  y <- data.frame(id = 1:4, v = c(1, 2, 3, 99))
  data <- plot_data(compare_dt(x, y))

  expect_equal(data$row_rank, 1:4)
  expect_equal(data$.row_x, 1:4)
  expect_equal(which(!data$matched), 4L)
})

test_that("every cell class that is not equal reads as a mismatch", {
  m <- mixed_comparison()
  data <- plot_data(m$cmp)
  classes <- data[, .(matched = unique(matched)), by = match_type]

  expect_equal(
    classes[match_type == "equal"]$matched, TRUE
  )
  expect_true(all(classes[match_type != "equal"]$matched == FALSE))
})

test_that("each tile carries a mark so fill is never the only encoding", {
  m <- mixed_comparison()
  data <- plot_data(m$cmp)

  expect_equal(
    data$symbol,
    c(equal = "", diff = "!", x_only = "X", y_only = "Y")[
      as.character(data$match_type)
    ],
    ignore_attr = TRUE
  )
  # The three mismatch classes are separable without colour; the equal
  # majority is deliberately unmarked.
  expect_true(all(nzchar(data$symbol[!data$matched])))
  expect_true(all(!nzchar(data$symbol[data$matched])))
  expect_true(all(vapply(data$symbol, function(s) all(charToRaw(s) < as.raw(128L)),
                         logical(1L))))
})

test_that("binning aggregates every row instead of discarding any", {
  n <- 40L
  x <- data.frame(id = seq_len(n), v = as.double(seq_len(n)))
  y <- x
  y$v[1:10] <- y$v[1:10] + 1
  data <- plot_data(compare_dt(x, y), max_rows = 10L, row_bins = 4L)

  expect_true(all(data$binned))
  expect_equal(nrow(data), 4L)
  expect_equal(data$n_rows_total[1L], n)
  expect_equal(data$n_rows_shown[1L], 4L)
  # No row is dropped: the bins partition every record.
  expect_equal(sum(data$n_rows), n)
  expect_equal(sum(data$n_cells), n)
  expect_equal(sum(data$n_mismatched), 10L)
  # Bins are contiguous and cover 1..n exactly once.
  expect_equal(data$row_start, c(1L, 11L, 21L, 31L))
  expect_equal(data$row_end, c(10L, 20L, 30L, 40L))
  # The first bin is entirely mismatched, the rest entirely clean.
  expect_equal(data$fraction_mismatched, c(1, 0, 0, 0))
})

test_that("binned tiles blank the per-cell columns they cannot represent", {
  x <- data.frame(id = 1:10, v = as.double(1:10))
  y <- x
  y$v[1] <- 99
  data <- plot_data(compare_dt(x, y), max_rows = 2L, row_bins = 2L)

  expect_true(all(is.na(data$match_type)))
  expect_true(all(is.na(data$symbol)))
  expect_true(all(is.na(data$value_x)))
  expect_true(all(is.na(data$diff)))
  expect_true(all(data$n_cells > 1L))
})

test_that("max_rows = NULL or Inf never bins", {
  x <- data.frame(id = 1:30, v = as.double(1:30))
  expect_false(any(plot_data(compare_dt(x, x), max_rows = NULL)$binned))
  expect_false(any(plot_data(compare_dt(x, x), max_rows = Inf)$binned))
})

test_that("columns filters the map and keeps input column order", {
  x <- data.frame(id = 1:2, b = c(1, 2), a = c(3, 4))
  y <- data.frame(id = 1:2, b = c(1, 9), a = c(3, 9))
  cmp <- compare_dt(x, y)

  expect_equal(unique(plot_data(cmp)$column), c("b", "a"))
  expect_equal(unique(plot_data(cmp, columns = "a")$column), "a")
  # Requesting them out of input order still draws them in input order, so
  # adjacent measures stay adjacent on the axis.
  expect_equal(unique(plot_data(cmp, columns = c("a", "b"))$column), c("b", "a"))
})

test_that("an unknown column is an error", {
  m <- mixed_comparison()
  expect_error(plot_data(m$cmp, columns = "nope"), class = "daffiz_error_columns")
})

test_that("invalid limits are rejected", {
  m <- mixed_comparison()
  for (bad in list(0L, -1L, 2.5, NA_integer_, "2", c(1L, 2L))) {
    expect_error(
      plot_data(m$cmp, max_rows = bad),
      class = "daffiz_error_plot_limit"
    )
  }
})

test_that("an empty comparison returns the documented empty schema", {
  x <- data.frame(id = integer(), a = numeric())
  data <- plot_data(compare_dt(x, x))

  expect_equal(nrow(data), 0L)
  expect_true(all(
    c("row_rank", "row_label", "column", "match_type", "symbol", "matched",
      "fraction_mismatched", "binned") %in% names(data)
  ))
})

test_that("plot_data requires a comparison object", {
  expect_error(plot_data(data.frame(a = 1)), class = "daffiz_error_comparison")
})
