test_that("labels are captured from bare symbols", {
  baseline <- data.frame(id = 1L, v = 1)
  candidate <- data.frame(id = 1L, v = 1)
  cmp <- compare_dt(baseline, candidate)
  expect_equal(cmp$settings$x_name, "baseline")
  expect_equal(cmp$settings$y_name, "candidate")
})

test_that("non-symbol arguments fall back to x and y", {
  cmp <- compare_dt(data.frame(id = 1L, v = 1), data.frame(id = 1L, v = 1))
  expect_equal(cmp$settings$x_name, "x")
  expect_equal(cmp$settings$y_name, "y")
})

test_that("magrittr's dot falls back rather than becoming a label", {
  # `.` is itself a symbol, so it needs the explicit exclusion.
  expect_equal(capture_label(quote(.), "x"), "x")
  expect_equal(capture_label(quote(baseline), "x"), "baseline")
  expect_equal(capture_label(quote(f(a)), "x"), "x")
})

test_that("the native pipe still yields a useful label", {
  baseline <- data.frame(id = 1L, v = 1)
  candidate <- data.frame(id = 1L, v = 1)
  cmp <- baseline |> compare_dt(candidate)
  expect_equal(cmp$settings$x_name, "baseline")
})

test_that("explicit names win over capture", {
  baseline <- data.frame(id = 1L, v = 1)
  cmp <- compare_dt(baseline, baseline, x_name = "before", y_name = "after")
  expect_equal(cmp$settings$x_name, "before")
  expect_equal(cmp$settings$y_name, "after")
})

test_that("large comparisons warn but return an identical result", {
  x <- data.frame(id = 1:50, a = as.double(1:50), b = as.double(1:50))
  y <- data.frame(id = 1:50, a = as.double(1:50), b = as.double(1:50))

  quiet <- all_cells(compare_dt(x, y))
  withr_opt <- options(daffiz.max_cells = 10)
  on.exit(options(withr_opt), add = TRUE)

  expect_warning(cmp <- compare_dt(x, y), class = "daffiz_warning_size")
  expect_equal(all_cells(cmp), quiet)
})

test_that("the size warning is silent below the threshold", {
  x <- data.frame(id = 1:5, v = as.double(1:5))
  withr_opt <- options(daffiz.max_cells = 1e6)
  on.exit(options(withr_opt), add = TRUE)
  expect_no_warning(compare_dt(x, x))
})

test_that("the benchmark-backed default warns above ten million cells", {
  previous <- options(daffiz.max_cells = NULL)
  on.exit(options(previous), add = TRUE)

  expect_no_warning(warn_projected_size(1e6, 10, "x", "y"))
  expect_warning(
    warning <- warn_projected_size(1000001, 10, "x", "y"),
    class = "daffiz_warning_size"
  )
  expect_equal(warning, 10000010)
})

test_that("the projection survives an integer-overflowing cell count", {
  # `compare_dt()` passes integers (nrow() and length()), whose product
  # overflows .Machine$integer.max at about 2.1e9 cells. That produced NA and
  # then `if (NA)`, so the guard against oversized comparisons was the first
  # thing to fail on one. The earlier test passed doubles and never saw it.
  previous <- options(daffiz.max_cells = NULL)
  on.exit(options(previous), add = TRUE)

  expect_true(is.na(suppressWarnings(100000000L * 30L)))
  expect_warning(
    projected <- warn_projected_size(100000000L, 30L, "x", "y"),
    class = "daffiz_warning_size"
  )
  expect_equal(projected, 3e9)
  expect_false(is.na(projected))
})

test_that("the size projection counts aligned records, not the larger side", {
  # Disjoint identities: the outer join yields n_x + n_y records, so projecting
  # from max(n_x, n_y) under-counted by half.
  x <- data.frame(id = 1:4, v = as.double(1:4))
  y <- data.frame(id = 5:8, v = as.double(1:4))
  opts <- options(daffiz.max_cells = 6)
  on.exit(options(opts), add = TRUE)

  # Disjoint identities are an error by default (gate 10), so this projection
  # test has to opt into the comparison it is measuring.
  warning <- withCallingHandlers(
    tryCatch(
      compare_dt(x, y, disjoint_keys = "warn"),
      daffiz_warning_size = identity
    ),
    daffiz_warning_disjoint = function(w) invokeRestart("muffleWarning")
  )
  expect_s3_class(warning, "daffiz_warning_size")
  expect_equal(warning$projected, 8)
  expect_equal(
    nrow(all_cells(suppressWarnings(compare_dt(x, y, disjoint_keys = "warn")))),
    8L
  )
})

test_that("the projection accounts for the duplicate policy", {
  # Two rows share identity 1, so pairing produces 3 records, not 2 groups.
  x <- data.frame(id = c(1L, 1L, 2L), v = c(1, 2, 3))
  y <- data.frame(id = c(1L, 1L, 2L), v = c(1, 2, 3))
  opts <- options(daffiz.max_cells = 0)
  on.exit(options(opts), add = TRUE)

  captured <- NULL
  paired <- suppressWarnings(withCallingHandlers(
    compare_dt(x, y),
    daffiz_warning_size = function(w) captured <<- w
  ))
  expect_equal(captured$projected, 3)
  expect_equal(nrow(all_cells(paired)), 3L)

  # "report" drops the ambiguous group, so it projects fewer records.
  captured <- NULL
  reported <- suppressWarnings(withCallingHandlers(
    compare_dt(x, y, duplicate_keys = "report"),
    daffiz_warning_size = function(w) captured <<- w
  ))
  expect_equal(captured$projected, 1)
  expect_equal(nrow(all_cells(reported)), 1L)
})

test_that("print is stable and mentions the resolved roles", {
  baseline <- data.frame(id = 1:2, amount = c(1, 2))
  candidate <- data.frame(id = 1:2, amount = c(1, 3))
  out <- format(compare_dt(baseline, candidate))
  expect_match(out[1], "baseline -> candidate")
  expect_true(any(grepl("Identity: id", out)))
  expect_true(any(grepl("Measures: amount", out)))
  expect_true(any(grepl("1 cell\\(s\\) not equal", out)))
})
