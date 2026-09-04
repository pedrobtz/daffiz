# Regression tests for defects found in review. Each test names the behaviour
# that was wrong, so a re-introduction fails with a readable message.

test_that("magnitude statistics count pairs whose subtraction overflows", {
  # Both operands are finite, so the pair is comparable; only the derived
  # difference overflows. Filtering on `.diff` dropped it, and `max_abs_diff`
  # reported 0 for the largest difference in the table.
  x <- data.frame(id = 1:2, a = c(1e308, 1))
  y <- data.frame(id = 1:2, a = c(-1e308, 1))
  cmp <- compare_dt(x, y)

  cells <- all_cells(cmp)
  expect_equal(as.character(cells$.match_type), c("diff", "equal"))
  expect_true(is.infinite(cells$.abs_diff[1L]))

  cs <- column_summary(cmp)
  expect_true(is.infinite(cs$max_abs_diff))
  expect_true(is.infinite(cs$mean_abs_diff))
  expect_true(is.infinite(cs$rmse))
})

test_that("an overflowing difference outranks a small one", {
  x <- data.frame(id = 1:2, a = c(1e308, 1))
  y <- data.frame(id = 1:2, a = c(-1e308, 2))
  rows <- diff_rows(compare_dt(x, y))
  expect_equal(rows$.row_x, c(1L, 2L))
  expect_true(is.infinite(rows$max_abs_diff[1L]))
})

test_that("rmse does not overflow for representable differences", {
  x <- data.frame(id = 1L, a = 0)
  y <- data.frame(id = 1L, a = 1e160)
  cs <- column_summary(compare_dt(x, y))
  expect_equal(cs$rmse, 1e160)
  expect_equal(cs$max_abs_diff, 1e160)
})

test_that("non-finite source values are still excluded from magnitudes", {
  x <- data.frame(id = 1:2, a = c(Inf, 4))
  y <- data.frame(id = 1:2, a = c(1, 1))
  cs <- column_summary(compare_dt(x, y))
  expect_equal(cs$max_abs_diff, 3)
  expect_equal(cs$rmse, 3)
})

test_that("role inference reads both inputs and stays symmetric", {
  # `v` is integer in one input and double in the other. Inferring from `x`
  # alone made this error one way round and compare fine the other.
  xi <- data.frame(id = 1:3, v = 1:3)
  yd <- data.frame(id = 1:3, v = c(1, 2, 9))

  # `v` is the only numeric column, so gate 8 reports it.
  forward <- tryCatch(compare_dt(xi, yd), daffiz_error = identity)
  reverse <- tryCatch(compare_dt(yd, xi), daffiz_error = identity)
  expect_s3_class(forward, "daffiz_error_roles")
  expect_s3_class(reverse, "daffiz_error_roles")
  expect_equal(forward$columns, "v")
  expect_equal(class(forward), class(reverse))

  # The message points at the actual remedy rather than a phantom class change.
  expect_match(conditionMessage(forward), "integer on one side and double")
  expect_match(conditionMessage(forward), "compare=")

  # And the suggested remedy works, in both directions.
  expect_equal(nrow(all_cells(compare_dt(xi, yd, compare = "v"))), 3L)
  expect_equal(
    diff_indices(compare_dt(xi, yd, compare = "v"), "x"),
    diff_indices(compare_dt(yd, xi, compare = "v"), "y")
  )
})

test_that("a skewed column beside a real measure is reported by gate 6", {
  # Here `w` is a shared double, so `v` survives as an identity column and the
  # type gate is the one that fires -- in both argument orders.
  xi <- data.frame(id = 1:3, v = 1:3, w = c(1, 2, 3))
  yd <- data.frame(id = 1:3, v = c(1, 2, 3), w = c(1, 2, 3))

  forward <- tryCatch(compare_dt(xi, yd), daffiz_error = identity)
  reverse <- tryCatch(compare_dt(yd, xi), daffiz_error = identity)
  expect_s3_class(forward, "daffiz_error_types")
  expect_equal(class(forward), class(reverse))
  expect_equal(forward$columns, "v")
  expect_match(conditionMessage(forward), "integer on one side and double")
  expect_true(is_matching(compare_dt(xi, yd, compare = c("v", "w"))))
})

test_that("shared doubles are still inferred as measures", {
  x <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  cmp <- compare_dt(x, x)
  expect_setequal(cmp$settings$compare, c("a", "b"))
  expect_equal(cmp$settings$by, "id")
})

test_that("identity columns of an unjoinable type are rejected by preflight", {
  # `is.atomic()` is TRUE for complex and raw, so an atomic test is not enough;
  # all three reached the grouped duplicate scan and failed with a raw
  # data.table message rather than a daffiz condition.
  unsupported <- list(
    list = list(1, 2),
    complex = complex(real = 1:2, imaginary = 0),
    raw = as.raw(1:2)
  )
  for (nm in names(unsupported)) {
    x <- data.frame(id = 1:2, a = c(1, 2))
    x$k <- unsupported[[nm]]
    err <- tryCatch(compare_dt(x, x), daffiz_error = identity, error = identity)
    expect_s3_class(err, "daffiz_error_types")
    expect_equal(err$columns, "k")
    expect_equal(err$role, "by")
    expect_match(conditionMessage(err), "cannot align rows")

    # Excluding it makes the comparison work.
    expect_true(is_matching(compare_dt(x, x, exclude = "k")))
  }
})

test_that("supported identity classes still pass the type gate", {
  x <- data.frame(
    lgl = c(TRUE, FALSE),
    int = 1:2,
    chr = c("a", "b"),
    dbl_id = c(1.5, 2.5),
    fct = factor(c("a", "b")),
    dte = as.Date("2020-01-01") + 0:1,
    tim = as.POSIXct("2020-01-01", tz = "UTC") + 0:1,
    v = c(1, 2)
  )
  cmp <- compare_dt(x, x, by = setdiff(names(x), "v"), compare = "v")
  expect_true(is_matching(cmp))
})

test_that("a repeated role name is rejected instead of multiplying cells", {
  # `compare = c("a", "a")` melted the measure twice on each side; the join
  # went cartesian and reported 12 cells and two identical column_summary rows
  # for a three-row, one-measure comparison.
  x <- data.frame(id = 1:3, a = c(1, 2, 3), b = c(4, 5, 6))
  y <- data.frame(id = 1:3, a = c(1, 2, 9), b = c(4, 5, 6))

  err <- tryCatch(compare_dt(x, y, compare = c("a", "a")),
                  daffiz_error = identity)
  expect_s3_class(err, "daffiz_error_roles")
  expect_equal(err$columns, "a")
  expect_equal(err$role, "compare")

  err <- tryCatch(compare_dt(x, y, by = c("id", "id")), daffiz_error = identity)
  expect_s3_class(err, "daffiz_error_roles")
  expect_equal(err$role, "by")

  # Naming each column once still works, and reports one cell per measure.
  cmp <- compare_dt(x, y, by = "id", compare = c("a", "b"))
  expect_equal(nrow(all_cells(cmp)), 6L)
  expect_equal(nrow(column_summary(cmp)), 2L)
})

test_that("identity columns must agree on join-relevant attributes", {
  # 60 secs and 1 min are the same duration, but difftime stores the raw
  # number and the join compares that. Type and class are identical, so the
  # signature had nothing to catch it and every row reported as side-only.
  x <- data.frame(id = 1:2, v = c(1, 2))
  x$d <- as.difftime(c(60, 120), units = "secs")
  y <- data.frame(id = 1:2, v = c(1, 2))
  y$d <- as.difftime(c(1, 2), units = "mins")

  err <- tryCatch(compare_dt(x, y), daffiz_error = identity)
  expect_s3_class(err, "daffiz_error_types")
  expect_equal(err$columns, "d")
  expect_match(conditionMessage(err), "units")

  # Same units still compares.
  expect_true(is_matching(compare_dt(x, x)))
})

test_that("time zone is not treated as a join-relevant attribute", {
  # Two POSIXct columns in different zones hold the same epoch seconds, so
  # they align correctly; rejecting them would be a false alarm.
  x <- data.frame(id = 1:2, v = c(1, 2))
  x$t <- as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC")
  y <- x
  attr(y$t, "tzone") <- "America/New_York"
  expect_true(is_matching(compare_dt(x, y)))
})

test_that("key profiles keep missing identity values", {
  # `sort()` drops NA by default, so `values` disagreed with `n_distinct` and
  # an NA key -- a common reason rows fail to align -- went unreported.
  x <- data.frame(g = c("a", NA), id = 1:2, v = c(1, 2))
  y <- data.frame(g = c("z", "w"), id = 3:4, v = c(1, 2))
  profile <- key_profile(
    suppressWarnings(compare_dt(x, y, disjoint_keys = "warn")), "x"
  )

  expect_equal(profile$n_distinct, lengths(profile$values))
  g_values <- profile$values[[which(profile$column == "g")]]
  expect_true(anyNA(g_values))
  expect_equal(sum(!is.na(g_values)), 1L)
})

test_that("a reserved-named column can be excluded instead of renamed", {
  x <- data.frame(id = 1:2, .diff = c(1, 2), a = c(1, 2))
  expect_error(compare_dt(x, x), class = "daffiz_error_columns")
  cmp <- compare_dt(x, x, exclude = ".diff")
  expect_true(is_matching(cmp))
  expect_false(".diff" %in% cmp$settings$compare)
})

test_that("a repeated tolerance name is an error, not a silent choice", {
  x <- data.frame(id = 1:2, a = c(1, 2))
  y <- data.frame(id = 1:2, a = c(1, 2.5))
  err <- tryCatch(
    compare_dt(x, y, abs_tol = c(a = 10, a = 0)),
    daffiz_error_tolerance = identity
  )
  expect_s3_class(err, "daffiz_error_tolerance")
  expect_equal(err$columns, "a")
})

test_that("expect_dt_equal refuses to pass on ambiguous identities", {
  # Row for row these tables disagree; only heuristic pairing makes them match.
  x <- data.frame(id = c("k", "k"), amount = c(100, 200))
  y <- data.frame(id = c("k", "k"), amount = c(200, 100))

  expect_error(expect_dt_equal(x, y), class = "daffiz_error_duplicates")
  # The heuristic remains available when asked for explicitly.
  expect_true(suppressWarnings(
    is_matching(compare_dt(x, y, duplicate_keys = "pair"))
  ))
})

test_that("duplicate pairing does not depend on the order of compare", {
  x <- data.frame(id = c("k", "k"), a = c(1, 2), b = c(10, 20))
  y <- data.frame(id = c("k", "k"), a = c(2, 1), b = c(20, 10))
  ab <- suppressWarnings(compare_dt(x, y, compare = c("a", "b")))
  ba <- suppressWarnings(compare_dt(x, y, compare = c("b", "a")))
  expect_equal(
    all_cells(ab)[, .(id, .occurrence, .row_x, .row_y, .metric)],
    all_cells(ba)[, .(id, .occurrence, .row_x, .row_y, .metric)]
  )
})

test_that("plot() draws the map instead of dispatching to plot.default", {
  skip_if_not_installed("ggplot2")
  x <- data.frame(id = 1:3, amount = c(10, 20, 30))
  y <- data.frame(id = 1:3, amount = c(10, 25, 27))
  cmp <- compare_dt(x, y)

  # Without an explicit device `print()` opens the default one and leaves an
  # Rplots.pdf behind in the test directory, which then ships in the tarball.
  device <- tempfile(fileext = ".pdf")
  grDevices::pdf(device)
  on.exit({
    grDevices::dev.off()
    unlink(device)
  }, add = TRUE)

  out <- plot(cmp, columns = "amount")
  expect_s3_class(out, "ggplot")
})

test_that("all.equal ignores the accessor cache", {
  x <- data.frame(id = 1:2, amount = c(10, 20))
  y <- data.frame(id = 1:2, amount = c(10, 25))
  a <- compare_dt(x, y)
  b <- compare_dt(x, y)
  expect_true(isTRUE(all.equal(a, b)))

  # Touching a cached accessor must not change the answer.
  invisible(row_summary(a))
  invisible(column_summary(a))
  expect_true(isTRUE(all.equal(a, b)))

  different <- compare_dt(x, data.frame(id = 1:2, amount = c(10, 99)))
  expect_false(isTRUE(all.equal(a, different)))
  expect_type(all.equal(a, "not a comparison"), "character")
})

test_that("y-side recovery keeps the aligned column order", {
  x <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  y <- data.frame(b = c(3, 9), id = 1:2, a = c(1, 2))
  cmp <- compare_dt(x, y)
  expect_equal(names(original_rows(cmp, "y", "all")), c(names(x), ".row_y"))
  expect_equal(names(original_rows(cmp, "x", "all")), c(names(x), ".row_x"))
  # Values still belong to y, not x.
  expect_equal(original_rows(cmp, "y", "all")$b, c(3, 9))
})

test_that("format() is self-contained and does not build a summary", {
  x <- data.frame(id = 1:3, amount = c(10, 20, 30))
  y <- data.frame(id = 1:3, amount = c(10, 25, 27))
  cmp <- compare_dt(x, y)
  out <- format(cmp)
  expect_match(out[1L], "^<daffiz_comparison>")
  expect_true(any(grepl("2 cell\\(s\\) not equal", out)))
  # No accessor cache was populated by formatting.
  expect_length(ls(cmp$cache), 0L)
})

test_that("the affected-row summary agrees with the full row summary", {
  m <- mixed_comparison()
  full <- row_summary(m$cmp)[row_type != "equal"]
  data.table::setorderv(
    full,
    c("n_diff", "max_abs_diff", ".row_x", ".row_y"),
    order = c(-1L, -1L, 1L, 1L),
    na.last = TRUE
  )
  expect_equal(diff_rows(m$cmp), full)
})
