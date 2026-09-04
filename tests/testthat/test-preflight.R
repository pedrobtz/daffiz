test_that("non-table inputs are rejected", {
  expect_error(compare_dt(1:3, data.frame(v = 1)), class = "daffiz_error_input")
})

test_that("duplicate column names are rejected", {
  x <- data.frame(v = 1, v2 = 2)
  names(x) <- c("v", "v")
  expect_error(compare_dt(x, x), class = "daffiz_error_columns")
})

test_that("reserved result names are rejected", {
  x <- data.frame(id = 1L, .diff = 1)
  expect_error(compare_dt(x, x), class = "daffiz_error_columns")
})

test_that("reserved-name collision fails before melting", {
  # Also has duplicate identities; the name error must win.
  x <- data.frame(id = c(1L, 1L), .value_x = c(1, 2), v = c(1, 2))
  expect_error(
    expect_no_warning(compare_dt(x, x)),
    class = "daffiz_error_columns"
  )
})

test_that("differing column sets are an error naming both sides", {
  x <- data.frame(id = 1L, a = 1, b = 2)
  y <- data.frame(id = 1L, a = 1, c = 3)
  err <- tryCatch(compare_dt(x, y), daffiz_error_columns = identity)
  expect_s3_class(err, "daffiz_error_columns")
  expect_equal(err$only_x, "b")
  expect_equal(err$only_y, "c")
})

test_that("column-set mismatch fails before duplicate detection", {
  x <- data.frame(id = c(1L, 1L), a = c(1, 2), b = c(1, 2))
  y <- data.frame(id = c(1L, 1L), a = c(1, 2))
  expect_error(
    expect_no_warning(compare_dt(x, y)),
    class = "daffiz_error_columns"
  )
})

test_that("column order may differ", {
  x <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  y <- data.frame(b = c(3, 4), id = 1:2, a = c(1, 2))
  expect_true(is_matching(compare_dt(x, y)))
})

test_that("exclude drops columns and warns about unknown names", {
  x <- data.frame(id = 1:2, keep = c(1, 2), drop = c(1, 2))
  y <- data.frame(id = 1:2, keep = c(1, 2), drop = c(9, 9))
  expect_true(is_matching(compare_dt(x, y, exclude = "drop")))
  expect_warning(
    compare_dt(x, y, exclude = c("drop", "nope")),
    class = "daffiz_warning_exclude"
  )
})

test_that("identity type mismatch errors before melting", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  y <- data.frame(id = c("1", "2"), v = c(1, 2))
  err <- tryCatch(compare_dt(x, y), daffiz_error_types = identity)
  expect_s3_class(err, "daffiz_error_types")
  expect_equal(err$role, "by")
  expect_equal(err$columns, "id")
})

test_that("factor identity columns must share levels", {
  x <- data.frame(id = factor("a", levels = c("a", "b")), v = 1)
  y <- data.frame(id = factor("a", levels = c("a", "c")), v = 1)
  expect_error(compare_dt(x, y), class = "daffiz_error_types")
})

test_that("classed measures requested explicitly are rejected", {
  x <- data.frame(id = 1:2, d = as.Date("2020-01-01") + 0:1)
  expect_error(compare_dt(x, x, by = "id", compare = "d"),
               class = "daffiz_error_types")
})

test_that("identity-type mismatch fails before duplicate detection", {
  x <- data.frame(id = c(1L, 1L), v = c(1, 2))
  y <- data.frame(id = c("1", "1"), v = c(1, 2))
  expect_error(
    expect_no_warning(compare_dt(x, y)),
    class = "daffiz_error_types"
  )
})

test_that("empty inputs compare cleanly", {
  x <- data.frame(id = integer(), v = numeric())
  cmp <- compare_dt(x, x)
  expect_equal(nrow(all_cells(cmp)), 0L)
  expect_true(is_matching(cmp))
})

test_that("unusual but legal column names are supported", {
  x <- data.frame(`my id` = 1:2, `v 1` = c(1, 2), check.names = FALSE)
  y <- data.frame(`my id` = 1:2, `v 1` = c(1, 3), check.names = FALSE)
  cmp <- compare_dt(x, y)
  expect_equal(sum(all_cells(cmp)$.match_type == "diff"), 1L)
})

# Gate 10 -- alignment ---------------------------------------------------------

test_that("fully disjoint identities are an error, not a silent non-comparison", {
  # Every cell such a comparison could produce is x_only or y_only, so
  # `n_compared` is zero and the "differences" it reports are just two
  # unrelated tables. It used to return that result without a word.
  x <- data.frame(id = 1:4, v = as.double(1:4))
  y <- data.frame(id = 5:8, v = as.double(1:4))

  err <- tryCatch(compare_dt(x, y), daffiz_error = identity)
  expect_s3_class(err, "daffiz_error_disjoint")
  expect_equal(err$by, "id")
  expect_equal(err$n_row_x, 4L)
  expect_equal(err$n_row_y, 4L)
})

test_that("the disjoint error carries the keys needed to debug it", {
  x <- data.frame(id = c("a", "b"), v = c(1, 2))
  y <- data.frame(id = c("c", "d"), v = c(1, 2))
  message <- conditionMessage(tryCatch(compare_dt(x, y), daffiz_error = identity))

  expect_match(message, "No identity value appears in both inputs")
  expect_match(message, "only in `x`: a, b")
  expect_match(message, "only in `y`: c, d")
  # The two remedies that actually fix it.
  expect_match(message, "by=")
  expect_match(message, "daffiz_row_number\\(\\)")
})

test_that("a compound identity samples keys as tuples", {
  x <- data.frame(g = c("a", "a"), id = 1:2, v = c(1, 2))
  y <- data.frame(g = c("b", "b"), id = 3:4, v = c(1, 2))
  message <- conditionMessage(tryCatch(compare_dt(x, y), daffiz_error = identity))

  expect_match(message, "Identity: g, id")
  expect_match(message, "\\(a, 1\\), \\(a, 2\\)")
  expect_match(message, "\\(b, 3\\), \\(b, 4\\)")
})

test_that("an empty input on one side is named as such", {
  empty <- data.frame(id = integer(), v = numeric())
  populated <- data.frame(id = 1:3, v = c(1, 2, 3))

  # The message uses the captured argument labels, so it names the caller's own
  # variables rather than `x` and `y`.
  x_empty <- tryCatch(compare_dt(empty, populated), daffiz_error = identity)
  expect_s3_class(x_empty, "daffiz_error_disjoint")
  expect_match(
    conditionMessage(x_empty), "`empty` has no rows, while `populated` has 3"
  )
  expect_equal(x_empty$n_row_x, 0L)
  expect_equal(x_empty$n_row_y, 3L)

  y_empty <- tryCatch(compare_dt(populated, empty), daffiz_error = identity)
  expect_s3_class(y_empty, "daffiz_error_disjoint")
  expect_match(
    conditionMessage(y_empty), "`empty` has no rows, while `populated` has 3"
  )
  expect_equal(y_empty$n_row_x, 3L)
  expect_equal(y_empty$n_row_y, 0L)

  # Whichever side is empty, the message says so with explicit labels too.
  both <- tryCatch(
    compare_dt(populated, empty, x_name = "before", y_name = "after"),
    daffiz_error = identity
  )
  expect_match(conditionMessage(both), "`after` has no rows, while `before` has 3")
})

test_that("two empty inputs are equal rather than ill-formed", {
  # They align no rows either, but that is a correct answer, not a broken
  # question -- and the plot and accessor paths depend on it working.
  empty <- data.frame(id = integer(), v = numeric())
  cmp <- expect_no_condition(compare_dt(empty, empty))
  expect_true(is_matching(cmp))
  expect_equal(nrow(all_cells(cmp)), 0L)
})

test_that("disjoint_keys = 'warn' restores the previous behaviour", {
  x <- data.frame(id = 1:4, v = as.double(1:4))
  y <- data.frame(id = 5:8, v = as.double(1:4))

  expect_warning(
    cmp <- compare_dt(x, y, disjoint_keys = "warn"),
    class = "daffiz_warning_disjoint"
  )
  expect_equal(nrow(all_cells(cmp)), 8L)
  expect_false(is_matching(cmp))
  expect_equal(sum(column_summary(cmp)$n_compared), 0L)
  # The escape hatch must not re-suggest itself.
  w <- tryCatch(compare_dt(x, y, disjoint_keys = "warn"),
                daffiz_warning_disjoint = identity)
  expect_no_match(conditionMessage(w), "Set `disjoint_keys")
})

test_that("a single aligned row is enough to be a real comparison", {
  # The gate is zero-overlap, not low-overlap: one shared identity among many
  # unmatched rows is a legitimate comparison that x_only()/y_only() describe.
  x <- data.frame(id = 1:50, v = as.double(1:50))
  y <- data.frame(id = 50:99, v = as.double(1:50))

  cmp <- expect_no_condition(compare_dt(x, y))
  expect_equal(sum(column_summary(cmp)$n_compared), 1L)
})

test_that("the gate fires before the melt", {
  # The disjoint case builds the largest possible cell table, so the guard is
  # worth nothing if it runs afterwards. A size threshold of zero would warn
  # first if the melt were reached.
  x <- data.frame(id = 1:4, v = as.double(1:4))
  y <- data.frame(id = 5:8, v = as.double(1:4))
  opts <- options(daffiz.max_cells = 0)
  on.exit(options(opts), add = TRUE)

  expect_error(
    expect_no_warning(compare_dt(x, y)),
    class = "daffiz_error_disjoint"
  )
})

test_that("expect_dt_equal inherits the strict default", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  y <- data.frame(id = 3:4, v = c(1, 2))
  expect_error(expect_dt_equal(x, y), class = "daffiz_error_disjoint")
})

test_that("positional alignment is unaffected when both sides have rows", {
  x <- data.frame(id = 1:3, v = c(1, 2, 3))
  y <- data.frame(id = 7:9, v = c(1, 2, 3))
  cmp <- expect_no_condition(compare_dt(x, y, by = daffiz_row_number()))
  expect_true(is_matching(cmp))
})
