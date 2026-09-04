test_that("defaults infer unclassed doubles as measures", {
  fx <- make_fixture(20L)
  cmp <- compare_dt(fx, copy(fx))
  expect_setequal(cmp$settings$compare, c("amount", "score", "ratio"))
  expect_true(all(c("code", "category", "grade", "event_date", "seen_at",
                    "is_active", "age") %in% cmp$settings$by))
})

test_that("Date and POSIXct are identity, not measures", {
  fx <- make_fixture(10L)
  cmp <- compare_dt(fx, copy(fx))
  expect_false("event_date" %in% cmp$settings$compare)
  expect_false("seen_at" %in% cmp$settings$compare)
  expect_true(all(c("event_date", "seen_at") %in% cmp$settings$by))
})

test_that("explicit by leaves remaining doubles as measures", {
  fx <- make_fixture(10L)
  cmp <- compare_dt(fx, copy(fx), by = "code")
  expect_equal(cmp$settings$by, "code")
  expect_setequal(cmp$settings$compare, c("amount", "score", "ratio"))
})

test_that("explicit compare never promotes an unselected double to identity", {
  fx <- make_fixture(10L)
  cmp <- compare_dt(fx, copy(fx), compare = "amount")
  expect_equal(cmp$settings$compare, "amount")
  expect_false("score" %in% cmp$settings$by)
  expect_true("score" %in% cmp$settings$ignored)
})

test_that("both explicit uses exactly the requested sets", {
  fx <- make_fixture(10L)
  cmp <- compare_dt(fx, copy(fx), by = "code", compare = "ratio")
  expect_equal(cmp$settings$by, "code")
  expect_equal(cmp$settings$compare, "ratio")
})

test_that("overlapping by and compare is an error", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  expect_error(compare_dt(x, x, by = "v", compare = "v"),
               class = "daffiz_error_roles")
})

test_that("unknown requested columns are an error", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  expect_error(compare_dt(x, x, by = "nope"), class = "daffiz_error_roles")
  expect_error(compare_dt(x, x, compare = "nope"), class = "daffiz_error_roles")
})

test_that("a column excluded then requested is an error", {
  x <- data.frame(id = 1:2, v = c(1, 2), w = c(1, 2))
  expect_error(compare_dt(x, x, exclude = "w", compare = "w"),
               class = "daffiz_error_roles")
})

test_that("no measures or no identity is an error", {
  no_measure <- data.frame(id = 1:2, tag = c("a", "b"))
  expect_error(compare_dt(no_measure, no_measure), class = "daffiz_error_roles")

  all_double <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_error(compare_dt(all_double, all_double), class = "daffiz_error_roles")
})

test_that("daffiz_row_number aligns by position", {
  x <- data.frame(v = c(1, 2, 3))
  y <- data.frame(v = c(1, 2, 3.5))
  cmp <- compare_dt(x, y, by = daffiz_row_number())
  expect_equal(cmp$settings$by, ".position")
  expect_equal(sum(all_cells(cmp)$.match_type == "diff"), 1L)
})

test_that("integer measures are promoted, never narrowed", {
  x <- data.frame(id = 1:2, n = c(1L, 2L))
  y <- data.frame(id = 1:2, n = c(1L, 3L))
  cmp <- compare_dt(x, y, by = "id", compare = "n")
  cells <- all_cells(cmp)
  expect_type(cells$.value_x, "double")
  expect_equal(sum(cells$.match_type == "diff"), 1L)
  # The snapshot keeps the original integer type.
  expect_type(cmp$x_original$n, "integer")
})

test_that("a double measure is never silently truncated toward integer", {
  x <- data.frame(id = 1:2, n = c(1L, 2L))
  y <- data.frame(id = 1:2, n = c(1.4, 2.6))
  cmp <- compare_dt(x, y, by = "id", compare = "n")
  expect_equal(sum(all_cells(cmp)$.match_type == "diff"), 2L)
})
