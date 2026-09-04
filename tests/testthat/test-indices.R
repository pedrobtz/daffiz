test_that("row indices refer to pre-sort input positions", {
  # Deliberately unsorted identity values.
  x <- data.frame(k = c("c", "a", "b"), v = c(3, 1, 2))
  y <- data.frame(k = c("a", "b", "c"), v = c(1, 2, 3))
  cells <- all_cells(compare_dt(x, y))

  expect_equal(cells$.row_x[cells$k == "c"], 1L)
  expect_equal(cells$.row_x[cells$k == "a"], 2L)
  expect_equal(cells$.row_x[cells$k == "b"], 3L)
  expect_equal(cells$.row_y[cells$k == "a"], 1L)
  expect_equal(cells$.row_y[cells$k == "c"], 3L)
})

test_that("row indices survive duplicate pairing", {
  x <- data.frame(k = c("a", "a"), v = c(9, 1))
  y <- data.frame(k = c("a", "a"), v = c(1, 9))
  suppressWarnings(cells <- all_cells(compare_dt(x, y)))
  # Occurrence 1 is the sorted-smallest value on each side.
  first <- cells[cells$.occurrence == 1L, ]
  expect_equal(first$.value_x, 1)
  expect_equal(first$.row_x, 2L)
  expect_equal(first$.value_y, 1)
  expect_equal(first$.row_y, 1L)
})

test_that("snapshots preserve original order, types and columns", {
  fx <- make_fixture(15L)
  fy <- copy(fx)
  cmp <- compare_dt(fx, fy, exclude = "ratio")

  # Excluded columns still live in the snapshot.
  expect_true("ratio" %in% names(cmp$x_original))
  expect_equal(cmp$x_original$code, fx$code)
  expect_s3_class(cmp$x_original$event_date, "Date")
  expect_type(cmp$x_original$age, "integer")
  expect_equal(cmp$x_original$.row_x, seq_len(nrow(fx)))
})

test_that("caller inputs are never modified", {
  fx <- make_fixture(20L)
  fy <- copy(fx)
  fy[3L, amount := amount + 1]
  before_x <- copy(fx)
  before_y <- copy(fy)

  compare_dt(fx, fy)
  expect_equal(fx, before_x)
  expect_equal(fy, before_y)
  expect_false(".row_x" %in% names(fx))
  expect_false(".row_y" %in% names(fy))
})

test_that("caller data.tables are not reordered by duplicate pairing", {
  x <- data.table::data.table(k = c("b", "a", "a"), v = c(3, 2, 1))
  y <- data.table::data.table(k = c("b", "a", "a"), v = c(3, 2, 1))
  before <- copy(x)
  suppressWarnings(compare_dt(x, y))
  expect_equal(x, before)
})

test_that("plain data.frame inputs are not converted in place", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  y <- data.frame(id = 1:2, v = c(1, 3))
  compare_dt(x, y)
  expect_s3_class(x, "data.frame")
  expect_false(data.table::is.data.table(x))
})

test_that("side-only rows carry NA on the absent side's index", {
  x <- data.frame(id = 1:2, v = c(1, 2))
  y <- data.frame(id = 2:3, v = c(2, 3))
  cells <- all_cells(compare_dt(x, y))
  expect_true(all(is.na(cells$.row_y[cells$.match_type == "x_only"])))
  expect_true(all(is.na(cells$.row_x[cells$.match_type == "y_only"])))
  expect_false(anyNA(cells$.row_x[cells$.match_type == "equal"]))
})
