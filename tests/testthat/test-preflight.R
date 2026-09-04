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
