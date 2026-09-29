# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

s1 <- summarize_dt(d1)

test_that("summarize_dt", {
  check("summarize_dt: one row per column", nrow(s1) == ncol(d1))
  check("summarize_dt: reports names in order", identical(s1$colname, names(d1)))
  check(
    "summarize_dt: reports classes",
    identical(s1$type, c("character", "character", "Date", "integer", "numeric", "numeric"))
  )
  check(
    "summarize_dt: counts distinct values",
    summarize_dt(data.table(x = c(1, 1, 2)))$n_unique == 2L
  )
  check(
    "summarize_dt: NA counts as a value",
    summarize_dt(data.table(x = c(1, NA)))$n_unique == 2L
  )
  check("summarize_dt: zero rows", all(summarize_dt(base[0]) $n_unique == 0L))
  check_error("summarize_dt: refuses a non-table", summarize_dt(1:3))
})

cmp_same <- compare_columns(d1, d1)

test_that("compare_columns", {
  check("compare_columns: identical tables match on type", all(cmp_same$type_match))
  check("compare_columns: identical tables overlap fully", all(cmp_same$over_pct == 100))
  check("compare_columns: identical tables agree on counts", all(cmp_same$n_unique_x == cmp_same$n_unique_y))
})

y_extra <- copy(base)[, w := 10]

cmp_extra <- compare_columns(base, y_extra)

test_that("compare_columns", {
  check("compare_columns: keeps x's column order, y-only columns last", identical(cmp_extra$colname, c("ID", "V", "W")))
  check("compare_columns: a y-only column has no x side", is.na(cmp_extra[colname == "W", type_x]))
  check("compare_columns: a y-only column has no overlap", is.na(cmp_extra[colname == "W", over_pct]))
})

cmp_missing <- compare_columns(y_extra, base)

test_that("compare_columns", {
  check("compare_columns: an x-only column has no y side", is.na(cmp_missing[colname == "W", type_y]))
})

cmp_disjoint <- compare_columns(base, data.table(id = c("x", "y", "z"), v = c(7, 8, 9)))

test_that("compare_columns", {
  check("compare_columns: disjoint values give 0 overlap", all(cmp_disjoint$over_pct == 0))
})

cmp_half <- compare_columns(base, data.table(id = c("a", "b", "z"), v = c(1, 2, 3)))

test_that("compare_columns", {
  check("compare_columns: partial overlap, 2 of 4 shared ids", cmp_half[colname == "ID", n_common == 2L & n_union == 4L])
  check("compare_columns: partial overlap gives 50 pct", cmp_half[colname == "ID", over_pct == 50])
})

cmp_type <- compare_columns(base, data.table(id = c("a", "b", "c"), v = c("1", "2", "3")))

test_that("compare_columns", {
  check("compare_columns: a changed type is flagged", isFALSE(cmp_type[colname == "V", type_match]))
  # `%in%` coerces before matching, so measuring the overlap across a type change
  # would report a full overlap for two columns that are not the same thing. It
  # is left uncomputed instead.
  check("compare_columns: a changed type leaves the overlap uncomputed", cmp_type[colname == "V", is.na(over_pct)])
  check("compare_columns: and the counts behind it too", cmp_type[colname == "V", is.na(n_common) & is.na(n_union)])
  check("compare_columns: the column that kept its type is still measured", cmp_type[colname == "ID", over_pct == 100])
  check(
    "compare_columns: the overlap is measured exactly where the types agree",
    cmp_type[!is.na(type_match), all(type_match == !is.na(n_union))]
  )
  check(
    "compare_columns: a POSIXct on both sides still counts as one type",
    !is.na(compare_columns(
      data.table(t = as.POSIXct("2024-01-01", tz = "UTC")),
      data.table(t = as.POSIXct("2024-01-01", tz = "UTC"))
    )[colname == "T", over_pct])
  )
  check(
    "compare_columns: a factor against its own labels is not measured",
    is.na(compare_columns(data.table(f = factor("a")), data.table(f = "a"))[colname == "F", over_pct])
  )
  check("compare_columns: two empty columns give NA overlap", is.na(compare_columns(base[0], base[0])[colname == "ID", over_pct]))
  check_error("compare_columns: refuses a non-table", compare_columns(base, 1:3))
})

test_that("compare_columns pairs columns by their normalized names", {
  x <- data.table(id = 1:2, `unit price` = c(1, 2))
  y <- data.table(ID = 1:2, Unit.Price = c(1, 2), extra = "e")
  cmp <- compare_columns(x, y)
  expect_identical(cmp$colname, c("ID", "UNIT_PRICE", "EXTRA"))
  expect_identical(cmp$name_x, c("id", "unit price", NA))
  expect_identical(cmp$name_y, c("ID", "Unit.Price", "extra"))
  expect_true(all(cmp[1:2, type_match]))
})

test_that("compare_columns reads IDate and Date as one type", {
  cmp <- compare_columns(
    data.table(d = as.Date("2024-01-01")),
    data.table(d = as.IDate("2024-01-01"))
  )
  expect_true(cmp$type_match)
  expect_identical(cmp$over_pct, 100)
})

test_that("compare_columns refuses names that collide after normalization", {
  expect_error(
    compare_columns(data.table(amount = 1, Amount = 2), data.table(amount = 1)),
    'AMOUNT <- "amount", "Amount"',
    class = "daffiz_error_columns"
  )
  expect_error(
    compare_columns(data.table(a = 1), data.table(a = 1, A = 2)),
    "`y` has columns whose names are the same"
  )
})
