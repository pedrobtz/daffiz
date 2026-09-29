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

cmp_same <- compare_dt(d1, d1)

test_that("compare_dt", {
  check("compare_dt: identical tables match on type", all(cmp_same$type_match))
  check("compare_dt: identical tables overlap fully", all(cmp_same$over_pct == 100))
  check("compare_dt: identical tables agree on counts", all(cmp_same$n_unique_x == cmp_same$n_unique_y))
})

y_extra <- copy(base)[, w := 10]

cmp_extra <- compare_dt(base, y_extra)

test_that("compare_dt", {
  check("compare_dt: keeps x's column order, y-only columns last", identical(cmp_extra$colname, c("id", "v", "w")))
  check("compare_dt: a y-only column has no x side", is.na(cmp_extra[colname == "w", type_x]))
  check("compare_dt: a y-only column has no overlap", is.na(cmp_extra[colname == "w", over_pct]))
})

cmp_missing <- compare_dt(y_extra, base)

test_that("compare_dt", {
  check("compare_dt: an x-only column has no y side", is.na(cmp_missing[colname == "w", type_y]))
})

cmp_disjoint <- compare_dt(base, data.table(id = c("x", "y", "z"), v = c(7, 8, 9)))

test_that("compare_dt", {
  check("compare_dt: disjoint values give 0 overlap", all(cmp_disjoint$over_pct == 0))
})

cmp_half <- compare_dt(base, data.table(id = c("a", "b", "z"), v = c(1, 2, 3)))

test_that("compare_dt", {
  check("compare_dt: partial overlap, 2 of 4 shared ids", cmp_half[colname == "id", n_common == 2L & n_union == 4L])
  check("compare_dt: partial overlap gives 50 pct", cmp_half[colname == "id", over_pct == 50])
})

cmp_type <- compare_dt(base, data.table(id = c("a", "b", "c"), v = c("1", "2", "3")))

test_that("compare_dt", {
  check("compare_dt: a changed type is flagged", isFALSE(cmp_type[colname == "v", type_match]))
  # `%in%` coerces before matching, so measuring the overlap across a type change
  # would report a full overlap for two columns that are not the same thing. It
  # is left uncomputed instead.
  check("compare_dt: a changed type leaves the overlap uncomputed", cmp_type[colname == "v", is.na(over_pct)])
  check("compare_dt: and the counts behind it too", cmp_type[colname == "v", is.na(n_common) & is.na(n_union)])
  check("compare_dt: the column that kept its type is still measured", cmp_type[colname == "id", over_pct == 100])
  check(
    "compare_dt: the overlap is measured exactly where the types agree",
    cmp_type[!is.na(type_match), all(type_match == !is.na(n_union))]
  )
  check(
    "compare_dt: a POSIXct on both sides still counts as one type",
    !is.na(compare_dt(
      data.table(t = as.POSIXct("2024-01-01", tz = "UTC")),
      data.table(t = as.POSIXct("2024-01-01", tz = "UTC"))
    )[colname == "t", over_pct])
  )
  check(
    "compare_dt: a factor against its own labels is not measured",
    is.na(compare_dt(data.table(f = factor("a")), data.table(f = "a"))[colname == "f", over_pct])
  )
  check("compare_dt: two empty columns give NA overlap", is.na(compare_dt(base[0], base[0])[colname == "id", over_pct]))
  check_error("compare_dt: refuses a non-table", compare_dt(base, 1:3))
})
