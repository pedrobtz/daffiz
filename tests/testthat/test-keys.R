# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

test_that("unique_by_key", {
  check("unique_by_key: distinct ids", isTRUE(unique_by_key(melt_dt(base))))
  check("unique_by_key: no duplicates to report", nrow(attr(unique_by_key(melt_dt(base)), "duplicates")) == 0L)
})

dup <- data.table(id = c("a", "a", "b"), v = c(1, 2, 3))

u_dup <- unique_by_key(melt_dt(dup))

test_that("unique_by_key", {
  check("unique_by_key: repeated ids", isFALSE(u_dup))
  check("unique_by_key: names the offending group", attr(u_dup, "duplicates")$id == "a")
  check("unique_by_key: counts the rows in it", attr(u_dup, "duplicates")$n_rows == 2L)
  check(
    "unique_by_key: the measure is part of the identity, so one row per metric is fine",
    isTRUE(unique_by_key(melt_dt(data.table(id = "a", v = 1, w = 2))))
  )
  check_error("unique_by_key: an unkeyed table", unique_by_key(data.table(a = 1)), "no key")
  check_error("unique_by_key: refuses a data.frame", unique_by_key(data.frame(a = 1)))
})

lx <- melt_dt(dup)

disambiguate_by_key(lx)

test_that("disambiguate_by_key", {
  check("disambiguate_by_key: adds the sequence column", "key_seq" %in% names(lx))
  check("disambiguate_by_key: numbers within the group", setequal(lx[id == "a", key_seq], c(1L, 2L)))
  check("disambiguate_by_key: a single-row group gets 1", all(lx[id == "b", key_seq] == 1L))
  check("disambiguate_by_key: the key now includes it", identical(key(lx), c("id", "key_seq")))
  check("disambiguate_by_key: the table is now unique by key", isTRUE(unique_by_key(lx)))
  check("disambiguate_by_key: modifies by reference", "key_seq" %in% names(lx))
})

renamed <- melt_dt(dup)

disambiguate_by_key(renamed, seq.name = "SEQ")

test_that("disambiguate_by_key", {
  check("disambiguate_by_key: the sequence column can be renamed", "SEQ" %in% names(renamed))
  check_error("disambiguate_by_key: an unkeyed table", disambiguate_by_key(data.table(a = 1)), "no key")
})

agg <- aggregate_by_key(melt_dt(dup))

test_that("aggregate_by_key", {
  check("aggregate_by_key: one row per key group and measure", nrow(agg) == 2L)
  check("aggregate_by_key: counts the rows in the group", agg[id == "a", n_rows] == 2L)
  check("aggregate_by_key: sums the values", agg[id == "a", value_sum] == 3)
  check("aggregate_by_key: sums the squares", agg[id == "a", value_sumsq] == 5)
  check("aggregate_by_key: reports the range", agg[id == "a", value_min == 1 & value_max == 2])
  check("aggregate_by_key: keyed by the group", identical(key(agg), "id"))
})

m13 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(1, 3))))

m22 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(2, 2))))

test_that("aggregate_by_key", {
  check("aggregate_by_key: the sum alone cannot separate {1,3} from {2,2}", m13$value_sum == m22$value_sum)
  check("aggregate_by_key: the sum of squares can", m13$value_sumsq != m22$value_sumsq)
})

na_grp <- melt_dt(data.table(id = c("a", "a"), v = c(1, NA_real_)))

agg_na <- aggregate_by_key(na_grp)

test_that("aggregate_by_key", {
  check("aggregate_by_key: NAs are counted, not dropped", agg_na$n_na == 1L && agg_na$n_rows == 2L)
  check("aggregate_by_key: the other values are still summarised", agg_na$value_sum == 1)
  check_clean(
    "aggregate_by_key: an all-NA group reports NA, without warning",
    is.na(aggregate_by_key(melt_dt(data.table(id = "a", v = NA_real_)))$value_min)
  )
})

ex <- aggregate_by_key(melt_dt(dup), stats = "exact")

test_that("aggregate_by_key", {
  check("aggregate_by_key: exact keeps a value key instead of moments", "value_key" %in% names(ex) && !"value_sum" %in% names(ex))
})

ex13 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(1, 3))), stats = "exact")

ex31 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(3, 1))), stats = "exact")

ex22 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(2, 2))), stats = "exact")

test_that("aggregate_by_key", {
  check("aggregate_by_key: exact is order-free", ex13$value_key == ex31$value_key)
  check("aggregate_by_key: exact separates different multisets", ex13$value_key != ex22$value_key)
  check(
    "aggregate_by_key: exact round-trips the bits, so 0.1+0.2 differs from 0.3",
    aggregate_by_key(melt_dt(data.table(id = "a", v = 0.1 + 0.2)), stats = "exact")$value_key !=
      aggregate_by_key(melt_dt(data.table(id = "a", v = 0.3)), stats = "exact")$value_key
  )
  check_error("aggregate_by_key: an unkeyed table", aggregate_by_key(data.table(a = 1)), "no key")
  check_error("aggregate_by_key: an unknown stats option", aggregate_by_key(melt_dt(dup), stats = "mean"))
})

test_that("exact aggregation treats -0 as 0, as == does", {
  zero <- aggregate_by_key(melt_dt(data.table(id = "a", v = 0)), stats = "exact")
  minus <- aggregate_by_key(melt_dt(data.table(id = "a", v = -0)), stats = "exact")
  expect_identical(zero$value_key, minus$value_key)
})

test_that("exact aggregation follows nan_is_na", {
  na <- melt_dt(data.table(id = "a", v = NA_real_))
  nan <- melt_dt(data.table(id = "a", v = NaN))
  key_of <- function(l, ...) aggregate_by_key(l, stats = "exact", ...)$value_key
  expect_identical(key_of(na), key_of(nan))
  expect_false(identical(key_of(na, nan_is_na = FALSE), key_of(nan, nan_is_na = FALSE)))
})

test_that("exact aggregation does not depend on the order of NA and NaN", {
  a <- melt_dt(data.table(id = c("a", "a"), v = c(NA, NaN)))
  b <- melt_dt(data.table(id = c("a", "a"), v = c(NaN, NA)))
  key_of <- function(l) aggregate_by_key(l, stats = "exact", nan_is_na = FALSE)$value_key
  expect_identical(key_of(a), key_of(b))
})

test_that("with no value column, aggregation counts the rows", {
  wide <- index_dt(data.table(ID = c("a", "a", "b")), "ID")
  out <- aggregate_by_key(wide, stats = "exact")
  expect_identical(names(out), c("ID", "n_rows"))
  expect_identical(out$n_rows, c(2L, 1L))
})
