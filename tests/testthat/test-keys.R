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

disambiguate_by_key(lx, "rowid")

test_that("disambiguate_by_key", {
  check("disambiguate_by_key: adds the sequence column", "KEY_SEQ" %in% names(lx))
  check("disambiguate_by_key: numbers within the group", setequal(lx[id == "a", KEY_SEQ], c(1L, 2L)))
  check("disambiguate_by_key: a single-row group gets 1", all(lx[id == "b", KEY_SEQ] == 1L))
  check("disambiguate_by_key: the key now includes it", identical(key(lx), c("id", "KEY_SEQ")))
  check("disambiguate_by_key: the table is now unique by key", isTRUE(unique_by_key(lx)))
  check("disambiguate_by_key: modifies by reference", "KEY_SEQ" %in% names(lx))
})

rank_dt <- melt_dt(data.table(id = c("a", "a", "a"), v = c(30, 10, 20)))

disambiguate_by_key(rank_dt, "value_rank")

test_that("disambiguate_by_key", {
  check("disambiguate_by_key: value_rank numbers by the value", identical(rank_dt[order(value), KEY_SEQ], c(1L, 2L, 3L)))
})

ties <- melt_dt(data.table(id = c("a", "a"), v = c(5, 5)))

disambiguate_by_key(ties, "value_rank")

test_that("disambiguate_by_key", {
  check("disambiguate_by_key: value_rank breaks ties by arrival", setequal(ties$KEY_SEQ, c(1L, 2L)))
})

renamed <- melt_dt(dup)

disambiguate_by_key(renamed, "rowid", seq.name = "SEQ")

test_that("disambiguate_by_key", {
  check("disambiguate_by_key: the sequence column can be renamed", "SEQ" %in% names(renamed))
  check_error("disambiguate_by_key: an unkeyed table", disambiguate_by_key(data.table(a = 1)), "no key")
  check_error("disambiguate_by_key: an unknown method", disambiguate_by_key(melt_dt(dup), "guess"))
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
