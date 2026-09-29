# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

# 1) all equal
t1 <- diff_table(d1, d1)

test_that("diff_table", {
  check("1) identical tables: every value is the same", all(t1$status == "same"))
  check("1) identical tables: one row per row x measure", nrow(t1) == nrow(d1) * 2L)
  check("1) identical tables: the difference is zero", all(t1$diff == 0))
})

# 2) same rows, some values changed
d2 <- copy(d1)

set(d2, i = 1L, j = "NUM_01", value = d1$NUM_01[1L] + 10)

t2 <- diff_table(d1, d2)

test_that("diff_table", {
  check("2) one changed value is found", sum(t2$status == "changed") == 1L)
  check("2) everything else is unchanged", sum(t2$status == "same") == nrow(t2) - 1L)
  check("2) the difference is reported", t2[status == "changed", abs(diff - 10) < 1e-9])
})

# 3) all four statuses at once
x3 <- data.table(id = c("a", "b", "c"), v = c(1, 2, 3))

y3 <- data.table(id = c("a", "b", "d"), v = c(1, 2.5, 4))

t3 <- diff_table(x3, y3)

test_that("diff_table", {
  check("3) an unchanged row is 'same'", t3[id == "a", status] == "same")
  check("3) a changed row is 'changed'", t3[id == "b", status] == "changed")
  check("3) a row only in x is 'only_x'", t3[id == "c", status] == "only_x")
  check("3) a row only in y is 'only_y'", t3[id == "d", status] == "only_y")
  check("3) only_x has no y value", t3[id == "c", is.na(value_y)])
  check("3) only_y has no x value", t3[id == "d", is.na(value_x)])
})

# 4) the two modes agree
t4 <- diff_table(x3, y3, mode = "new")

test_that("diff_table", {
  check("4) mode = 'new' reaches the same verdicts", identical(t4[order(id), status], t3[order(id), status]))
  check("4) mode = 'new' is a full outer join", nrow(t4) == 4L)
})

# 5) tolerance
x5 <- data.table(id = c("a", "b"), v = c(1, 2))

y5 <- data.table(id = c("a", "b"), v = c(1.005, 2))

test_that("diff_table", {
  check("5) without a tolerance a tiny drift is a change", diff_table(x5, y5)[id == "a", status] == "changed")
  check("5) inside the tolerance it is the same", diff_table(x5, y5, tolerance = 0.01)[id == "a", status] == "same")
  check("5) the tolerance is absolute, so it is not one-sided", diff_table(y5, x5, tolerance = 0.01)[id == "a", status] == "same")
  check("5) exactly at the tolerance still counts as same", diff_table(x5, y5, tolerance = 0.005)[id == "a", status] == "same")
})

# Inf - Inf is NaN, so a tolerance test alone called two identical infinities a
# change. Finding 4.
inf_x <- data.table(id = c("a", "b"), v = c(Inf, -Inf))

test_that("diff_table", {
  check("5) identical infinities are not a change", all(diff_table(inf_x, copy(inf_x))$status == "same"))
  check(
    "5) but opposite infinities are",
    diff_table(inf_x, data.table(id = c("a", "b"), v = c(-Inf, Inf)))[id == "a", status] == "changed"
  )
  check(
    "5) an infinity against a finite value is a change",
    diff_table(data.table(id = "a", v = Inf), data.table(id = "a", v = 1))$status == "changed"
  )
  check(
    "5) equality still wins with a tolerance set",
    diff_table(inf_x, copy(inf_x), tolerance = 0.5)[id == "a", status] == "same"
  )
  check(
    "5) NA on one side only is a change",
    diff_table(data.table(id = "a", v = NA_real_), data.table(id = "a", v = 1))$status == "changed"
  )
})

# 6) casting the candidate first
x6 <- data.table(id = c("a", "b"), v = c(1, 2))

y6 <- data.table(id = c("a", "b"), v = c("1", "2"))

test_that("diff_table", {
  check("6) a text column is cast to x's type first", all(diff_table(x6, y6)$status == "same"))
  check_error("6) with cast = FALSE the type mismatch is reported up front", diff_table(x6, y6, cast = FALSE), "disagree on the type of: v")
  check_error("6) a value that cannot be cast is refused", diff_table(x6, data.table(id = c("a", "b"), v = c("1", "two"))), "not a number")
})

# 7) which columns count as measures
x7 <- data.table(id = c("a", "b"), n = c(1L, 2L), v = c(1, 2))

y7 <- data.table(id = c("a", "b"), n = c(1L, 9L), v = c(1, 2))

test_that("diff_table", {
  check("7) integers are ids by default, so a change to one moves the row", diff_table(x7, y7)[, any(status %in% c("only_x", "only_y"))])
})

t7 <- diff_table(x7, y7, measures = "numeric+integer")

test_that("diff_table", {
  check("7) numeric+integer measures them instead", t7[metric == "n" & id == "b", status] == "changed")
  check("7) and the unchanged double is still same", t7[metric == "v" & id == "b", status] == "same")
})

# 8) ids that do not tell rows apart
x8 <- data.table(id = c("a", "a", "b"), v = c(1, 2, 3))

y8 <- data.table(id = c("a", "a", "b"), v = c(1, 2, 30))

t8 <- diff_table(x8, y8)

test_that("diff_table", {
  check("8) duplicates = 'disambiguate' pairs rows by arrival order", nrow(t8) == 3L)
  check("8) and finds the change", sum(t8$status == "changed") == 1L)
  check("8) the rows carry their sequence number", "KEY_SEQ" %in% names(t8))
})

t8a <- diff_table(x8, y8, duplicates = "aggregate")

test_that("diff_table", {
  check("8) duplicates = 'aggregate' answers per group instead", nrow(t8a) == 2L)
  check("8) it compares value multisets, not values", "value_key_x" %in% names(t8a))
  check("8) the untouched group is the same", t8a[id == "a", status] == "same")
  check("8) the changed group is changed", t8a[id == "b", status] == "changed")
  check(
    "8) aggregate is order-free: a reordered group is still the same",
    all(diff_table(x8, data.table(id = c("a", "a", "b"), v = c(2, 1, 3)), duplicates = "aggregate")$status == "same")
  )
  check(
    "8) while disambiguate pairs by position, so reordering shows up",
    any(diff_table(x8, data.table(id = c("a", "a", "b"), v = c(2, 1, 3)))$status == "changed")
  )
  check_error("8) duplicates = 'error' refuses", diff_table(x8, y8, duplicates = "error"), "do not tell rows apart")
})

# 9) NA values
x9 <- data.table(id = c("a", "b"), v = c(1, NA_real_))

y9 <- data.table(id = c("a", "b"), v = c(1, NA_real_))

# Membership comes from the row identity, not from the value: NA says nothing
# about whether the row was there. Two rows that both hold NA are unchanged,
# and a value turning into NA is a change to that row, not a deletion of it.
t9 <- diff_table(x9, y9)

test_that("diff_table", {
  check("9) an unchanged non-NA row is same", t9[id == "a", status] == "same")
  check("9) NA on both sides is same", t9[id == "b", status] == "same")
  check("9) a value that became NA is changed", diff_table(data.table(id = "a", v = 1), x9[1L][, v := NA_real_])[, status] == "changed")
  check("9) a value that arrived is changed", diff_table(data.table(id = "a", v = NA_real_), data.table(id = "a", v = 1))[, status] == "changed")
})

# 10) the inputs are never modified
x10 <- data.table(id = c("a", "b"), v = c(1, 2))

y10 <- data.table(id = c("a", "b"), v = c("1", "2"))

before_x <- copy(x10)

before_y <- copy(y10)

invisible(diff_table(x10, y10))

test_that("diff_table", {
  check("10) x is untouched", identical(x10, before_x))
  check("10) y is untouched, even though it was cast", identical(y10, before_y))
  # 11) shapes and refusals
  check("11) a plain data.frame works too", all(diff_table(as.data.frame(x3), as.data.frame(x3))$status == "same"))
  check("11) the result is keyed by the ids and the measure", identical(key(t3), c("id", "metric")))
  check("11) a table compared with itself has no changes", all(diff_table(d1, d1)$status == "same"))
  check_error("11) refuses a non-table", diff_table(x3, 1:3))
  check_error("11) an unknown mode", diff_table(x3, x3, mode = "sideways"))
  check_error("11) an unknown duplicates option", diff_table(x3, x3, duplicates = "ignore"))
  check_error("11) nothing to measure", diff_table(data.table(id = "a"), data.table(id = "a")), "no columns of type")
})

# 13) the baseline has to be in the baseline types
x13 <- data.table(t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 10:00"), tz = "UTC"), v = c(1, 2))

test_that("diff_table", {
  check_error("13) a POSIXct baseline is refused", diff_table(x13, copy(x13)), "unsupported type")
  check_error("13) the refusal names the column and points at the fix", diff_table(x13, copy(x13)), "t \\(POSIXct\\).*normalize_dt")
  check_error("13) a factor baseline is refused too", diff_table(data.table(f = factor("a"), v = 1), data.table(f = factor("a"), v = 1)), "f \\(factor\\)")
  check_error("13) the gate applies with cast = FALSE as well", diff_table(x13, copy(x13), cast = FALSE), "unsupported type")
})

# Every offending column is named in one message, not just the first one hit.
x13m <- data.table(
  ok = c("a", "b"),
  t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 10:00"), tz = "UTC"),
  f = factor(c("p", "q")),
  z = c(complex(real = 1), complex(real = 2)),
  v = c(1, 2)
)

msg13 <- tryCatch(diff_table(x13m, copy(x13m)), error = function(e) conditionMessage(e))

test_that("diff_table", {
  check("13) all non-baseline columns are reported, not just the first", grepl("t (POSIXct), f (factor), z (complex)", msg13, fixed = TRUE))
  check("13) the baseline columns stay out of the message", !grepl("ok ", msg13) && !grepl("v (", msg13, fixed = TRUE))
})

msg13b <- tryCatch(
  normalize_dt(data.table(z = complex(real = 1), w = complex(real = 2))),
  error = function(e) conditionMessage(e)
)

test_that("diff_table", {
  check("13) normalize_dt reports every column it cannot convert", grepl("z (complex), w (complex)", msg13b, fixed = TRUE))
  check("13) and stays quiet about the ones it can", !grepl("POSIXct", tryCatch(normalize_dt(x13m), error = function(e) conditionMessage(e))))
  check("13) converting the baseline first makes it work", all(diff_table(normalize_dt(x13), copy(x13))$status == "same"))
  check(
    "13) a POSIXct candidate needs no conversion of its own",
    all(diff_table(normalize_dt(x13), data.table(t = as.POSIXct(c("2024-01-01 22:00", "2024-01-02 22:00"), tz = "UTC"), v = c(1, 2)))$status == "same")
  )
  check(
    "13) a Date baseline still converts a POSIXct candidate in its own zone",
    diff_table(
      data.table(d = as.Date("2024-01-01"), v = 1),
      data.table(d = as.POSIXct("2024-01-01 23:30:00", tz = "Europe/Lisbon"), v = 1)
    )$status == "same"
  )
})

# 14) cast = FALSE checks the types up front
x14 <- data.table(id = c("1", "2"), v = c(1, 2))

y14 <- data.table(id = c(1, 2), v = c(1, 2))

test_that("diff_table", {
  check_error("14) a type mismatch is named, not left to the join", diff_table(x14, y14, cast = FALSE), "disagree on the type of: id")
  check_error("14) it reports both types", diff_table(x14, y14, cast = FALSE), "character vs numeric")
  check_error(
    "14) a mismatch on a non-id column is caught too",
    diff_table(data.table(id = "a", v = 1), data.table(id = "a", v = "1"), cast = FALSE),
    "v \\(numeric vs character\\)"
  )
  check("14) matching types pass the check", all(diff_table(x14, copy(x14), cast = FALSE)$status == "same"))
})

# 12) a realistic fuzzed table, end to end
d12 <- fake_dt(50, n_cols = c(character = 2, date = 1, integer = 1, numeric = 2), seed = 3)

num12 <- summarize_dt(d12)[type == "numeric", colname]

f12 <- fuzz_dt(d12, pct_replace = 0.2, cols = num12, seed = 5)

t12 <- diff_table(d12, f12)

test_that("diff_table", {
  check("12) a fuzzed table reports changes", any(t12$status == "changed"))
  check("12) and leaves the rest alone", any(t12$status == "same"))
  check("12) every row gets a status", !anyNA(t12$status))
})

f12b <- fuzz_dt(d12, pct_replace = 0, pct_new = 0.1, cols = num12, seed = 6)

test_that("diff_table", {
  check("12) appended rows show up as only_y", any(diff_table(d12, f12b)$status == "only_y"))
  check("12) dropped rows show up as only_x", any(diff_table(d12, d12[1:40])$status == "only_x"))
})
