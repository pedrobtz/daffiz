# Modes ---------------------------------------------------------------------

test_that("benchmark mode drops y's extra columns, with a message", {
  x <- data.table(id = 1:2, v = c(1, 2))
  y <- data.table(id = 1:2, v = c(1, 2), loaded_at = "today")
  expect_message(d <- diff_table(x, y), "loaded_at", class = "daffiz_message_dropped_columns")
  expect_true(all(d$status == "same"))
  expect_identical(attr(d, "dropped"), "loaded_at")
  expect_false(any(grepl("LOADED", names(d))))
})

test_that("benchmark mode refuses a column the benchmark has and y lacks", {
  x <- data.table(id = 1:2, v = c(1, 2), w = c(3, 4))
  y <- data.table(id = 1:2, v = c(1, 2))
  expect_error(diff_table(x, y), "missing column\\(s\\) the benchmark `x` has: w", class = "daffiz_error_columns")
})

test_that("equal mode needs every column on both sides, and says which", {
  x <- data.table(id = 1:2, v = c(1, 2), w = c(3, 4))
  y <- data.table(id = 1:2, v = c(1, 2), z = "z")
  err <- expect_error(diff_table(x, y, mode = "equal"), class = "daffiz_error_columns")
  expect_match(conditionMessage(err), "only in `x`: w")
  expect_match(conditionMessage(err), "only in `y`: z")
})

test_that("equal mode casts nothing, and refuses to be asked to", {
  x <- data.table(id = 1:2, v = c(1, 2))
  y <- data.table(id = 1:2, v = c("1", "2"))
  expect_error(diff_table(x, y, mode = "equal"), "does not cast.*v \\(numeric vs character\\)", class = "daffiz_error_types")
  expect_error(diff_table(x, x, mode = "equal", cast = TRUE), "never casts", class = "daffiz_error_input")
  # The same tables compare in benchmark mode, which casts.
  expect_true(all(diff_table(x, y)$status == "same"))
})

test_that("equal mode is symmetric", {
  x <- data.table(id = c("a", "b", "c"), n = c(1L, 2L, 3L), v = c(1, 2, 3))
  y <- data.table(id = c("a", "b", "d"), n = c(1L, 5L, 4L), v = c(1.5, 2, 4))
  for (measures in list("numeric", "numeric+integer", "v")) {
    for (by in list(NULL, c("id", "n"))) {
      if (identical(measures, "numeric+integer") && !is.null(by)) next
      xy <- diff_table(x, y, mode = "equal", measures = measures, by = by)
      yx <- diff_table(y, x, mode = "equal", measures = measures, by = by)
      swap <- c(same = "same", changed = "changed", only_x = "only_y", only_y = "only_x")
      expect_identical(unname(swap[as.character(yx$status)]), as.character(xy$status))
      expect_equal(yx$diff, -xy$diff)
      expect_identical(yx$row_id_x, xy$row_id_y)
    }
  }
})

test_that("both modes give the same answer when there is nothing to cast or drop", {
  x <- data.table(id = c("a", "b"), v = c(1, 2))
  y <- data.table(id = c("a", "b"), v = c(1, 3))
  expect_identical(
    as.character(diff_table(x, y)$status),
    as.character(diff_table(x, y, mode = "equal")$status)
  )
})

# Column names --------------------------------------------------------------

test_that("columns pair by their normalized names, and the result uses them", {
  x <- data.table(id = 1:2, `unit price` = c(1, 2))
  y <- data.table(ID = 1:2, Unit.Price = c(1, 2))
  d <- diff_table(x, y, mode = "equal")
  expect_identical(names(d)[1:2], c("ID", "metric"))
  expect_identical(unique(d$metric), "UNIT_PRICE")
  expect_identical(attr(d, "name_map")$name_y, c("ID", "Unit.Price"))
})

test_that("a user column named like an internal one cannot collide with it", {
  x <- data.table(row_id = c(10L, 20L), metric = c("m", "n"), status = c("p", "q"), value = c(1, 2))
  y <- x[2:1]
  d <- diff_table(x, y)
  expect_identical(key(d), c("ROW_ID", "METRIC", "STATUS", "metric"))
  expect_identical(d$STATUS, c("p", "q"))
  expect_true(all(d$status == "same"))
})

test_that("a name collision is an error, in either table", {
  expect_error(
    diff_table(data.table(a = 1, A = 2), data.table(a = 1)),
    'A <- "a", "A"', class = "daffiz_error_columns"
  )
  expect_error(
    diff_table(data.table(id = "a", v = 1), data.table(id = "a", v = 1, V = 2)),
    '`y` has columns whose names are the same.*V <- "v", "V"'
  )
})

test_that("in benchmark mode a collision among y's dropped columns does not matter", {
  x <- data.table(id = "a", v = 1)
  y <- data.table(id = "a", v = 1, foo = 1, Foo = 2)
  expect_true(suppressMessages(diff_table(x, y))$status == "same")
  expect_error(
    diff_table(x, y, mode = "equal"),
    class = "daffiz_error_columns"
  )
})

# Casting and truncation ------------------------------------------------------

test_that("truncation in benchmark mode is announced and recorded", {
  x <- data.table(id = c("a", "b"), n = c(1L, 2L))
  y <- data.table(id = c("a", "b"), n = c(1.7, 2))
  expect_message(
    d <- diff_table(x, y, measures = "numeric+integer"),
    "n: 1 \\(1.7 -> 1\\)", class = "daffiz_message_truncation"
  )
  expect_true(all(d$status == "same"))
  expect_identical(attr(d, "truncated")$column, "n")
  expect_error(
    diff_table(x, y, measures = "numeric+integer", truncate = FALSE),
    "column n: numeric -> integer, not a whole number", class = "daffiz_error_cast"
  )
})

test_that("a cast error names the column in the tested table's spelling", {
  expect_error(
    diff_table(data.table(id = 1, `unit price` = 1), data.table(id = 1, Unit.Price = "x")),
    'column "Unit.Price" \\(UNIT_PRICE\\): character -> numeric, not a number'
  )
})

# Measures and identity -------------------------------------------------------

test_that("measures can be chosen by type or by name, in either spelling", {
  x <- data.table(id = c("a", "b"), n = c(1L, 2L), v = c(1, 2), w = c(3, 4))
  y <- data.table(id = c("a", "b"), n = c(1L, 2L), v = c(1, 9), w = c(3, 4))
  expect_setequal(diff_table(x, y)$metric, c("V", "W"))
  expect_setequal(diff_table(x, y, measures = "numeric+integer")$metric, c("N", "V", "W"))
  d <- diff_table(x, y, measures = c("V", "n"))
  expect_setequal(d$metric, c("V", "N"))
  # W was not named, so it is part of the identity, matched exactly.
  expect_true("W" %in% key(d))
})

test_that("a named measure that is not numeric is refused", {
  x <- data.table(id = c("a", "b"), v = c(1, 2))
  expect_error(diff_table(x, x, measures = "id"), "not numeric or integer: ID \\(character\\)", class = "daffiz_error_measures")
})

test_that("by picks the identity; other columns are ignored in benchmark mode", {
  x <- data.table(id = c("a", "b"), note = c("p", "q"), v = c(1, 2))
  y <- data.table(id = c("a", "b"), note = c("p", "CHANGED"), v = c(1, 2))
  expect_message(d <- diff_table(x, y, by = "id"), "note", class = "daffiz_message_ignored_columns")
  expect_true(all(d$status == "same"))
  expect_identical(attr(d, "ignored"), "NOTE")
  expect_identical(key(d), c("ID", "metric"))
})

test_that("equal mode refuses to ignore a column", {
  x <- data.table(id = c("a", "b"), note = c("p", "q"), v = c(1, 2))
  expect_error(diff_table(x, x, mode = "equal", by = "id"), "neither `by` nor `measures`: note", class = "daffiz_error_columns")
  # Named in `by`, it takes part and the comparison runs.
  expect_true(all(diff_table(x, x, mode = "equal", by = c("id", "note"))$status == "same"))
})

test_that("by is validated", {
  x <- data.table(id = c("a", "b"), v = c(1, 2))
  expect_error(diff_table(x, x, by = "nope"), "`by` names column\\(s\\) not in the tables: NOPE")
  expect_error(diff_table(x, x, by = c("id", "ID")), "`by` names a column more than once: ID")
  expect_error(diff_table(x, x, by = "v", measures = "v"), "`by` and `measures` both name: v", class = "daffiz_error_measures")
  expect_error(diff_table(x, x, by = NA_character_), class = "daffiz_error_input")
})

test_that("a table whose every column is a measure has no identity", {
  x <- data.table(v = c(1, 2))
  expect_error(diff_table(x, x), "No identity columns", class = "daffiz_error_keys")
})

# Values ------------------------------------------------------------------------

test_that("NA and NaN: the same missing value by default, told apart on request", {
  x <- data.table(id = c("a", "b", "c", "d"), v = c(NA, NaN, NA, NaN))
  y <- data.table(id = c("a", "b", "c", "d"), v = c(NA, NaN, NaN, NA))
  expect_identical(as.character(diff_table(x, y)$status), rep("same", 4L))
  expect_identical(
    as.character(diff_table(x, y, nan_is_na = FALSE)$status),
    c("same", "same", "changed", "changed")
  )
  # A missing value against a number is always a change.
  z <- data.table(id = c("a", "b", "c", "d"), v = 1)
  expect_true(all(diff_table(x, z)$status == "changed"))
})

test_that("NaN and NA key values align when they are the same missing value", {
  x <- data.table(k = c(NA, 1), v = c(1, 2))
  y <- data.table(k = c(NaN, 1), v = c(1, 2))
  d <- diff_table(x, y, by = "k")
  expect_true(all(d$status == "same"))
  d2 <- diff_table(x, y, by = "k", nan_is_na = FALSE)
  expect_identical(sort(as.character(d2$status)), c("only_x", "only_y", "same"))
})

test_that("NA and NaN follow nan_is_na under aggregation too", {
  x <- data.table(id = c("a", "a"), v = c(1, NA))
  y <- data.table(id = c("a", "a"), v = c(NaN, 1))
  expect_identical(as.character(diff_table(x, y, duplicates = "aggregate")$status), "same")
  expect_identical(
    as.character(diff_table(x, y, duplicates = "aggregate", nan_is_na = FALSE)$status),
    "changed"
  )
})

test_that("tolerance must be a single finite number >= 0", {
  x <- data.table(id = "a", v = 1)
  for (bad in list(-1, NA_real_, c(1, 0), "1", Inf)) {
    expect_error(diff_table(x, x, tolerance = bad), "`tolerance` must be", class = "daffiz_error_input")
  }
  expect_error(diff_table(x, x, duplicates = "aggregate", tolerance = 0.1), "compares values exactly")
})

test_that("status is a factor with every level", {
  x <- data.table(id = c("a", "b"), v = c(1, 2))
  d <- diff_table(x, x)
  expect_identical(levels(d$status), c("same", "changed", "only_x", "only_y"))
  expect_identical(attr(d, "shape"), "cells")
  expect_identical(attr(d, "mode"), "benchmark")
})

# No measures: do the tables have the same rows? ---------------------------

test_that("with no measures, rows are compared as wholes", {
  x <- data.table(k = c("a", "b", "c"))
  y <- data.table(k = c("c", "a", "d"))
  expect_message(d <- diff_table(x, y), class = "daffiz_message_no_measures")
  expect_identical(attr(d, "shape"), "rows")
  expect_identical(names(d), c("K", "row_id_x", "row_id_y", "status"))
  expect_identical(as.character(d$status), c("same", "only_x", "same", "only_y"))
  expect_identical(d$row_id_y, c(2L, NA, 1L, 3L))
})

test_that("with no measures, duplicates compare as multisets under every policy", {
  x <- data.table(k = c("a", "a", "b"))
  y <- data.table(k = c("a", "b", "b"))
  quiet <- function(...) suppressMessages(diff_table(...))
  # Pairing identical rows assumes nothing, so there is no warning.
  expect_no_warning(d <- quiet(x, y))
  expect_identical(sort(as.character(d$status)), c("only_x", "only_y", "same", "same"))
  g <- quiet(x, y, duplicates = "aggregate")
  expect_identical(attr(g, "shape"), "groups")
  expect_identical(as.character(g$status), c("changed", "changed"))
  expect_identical(as.character(quiet(x, x, duplicates = "aggregate")$status), c("same", "same"))
  expect_error(quiet(x, y, duplicates = "error"), class = "daffiz_error_duplicates")
})

test_that("with no measures, equal mode still compares every column", {
  x <- data.table(k = c("a", "b"), n = c(1L, 2L))
  d <- suppressMessages(diff_table(x, x[2:1], mode = "equal"))
  expect_true(all(d$status == "same"))
})

# Alignment -----------------------------------------------------------------

test_that("a comparison in which nothing aligns is refused", {
  x <- data.table(id = c("a", "b"), v = c(1, 2))
  y <- data.table(id = c("A", "B"), v = c(1, 2))
  expect_error(diff_table(x, y), "No identity value of `x` appears in `y`", class = "daffiz_error_disjoint")
  expect_error(diff_table(x, y[0]), "one table is empty", class = "daffiz_error_disjoint")
  expect_error(
    suppressMessages(diff_table(data.table(k = "a"), data.table(k = "b"))),
    "No row of `x` appears in `y`"
  )
})

test_that("two empty tables are equal, not ill-formed", {
  x <- data.table(id = character(), v = numeric())
  d <- diff_table(x, x)
  expect_identical(nrow(d), 0L)
})

# The inputs ----------------------------------------------------------------

test_that("the caller's tables are never modified", {
  x <- data.table(id = c("b", "a"), n = c(2L, 1L), v = c(2, NaN))
  y <- data.table(ID = c("a", "b"), N = c("1", "2.5"), V = c(NA, 2), extra = 1)
  before_x <- copy(x)
  before_y <- copy(y)
  suppressMessages(diff_table(x, y, measures = "numeric+integer"))
  suppressMessages(diff_table(x, y, by = "id"))
  suppressMessages(diff_table(x, x, mode = "equal", duplicates = "aggregate"))
  expect_identical(x, before_x)
  expect_identical(y, before_y)
})

test_that("data.frames work as well as data.tables", {
  x <- data.frame(id = c("a", "b"), v = c(1, 2))
  expect_true(all(diff_table(x, x)$status == "same"))
})
