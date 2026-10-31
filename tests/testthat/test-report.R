x3 <- data.table(id = c("a", "b", "c"), value = c(1, 2, 3))
y3 <- data.table(id = c("a", "b", "d"), value = c(1, 2.5, 4))

test_that("diff_report counts rows by alignment, a row changed if any measure is", {
  x <- data.table(id = 1:4, a = c(1, 2, 3, 4), b = c(1, 2, 3, 4))
  y <- data.table(id = c(1:3, 5L), a = c(1, 9, 3, 5), b = c(1, 9, 7, 5))
  r <- diff_report(diff_table(x, y))
  expect_identical(
    unlist(r$rows),
    c(n_x = 4L, n_y = 4L, n_only_x = 1L, n_same = 1L, n_changed = 2L, n_only_y = 1L)
  )
  expect_identical(r$by_measure$measure, c("a", "b"))
  expect_identical(r$by_measure$match_rate, c(2 / 3, 1 / 3))
  expect_identical(r$by_measure$n_distinct_changes, c(1L, 2L))
  expect_false(r$equal)
})

test_that("top changes are the most frequent pairs, each with an example key", {
  x <- data.table(id = c("p", "q", "r", "s"), v = c(1, 1, 2, 3))
  y <- data.table(id = c("p", "q", "r", "s"), v = c(5, 5, 9, 3))
  r <- diff_report(diff_table(x, y), top_k = 1)
  expect_identical(r$changes$value_x, 1)
  expect_identical(r$changes$value_y, 5)
  expect_identical(r$changes$n, 2L)
  expect_identical(r$changes$example, "\"p\"")
  expect_match(format(r, unicode = FALSE), "(...and 1 other)", fixed = TRUE, all = FALSE)
})

test_that("among equally frequent changes the largest comes first", {
  x <- data.table(id = 1:2, v = c(1, 1))
  y <- data.table(id = 1:2, v = c(2, 100))
  r <- diff_report(diff_table(x, y), top_k = 1)
  expect_identical(r$changes$value_y, 100)
})

test_that("example keys of several columns are tuples, in the caller's spelling", {
  x <- data.table(Id = 1:2, `Sub Key` = c("u", "v"), v = c(1, 2))
  y <- data.table(id = 1:2, sub_key = c("u", "v"), v = c(1, 3))
  r <- diff_report(diff_table(x, y))
  expect_identical(r$identity, c("Id", "Sub Key"))
  expect_identical(r$changes$example, "(2, \"v\")")
})

test_that("only-in-one-table keys are sampled, bounded by sample_k", {
  x <- data.table(id = 1:10, v = 1)
  y <- data.table(id = 1L, v = 1)
  r <- diff_report(diff_table(x, y), sample_k = 3)
  expect_identical(r$only_x$id, 2:4)
  expect_identical(nrow(r$only_y), 0L)
  expect_match(format(r, unicode = FALSE), "(...and 6 more)", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("only in x\\s*$", format(diff_report(diff_table(x, y), sample_k = 0), unicode = FALSE))))
})

test_that("diff_report reads a filtered or re-sorted result", {
  d <- diff_table(x3, y3)
  r <- diff_report(d[order(-value_x)])
  expect_identical(unlist(r$rows), unlist(diff_report(d)$rows))
})

test_that("diff_report works on every shape", {
  g <- diff_report(diff_table(
    data.table(id = c("a", "a", "b"), v = c(1, 2, 3)),
    data.table(id = c("a", "b", "b"), v = c(1, 3, 3)),
    duplicates = "aggregate"
  ))
  expect_identical(g$unit, "keys")
  expect_identical(c(g$rows$n_x, g$rows$n_changed), c(2L, 2L))
  expect_null(g$changes)

  w <- diff_report(suppressMessages(diff_table(data.table(k = c("a", "b")), data.table(k = c("b", "c", "a")))))
  expect_identical(w$measures, character())
  expect_identical(c(w$rows$n_same, w$rows$n_only_y), c(2L, 1L))
})

test_that("two empty tables report as equal, without warnings", {
  e <- data.table(id = character(), v = numeric())
  expect_no_warning(r <- diff_report(diff_table(e, e)))
  expect_true(r$equal)
  expect_match(format(r, unicode = FALSE), "both empty", all = FALSE)
})

test_that("counts past 2^31 are formatted", {
  expect_identical(fmt_count(3e9), "3,000,000,000")
  expect_identical(fmt_pct(c(0, 1e-9, 1 - 1e-9, 1)), c("0.00%", "0.01%", "99.99%", "100.00%"))
})

test_that("diff_report validates its arguments", {
  d <- diff_table(x3, y3)
  expect_error(diff_report(data.table(a = 1)), class = "daffiz_error_input")
  expect_error(diff_report(d, top_k = -1), class = "daffiz_error_input")
  expect_error(diff_report(d, sample_k = 1.5), class = "daffiz_error_input")
  expect_error(diff_report(d, x_name = NA_character_), class = "daffiz_error_input")
  expect_error(format(diff_report(d), unicode = NA), class = "daffiz_error_input")
})

test_that("the drawing", {
  expect_snapshot(print(diff_report(diff_table(x3, y3)), unicode = FALSE))
  expect_snapshot(print(diff_report(diff_table(x3, x3)), unicode = FALSE))

  bench <- data.table(
    id = 1:6, region = c("n", "s", "n", "s", "e", "w"),
    amount = c(10, 20, 30, 40, 50, 60), rate = c(1, 1, 1, 2, 2, 2), note = "z"
  )
  tested <- data.table(
    ID = c("1", "2", "3", "4", "5", "7", "8"), Region = c("n", "s", "n", "s", "e", "w", "w"),
    Amount = c(10, 21, 30, 41, 50, 70, 80), rate = c(1, 1, 1, 2.5, 2, 2, 2), note = "z", loaded = "t"
  )
  d <- suppressMessages(diff_table(bench, tested, by = c("id", "region"), measures = c("amount", "rate")))
  expect_snapshot(print(diff_report(d, x_name = "bench", y_name = "tested", top_k = 1), unicode = FALSE))
})

test_that("the drawing, in box-drawing characters", {
  skip_if_not(l10n_info()[["UTF-8"]])
  expect_snapshot(print(diff_report(diff_table(x3, y3)), unicode = TRUE))
})
