test_that("canonical_names keeps only A-Z, 0-9 and _", {
  nms <- c(
    "amount", "Unit Price", "unit.price", "unit-price", "unit  price",
    "amount ", " _id_ ", "prix_unité", "Q1", "2024", "%"
  )
  expect_identical(canonical_names(nms), c(
    "AMOUNT", "UNIT_PRICE", "UNIT_PRICE", "UNIT_PRICE", "UNIT_PRICE",
    "AMOUNT", "ID", "PRIX_UNIT", "Q1", "2024", ""
  ))
})

test_that("every column daffiz creates contains a lowercase letter", {
  # This is what makes a collision with a normalized user column impossible.
  # A new internal name without one would silently break that guarantee.
  internal <- c(
    "row_id", "row_id_x", "row_id_y", "key_seq", "metric", "value",
    "value_x", "value_y", "diff", "status", "n_rows", "n_rows_x", "n_rows_y",
    "n_na", "n_na_x", "n_na_y", "value_key", "value_key_x", "value_key_y"
  )
  expect_true(all(grepl("[a-z]", internal)))
  expect_true(all(grepl("[a-z]", c(names(MEASURE_TYPES)))))
})

test_that("name_map labels a column by its own spelling, adding the normalized one only when it is more than a change of case", {
  map <- name_map(data.table(ID = 1, amount = 2, `unit price` = 3))
  expect_identical(map$column, c("ID", "AMOUNT", "UNIT_PRICE"))
  expect_identical(map$label, c("ID", "amount", "\"unit price\" (UNIT_PRICE)"))
})

test_that("check_names refuses empty and colliding names, naming each group", {
  expect_error(
    check_names(name_map(data.table(`%` = 1, a = 2)), "x"),
    'no letter or digit.*"%"',
    class = "daffiz_error_columns"
  )
  err <- expect_error(
    check_names(name_map(data.table(a = 1, A = 2, b.c = 3, `b c` = 4)), "x"),
    class = "daffiz_error_columns"
  )
  expect_match(conditionMessage(err), 'A <- "a", "A"', fixed = TRUE)
  expect_match(conditionMessage(err), 'B_C <- "b.c", "b c"', fixed = TRUE)
})

test_that("check_names can be limited to the names that matter", {
  map <- name_map(data.table(foo = 1, Foo = 2, id = 3))
  expect_silent(check_names(map, "y", only = "ID"))
  expect_error(check_names(map, "y", only = c("ID", "FOO")), "FOO")
})

test_that("shallow_dt renames without copying the caller's vectors", {
  dt <- data.table(a = c(1, 2))
  out <- shallow_dt(dt, new = "A")
  expect_identical(names(out), "A")
  expect_identical(address(out$A), address(dt$a))
  # Replacing a column swaps a pointer and leaves the caller's table alone.
  set(out, j = "A", value = c(9, 9))
  expect_identical(dt$a, c(1, 2))
})
