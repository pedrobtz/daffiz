# daffiz 0.1.0

First release.

* `diff_table()` compares two tables value by value, returning a
  `data.table` with one row per compared value, traceable to its source row
  in both inputs (`row_id_x`, `row_id_y`), and a `status` of `same`,
  `changed`, `only_x` or `only_y`.
* Two modes:
  * `mode = "benchmark"` (the default) tests `y` against the benchmark `x`.
    `y`'s columns are cast to `x`'s types, its extra columns are dropped
    with a message, and a column it lacks is an error.
  * `mode = "equal"` compares the two as peers. Every column must be in
    both tables with the same type, nothing is cast, and the result is
    symmetric.
* Casts that drop a fraction -- a double to an integer, a number or a
  timestamp to a whole day -- are made by default, since storage such as
  Spark or Delta narrows types the same way. Each one is announced and
  recorded; `truncate = FALSE` refuses them.
* Column names in both tables are normalized to `A-Z`, `0-9` and `_`, so
  `amount` and `Amount` are one column. Two names in one table that
  normalize to the same thing are an error.
* `by` names the row identity. `measures` chooses the compared columns, by
  type (`"numeric"`, `"numeric+integer"`) or by name. With no measures,
  `diff_table()` compares whole rows.
* Duplicate keys are paired in arrival order with a warning, compared as
  exact multisets with `duplicates = "aggregate"`, or refused with
  `duplicates = "error"`.
* `nan_is_na = TRUE` treats NaN as NA, in values and in key columns alike.
* A comparison in which no row aligns is an error, not a report of two
  unrelated tables.
* `diff_summary()` counts each status per measure and measures the size of
  the differences.
* `expect_table_equal()` is a testthat expectation built on `diff_table()`.
* `compare_columns()` shows how the columns of two tables line up,
  `normalize_dt()` converts other types (factors, timestamps) to the ones
  `diff_table()` compares, and `cast_rules()` lists the conversions it
  makes.
