# daffiz

<!-- badges: start -->
[![R-CMD-check](https://github.com/pedrobtz/daffiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pedrobtz/daffiz/actions/workflows/R-CMD-check.yaml)
![Coverage](https://github.com/pedrobtz/daffiz/raw/main/.github/badges/coverage.svg)
<!-- badges: end -->

`daffiz` compares two tables value by value. Its one main function,
`diff_table()`, answers one of two questions:

- **Does this table match the benchmark?** (`mode = "benchmark"`, the
  default.) The benchmark's column types are the truth. The tested table's
  columns are cast to them, and its extra columns are dropped.
- **Are these two tables equal?** (`mode = "equal"`.) The tables are peers:
  every column must be in both, with the same type, and nothing is cast.

The result is a `data.table` with one row per compared value. Each row traces
back to its source row in both tables.

## Installation

Install the released version from CRAN:

```r
install.packages("daffiz")
```

Or the development version from GitHub:

```r
# install.packages("pak")
pak::pak("pedrobtz/daffiz")
```

## Comparing against a benchmark

A benchmark table, and the same data after a round trip through storage. On
the way, the column names changed case, the ids became text, `units` came back
as a double, a load timestamp was added, one row was lost and one was added:

```r
library(daffiz)

benchmark <- data.frame(
  id     = c(1L, 2L, 3L, 4L),
  region = c("north", "south", "east", "west"),
  amount = c(100, 200, 300, 400),
  units  = c(10L, 20L, 30L, 40L)
)
stored <- data.frame(
  ID        = c("1", "2", "3", "5"),
  Region    = c("north", "south", "east", "north"),
  Amount    = c(100, 200.5, 300, 500),
  Units     = c(10.4, 20, 30, 50),
  loaded_at = "2026-09-29"
)

d <- diff_table(benchmark, stored, by = c("id", "region"), measures = "numeric+integer")
#> Dropped column(s) of `y` that the benchmark `x` does not have: loaded_at.
#> Truncated 1 value(s) while casting to the reference types:
#>   Units: 1 (10.4 -> 10)
d
#> Key: <ID, REGION, metric>
#>        ID REGION metric row_id_x row_id_y value_x value_y  diff  status
#>     <int> <char> <char>    <int>    <int>   <num>   <num> <num>  <fctr>
#>  1:     1  north AMOUNT        1        1     100   100.0   0.0    same
#>  2:     1  north  UNITS        1        1      10    10.0   0.0    same
#>  3:     2  south AMOUNT        2        2     200   200.5   0.5 changed
#>  4:     2  south  UNITS        2        2      20    20.0   0.0    same
#>  5:     3   east AMOUNT        3        3     300   300.0   0.0    same
#>  6:     3   east  UNITS        3        3      30    30.0   0.0    same
#>  7:     4   west AMOUNT        4       NA     400      NA    NA  only_x
#>  8:     4   west  UNITS        4       NA      40      NA    NA  only_x
#>  9:     5  north AMOUNT       NA        4      NA   500.0    NA  only_y
#> 10:     5  north  UNITS       NA        4      NA    50.0    NA  only_y
```

Reading the result:

- **`status`** is `same`, `changed`, `only_x` (the row is missing from the
  tested table) or `only_y` (the row exists only there).
- **`row_id_x` and `row_id_y`** are the row numbers in the two inputs.
- **`diff`** is `value_y - value_x`.
- **Column names are normalized** in both tables: upper case, with other
  characters turned into `_`. That is why `id` and `ID` pair up.
- **`units` was cast to the benchmark's integer type.** Its `10.4` was
  truncated to `10`, which is announced in a message and recorded in
  `attr(d, "truncated")`. Pass `truncate = FALSE` to refuse such casts
  instead.

`diff_summary()` condenses the result per measure:

```r
diff_summary(d)
#>    metric     n n_same n_changed n_only_x n_only_y max_abs_diff mean_abs_diff      rmse
#>    <char> <int>  <int>     <int>    <int>    <int>        <num>         <num>     <num>
#> 1: AMOUNT     5      2         1        1        1          0.5     0.1666667 0.2886751
#> 2:  UNITS     5      3         0        1        1          0.0     0.0000000 0.0000000
```

## Comparing two tables as equals

In `mode = "equal"` nothing is cast or dropped, so the same two tables do not
even compare:

```r
diff_table(benchmark, stored, mode = "equal")
#> Error: mode = "equal" needs the same columns in both tables.
#>   only in `x`: <none>
#>   only in `y`: loaded_at
```

Row order never matters; rows are aligned on their identity:

```r
diff_table(benchmark, benchmark[c(4, 1, 2, 3), ], mode = "equal")
#> Key: <ID, REGION, UNITS, metric>
#>       ID REGION UNITS metric row_id_x row_id_y value_x value_y  diff status
#>    <int> <char> <int> <char>    <int>    <int>   <num>   <num> <num> <fctr>
#> 1:     1  north    10 AMOUNT        1        2     100     100     0   same
#> 2:     2  south    20 AMOUNT        2        3     200     200     0   same
#> 3:     3   east    30 AMOUNT        3        4     300     300     0   same
#> 4:     4   west    40 AMOUNT        4        1     400     400     0   same
```

## Do they have the same rows?

With no numeric measures, rows are compared as wholes. A repeated row counts
once per copy:

```r
diff_table(
  benchmark[c("id", "region")],
  data.frame(ID = c(3L, 1L, 2L, 2L), REGION = c("east", "north", "south", "south"))
)
#> No measure columns (measures = "numeric"), so rows are compared as wholes: do the two tables have the same rows?
#> Key: <ID, REGION, key_seq>
#>       ID REGION key_seq row_id_x row_id_y status
#>    <int> <char>   <int>    <int>    <int> <fctr>
#> 1:     1  north       1        1        2   same
#> 2:     2  south       1        2        3   same
#> 3:     2  south       2       NA        4 only_y
#> 4:     3   east       1        3        1   same
#> 5:     4   west       1        4       NA only_x
```

## In tests

`expect_table_equal()` is a testthat expectation built on `diff_table()`.
Its second argument, `expected`, is the benchmark:

```r
test_that("the pipeline reproduces the reference output", {
  expect_table_equal(run_pipeline(), reference, tolerance = 1e-8)
})
```

A failure shows `diff_summary()` and the first records that differ. A key
that repeats fails the expectation instead of being paired by arrival order,
unless you pass `duplicates = "disambiguate"` or `duplicates = "aggregate"`.

## More

- **Measures** are the numeric columns compared value by value. They default
  to the double columns; `measures` takes `"numeric+integer"` or column
  names.
- **The row identity** is `by`, or every column that is not a measure. A
  table needs at least one key column. When every column is a measure, add a
  virtual one: `row_key = "position"` pairs rows as they come, and
  `row_key = "sorted"` pairs them after sorting both tables by their values.
- **Duplicate keys** are paired in arrival order with a warning, compared as
  multisets (`duplicates = "aggregate"`), or refused
  (`duplicates = "error"`).
- **NaN and NA** are the same missing value unless `nan_is_na = FALSE`.
- **Types other than character, logical, Date, numeric and integer** go
  through `normalize_dt()` first. `cast_rules()` lists the conversions
  `diff_table()` will make, and `compare_columns()` shows how the columns of
  two tables line up.

See `?diff_table` for the details.
