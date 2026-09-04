# daffiz

<!-- badges: start -->
[![R-CMD-check](https://github.com/pedrobtz/daffiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pedrobtz/daffiz/actions/workflows/R-CMD-check.yaml)
![Coverage](https://github.com/pedrobtz/daffiz/raw/main/.github/badges/coverage.svg)
<!-- badges: end -->

`daffiz` finds numeric regressions between two tables that should be nearly
identical. It aligns rows, applies absolute and relative tolerances, and returns
an explainable cell-level result with the original row number from each input.

The package accepts data frames, data tables, and tibbles without modifying
them. Its API is experimental and may change before the first stable release.

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

## Compare two tables

```r
library(daffiz)

baseline <- data.frame(
  id = 1:3,
  amount = c(100, 200, 300)
)

candidate <- data.frame(
  id = 1:3,
  amount = c(100, 200.03, 330)
)

comparison <- compare_dt(baseline, candidate, abs_tol = 0.05)
cat(format(comparison), sep = "\n")
#> <daffiz_comparison> baseline -> candidate
#> Rows: 3 / 3 | measures: 1 | cells: 3
#> Identity: id
#> Measures: amount
#> Result: 1 cell(s) not equal

is_matching(comparison)
#> [1] FALSE

diff_cells(comparison)
#> Key: <id, .metric>
#>       id .row_x .row_y .metric .value_x .value_y .diff .abs_diff .rel_diff
#>    <int>  <int>  <int>  <char>    <num>    <num> <num>     <num>     <num>
#> 1:     3      3      3  amount      300      330   -30        30 0.0909091
#>    .match_type
#>          <fctr>
#> 1:         diff
```

By default, ordinary double columns are measures and all other columns form the
row identity. `Date` and `POSIXct` columns are identity columns by default even
though R stores them as doubles. Use `by` and `compare` when inference is not
appropriate:

```r
compare_dt(
  baseline,
  candidate,
  by = "id",
  compare = "amount"
)
```

To align rows by position instead of identity values, use:

```r
compare_dt(baseline, candidate, by = daffiz_row_number())
```

## Tolerances

A cell matches when:

```text
abs(x - y) <= max(abs_tol, rel_tol * max(abs(x), abs(y)))
```

Both tolerances can be a single value or a named vector. Use `.default` as the
fallback for measures without a specific setting:

```r
compare_dt(
  baseline,
  candidate,
  abs_tol = c(.default = 0, amount = 0.05),
  rel_tol = 1e-6
)
```

The rule is symmetric: swapping the inputs does not change whether a cell
matches.

## Duplicate identities

Duplicated identity values are ambiguous because multiple source rows can align
to the same key. Choose a policy with `duplicate_keys`:

- `"pair"` (the default) sorts rows within each duplicated group and pairs them
  deterministically. A warning explains that the pairing is heuristic. Because
  the sort is on the measure values, the pairing minimizes apparent
  differences: two tables whose duplicated rows disagree row for row can still
  report as matching.
- `"report"` records duplicated groups in `duplicate_info()` and excludes them
  from cell comparison. The result is incomplete, so `is_matching()` returns
  `FALSE` even if every retained cell is equal.
- `"error"` stops and asks for a more specific identity.

For paired duplicates, `.row_x` and `.row_y` show exactly which source rows were
aligned.

## Comparisons that align nothing

If no identity value occurs in both inputs — or one input is empty while the
other is not — there is nothing to compare: every cell would be `x_only` or
`y_only`. `compare_dt()` stops before doing any work, and shows the keys:

```r
baseline  <- data.frame(order_id = c("A001", "A002"), amount = c(10, 20))
candidate <- data.frame(order_id = c(" A001", " A002"), amount = c(10, 21))

compare_dt(baseline, candidate)
#> Error: Comparison aligns no rows.
#>   No identity value appears in both inputs (2 row(s) in `baseline`, 2 in `candidate`).
#>   Identity: order_id
#>   only in `baseline`: A001, A002
#>   only in `candidate`:  A001,  A002
#>   Check that `by=` names the right columns and that the key values have the same
#>   format on both sides, or use `by = daffiz_row_number()` to align by position.
```

The key sample is the diagnosis — here it shows the leading whitespace. Pass
`disjoint_keys = "warn"` when a partition legitimately has no overlap and you
want the `x_only`/`y_only` records anyway. Two empty inputs are equal, not
ill-formed, and are exempt. A single aligned row is a real comparison: the rule
is zero overlap, not low overlap.

## Result model

`compare_dt()` returns a `daffiz_comparison`. Start with the bounded report:

```r
summary(comparison)
```

Then inspect the affected measures and source records without writing joins or
aggregations:

```r
diff_columns(comparison)
diff_rows(comparison)
diff_rows(comparison, view = "paired")

diff_indices(comparison, "x")
original_rows(comparison, "x")
```

The public accessors are:

- `is_matching()` for the overall result;
- `all_cells()` and `diff_cells()` for auditable cell-level results;
- `column_summary()` and `diff_columns()` for measure diagnostics;
- `row_summary()` and `diff_rows()` for record diagnostics;
- `diff_indices()` and `original_rows()` for source-row recovery;
- `x_only()` and `y_only()` for unmatched source records;
- `duplicate_info()` and `key_profile()` for alignment diagnostics.

## Visualize difference patterns

There is one plot: an Amelia-style difference map, in the spirit of
`Amelia::missmap()`. Observations run down the y axis and numeric measures
across the x axis. Green means the two tables agree, red means they do not.

```r
plot_diff(comparison)
plot(comparison)          # the same map
```

Rows keep their **original source order** and are never ranked, because
position order is what makes a run of regressions in one region of the input
read as a band. Narrow the measures with `columns`:

```r
plot_diff(comparison, columns = c("amount", "score"))
```

Above `max_rows` records the map aggregates contiguous rows into at most
`row_bins` bins and shades each tile by the share of its cells that differ. No
row is ever dropped, and the subtitle says when binning applied:

```r
plot_diff(comparison, max_rows = 500, row_bins = 100)
```

The fill colours are the Okabe-Ito bluish green and vermillion, which read as
green and red but remain distinguishable under the common forms of colour
blindness. While the map is small enough to label, each differing tile also
carries a mark — `!` different, `X` only in `x`, `Y` only in `y`, and nothing
where the tables agree — so the classification never depends on colour alone.

`plot_data()` returns the exact table behind the map and does not require
`ggplot2`, which is useful for audits and custom graphics:

```r
plot_data(comparison)
plot_data(comparison, columns = "amount")
```

Each cell is classified as `equal`, `diff`, `x_only`, or `y_only`. Missing rows
are distinct from present rows containing `NA`, and `.row_x`/`.row_y` always
refer to the original input positions.

For measure- and record-level detail, use the data accessors — `column_summary()`,
`diff_columns()`, `row_summary()` and `diff_rows()` — rather than more charts.

For regression tests, `expect_dt_equal()` uses the same comparison object and
includes bounded column, row, and cell diagnostics in failures:

```r
expect_dt_equal(baseline, candidate, abs_tol = 0.05)
```

Unlike `compare_dt()`, it defaults to `duplicate_keys = "error"`: an assertion
should not pass on a heuristic row alignment. Pass `duplicate_keys = "pair"`
explicitly if you want the heuristic in a test.

The comparison materializes approximately `n_rows * n_measures` cells in
memory. Narrow wide comparisons with `compare` or `exclude`. For wide inputs,
`batch` limits how many measures are melted and joined at once without changing
the retained result:

```r
compare_dt(baseline, candidate, batch = 10)
```

Benchmarks measured the retained result at about 64 bytes per unique-key cell,
while construction required substantially more working memory. By default, a
projected result above 10 million cells therefore produces an early warning.
Narrow the measures or use `batch` before raising the configurable threshold
with `options(daffiz.max_cells = ...)`. See the
[benchmark validation](https://github.com/pedrobtz/daffiz/blob/main/benchmarks/results/threshold-validation-2026-09-01.md)
for the raw methodology and decision.

## Current scope

`daffiz` deliberately focuses on numeric regression comparison. The current
release requires the post-exclusion column sets to match and supports unclassed
integer and double measures. It does not perform schema matching, fuzzy row
alignment, or comparisons of dates, factors, or other classed values as
measures.

The [comparison workflow](https://pedrobtz.github.io/daffiz/articles/comparing-tables.html)
vignette gives a complete walkthrough.
