# daffiz Design and Implementation Plan

Status: revised design, 2026-08-29

This document is the implementation contract for `daffiz`. It replaces any
conflicting recommendations in `features.md`; `state-of-the-art.md` remains the
research background.

## 1. Product definition

`daffiz` is primarily a numeric regression-comparison package for two
`data.table`s that are expected to be almost the same. The typical case is:

- the tables have the same schema;
- non-measure columns identify rows and are normally unchanged;
- ordinary, unclassed double columns contain measures;
- only a small number of numeric cells differ;
- the user needs to find the affected columns and rows quickly.

The existing `compare_dt()` pipeline is the right foundation for this use case:
copy, resolve identifiers and numeric measures, melt both tables, full-join the
long tables, calculate a numeric delta, apply tolerance, and classify every
numeric cell.

The package should answer four questions in order:

1. Do the numeric measures match within tolerance?
2. Which numeric columns differ, and by how much?
3. Which original rows differ, and how can the caller recover them?
4. What is the global shape of the differences?

The first release targets in-memory `data.table` inputs, while accepting
ordinary `data.frame` and tibble inputs through conversion. The engine remains
`data.table`; inputs are never modified by reference.

## 2. Scope

### V1 scope

- A canonical melted `data.table` with one row per aligned numeric cell.
- Exact and tolerant comparison of ordinary, unclassed double columns.
- Optional explicit comparison of other numeric columns after safe promotion
  to double.
- Default identity based on columns not inferred as measures, preserving the
  intent of the current draft behavior.
- An explicit `by=` override when the default identity is not appropriate.
- Column-level and row-level summaries derived from the melted table.
- Original input row indices and accessors for recovering original rows.
- Concise reports, regression-test helpers, and optional overview plots.

### Not required for V1

- A general comparator for every R class.
- A heterogeneous all-cell diff.
- Content-similarity row alignment.
- Automatic column rename detection.
- Patch or three-way merge.
- Database or out-of-memory backends.

Those may be added later, but they must not complicate the numeric comparison
path.

## 3. What to retain and what to correct

### Retain from the current draft

1. `data.table` as the engine.
2. Copies of caller inputs before any by-reference operation.
3. `exclude=` before schema and type checks.
4. The draft's intent that contextual columns form the default row identity.
5. The draft's intent that ordinary double columns are the default measures.
6. Deterministic duplicate-group pairing through an occurrence key.
7. One `melt()` per input followed by a full outer join.
8. Side-presence columns to distinguish absent rows from present `NA` values.
9. The symmetric tolerance rule.
10. The long `all` result as the source for every summary and accessor.
11. `key_profile()` for describing unmatched rows.

### Correct before extending it

1. Add original row indices before sorting or melting and carry both through
   the join.
2. Do not coerce a double column toward integer. Explicitly selected integer
   measures may be promoted to double; narrowing is forbidden.
3. Allow `by=` and `compare=` overrides without removing the convenient
   defaults.
4. Give the melted result a documented, stable schema and factor levels.
5. Aggregate the cell result into first-class column and row diagnostics.
6. Keep snapshots of the original inputs so row accessors are stable even if a
   caller later mutates a `data.table` by reference.
7. Bound printed samples and visualizations so a large comparison remains
   usable.
8. Replace attachment-dependent calls with imports or qualified
   `data.table::` calls.
9. Do not use `is.double()` alone for role inference: it is also true for
   `Date` and `POSIXct`. Default measures must be unclassed doubles; classed
   doubles remain identity/context columns unless a later comparator supports
   them explicitly.

The current code should therefore be refactored incrementally rather than
replaced with a different comparison model.

## 4. Alignment and column selection

### 4.1 Fail-fast preflight

Comparison is a sequence of gates. A failed gate stops immediately, before any
sorting, melting, joining, or summary work:

1. Both inputs are table-like and have unique column names.
2. Input names do not collide with the reserved result columns (`.row_x`,
   `.row_y`, `.occurrence`, `.metric`, `.value_x`, `.value_y`, `.diff`,
   `.abs_diff`, `.rel_diff`, `.match_type`).
3. Resolve `exclude` logically for schema validation; do not mutate the inputs.
4. The remaining column **sets are identical**. Column order may differ; `y` is
   reordered to the order of `x` after this check.
5. Every explicit `by` and `compare` column exists, and the two sets do not
   overlap.
6. Corresponding identity columns have identical `typeof()`, class, and
   relevant join attributes.
7. Corresponding measure columns are unclassed integer/double vectors and can
   be promoted to double without lossy narrowing.
8. At least one identity and one measure column have been resolved.
9. Tolerances are finite, non-negative, and name only resolved measures.

The column-set check is intentionally strict for V1. A missing or extra column
usually means the regression output contract changed, so returning a partial
numeric comparison would hide the more important failure. The error lists
columns only in `x` and only in `y`.

Row counts and row identities are not fail-fast conditions: different rows are
represented as `x_only` and `y_only` results. Duplicate identities are handled
later according to `duplicate_keys`.

Each gate raises a stable condition subclass—such as
`daffiz_error_columns`, `daffiz_error_roles`, `daffiz_error_types`, or
`daffiz_error_tolerance`—with structured fields as well as readable text. Tests
can therefore verify the failure stage without parsing an error message.

### 4.2 Default behavior

After applying `exclude`:

```r
is_default_measure <- function(column) {
  typeof(column) == "double" && !is.object(column)
}

compare_columns <- names(x)[vapply(x, is_default_measure, logical(1))]
by_columns      <- setdiff(names(x), compare_columns)
```

This default is intentionally optimized for the stated use case. It is also
reported prominently because a change in a default identity column appears as
one `x_only` row and one `y_only` row rather than as a numeric cell change.

The comparison errors if there are no numeric measures or no usable identity
columns. Positional comparison can be requested explicitly with
`by = daffiz_row_number()` rather than inferred silently.

### 4.3 Explicit overrides

- `by=` supplies one or more identity columns.
- `compare=` supplies unclassed numeric measures to melt and compare.
- `exclude=` removes columns before either set is resolved.
- `by` and `compare` must not overlap.
- Every requested column must exist on both sides.
- Explicit integer measures are safely promoted to double in the melted value
  columns; source snapshots retain their original integer type.

This keeps the zero-configuration intent of the current behavior while making
the important exceptions explicit: changing categorical columns,
double-valued identifiers, or integer measures. Dates and datetimes are not
silently treated as numeric measures.

Role resolution is deterministic:

| `by` | `compare` | Resolved roles |
|---|---|---|
| `NULL` | `NULL` | Default context identity and all default measure candidates |
| explicit | `NULL` | Explicit identity and default measure candidates not used by it |
| `NULL` | explicit | Default context identity excluding explicit measures; only requested measures are compared |
| explicit | explicit | Exactly the two requested sets |

When either set is explicit, columns assigned to neither role are ignored by
numeric alignment/comparison but remain subject to the identical-column
preflight and remain available in original-row views. An unselected double must
not silently become an identity column.

### 4.4 Duplicate identities

The draft's occurrence key is retained. Duplicate groups are reported and then
paired deterministically according to `duplicate_keys`:

- `"pair"` — default; sort deterministically, add a within-group occurrence
  number, and compare paired rows;
- `"report"` — report ambiguous groups without cell-level pairing;
- `"error"` — stop before comparison.

`"pair"` is useful for near-identical data but is heuristic. Reports and test
failures must state when it was used. Original row indices remain attached, so
every pairing can be audited. Sorting uses comparison data and then original
row position as a final tie-breaker; the internal row-id column must not become
a leading sort key.

## 5. Proposed public API

Keep `compare_dt()` as the primary function and evolve it compatibly:

```r
cmp <- compare_dt(
  x,
  y,
  by = NULL,                         # default: columns not inferred as measures
  compare = NULL,                    # default: shared unclassed doubles
  exclude = NULL,
  abs_tol = c(.default = 0, amount = 0.01),
  rel_tol = 0,
  duplicate_keys = c("pair", "report", "error"),
  x_name = NULL,
  y_name = NULL
)
```

`compare_df()` may be a friendly alias, but it must call the same engine.
`x_name` and `y_name` default to captured argument expressions. Scalar
tolerances apply to every measure; named vectors use `.default` for unspecified
columns.

The main accessors are:

```r
matches(cmp)
summary(cmp)

all_cells(cmp)
diff_cells(cmp, columns = NULL)

column_summary(cmp)
diff_columns(cmp)

row_summary(cmp)
diff_rows(cmp, view = c("summary", "paired", "x", "y"))
diff_indices(cmp, side = c("x", "y"))
original_rows(
  cmp,
  side = c("x", "y"),
  type = c("affected", "changed", "only", "all")
)

x_only(cmp)
y_only(cmp)
key_profile(cmp, side = c("x", "y"))

plot_data(cmp, type = c("overview", "columns", "matrix", "delta"))
plot_diff(cmp, type = c("overview", "columns", "matrix", "delta"))
```

`print.daffiz_comparison()` shows the overall result followed by the worst
columns and rows. Every printed number and every plot is backed by a data
accessor.

## 6. Comparison object and canonical melted table

`compare_dt()` returns an S3 object of class `daffiz_comparison`. Conceptually
it contains:

```text
settings          resolved by/compare columns, tolerances and labels
x_original        private snapshot in original order and original types
y_original        private snapshot in original order and original types
cells             canonical melted numeric comparison table
duplicate_info    duplicated identities and occurrence-pairing details
cache             derived column, row, report and plot data
```

Accessors return copies so callers cannot mutate cached state by reference.

### 6.1 Stable cell-table schema

The canonical `cells` table contains:

```text
<by columns>
.occurrence       present only when duplicate pairing is needed
.row_x            original one-based row number in x
.row_y            original one-based row number in y
.metric           compared numeric column name
.value_x          numeric value from x
.value_y          numeric value from y
.diff             .value_x - .value_y
.abs_diff         abs(.diff)
.rel_diff         symmetric relative difference
.match_type       x_only | y_only | equal | diff
```

`.row_x` and `.row_y` are data, not join keys. The join key is
`c(by_columns, ".occurrence", ".metric")`; therefore different original row
numbers do not prevent corresponding rows from joining.

The side-presence helpers used during the join are removed from the public
table after `.match_type` is calculated. `NA` indices identify a row absent from
that side.

The table includes equal cells because they provide denominators for summary
rates and data for global visualization. `diff_cells()` is the cheap filtered
view used most often.

For finite, present pairs, `.rel_diff` is
`.abs_diff / max(abs(.value_x), abs(.value_y))`; it is zero when both values
are zero and `NA` when the ratio is undefined.

### 6.2 Match classification

For each melted cell:

```text
x presence missing                         -> y_only
y presence missing                         -> x_only
both source values missing (NA or NaN)     -> equal
exactly one source value missing           -> diff
same signed infinity                       -> equal
different infinities or finite/infinite    -> diff
abs(.diff) <= max(abs_tol,
                 rel_tol * max(abs(x), abs(y))) -> equal
otherwise                                  -> diff
```

Both missing values, including two `NaN` values, compare equal. `NA` versus
`NaN` follows the draft's missing-value semantics and also compares equal in
V1. Infinities and tolerance boundaries require explicit tests. Equality is
symmetric: reversing the tables changes the sign of finite `.diff`, not the
match decision.

## 7. Column diagnostics

The first follow-up question is usually “which measures changed?”

`column_summary()` returns one row per numeric measure, sorted by input column
order unless the caller requests another order:

```text
column
n_cells             total joined numeric cells
n_compared          cells present on both sides
n_equal
n_diff
n_x_only
n_y_only
fraction_diff       n_diff / n_compared
mean_diff           mean(.value_x - .value_y) over finite pairs
mean_abs_diff
rmse
max_abs_diff
p95_abs_diff
```

`diff_columns()` filters to columns with `n_diff`, `n_x_only`, or `n_y_only`
greater than zero and sorts by `n_diff`, `fraction_diff`, then
`max_abs_diff`. This table is printed near the top of the summary.

`diff_cells(cmp, columns = "amount")` provides the exact contributing cells,
so every aggregate can be audited.

## 8. Row diagnostics and original-row recovery

The second follow-up question is “which source records changed?” Original row
identity must survive all sorting, duplicate pairing, melting, and joining.

### 8.1 Capture indices before transformation

Immediately after copying the inputs, assign the reserved row ids:

```r
x[, .row_x := seq_len(.N)]
y[, .row_y := seq_len(.N)]
```

Reserved-name collisions have already failed in preflight. The indices refer
to the original input order, before exclusion or duplicate sorting.

### 8.2 Row summary

`row_summary()` groups the melted cell table by the resolved identity and
original indices. It returns one row per aligned or unmatched record:

```text
<by columns>
.occurrence
.row_x
.row_y
row_type            equal | diff | x_only | y_only
n_columns
n_diff
diff_columns        list column of changed numeric column names
mean_abs_diff
max_abs_diff
```

`diff_rows(view = "summary")` filters this table to non-equal rows and sorts by
`n_diff`, `max_abs_diff`, and original index.

### 8.3 Recovering original data

`diff_indices(cmp, "x")` and `diff_indices(cmp, "y")` return sorted, unique,
one-based integer indices for every non-equal row available on that side,
including matched numeric changes and side-only rows. This supports direct
subsetting:

```r
x[diff_indices(cmp, "x")]
y[diff_indices(cmp, "y")]
```

`original_rows()` performs the same operation against the stored snapshots and
returns the exact original columns, order, classes, and values. It adds the
appropriate `.row_x` or `.row_y` column for traceability. `type = "changed"`
restricts to matched rows with numeric differences; `"only"` restricts to
unmatched rows.

`diff_rows(view = "paired")` returns a convenient wide diagnostic with the
identity, both row indices, and selected original values suffixed `_x` and
`_y`. The `"x"` and `"y"` views return original differing rows from one side.

Unmatched rows are deduplicated by source index because the melted table
contains one `x_only` or `y_only` cell per numeric measure.
`x_only()` and `y_only()` return these deduplicated original rows; their melted
cells remain available through `all_cells()`.

## 9. Summary and reporting

The default print should fit on a terminal screen and lead with the outcome:

```text
daffiz comparison: baseline -> candidate
Rows: 100,000 / 100,000 | numeric columns: 18 | compared cells: 1,800,000
Result: 247 cells differ in 84 rows across 3 columns
Tolerance: abs 1e-08, rel 0

Columns with differences
column       n_diff  fraction_diff  max_abs_diff
amount           92          0.09%          41.2
score            81          0.08%           3.0
ratio            74          0.07%           0.2

Rows with differences
.row_x  .row_y  n_diff  columns            max_abs_diff
   712     712       3  amount,score,ratio          41.2
  9184    9184       2  amount,ratio                8.7
```

The report also states:

- inferred or explicit identity columns;
- inferred or explicit numeric measures, including exclusion of classed
  doubles such as dates from the default measure set;
- duplicate identities and the pairing policy;
- unmatched row counts;
- excluded or unknown columns;
- bounded examples of changed cells.

## 10. Visualization

Visualization is a view over the comparison object, never part of the matching
logic. `ggplot2` belongs in `Suggests`, so numeric comparison works without it.

### 10.1 Plot types

`plot_diff(cmp, type = "overview")` is the default global perspective and
combines three small views:

1. **Column bar chart** — mismatch fraction and count per numeric column.
2. **Row-impact distribution** — number of changed numeric columns per affected
   row.
3. **Difference-incidence matrix** — rows or row bins by numeric column,
   showing where differences concentrate.

Additional types are:

- `"columns"` — ranked column mismatch counts/rates;
- `"matrix"` — a row-by-column heatmap of `equal`, `diff`, `x_only`, and
  `y_only` cells;
- `"delta"` — per-column distributions of signed or absolute numeric deltas.

### 10.2 Scale and honesty

A literal heatmap is useful for hundreds, not millions, of rows. The plotting
contract therefore includes:

- `max_rows` for a detailed matrix;
- deterministic selection of the most affected rows when detail is requested;
- row-bin aggregation for the global overview, so the whole data set is
  represented rather than a random sample;
- optional column filtering and top-N selection;
- clear labels when aggregation or truncation is applied;
- tolerance-normalized magnitude only when the denominator is defined.

`plot_data()` returns the exact aggregated data used by each plot. This keeps
visual results testable and allows users to build their own plots.

## 11. Execution pipeline

V1 follows the current draft closely:

1. Run the fail-fast preflight gates from section 4.1, applying `exclude` before
   requiring identical column sets.
2. Reorder `y` columns to the validated order of `x`.
3. Copy the complete inputs and add original row indices; retain these as the
   private source snapshots.
4. Derive working copies with the validated exclusions and resolve the
   validated `compare` measures and `by` identity columns.
5. Detect duplicate identities; warn/report and add `.occurrence` according to
   policy.
6. Melt each table using `by` plus the original row id as retained columns and
   `compare` as measure columns.
7. Add side-presence columns.
8. Full-join on identity, optional occurrence, and numeric column name while
   retaining `.row_x` and `.row_y` as values.
9. Calculate `.diff`, `.abs_diff`, `.rel_diff`, and `.match_type` using
   vectorized `data.table` expressions.
10. Store the complete long result in a `daffiz_comparison` object.
11. Compute column, row, original-row, report, and plot views on demand and
    cache them.

There is no row-wise R loop in the comparison path. A loop over numeric columns
is permitted only to resolve named tolerances or calculate per-column summary
statistics.

The long representation has `n_rows * n_measures` rows by design. Benchmarks
and documentation must make that cost explicit. A warning threshold may be
added for unusually large products, but the package should not silently switch
to a different result model.

## 12. Regression-testing interface

The same object backs interactive analysis and test failures:

```r
expect_dt_equal(
  object,
  expected,
  ...,
  max_columns = 10L,
  max_rows = 10L,
  max_cells = 20L
)
```

A failure includes:

- the overall differing cell, row, and column counts;
- the top entries from `diff_columns()`;
- original `.row_x` and `.row_y` indices from `diff_rows()`;
- a bounded cell sample with `.value_x`, `.value_y`, and `.diff`;
- the exact identity and tolerance policy.

Failure text is deterministic and snapshot-friendly. The complete comparison
object should be attached to the condition where testthat supports it.

## 13. Package structure and dependencies

Proposed source layout:

```text
R/
  compare-dt.R          constructor and melted comparison pipeline
  comparison.R          S3 object, validation and print
  resolve-columns.R     by/compare/exclude resolution
  duplicates.R          occurrence pairing and diagnostics
  accessors-cells.R
  accessors-columns.R
  accessors-rows.R
  report.R
  plot.R
  expectations.R
  key-profile.R
```

Dependencies:

- `data.table` in `Imports`;
- `testthat (>= 3.0.0)` in `Suggests`;
- `ggplot2` in `Suggests` for `plot_diff()`;
- base R for conditions and plain-text rendering.

`charlatan` should not be a runtime dependency. Deterministic fixtures belong
in tests and can be generated with base R.

## 14. Test strategy

### Core cases

- identical tables;
- one changed numeric cell;
- several changed measures in one row;
- one measure changed across many rows;
- `x_only` and `y_only` rows;
- both values `NA`, one value `NA`, `NaN`, and signed infinities;
- tolerance just below, exactly at, and above the boundary;
- scalar and named per-column tolerances;
- explicit `by` and inferred non-double identity;
- explicit integer measures promoted safely;
- dates and datetimes are not inferred as double measures;
- duplicate identity policies;
- excluded and missing requested columns;
- column-set mismatch fails before duplicate detection or melting;
- identity-type and measure-type mismatch fail before melting;
- empty inputs and no numeric measures;
- unusual column names are supported; reserved result-name collisions fail
  before melting;
- no mutation of either caller input.

### Index and accessor invariants

- `.row_x` and `.row_y` refer to pre-sort input rows.
- after removing the added `.row_x`, `original_rows(cmp, "x")` equals
  `x[diff_indices(cmp, "x")]` including classes and column order.
- Recombining `diff_cells()` by column reproduces `diff_columns()` counts.
- Recombining `diff_cells()` by row indices reproduces `diff_rows()` counts.
- `sum(column_summary$n_diff)` equals the number of `.match_type == "diff"`
  cells.
- Reversing inputs swaps indices/sides and negates finite `.diff` values without
  changing equality.
- Unmatched source rows appear once in row accessors even though they have one
  melted record per numeric measure.

### Visualization tests

- `plot_data()` counts reconcile with column and row accessors.
- row-bin aggregation includes every eligible comparison row exactly once;
- top-row selection is deterministic;
- plots label truncation and aggregation;
- plotting fails with an actionable message when `ggplot2` is unavailable.

### Performance tests

Benchmark the actual long-table workload:

- 1e3, 1e5, and 1e6 input rows;
- 5, 20, and 100 numeric measures;
- 0%, 0.1%, 1%, and 20% differing cells;
- unique and duplicate identities;
- time and peak allocation for construction, column summary, row summary, and
  overview plot data.

The benchmark report must state both input dimensions and the resulting melted
cell count.

## 15. Staged implementation

### Phase 0 — buildable package

- Complete `DESCRIPTION`, license, imports, exports, and roxygen documentation.
- Rename the R files and remove the runtime `charlatan` dependency.
- Add testthat edition 3 and a clean `R CMD check` baseline.

### Phase 1 — stabilize the melted engine

- Retain `compare_dt()` and refactor it into small internal steps.
- Implement the ordered fail-fast validation gates, including strict identical
  column sets after `exclude`.
- Add `by`, `compare`, named tolerances, original row indices, and stable
  presence/occurrence names.
- Remove lossy double-to-integer coercion.
- Return `daffiz_comparison` while keeping `all_cells()` equivalent to the
  current `all` table.
- Test equality, tolerance, missingness, unmatched rows, duplicates, and index
  preservation.

**Exit criterion:** the canonical melted table is correct, documented, and
traceable back to both original inputs.

### Phase 2 — column and row experience

- Add cell, column, row, index, and original-row accessors.
- Add compact `print()` and detailed `summary()` methods.
- Retain `key_profile()` as an on-demand accessor.
- Add `expect_dt_equal()` with bounded deterministic diagnostics.

**Exit criterion:** a user can move from a failed comparison to the responsible
columns, source row numbers, original records, and exact cells without writing
a join or aggregation.

### Phase 3 — global visualization

- Add `plot_data()` and optional `ggplot2`-based `plot_diff()`.
- Implement column, matrix, delta, and composite overview plots.
- Add deterministic top-row and row-bin behavior for large tables.

**Exit criterion:** plots reconcile exactly with accessors and communicate any
aggregation or truncation.

### Phase 4 — extensions driven by real use

- Additional numeric classes or temporal tolerance.
- General per-column comparators only if numeric regression cases require them.
- Schema diff, patch/merge, or DuckDB backends as separate layers.

These extensions must preserve the canonical numeric melted table and must not
slow or obscure the default path.

## 16. V1 release boundary

The smallest useful release is the end of Phase 2. It provides:

- the retained melted numeric `data.table` design;
- default contextual identity/unclassed-double measures plus explicit `by` and
  `compare` overrides;
- symmetric absolute/relative tolerance;
- original row indices and direct original-row recovery;
- clear column, row, and cell diagnostics;
- deterministic reports and regression-test failures;
- documented time and memory characteristics;
- a clean package check.

Visualization is the next release milestone and can be developed without
changing comparison semantics because all plot data derives from the same
melted result.
