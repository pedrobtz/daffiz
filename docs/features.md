# Feature Extraction Notes

This is the implementation backlog derived from the four reference tools and
the current prototype. The contracts and release phases in
[design-plan.md](design-plan.md) are authoritative.

Sources are abbreviated as **daff**, **DCP** (DataComPy), **diffly**,
**anofox**, and **draft** (the current `compare_dt()`).

## Summary

| # | Feature | Source | Priority |
|---|---|---|---|
| 0 | Fail-fast identical-column validation | draft | Prerequisite |
| 1 | Canonical melted numeric cell table | draft | Core |
| 2 | Original row indices through melt/join | new | Core |
| 3 | `by=` and `compare=` overrides | DCP, diffly, anofox | Core |
| 4 | Scalar and per-column numeric tolerance | DCP, diffly | Core |
| 5 | Stable presence and difference classification | draft, anofox | Core |
| 6 | Column difference summary | DCP, diffly | Diagnostics |
| 7 | Row difference summary and original-row recovery | DCP, diffly | Diagnostics |
| 8 | Interrogable cached comparison object | diffly | Diagnostics |
| 9 | Concise report and bounded samples | DCP, diffly | Diagnostics |
| 10 | Regression-test assertion | diffly | Testing |
| 11 | Global overview visualization | daff, diffly | Visualization |
| 12 | Key profile for unmatched rows | draft | Differentiator |

## 0. Fail-fast preflight

Keep the draft's staged behavior. Before sorting or melting:

1. validate table inputs and unique names;
2. resolve `exclude`;
3. require identical remaining column sets;
4. validate explicit `by` and `compare` columns;
5. validate join and numeric types;
6. validate tolerances and ensure at least one identity and measure.

The identical-column error lists columns unique to each side. Column order is
not significant; after validation, reorder `y` to match `x`. Different row
counts are allowed because unmatched rows are part of the diff.

## 1. Canonical melted numeric cell table

Retain the prototype's central representation:

```r
m_x <- melt(x, id.vars = by, measure.vars = compare,
            variable.name = ".metric", value.name = ".value_x")
m_y <- melt(y, id.vars = by, measure.vars = compare,
            variable.name = ".metric", value.name = ".value_y")
cells <- merge(m_x, m_y, by = c(by, ".occurrence", ".metric"), all = TRUE)
```

The public table also contains original source indices, `.diff`, `.abs_diff`,
`.rel_diff`, and `.match_type`. Equal cells stay in the canonical table; the
filtered difference table is an accessor.

Restricting V1 to numeric measure columns is what makes this long result simple
and type-stable. Do not broaden the melt to heterogeneous character, date, or
list measures in V1.

## 2. Original row indices

After fail-fast schema validation but before sorting, duplicate pairing, or
melting, stamp both copies with their original one-based row positions. Column
exclusion does not change those positions. Retain `.row_x` and `.row_y` as value
columns through the full join; they are not part of the join key.

These indices enable both direct subsetting and stored-snapshot access:

```r
x[diff_indices(cmp, "x")]
original_rows(cmp, "x", type = "affected")
```

Row accessors deduplicate indices because an affected input row may contribute
several melted numeric cells.

## 3. Default roles with explicit overrides

Preserve the convenient intent while excluding classed doubles such as dates:

```r
compare <- shared columns with typeof(x) == "double" and no class
by      <- all remaining shared columns
```

Add explicit `by=` and `compare=` for cases where a categorical column may
change, a double is an identifier, or an integer is a measure. `Date` and
`POSIXct` are classed doubles and must not be inferred as ordinary measures.
The report must show whether roles were inferred or explicit.

An explicit `compare` restricts the measures; unselected doubles do not become
identity columns. With explicit `by` and/or `compare`, columns assigned to
neither role are ignored by numeric matching but remain part of strict schema
validation and original-row output.

Duplicate identities retain deterministic occurrence pairing by default, with
a visible warning and auditable original indices. `report` and `error` policies
remain available.

## 4. Numeric tolerance

Adopt DCP/diffly's scalar and per-column configuration while keeping the
draft's symmetric equality rule:

```r
abs(.value_x - .value_y) <=
  pmax(abs_tol, rel_tol * pmax(abs(.value_x), abs(.value_y)))
```

```r
compare_dt(
  x,
  y,
  abs_tol = c(.default = 1e-8, amount = 0.01),
  rel_tol = 0
)
```

Do not narrow doubles to integers. Explicit integer measures may be promoted to
double for comparison while original snapshots retain their types.

## 5. Presence and match classification

Keep the side-presence-column technique. It distinguishes a missing row from a
present row containing `NA` without inventing sentinel data values.

The stable `.match_type` vocabulary is:

```text
x_only, y_only, equal, diff
```

Both source values missing compare equal; exactly one missing compares
different. Tolerance applies only to present finite numeric pairs.

## 6. Column diagnostics

`column_summary()` aggregates the melted table by numeric column. It includes:

```text
n_compared, n_equal, n_diff, n_x_only, n_y_only,
fraction_diff, mean_diff, mean_abs_diff, rmse, max_abs_diff, p95_abs_diff
```

`diff_columns()` filters and ranks affected columns. This should be the first
table in `print()` and in a regression-test failure.

## 7. Row diagnostics

`row_summary()` groups by identity plus `.row_x` and `.row_y` and includes:

```text
row_type, n_diff, diff_columns, mean_abs_diff, max_abs_diff
```

`diff_rows()` provides summary, paired-wide, `x`, and `y` views.
`diff_indices()` makes source subsetting trivial, and `original_rows()` returns
the exact stored input rows.

## 8. Comparison object

Following diffly, `compare_dt()` returns one `daffiz_comparison` object rather
than an eager list of unrelated outputs. The canonical melted table is computed
once. Column/row summaries, original-row views, reports, and plot data are
derived on demand and cached.

The old list elements can be exposed temporarily through compatibility
accessors rather than remain the public contract.

## 9. Report

Following DataComPy and diffly, report in this order:

1. result and resolved identity/measures;
2. input rows and melted cell count;
3. differing cell, row, and column counts;
4. ranked differing columns;
5. ranked differing rows with original indices;
6. bounded changed-cell examples;
7. duplicate-pairing and unmatched-row diagnostics.

Every number must reconcile with a public accessor.

## 10. Testing assertion

`expect_dt_equal()` uses the same engine and settings as interactive analysis.
Failure output contains bounded column, row, and cell diagnostics, including
the original row indices needed to reproduce the failure.

## 11. Global visualization

Add an optional `ggplot2` layer after the diagnostic accessors:

- mismatch count/rate by numeric column;
- distribution of changed-column counts per affected row;
- row-by-column incidence heatmap;
- signed or absolute delta distributions.

For large tables, the overview bins all rows rather than using an unlabelled
random sample. `plot_data()` returns the exact aggregate behind each plot, and
plots label any top-N filtering, row selection, or binning.

## 12. Keep `key_profile()`

The prototype's profile ranks identity columns by distinct-value count among
unmatched rows. It remains useful for explaining concentrations of `x_only` or
`y_only` rows and should become an on-demand accessor.

## Later

These should not delay the numeric regression workflow:

- character, temporal, nested, or custom measure comparators;
- schema diff beyond the fail-fast column contract;
- ordered row/column edit operations;
- patch and three-way merge;
- HTML reports or database backends.
