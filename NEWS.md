# daffiz 0.1.0

## Initial release

* Added `compare_dt()` for numeric, tolerance-aware comparison of two tables.
* Added inferred or explicit row identities and measure selection. Role
  inference reads both inputs, so `compare_dt()` resolves the same roles in
  either argument order. A column that is an unclassed integer on one side and
  an unclassed double on the other is not a default measure; the error names it
  and points at `compare=`.
* Added traceable cell results, deterministic duplicate-key policies, and
  structured validation conditions.
* Added `all_cells()`, `is_matching()`, and `duplicate_info()` accessors.
* Added column and row summaries, original-row recovery, side-only accessors,
  key profiles, bounded reports, and `expect_dt_equal()`.
* `expect_dt_equal()` defaults to `duplicate_keys = "error"`. Under the
  `compare_dt()` default of `"pair"`, duplicated groups are paired by sorted
  measure value, which minimizes apparent differences and can pass an assertion
  on tables that disagree row for row.
* Comparisons using `duplicate_keys = "report"` are marked incomplete and do
  not match because ambiguous groups were excluded from cell comparison.
* Duplicate pairing sorts measures by name in the C locale, so the pairing does
  not depend on the order `compare=` was given or on the session locale.
* Magnitude statistics (`mean_abs_diff`, `rmse`, `max_abs_diff`,
  `p95_abs_diff`, and the plot deltas) include every cell whose two source
  values are finite. They previously filtered on the derived difference, which
  discarded a genuine difference whose subtraction overflowed to infinity;
  `max_abs_diff` then reported `0` for the largest difference in a table.
  `rmse` is computed with scaling so squaring cannot overflow.
* Identity columns must have a joinable type (`logical`, `integer`, `double`,
  `character`, or a class built on one of those). `list`, `complex` and `raw`
  columns previously passed preflight and failed later with a raw
  `data.table` message.
* Identity columns must agree on join-relevant attributes. Two `difftime`
  columns with different `units` have the same type and class but different
  underlying numbers, so every row previously reported as side-only. Time zone
  is not treated as join-relevant: `POSIXct` columns in different zones hold
  the same epoch seconds and align correctly.
* `by=` and `compare=` reject a repeated column name. A repeated `compare` name
  melted the measure twice on each side, so the join went cartesian and every
  cell, count and statistic was silently multiplied.
* `key_profile()` keeps missing identity values in its `values` list, so
  `n_distinct` and `lengths(values)` agree and an `NA` key is visible.
* A column whose name is reserved by daffiz results can now be dropped with
  `exclude=` instead of having to be renamed.
* A tolerance vector that names the same column twice is an error rather than a
  silent choice between the values.
* The projected-size warning counts aligned records exactly, using the identity
  counts the duplicate scan already computes. It previously projected from the
  larger input, under-counting by up to half when the identities were disjoint.
* Added transparent measure batching through `compare_dt(batch=)` to reduce
  peak intermediate memory for wide comparisons without discarding cells.
* Added a single visualization: an Amelia-style difference map, via
  `plot_data()` and the optional `ggplot2`-based `plot_diff()`. Observations run
  down the y axis and numeric measures across the x axis, filled green where the
  two tables agree and red where they do not. Measure- and record-level
  questions are answered by the data accessors rather than by further chart
  types, so there are no ranked-measure, delta-distribution or overview plots.
* The map keeps rows in their **original source order** and never ranks them.
  Position order is what makes a run of regressions in one region of the input
  read as a band, which ranking destroys. Above `max_rows` records contiguous
  rows are aggregated into at most `row_bins` bins and shaded by the share of
  cells that differ; no row is ever dropped, and the subtitle discloses it.
* The map's fill colours are the Okabe-Ito bluish green and vermillion. They
  read as green and red but, unlike true red/green, separate on both the
  blue-yellow axis and in lightness, so they survive the common forms of colour
  blindness. Each differing tile additionally carries an ASCII mark -- `!`
  different, `X` only in `x`, `Y` only in `y` -- so the four cell classes never
  depend on fill alone.
* Added a `plot()` method so `plot(comparison)` draws the map, and an
  `all.equal()` method that ignores the internal accessor cache. `plot()`
  now errors on a positional second argument rather than silently discarding
  it: `y` belongs to the `plot()` generic and is unused, so `plot(cmp, "amount")`
  -- the natural mirror of `plot_diff(cmp, "amount")` -- drew every measure and
  reported nothing.
* Dropped the `grid` dependency, which only the removed multi-panel overview
  needed.
* Reporting no longer summarizes the all-equal records it immediately
  discards, and `format()` no longer builds a full summary. `print()` on a
  300,000-row, three-measure comparison went from about 9.6s to under 1s.
* Working tables are deep-copied only when duplicate pairing needs to sort
  them, rather than on every comparison.
* Validated long-table memory and timing with isolated benchmarks. The default
  warning now fires above 10 million projected cells; the earlier 50-million
  threshold was too late relative to measured construction memory.
* The projected-size guard no longer fails on the comparisons it exists to warn
  about. `compare_dt()` passes integer row and measure counts, whose product
  overflows `.Machine$integer.max` at about 2.1e9 cells; the resulting `NA` then
  reached `if (NA)` and aborted with `missing value where TRUE/FALSE needed`.
  The existing test passed doubles and never exercised the integer path.
* A comparison that aligns no rows is now an error (`daffiz_error_disjoint`)
  rather than a silent non-comparison. Four cases reached the end of the
  pipeline without a word: fully disjoint identities, and either input being
  empty while the other has rows. All four produced a result whose every cell
  was `x_only` or `y_only`, with `n_compared` of zero, reporting "differences"
  that were really two unrelated tables. The error carries a bounded sample of
  the keys on each side, which is what actually identifies the cause -- usually
  a wrong `by=`, or key values that agree in type but not in format (padded or
  trimmed codes, a locale or encoding skew).
* The alignment gate runs *before* the melt. The disjoint case is the one that
  builds the largest possible cell table -- the outer join yields `n_x + n_y`
  records, every one of them useless -- so a guard placed after it would be
  worth nothing.
* Added `compare_dt(disjoint_keys=)`, `"error"` (default) or `"warn"`, for the
  batch workflow that legitimately expects some partitions not to overlap.
  `expect_dt_equal()` inherits the strict default. Two empty inputs remain
  equal rather than ill-formed and are exempt from the gate; a single aligned
  row is enough to be a real comparison, so the gate is zero-overlap rather
  than an arbitrary low-overlap threshold.
* One identity scan now serves both the alignment gate and the duplicate
  policy, instead of the counts being built twice.
* Removed `duplicate_groups()`, which was defined but never called.
