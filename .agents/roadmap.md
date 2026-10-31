# Roadmap: features dropped in the core rebuild

Rebuilding around the new core (see `plan.md`) removes most of the draft's
surface. This file records each dropped feature:

- **Old code:** where it lived. Once Phase 0 tags the release, read it with
  `git show draft-0.1.0:<path>`.
- **Why:** why it was dropped.
- **Re-add:** what bringing it back on top of the core would take.

Priority: **H** is wanted soon after 0.1.0, **M** is useful, **L** only if
someone asks.

## Comparison semantics

| Feature | Old code | Why dropped | Re-add on the core | Pri |
|---|---|---|---|---|
| `rel_tol`, and tolerance per measure (`c(.default = 0, amount = 0.01)`) | `R/preflight.R` `resolve_tolerance()`, `R/compare-dt.R` `classify_cells()` | The core has one absolute tolerance | `rel_tol` is done: one value for every measure, same when `abs_diff <= tolerance` or `rel_diff <= rel_tol`, finite pairs only. Still open: a value per measure (resolve a named vector over the measures, match it on `metric`); port the rest of the draft's `test-tolerance.R` | M |
| `.abs_diff`, `.rel_diff` columns | `classify_cells()` | They can be derived from `diff` | Done as `abs_diff` and `rel_diff` (relative to the larger magnitude, scaled before subtracting so it cannot overflow) | done |
| `exclude=` | `R/preflight.R` gate 3 | Not in the core | Drop the columns right after `canonical_names()`, before the column step; warn on unknown names | M |
| Role inference: shared unclassed doubles, and hints for numeric type skew | `R/resolve-columns.R`, `R/preflight.R` | Replaced by casting (`CAST_RULES`) and selecting measures by type | Probably never; casting covers the int/double skew | — |
| Checks on identity attributes (factor `levels`, difftime `units`) | `R/preflight.R` `column_signature()` | Factor and difftime are not cast target types, so `x` refuses them and `normalize_dt()` converts them | Only if difftime/units become cast types | L |
| Allow-list of identity types (reject complex/raw/list) | `daffiz_joinable_types` | `CAST_TYPES` is itself an allow-list | — (covered) | — |
| Zero-overlap gate (`disjoint_keys = "error"/"warn"`) | `R/preflight.R` `preflight_alignment()` | Not in the core | 0.1.0 has the error (`plan.md`); the `"warn"` opt-out stays here | M |
| Projected-size warning (`options(daffiz.max_cells)`) | `R/compare-dt.R` `warn_projected_size()` | Not in the core | `merge_dry_run()` gives exact row counts cheaply; multiply by the number of measures, in doubles | M |
| Batching by measure (`batch=`) | `R/batching.R` | An optimisation, not a feature; re-benchmark on the new pipeline first | Loop `melt → merge → classify` over groups of measures, then `rbindlist()` | L |
| Lossless type widening in `mode = "equal"` (integer ↔ double compared as double) | The draft promoted integer measures (`R/compare-dt.R`) | `"equal"` mode requires identical types, since it has no source of truth to cast to | Widen both sides to double for the integer/numeric pair only; never narrow | M |
| Report labels (`x_name`/`y_name`) | `capture_label()` | No report to label yet | Arguments of `diff_report()`; not captured from the call, since the report sees only the result | done |

## Duplicate keys

| Feature | Old code | Why dropped | Re-add on the core | Pri |
|---|---|---|---|---|
| `duplicate_keys = "pair"` (sort by measure values, then pair) | `R/duplicates.R` | It can produce false passes: it pairs rows so as to minimise the differences (`review.md` P1 #3) | Do not re-add as a default. If ever wanted, add it as an opt-in to `disambiguate_by_key()` that sorts on all measures at once, never per measure | L |
| `duplicate_keys = "report"` (leave ambiguous groups uncompared; the result is "incomplete") | `R/duplicates.R` | `"aggregate"` answers the same question without dropping rows | Anti-join the duplicated keys before the melt and return them as an attribute | L |
| `disambiguate_by_key(method = "value_rank")` (core) | `.agents/compare.R` | Ranks within each measure, so a "row" means a different source row for each measure (`core-review.md` D-d) | Rank on a single chosen measure, or on the whole row | L |
| `aggregate_by_key(stats = "moments")` (core) | `.agents/compare.R` | `diff_table()` uses only `"exact"` | Keep internal; expose it if tolerance-aware aggregate comparison is wanted | L |
| `duplicate_info()` accessor | `R/comparison.R` | No comparison object | Expose the `"duplicates"` attribute of `unique_by_key()` | M |

## Result object and accessors

The draft returned a `daffiz_comparison` S3 object holding the settings, deep
copies of both inputs, the cell table and a cache environment. The rebuild
returns a plain `data.table` (see `plan.md`), so everything that relied on the
stored snapshots or settings is gone for now.

| Feature | Old code | Re-add on the core | Pri |
|---|---|---|---|
| `column_summary()`, `diff_columns()` | `R/accessors-columns.R` | Mostly returns in 0.1.0 as `diff_summary()`; the `p95_abs_diff` and `fraction_diff` ranking are dropped | H |
| `original_rows()`, `x_only()`, `y_only()`, `diff_indices()` | `R/accessors-rows.R` | Needs the inputs: either `original_rows(d, x, side)` taking them back, or an S3 object that stores them. The indices are `unique(d[status != "same", row_id_x])` | H |
| `row_summary()`, `diff_rows(view = "summary"/"paired"/"x"/"y")` | `R/accessors-rows.R` | Group the diff table by key/`key_seq`; the `"paired"` view pivots `value_x`/`value_y` wide | M |
| `all_cells()`, `diff_cells()` | `R/accessors-cells.R` | The table *is* all cells; `d[status != "same"]` gives the rest | — |
| `is_matching()` | `R/comparison.R` | `all(d$status == "same")`, plus "no duplicate groups dropped" if `"report"` returns | M |
| `key_profile()` | `R/key-profile.R` | Profile the identity columns of the `only_x`/`only_y` rows | L |
| `summary()`, `print()`, `format()` methods with bounded sections | `R/report.R`, `R/comparison.R` | Re-added as `diff_report()` (`R/report.R`), a `daffiz_report` object with `format()`/`print()`, laid out after the summary of the Python package diffly. The result stays a plain `data.table`, so the report is asked for, not printed by default. Still open: a JSON/list export, and using the report in `expect_table_equal()`'s failure text | done |
| `all.equal.daffiz_comparison()` | `R/comparison.R` | It existed only because of the cache environment; not needed without one | — |
| Structured conditions for every gate (`daffiz_error_input/columns/roles/types/tolerance/duplicates/disjoint/batch`, `daffiz_warning_*`) | `R/conditions.R` + call sites | `conditions.R` is kept; every condition in 0.1.0 has a `daffiz_*` class | H |

## Testing and visualisation

| Feature | Old code | Re-add on the core | Pri |
|---|---|---|---|
| `expect_dt_equal()`, with bounded column/row/cell failure text | `R/expectations.R` | Returns in 0.1.0 as `expect_table_equal()`, with simpler failure text | H |
| Difference map: `plot_data()`, `plot_diff()`, `plot()`. Rows in source order, contiguous binning, Okabe-Ito colours, ASCII tile marks | `R/plot.R` | Superseded by `diff_report()`, whose row diagram and per-measure table answer the same question (where do the tables differ, and how much) in text, with no graphics device, colour or ggplot dependency. Do not re-add unless asked; the code stays at `draft-0.1.0:R/plot.R` | — |

Lessons from the draft's plot, should it ever return:

- Never rank the rows; source order is what makes a band of failures visible.
- Bin contiguous rows rather than dropping any.
- Use ASCII marks, because a delta glyph breaks `pdf()` under `R CMD check`.
- Guard `plot()`'s positional `y` argument.
- Open a device in tests.

## Ideas from diffly

[diffly](https://github.com/Quantco/diffly) (Quantco, BSD-3) compares polars
data frames on a primary key. `diff_report()` follows its summary; these are
the other ideas worth taking from it, read from `diffly/comparison.py`,
`_conditions.py`, `summary.py` and `metrics/`. Each is weighed against the
load-bearing details in `.claude/CLAUDE.md`, not copied as is.

| Idea | In diffly | On the daffiz core | Pri |
|---|---|---|---|
| Report in the failure text of `expect_table_equal()` | `diffly.testing.assert_frame_equal()` prints the summary on failure | Append `format(diff_report(d, "expected", "actual"), unicode = FALSE)` to the failure message, bounded by `top_k`/`sample_k` | H |
| Relative tolerance per measure | `abs_tol`, `rel_tol`, each a number or a mapping per column; `is_close()`, so equality first and NaN equal to NaN | `rel_tol` is done (one value). Per measure is the `rel_tol` row above. diffly's defaults differ between comparing (`rel_tol = 1e-9`) and its test helper (polars' `1e-5`/`1e-8`); `expect_table_equal()` now defaults to `rel_tol = sqrt(.Machine$double.eps)`, testthat's default, rather than polars' `1e-5`, which passes 9 units on a million; `diff_table()` stays exact | M |
| Change counts per measure as data | `change_counts(col, include_sample_primary_key=)`: (old, new, count, example key), most frequent first | Export what `top_changes()` in `R/report.R` computes, uncapped, as `diff_changes(d, measure = NULL)` | M |
| Non-numeric measures | Every non-key column is compared: text, booleans, categoricals (as text when their levels differ), lists element by element, structs field by field | daffiz compares numeric measures only; other columns become the identity or are ignored. Text, logical and Date measures would need a typed value column per type in the melt (or one melt per type, `rbindlist()`-ed). Must be opt-in (`measures = "all"` or by name): with `by = NULL` those columns are the identity today. Change counts matter most for text | M |
| Time tolerance | `abs_tol_temporal`, a `timedelta` for Date/Datetime pairs | Comes with non-numeric measures: a Date or POSIXct measure compared within a tolerance in days or seconds. Keys stay exact | L |
| Row accessors with a column subset | `joined()`, `joined_equal(*cols)`, `joined_unequal(*cols)`, `left_only()`, `right_only()` | The `original_rows()` / `x_only()` / `y_only()` row above; a `measures` subset is `d[metric %in% m & status == "changed"]` grouped back to rows | H (as above) |
| Extra change statistics | `ChangeMetric`s: mean, median, min, max, std, a quantile of `right - left`, the mean relative difference; selectable columns in the summary | Add the fixed few that are cheap (median, mean relative diff) to `diff_summary()` before any plug-in API. Filter on the source values, as the magnitudes do | L |
| Per-table statistics | `DataMetric`s, e.g. the fraction of nulls on each side | `summarize_dt()` already profiles columns; show NA counts of `x` and `y` per measure in the report | L |
| Hide sensitive values | `hidden_columns`: counts only, no values or example keys | A `hide = ` argument of `diff_report()`; refuse hiding a key column while `sample_k > 0` or examples are shown, as diffly does | L |
| Structured export of the report | `Summary.to_json()` | The report is already a list; document its fields, and add `jsonlite` to Suggests only if asked | L |
| Smaller report | `slim = True` drops the header; `show_perfect_column_matches` lists measures that match | Arguments of `format.daffiz_report()` | L |
| Command line | `diffly left.parquet right.parquet --primary-key id` | An `exec/` script (Rapp) reading CSV, or parquet through `arrow` in Suggests | L |
| Types checked apart from values | `equal(check_dtypes = False)` | Covered by the lossless-widening row above for `mode = "equal"`; benchmark mode casts | — |
| Comparing without a key by sorting both tables | `equal()` sorts both frames by every column when there is no primary key | Covered: `row_key = "sorted"` | — |

## Documentation and infrastructure

| Item | Status |
|---|---|
| Vignette `comparing-tables.Rmd` | Dropped in Phase 1; rewrite once the API settles after 0.1.0 (M) |
| pkgdown reference index and site checks (`review.md` P1 #7) | Revisit with the vignette (L) |
| `benchmarks/` | These measure the draft's pipeline. Keep the harness and re-run it against the core after 0.1.0 |
| Open items in `review.md` against the draft | NA/NaN → now an option (`nan_is_na`, default `TRUE`), so the review's "always different" is available as `FALSE`. Trace-column collisions → solved by column-name normalization. Unsupported identity shapes → covered by `CAST_TYPES`. Pairing opt-in → dropped (see above). Plot accessibility claim and plot measure order → moot, the plot is superseded by `diff_report()` |
