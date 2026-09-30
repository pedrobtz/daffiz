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
| `rel_tol`, and tolerance per measure (`c(.default = 0, amount = 0.01)`) | `R/preflight.R` `resolve_tolerance()`, `R/compare-dt.R` `classify_cells()` | The core has one absolute tolerance | Resolve a named vector over the measures, match it on `metric`, and use `pmax(abs, rel * pmax(abs(x), abs(y)))`. Port the tests in `test-tolerance.R`. | H |
| `.abs_diff`, `.rel_diff` columns | `classify_cells()` | They can be derived from `diff` | Add them together with `rel_tol` | L |
| `exclude=` | `R/preflight.R` gate 3 | Not in the core | Drop the columns right after `canonical_names()`, before the column step; warn on unknown names | M |
| Role inference: shared unclassed doubles, and hints for numeric type skew | `R/resolve-columns.R`, `R/preflight.R` | Replaced by casting (`CAST_RULES`) and selecting measures by type | Probably never; casting covers the int/double skew | — |
| Checks on identity attributes (factor `levels`, difftime `units`) | `R/preflight.R` `column_signature()` | Factor and difftime are not cast target types, so `x` refuses them and `normalize_dt()` converts them | Only if difftime/units become cast types | L |
| Allow-list of identity types (reject complex/raw/list) | `daffiz_joinable_types` | `CAST_TYPES` is itself an allow-list | — (covered) | — |
| Zero-overlap gate (`disjoint_keys = "error"/"warn"`) | `R/preflight.R` `preflight_alignment()` | Not in the core | 0.1.0 has the error (`plan.md`); the `"warn"` opt-out stays here | M |
| Projected-size warning (`options(daffiz.max_cells)`) | `R/compare-dt.R` `warn_projected_size()` | Not in the core | `merge_dry_run()` gives exact row counts cheaply; multiply by the number of measures, in doubles | M |
| Batching by measure (`batch=`) | `R/batching.R` | An optimisation, not a feature; re-benchmark on the new pipeline first | Loop `melt → merge → classify` over groups of measures, then `rbindlist()` | L |
| Lossless type widening in `mode = "equal"` (integer ↔ double compared as double) | The draft promoted integer measures (`R/compare-dt.R`) | `"equal"` mode requires identical types, since it has no source of truth to cast to | Widen both sides to double for the integer/numeric pair only; never narrow | M |
| Report labels (`x_name`/`y_name`) | `capture_label()` | No report to label yet | Needed together with `print`/`summary` | L |

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
| `summary()`, `print()`, `format()` methods with bounded sections | `R/report.R`, `R/comparison.R` | Needs a class on the result. Keep the lesson from the draft: summarise only the affected rows on the print path (`affected_row_summary()`) | M |
| `all.equal.daffiz_comparison()` | `R/comparison.R` | It existed only because of the cache environment; not needed without one | — |
| Structured conditions for every gate (`daffiz_error_input/columns/roles/types/tolerance/duplicates/disjoint/batch`, `daffiz_warning_*`) | `R/conditions.R` + call sites | `conditions.R` is kept; every condition in 0.1.0 has a `daffiz_*` class | H |

## Testing and visualisation

| Feature | Old code | Re-add on the core | Pri |
|---|---|---|---|
| `expect_dt_equal()`, with bounded column/row/cell failure text | `R/expectations.R` | Returns in 0.1.0 as `expect_table_equal()`, with simpler failure text | H |
| Difference map: `plot_data()`, `plot_diff()`, `plot()`. Rows in source order, contiguous binning, Okabe-Ito colours, ASCII tile marks | `R/plot.R` | Build from `row_id_x`, `metric` and `status`. Keep the lessons listed below | L |

Lessons from the draft's plot to keep if it returns:

- Never rank the rows; source order is what makes a band of failures visible.
- Bin contiguous rows rather than dropping any.
- Use ASCII marks, because a delta glyph breaks `pdf()` under `R CMD check`.
- Guard `plot()`'s positional `y` argument.
- Open a device in tests.

## Documentation and infrastructure

| Item | Status |
|---|---|
| Vignette `comparing-tables.Rmd` | Dropped in Phase 1; rewrite once the API settles after 0.1.0 (M) |
| pkgdown reference index and site checks (`review.md` P1 #7) | Revisit with the vignette (L) |
| `benchmarks/` | These measure the draft's pipeline. Keep the harness and re-run it against the core after 0.1.0 |
| Open items in `review.md` against the draft | NA/NaN → now an option (`nan_is_na`, default `TRUE`), so the review's "always different" is available as `FALSE`. Trace-column collisions → solved by column-name normalization. Unsupported identity shapes → covered by `CAST_TYPES`. Pairing opt-in → dropped (see above). Plot accessibility claim → only relevant if the plot returns |
