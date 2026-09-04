# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`daffiz` is an early-stage R package building a more complete and flexible
data.frame/data.table comparison tool. Only a first draft exists; nothing is
API-stable and everything in `R/` is open to being rewritten.

The design documents that used to live in `docs/` (design-plan, roadmap,
state-of-the-art, features) have been removed; `docs/` is now the generated
pkgdown site and is `.Rbuildignore`d. Design rationale now lives where it is
enforced: in the comments above each stage in `R/`, in `NEWS.md`, and in the
"Load-bearing details" list below. Comments in this codebase carry reasoning
that is not recoverable from the code — read them before rewriting a stage.

## Current state

Phases 0 through 3 of the roadmap are implemented. Version 0.1.0 is the V1
release boundary: `R CMD check --as-cran` reports no errors or warnings (only
the expected "New submission" note), with 140 test cases and 458 passing
expectations. The canonical melted comparison table is correct and traceable to
both inputs, and its column, row, and original-row views reconcile with the
cell records.

`compare_dt()` returns a `daffiz_comparison` object. The V1 public surface is
`compare_dt()`, `daffiz_row_number()`, cell/column/row accessors, original-row
recovery, side-only rows, duplicate diagnostics, `summary()`/`print()`/`plot()`,
`all.equal()`, `expect_dt_equal()`, and `plot_data()`/`plot_diff()`.

Do not claim the package "checks cleanly" without re-running
`R CMD check --as-cran` on a fresh tarball; that claim was wrong once already.

## Commands

Standard `devtools` workflow, run from the package root:

```r
devtools::load_all()      # load without installing
devtools::document()      # regenerate NAMESPACE + man/ from roxygen
devtools::test()          # run testthat suite (once tests/ exists)
devtools::check()         # full R CMD check
```

Run a single test file or a single test:

```r
devtools::test(filter = "compare")            # runs tests/testthat/test-compare*.R
testthat::test_file("tests/testthat/test-compare.R")
```

From the shell:

```sh
R -q -e 'devtools::check()'
R CMD build . && R CMD check daffiz_*.tar.gz
```

## Architecture

`compare_dt()` runs the design plan's section 11 pipeline. Each stage lives in
its own file and is independently testable:

| File | Responsibility |
|---|---|
| `R/daffiz-package.R` | Reserved column names, `.match_type` factor levels, `data.table` awareness |
| `R/conditions.R` | Subclassed conditions (`daffiz_error_*`, `daffiz_warning_*`) and message formatting |
| `R/preflight.R` | Gates 1–4, 6–7, 9 — see design plan §4.1 |
| `R/resolve-columns.R` | Role resolution and gates 5, 8; `daffiz_row_number()` |
| `R/duplicates.R` | Duplicate detection and the `pair`/`report`/`error` policies |
| `R/batching.R` | Transparent measure batching during melt/join construction |
| `R/compare-dt.R` | Entry point, snapshots, melt/join, `classify_cells()` |
| `R/comparison.R` | The S3 object and its accessors |
| `R/accessors-cells.R` | Filtered auditable cell records |
| `R/accessors-columns.R` | Per-measure summaries and ranking |
| `R/accessors-rows.R` | Per-record summaries and original-row recovery |
| `R/key-profile.R` | Identity profiles for unmatched rows |
| `R/report.R` | Bounded structured and printed reports |
| `R/expectations.R` | The testthat regression expectation |
| `R/plot.R` | Difference-map data and the optional `ggplot2` view |

Load-bearing details, each covered by a test:

- **Gate order matters.** A column-set mismatch or a type mismatch must be
  reported *before* duplicate detection or melting, because it usually means
  the output contract changed and a partial numeric comparison would hide that.
- **Working copies are deep copies.** A column subset of a `data.table` shares
  its column vectors, so `setorderv()` during duplicate pairing would otherwise
  reorder the snapshots by reference and break `.row_x`/`.row_y`.
- **`.row_x`/`.row_y` are data, not join keys.** The join key is
  `c(by, ".occurrence", ".metric")`, so rows at different original positions
  still align.
- **Classification order is the spec.** `classify_cells()` follows design plan
  §6.2 exactly; each rule may assume the earlier ones did not fire, so the
  tolerance comparison only ever sees two finite values.
- **Default measures are *unclassed* doubles in _both_ inputs.** `is.double()`
  alone is `TRUE` for `Date` and `POSIXct`, which are identity columns here.
  Reading only `x` made role inference asymmetric: the same two tables errored
  or compared depending on argument order. A column that is integer on one side
  and double on the other is a "numeric type skew" and gets its own hint from
  whichever gate fires first (6 or 8).
- **Identity columns are allow-listed by type, not tested with `is.atomic()`.**
  `is.atomic()` is `TRUE` for `complex` and `raw`, which data.table cannot join
  on. See `daffiz_joinable_types`.
- **`column_signature()` carries `levels` and `units`, but not `tzone`.** The
  criterion is whether the attribute changes what an equal underlying value
  means. `units` rescales a difftime; `tzone` does not move an instant.
- **Role names must be unique.** A repeated `compare=` name melts the measure
  twice and the join goes cartesian, silently multiplying every count.
- **Magnitude statistics filter on the source values, not on `.diff`.** Two
  finite operands can overflow to `Inf`; filtering the derived difference threw
  that cell away and reported `max_abs_diff = 0` for the largest difference in
  the table. See `is_finite_pair()` and `scaled_rmse()` in `R/comparison.R`.
- **Working tables are shallow subsets; only duplicate pairing deep-copies.**
  `setorderv()` is the one operation that writes through shared column vectors,
  so `apply_duplicate_policy()` copies in its `"pair"` branch. Adding a column
  or replacing a promoted integer measure only swaps a pointer, which is safe.
- **Reporting uses `affected_row_summary()`, not `row_summary()`.** The public
  `row_summary()` covers every record by contract; summarizing the all-equal
  majority on the `print()` path cost an order of magnitude more than the
  comparison itself.
- **`expect_dt_equal()` defaults to `duplicate_keys = "error"`.** The
  `compare_dt()` default of `"pair"` sorts each duplicated group by its measure
  values, which minimizes apparent differences, so an assertion would pass on
  tables that disagree row for row.
- **Batching never changes the result model.** `batch=` limits measures in each
  melt/join intermediate, but all equal and non-equal cells remain in the
  canonical table and all accessors are byte-identical across batch sizes.
- **The size projection must be double, not integer.** `nrow() * length()` are
  both integers and overflow at about 2.1e9 cells, turning the guard's own
  comparison into `if (NA)`. Test it with integer literals; doubles hide it.
- **There is exactly one plot.** `R/plot.R` draws an Amelia-style difference
  map and nothing else; measure and record diagnostics belong to the data
  accessors. Rows stay in original source order — ranking them destroys the
  positional band that is the whole point of the map — and truncation bins
  contiguous rows rather than selecting the worst ones, so no row is dropped.
- **Tile marks are ASCII.** A delta glyph raises "conversion failure in
  mbcsToSbcs" on the default `pdf()` device, which is what `R CMD check`
  renders examples to. The marks are a required redundant encoding, not
  decoration: fill alone must never carry the classification.
- **`plot()`'s `y` argument is a trap, and is now guarded.** It belongs to the
  generic and is unused, so an unchecked positional second argument was
  silently swallowed — `plot(cmp, "amount")` drew every measure.

Test fixtures live in `tests/testthat/helper-fixtures.R` (`make_fixture()`,
`simple_pair()`, `classify_one()`, `mixed_comparison()`), written in base R —
the draft's `charlatan`-based generator is gone.

## Conventions

- `data.table` is the engine. Express comparisons as joins and grouped
  operations; avoid row-wise R loops. Copy inputs (`copy()`) rather than
  modifying a caller's table by reference.
- Preserve the canonical melted numeric cell table; build column, row,
  original-row, report, and visualization accessors from it.
- Duplicate identities may use deterministic occurrence pairing by default,
  but the result must report the heuristic and retain both original row indices.
- Invalid calls (missing requested columns, incompatible join types, invalid
  tolerances) should raise clearly — see the design plan.
- `docs/`, `CLAUDE.md`, `.claude/`, `benchmarks/`, `inst/WORDLIST` and any
  stray `Rplots.pdf` are listed in `.Rbuildignore`. A test or example that
  calls `plot()` must open its own device; otherwise it leaves an `Rplots.pdf`
  behind that ships in the tarball and earns a top-level-files NOTE.
